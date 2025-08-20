const express = require('express');
const router = express.Router();
const { auth, requireRole } = require('../middleware/auth');
const { upload } = require('../middleware/upload');
const productService = require('../services/productService');
const { validateProduct } = require('../validators/productValidator');

/**
 * GET /api/products
 * Browse marketplace products with filtering and pagination
 */
router.get('/', async (req, res) => {
  try {
    const {
      category,
      minPrice,
      maxPrice,
      tags,
      search,
      sortBy = 'created_at',
      sortOrder = 'desc',
      page = 1,
      limit = 20
    } = req.query;

    const filters = {
      category,
      minPrice: minPrice ? parseInt(minPrice) : undefined,
      maxPrice: maxPrice ? parseInt(maxPrice) : undefined,
      tags: tags ? tags.split(',') : undefined,
      search,
      status: 'approved' // Only show approved products in marketplace
    };

    const products = await productService.getProducts(filters, {
      sortBy,
      sortOrder,
      page: parseInt(page),
      limit: parseInt(limit)
    });

    res.json({
      products: products.data,
      pagination: {
        page: products.page,
        limit: products.limit,
        totalPages: products.totalPages,
        totalCount: products.totalCount
      }
    });
  } catch (error) {
    console.error('Error fetching products:', error);
    res.status(500).json({ error: 'Failed to fetch products' });
  }
});

/**
 * GET /api/products/:id
 * Get detailed product information
 */
router.get('/:id', async (req, res) => {
  try {
    const { id } = req.params;
    const product = await productService.getProductById(id, {
      includeCreator: true,
      includeReviews: true,
      includeStats: true
    });

    if (!product) {
      return res.status(404).json({ error: 'Product not found' });
    }

    // Only show approved products to non-creators
    if (product.status !== 'approved' && 
        (!req.user || req.user.id !== product.creator_id)) {
      return res.status(404).json({ error: 'Product not found' });
    }

    res.json({ product });
  } catch (error) {
    console.error('Error fetching product:', error);
    res.status(500).json({ error: 'Failed to fetch product' });
  }
});

/**
 * POST /api/products
 * Create new product (creators only)
 */
router.post('/', 
  auth, 
  requireRole(['creator', 'admin']),
  upload.fields([
    { name: 'productFile', maxCount: 1 },
    { name: 'previewImages', maxCount: 5 }
  ]),
  validateProduct,
  async (req, res) => {
    try {
      const {
        title,
        description,
        category,
        price,
        tags
      } = req.body;

      // Validate required files
      if (!req.files?.productFile?.[0]) {
        return res.status(400).json({ error: 'Product file is required' });
      }

      const productData = {
        creator_id: req.user.id,
        title,
        description,
        category,
        price_cents: Math.round(parseFloat(price) * 100),
        tags: tags ? tags.split(',').map(tag => tag.trim()) : [],
        file_url: req.files.productFile[0].location, // S3 URL
        preview_images: req.files.previewImages?.map(file => file.location) || [],
        status: 'pending' // Requires admin approval
      };

      const product = await productService.createProduct(productData);

      // Send notification to admins for approval
      await notificationService.notifyAdmins('product_pending_approval', {
        productId: product.id,
        creatorName: req.user.name,
        productTitle: title
      });

      res.status(201).json({ 
        product,
        message: 'Product created successfully. It will be reviewed before appearing in the marketplace.'
      });
    } catch (error) {
      console.error('Error creating product:', error);
      res.status(500).json({ error: 'Failed to create product' });
    }
  }
);

/**
 * PUT /api/products/:id
 * Update existing product
 */
router.put('/:id',
  auth,
  requireRole(['creator', 'admin']),
  upload.fields([
    { name: 'productFile', maxCount: 1 },
    { name: 'previewImages', maxCount: 5 }
  ]),
  async (req, res) => {
    try {
      const { id } = req.params;
      const product = await productService.getProductById(id);

      if (!product) {
        return res.status(404).json({ error: 'Product not found' });
      }

      // Only creator or admin can update
      if (product.creator_id !== req.user.id && req.user.role !== 'admin') {
        return res.status(403).json({ error: 'Access denied' });
      }

      const updates = {
        ...req.body
      };

      // Handle file updates
      if (req.files?.productFile?.[0]) {
        updates.file_url = req.files.productFile[0].location;
        // Delete old file from S3
        await fileService.deleteFile(product.file_url);
      }

      if (req.files?.previewImages) {
        updates.preview_images = req.files.previewImages.map(file => file.location);
        // Delete old preview images
        if (product.preview_images) {
          await Promise.all(
            product.preview_images.map(url => fileService.deleteFile(url))
          );
        }
      }

      // Convert price to cents if provided
      if (updates.price) {
        updates.price_cents = Math.round(parseFloat(updates.price) * 100);
        delete updates.price;
      }

      // Handle tags
      if (updates.tags && typeof updates.tags === 'string') {
        updates.tags = updates.tags.split(',').map(tag => tag.trim());
      }

      // If content was updated, require re-approval
      if (req.files?.productFile || updates.title || updates.description) {
        updates.status = 'pending';
      }

      const updatedProduct = await productService.updateProduct(id, updates);

      res.json({ 
        product: updatedProduct,
        message: updates.status === 'pending' ? 
          'Product updated and submitted for re-approval.' : 
          'Product updated successfully.'
      });
    } catch (error) {
      console.error('Error updating product:', error);
      res.status(500).json({ error: 'Failed to update product' });
    }
  }
);

/**
 * DELETE /api/products/:id
 * Delete product
 */
router.delete('/:id', auth, async (req, res) => {
  try {
    const { id } = req.params;
    const product = await productService.getProductById(id);

    if (!product) {
      return res.status(404).json({ error: 'Product not found' });
    }

    // Only creator or admin can delete
    if (product.creator_id !== req.user.id && req.user.role !== 'admin') {
      return res.status(403).json({ error: 'Access denied' });
    }

    // Check if product has been purchased
    const orderCount = await productService.getProductOrderCount(id);
    if (orderCount > 0) {
      return res.status(400).json({ 
        error: 'Cannot delete product that has been purchased. Contact support.' 
      });
    }

    // Delete associated files
    if (product.file_url) {
      await fileService.deleteFile(product.file_url);
    }
    if (product.preview_images) {
      await Promise.all(
        product.preview_images.map(url => fileService.deleteFile(url))
      );
    }

    await productService.deleteProduct(id);

    res.json({ message: 'Product deleted successfully' });
  } catch (error) {
    console.error('Error deleting product:', error);
    res.status(500).json({ error: 'Failed to delete product' });
  }
});

/**
 * GET /api/products/:id/reviews
 * Get product reviews with pagination
 */
router.get('/:id/reviews', async (req, res) => {
  try {
    const { id } = req.params;
    const { page = 1, limit = 10 } = req.query;

    const reviews = await productService.getProductReviews(id, {
      page: parseInt(page),
      limit: parseInt(limit)
    });

    res.json(reviews);
  } catch (error) {
    console.error('Error fetching reviews:', error);
    res.status(500).json({ error: 'Failed to fetch reviews' });
  }
});

/**
 * POST /api/products/:id/report
 * Report product for violation
 */
router.post('/:id/report', auth, async (req, res) => {
  try {
    const { id } = req.params;
    const { reason, description } = req.body;

    if (!reason) {
      return res.status(400).json({ error: 'Report reason is required' });
    }

    await productService.reportProduct({
      product_id: id,
      reporter_id: req.user.id,
      reason,
      description
    });

    // Notify moderators
    await notificationService.notifyModerators('product_reported', {
      productId: id,
      reason,
      reporterName: req.user.name
    });

    res.json({ message: 'Product reported successfully' });
  } catch (error) {
    console.error('Error reporting product:', error);
    res.status(500).json({ error: 'Failed to report product' });
  }
});

module.exports = router;