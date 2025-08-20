# ContentHub Marketplace - Creator/Buyer Platform

## Overview
A digital content marketplace where creators can sell digital products (courses, templates, graphics, etc.) and buyers can discover and purchase content.

## Tech Stack
- **Backend**: Node.js with Express.js
- **Frontend**: React 18 with TypeScript
- **Database**: PostgreSQL (Render or Neon)
- **Authentication**: Auth0 with marketplace roles
- **Payments**: Stripe with marketplace features (Connect)
- **Storage**: AWS S3 or Render static files
- **Deployment**: Render (staging + production)

## Features

### Core Functionality
- **Content Marketplace**: Browse, search, and filter digital products
- **Creator Dashboard**: Upload products, manage sales, analytics
- **Buyer Experience**: Purchase, download, reviews and ratings
- **Payment Processing**: Stripe Connect for marketplace payments
- **Content Delivery**: Secure download links with expiration

### User Roles
- **Creator**: Can upload and sell digital products
- **Buyer**: Can purchase and download content
- **Admin**: Platform management, dispute resolution
- **Moderator**: Content approval, community management

### Product Categories
- **Digital Courses**: Video courses, tutorials
- **Design Assets**: Graphics, templates, icons
- **Software Tools**: Plugins, extensions, apps
- **Documents**: Ebooks, guides, reports

### Revenue Model
- **Commission**: 10% platform fee on all sales
- **Creator Plus ($29/month)**: Reduced 5% fee, analytics, promotion
- **Creator Pro ($99/month)**: 3% fee, priority support, white-label

## Database Schema

### Users Table
```sql
CREATE TABLE users (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  email VARCHAR(255) UNIQUE NOT NULL,
  auth0_id VARCHAR(255) UNIQUE,
  username VARCHAR(100) UNIQUE NOT NULL,
  role VARCHAR(50) DEFAULT 'buyer',
  stripe_account_id VARCHAR(255), -- For creators
  stripe_customer_id VARCHAR(255), -- For buyers
  subscription_tier VARCHAR(50) DEFAULT 'free',
  created_at TIMESTAMP DEFAULT NOW()
);
```

### Products Table
```sql
CREATE TABLE products (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  creator_id UUID REFERENCES users(id),
  title VARCHAR(255) NOT NULL,
  description TEXT,
  category VARCHAR(100),
  price_cents INTEGER NOT NULL,
  file_url VARCHAR(500),
  preview_images TEXT[], -- Array of image URLs
  tags VARCHAR(100)[],
  status VARCHAR(50) DEFAULT 'pending', -- pending, approved, rejected
  downloads_count INTEGER DEFAULT 0,
  rating_average DECIMAL(3,2) DEFAULT 0,
  rating_count INTEGER DEFAULT 0,
  created_at TIMESTAMP DEFAULT NOW()
);
```

### Orders Table
```sql
CREATE TABLE orders (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  buyer_id UUID REFERENCES users(id),
  product_id UUID REFERENCES products(id),
  amount_cents INTEGER NOT NULL,
  platform_fee_cents INTEGER NOT NULL,
  creator_amount_cents INTEGER NOT NULL,
  stripe_payment_intent_id VARCHAR(255),
  status VARCHAR(50) DEFAULT 'pending',
  download_expires_at TIMESTAMP,
  created_at TIMESTAMP DEFAULT NOW()
);
```

### Reviews Table
```sql
CREATE TABLE reviews (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  order_id UUID REFERENCES orders(id),
  product_id UUID REFERENCES products(id),
  buyer_id UUID REFERENCES users(id),
  rating INTEGER CHECK (rating >= 1 AND rating <= 5),
  comment TEXT,
  created_at TIMESTAMP DEFAULT NOW()
);
```

## API Endpoints

### Authentication
- `POST /api/auth/callback` - Auth0 callback
- `GET /api/auth/user` - Get current user
- `POST /api/auth/logout` - Logout user

### Products
- `GET /api/products` - Browse marketplace products
- `GET /api/products/:id` - Get product details
- `POST /api/products` - Create new product (creators only)
- `PUT /api/products/:id` - Update product
- `DELETE /api/products/:id` - Delete product

### Orders & Purchases
- `POST /api/orders/create-checkout` - Create Stripe checkout
- `GET /api/orders` - Get user's orders
- `GET /api/orders/:id/download` - Download purchased content
- `POST /api/orders/:id/review` - Leave product review

### Creator Dashboard
- `GET /api/creator/dashboard` - Sales analytics
- `GET /api/creator/products` - Creator's products
- `GET /api/creator/earnings` - Revenue breakdown
- `POST /api/creator/payout` - Request payout

### Admin
- `GET /api/admin/products/pending` - Products awaiting approval
- `PUT /api/admin/products/:id/approve` - Approve product
- `PUT /api/admin/products/:id/reject` - Reject product

## Frontend Components

### Public Pages
- `/` - Homepage with featured products
- `/browse` - Product marketplace with filters
- `/product/:id` - Product detail page
- `/creator/:username` - Creator profile page

### User Dashboard
- `/dashboard` - User dashboard (buyer or creator)
- `/dashboard/purchases` - Purchased products
- `/dashboard/products` - Creator's products (creators only)
- `/dashboard/analytics` - Sales analytics (creators only)
- `/dashboard/earnings` - Revenue tracking (creators only)

### Components
- `ProductCard` - Product listing component
- `ProductDetail` - Detailed product view
- `CreatorProfile` - Creator information
- `CheckoutModal` - Stripe checkout integration
- `DownloadManager` - Secure download handling
- `ReviewSection` - Product reviews and ratings

## Payment Flow

### Purchase Process
1. Buyer clicks "Purchase" on product
2. Stripe Checkout created with marketplace configuration
3. Payment processed through Stripe Connect
4. Platform fee automatically deducted
5. Creator receives payment (minus fees)
6. Buyer gets secure download link

### Creator Onboarding
1. Creator signs up and verifies identity
2. Stripe Connect account created
3. Tax information collected
4. Bank account connected for payouts

## Content Security

### File Protection
- Signed URLs with expiration (24-48 hours)
- Download limits per purchase
- User authentication required
- No direct file access

### Digital Rights Management
- Watermarking for premium content
- License tracking and enforcement
- Piracy detection and reporting

## Revenue Tracking

### Creator Analytics
- Sales volume and trends
- Geographic breakdown
- Product performance metrics
- Conversion rates

### Platform Metrics
- Total marketplace volume
- Creator acquisition and retention
- Product category performance
- User engagement metrics

## Deployment Configuration

### Environment Variables
```bash
# Database
DATABASE_URL=postgresql://...

# Auth0
NEXT_PUBLIC_AUTH0_DOMAIN=contenthub.auth0.com
NEXT_PUBLIC_AUTH0_CLIENT_ID=...
AUTH0_CLIENT_SECRET=...

# Stripe
NEXT_PUBLIC_STRIPE_PUBLISHABLE_KEY=pk_test_...
STRIPE_SECRET_KEY=sk_test_...
STRIPE_WEBHOOK_SECRET=whsec_...

# File Storage
AWS_ACCESS_KEY_ID=...
AWS_SECRET_ACCESS_KEY=...
AWS_S3_BUCKET=contenthub-files

# App
NEXT_PUBLIC_APP_URL=https://contenthub-marketplace.onrender.com
API_URL=https://contenthub-api.onrender.com
```

### Render Services
- **Frontend**: `contenthub-frontend` (static site)
- **Backend**: `contenthub-api` (web service)
- **Database**: `contenthub-db` (PostgreSQL)

## Success Metrics
- Monthly marketplace volume (GMV)
- Creator sign-ups and active creators
- Product uploads and approval rates
- Customer acquisition cost (CAC)
- Average order value (AOV)
- Creator retention and satisfaction

## Development Timeline
- **Setup & Scaffolding**: 15 minutes
- **Backend API Development**: 4-5 hours
- **Frontend Marketplace**: 4-5 hours
- **Payment Integration**: 2-3 hours
- **File Upload/Download**: 2 hours
- **Testing & Deployment**: 1 hour

**Total Development Time**: 13-16 hours from scaffold to production