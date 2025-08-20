#!/usr/bin/env node

/**
 * Stripe Setup Script
 * Complete Stripe configuration for staging (test mode) and production (live mode)
 */

const stripe = require('stripe');
const fs = require('fs').promises;
const path = require('path');

// Configuration
const PROJECT_NAME = process.argv[2] || 'myapp';
const FRONTEND_URL_PROD = process.argv[3] || `https://${PROJECT_NAME}.onrender.com`;
const FRONTEND_URL_STAGING = process.argv[4] || `https://${PROJECT_NAME}-staging.onrender.com`;
const API_URL_PROD = process.argv[5] || `https://${PROJECT_NAME}-api.onrender.com`;
const API_URL_STAGING = process.argv[6] || `https://${PROJECT_NAME}-api-staging.onrender.com`;

// Stripe API keys (should be set as environment variables)
const STRIPE_SECRET_KEY_LIVE = process.env.STRIPE_SECRET_KEY_LIVE;
const STRIPE_SECRET_KEY_TEST = process.env.STRIPE_SECRET_KEY_TEST;

if (!STRIPE_SECRET_KEY_TEST) {
    console.error('❌ STRIPE_SECRET_KEY_TEST environment variable is required');
    process.exit(1);
}

// Initialize Stripe clients
const stripeTest = stripe(STRIPE_SECRET_KEY_TEST);
const stripeLive = STRIPE_SECRET_KEY_LIVE ? stripe(STRIPE_SECRET_KEY_LIVE) : null;

// Pricing configurations
const PRICING_TIERS = {
    simple: [
        {
            name: 'Free',
            price: 0,
            interval: null,
            features: [
                'Up to 3 users',
                'Basic features',
                'Community support'
            ],
            metadata: {
                tier: 'free',
                max_users: '3'
            }
        },
        {
            name: 'Pro',
            price: 2900, // $29.00 in cents
            interval: 'month',
            features: [
                'Unlimited users',
                'All features',
                'Priority support',
                'Advanced analytics'
            ],
            metadata: {
                tier: 'pro',
                max_users: 'unlimited'
            }
        }
    ],
    tiered: [
        {
            name: 'Free',
            price: 0,
            interval: null,
            features: [
                'Up to 3 users',
                'Basic features',
                'Community support'
            ],
            metadata: {
                tier: 'free',
                max_users: '3'
            }
        },
        {
            name: 'Starter',
            price: 900, // $9.00
            interval: 'month',
            features: [
                'Up to 10 users',
                'Core features',
                'Email support'
            ],
            metadata: {
                tier: 'starter',
                max_users: '10'
            }
        },
        {
            name: 'Professional',
            price: 2900, // $29.00
            interval: 'month',
            features: [
                'Up to 50 users',
                'All features',
                'Priority support',
                'Advanced analytics'
            ],
            metadata: {
                tier: 'professional',
                max_users: '50'
            }
        },
        {
            name: 'Enterprise',
            price: 9900, // $99.00
            interval: 'month',
            features: [
                'Unlimited users',
                'All features',
                'Dedicated support',
                'Custom integrations',
                'SLA'
            ],
            metadata: {
                tier: 'enterprise',
                max_users: 'unlimited',
                custom: 'true'
            }
        }
    ],
    usage: [
        {
            name: 'Pay As You Go',
            price: 100, // $1.00 per unit
            interval: null,
            metered: true,
            usage_type: 'licensed',
            aggregate_usage: 'sum',
            features: [
                'Pay only for what you use',
                'No minimum commitment',
                'Automatic scaling'
            ],
            metadata: {
                tier: 'usage_based',
                billing: 'metered'
            }
        }
    ],
    seat: [
        {
            name: 'Per User',
            price: 1000, // $10.00 per user
            interval: 'month',
            per_seat: true,
            features: [
                'Per user pricing',
                'Add or remove users anytime',
                'All features included'
            ],
            metadata: {
                tier: 'seat_based',
                billing: 'per_seat'
            }
        }
    ]
};

/**
 * Create products and prices
 */
async function createProductsAndPrices(stripeClient, pricingModel = 'simple', isTest = true) {
    const tiers = PRICING_TIERS[pricingModel] || PRICING_TIERS.simple;
    const environment = isTest ? 'test' : 'live';
    const products = [];
    
    console.log(`\n📦 Creating products for ${environment} environment (${pricingModel} model)...`);
    
    for (const tier of tiers) {
        // Skip free tier for product creation (no payment needed)
        if (tier.price === 0) {
            console.log(`  ⏭️  Skipping free tier (no product needed)`);
            continue;
        }
        
        try {
            // Create product
            const product = await stripeClient.products.create({
                name: `${PROJECT_NAME} - ${tier.name}`,
                description: tier.features.join(', '),
                metadata: {
                    ...tier.metadata,
                    environment,
                    project: PROJECT_NAME
                }
            });
            
            console.log(`  ✅ Created product: ${product.name} (${product.id})`);
            
            // Create price
            const priceData = {
                product: product.id,
                currency: 'usd',
                metadata: {
                    ...tier.metadata,
                    environment
                }
            };
            
            if (tier.metered) {
                // Usage-based pricing
                priceData.recurring = {
                    interval: 'month',
                    usage_type: tier.usage_type,
                    aggregate_usage: tier.aggregate_usage
                };
                priceData.billing_scheme = 'per_unit';
                priceData.unit_amount = tier.price;
            } else if (tier.per_seat) {
                // Seat-based pricing
                priceData.recurring = {
                    interval: tier.interval || 'month'
                };
                priceData.billing_scheme = 'per_unit';
                priceData.unit_amount = tier.price;
            } else if (tier.interval) {
                // Regular subscription pricing
                priceData.recurring = {
                    interval: tier.interval
                };
                priceData.unit_amount = tier.price;
            } else {
                // One-time payment
                priceData.unit_amount = tier.price;
            }
            
            const price = await stripeClient.prices.create(priceData);
            
            console.log(`  ✅ Created price: ${price.id} ($${(price.unit_amount / 100).toFixed(2)}/${price.recurring?.interval || 'one-time'})`);
            
            products.push({
                product,
                price,
                tier: tier.name
            });
            
        } catch (error) {
            console.error(`  ❌ Error creating product ${tier.name}:`, error.message);
        }
    }
    
    return products;
}

/**
 * Create webhooks
 */
async function createWebhooks(stripeClient, isTest = true) {
    const environment = isTest ? 'staging' : 'production';
    const webhookUrl = isTest ? 
        `${API_URL_STAGING}/api/webhooks/stripe` : 
        `${API_URL_PROD}/api/webhooks/stripe`;
    
    console.log(`\n🔗 Creating webhook for ${environment}...`);
    
    try {
        const webhook = await stripeClient.webhookEndpoints.create({
            url: webhookUrl,
            enabled_events: [
                'customer.created',
                'customer.updated',
                'customer.deleted',
                'customer.subscription.created',
                'customer.subscription.updated',
                'customer.subscription.deleted',
                'customer.subscription.trial_will_end',
                'invoice.created',
                'invoice.payment_succeeded',
                'invoice.payment_failed',
                'payment_intent.succeeded',
                'payment_intent.payment_failed',
                'checkout.session.completed',
                'checkout.session.expired'
            ],
            metadata: {
                environment,
                project: PROJECT_NAME
            }
        });
        
        console.log(`  ✅ Webhook created: ${webhook.url}`);
        console.log(`  🔑 Webhook secret: ${webhook.secret}`);
        
        return webhook;
        
    } catch (error) {
        console.error(`  ❌ Error creating webhook:`, error.message);
        return null;
    }
}

/**
 * Create customer portal configuration
 */
async function createCustomerPortal(stripeClient, isTest = true) {
    const returnUrl = isTest ? 
        `${FRONTEND_URL_STAGING}/account` : 
        `${FRONTEND_URL_PROD}/account`;
    
    console.log(`\n🚪 Creating customer portal configuration...`);
    
    try {
        const portalConfig = await stripeClient.billingPortal.configurations.create({
            business_profile: {
                headline: `${PROJECT_NAME} Customer Portal`,
                privacy_policy_url: `${returnUrl}/privacy`,
                terms_of_service_url: `${returnUrl}/terms`
            },
            features: {
                customer_update: {
                    enabled: true,
                    allowed_updates: ['email', 'name', 'address', 'phone', 'tax_id']
                },
                invoice_history: {
                    enabled: true
                },
                payment_method_update: {
                    enabled: true
                },
                subscription_cancel: {
                    enabled: true,
                    mode: 'at_period_end',
                    cancellation_reason: {
                        enabled: true,
                        options: [
                            'too_expensive',
                            'missing_features',
                            'switched_service',
                            'unused',
                            'other'
                        ]
                    }
                },
                subscription_pause: {
                    enabled: false
                },
                subscription_update: {
                    enabled: true,
                    default_allowed_updates: ['price', 'quantity'],
                    proration_behavior: 'create_prorations'
                }
            },
            default_return_url: returnUrl,
            metadata: {
                project: PROJECT_NAME,
                environment: isTest ? 'test' : 'live'
            }
        });
        
        console.log(`  ✅ Customer portal configured`);
        console.log(`  🔗 Portal URL: ${returnUrl}`);
        
        return portalConfig;
        
    } catch (error) {
        console.error(`  ❌ Error creating customer portal:`, error.message);
        return null;
    }
}

/**
 * Create checkout session example
 */
async function createCheckoutExample(stripeClient, priceId, isTest = true) {
    const successUrl = isTest ?
        `${FRONTEND_URL_STAGING}/success?session_id={CHECKOUT_SESSION_ID}` :
        `${FRONTEND_URL_PROD}/success?session_id={CHECKOUT_SESSION_ID}`;
    
    const cancelUrl = isTest ?
        `${FRONTEND_URL_STAGING}/pricing` :
        `${FRONTEND_URL_PROD}/pricing`;
    
    console.log(`\n🛒 Creating example checkout session...`);
    
    try {
        const session = await stripeClient.checkout.sessions.create({
            payment_method_types: ['card'],
            line_items: [
                {
                    price: priceId,
                    quantity: 1
                }
            ],
            mode: 'subscription',
            success_url: successUrl,
            cancel_url: cancelUrl,
            allow_promotion_codes: true,
            billing_address_collection: 'auto',
            customer_email: 'test@example.com',
            metadata: {
                project: PROJECT_NAME,
                environment: isTest ? 'test' : 'live'
            }
        });
        
        console.log(`  ✅ Checkout session created`);
        console.log(`  🔗 Checkout URL: ${session.url}`);
        
        return session;
        
    } catch (error) {
        console.error(`  ❌ Error creating checkout session:`, error.message);
        return null;
    }
}

/**
 * Create test customers
 */
async function createTestCustomers(stripeClient) {
    console.log(`\n👥 Creating test customers...`);
    
    const testCustomers = [
        {
            email: `admin@${PROJECT_NAME}.com`,
            name: 'Admin User',
            description: 'Test admin user',
            metadata: {
                role: 'admin',
                auth0_id: 'auth0|admin123'
            }
        },
        {
            email: `user@${PROJECT_NAME}.com`,
            name: 'Test User',
            description: 'Test regular user',
            metadata: {
                role: 'user',
                auth0_id: 'auth0|user123'
            }
        }
    ];
    
    const customers = [];
    
    for (const customerData of testCustomers) {
        try {
            const customer = await stripeClient.customers.create(customerData);
            console.log(`  ✅ Created customer: ${customer.email} (${customer.id})`);
            customers.push(customer);
        } catch (error) {
            console.error(`  ❌ Error creating customer ${customerData.email}:`, error.message);
        }
    }
    
    return customers;
}

/**
 * Generate environment variables file
 */
async function generateEnvFile(testProducts, liveProducts, testWebhook, liveWebhook) {
    console.log(`\n📝 Generating environment variables...`);
    
    const envContent = `# Stripe Environment Variables
# Generated for ${PROJECT_NAME}

# Test Mode (Staging)
STRIPE_PUBLISHABLE_KEY_TEST=${process.env.STRIPE_PUBLISHABLE_KEY_TEST || 'pk_test_YOUR_KEY'}
STRIPE_SECRET_KEY_TEST=${STRIPE_SECRET_KEY_TEST}
STRIPE_WEBHOOK_SECRET_TEST=${testWebhook?.secret || 'whsec_YOUR_WEBHOOK_SECRET'}

# Live Mode (Production)
STRIPE_PUBLISHABLE_KEY_LIVE=${process.env.STRIPE_PUBLISHABLE_KEY_LIVE || 'pk_live_YOUR_KEY'}
STRIPE_SECRET_KEY_LIVE=${STRIPE_SECRET_KEY_LIVE || 'sk_live_YOUR_KEY'}
STRIPE_WEBHOOK_SECRET_LIVE=${liveWebhook?.secret || 'whsec_YOUR_WEBHOOK_SECRET'}

# Price IDs - Test Mode
${testProducts.map(p => `STRIPE_PRICE_${p.tier.toUpperCase().replace(' ', '_')}_TEST=${p.price.id}`).join('\n')}

# Price IDs - Live Mode
${liveProducts.length > 0 ? 
    liveProducts.map(p => `STRIPE_PRICE_${p.tier.toUpperCase().replace(' ', '_')}_LIVE=${p.price.id}`).join('\n') :
    '# Configure live prices after going to production'
}

# Product IDs - Test Mode
${testProducts.map(p => `STRIPE_PRODUCT_${p.tier.toUpperCase().replace(' ', '_')}_TEST=${p.product.id}`).join('\n')}

# Product IDs - Live Mode
${liveProducts.length > 0 ?
    liveProducts.map(p => `STRIPE_PRODUCT_${p.tier.toUpperCase().replace(' ', '_')}_LIVE=${p.product.id}`).join('\n') :
    '# Configure live products after going to production'
}
`;
    
    await fs.writeFile('stripe-env-vars.env', envContent);
    console.log(`  ✅ Environment variables saved to stripe-env-vars.env`);
}

/**
 * Main setup function
 */
async function main() {
    console.log(`🎯 Stripe Setup for ${PROJECT_NAME}`);
    console.log('==================================');
    
    // Get pricing model from user
    const pricingModel = process.argv[7] || 'simple';
    console.log(`Using pricing model: ${pricingModel}`);
    
    // Test mode setup
    console.log('\n📋 Setting up TEST mode...');
    const testProducts = await createProductsAndPrices(stripeTest, pricingModel, true);
    const testWebhook = await createWebhooks(stripeTest, true);
    const testPortal = await createCustomerPortal(stripeTest, true);
    const testCustomers = await createTestCustomers(stripeTest);
    
    if (testProducts.length > 0) {
        await createCheckoutExample(stripeTest, testProducts[0].price.id, true);
    }
    
    // Live mode setup (if key provided)
    let liveProducts = [];
    let liveWebhook = null;
    
    if (stripeLive) {
        console.log('\n📋 Setting up LIVE mode...');
        liveProducts = await createProductsAndPrices(stripeLive, pricingModel, false);
        liveWebhook = await createWebhooks(stripeLive, false);
        await createCustomerPortal(stripeLive, false);
    } else {
        console.log('\n⚠️  Skipping LIVE mode setup (no live key provided)');
    }
    
    // Generate environment variables
    await generateEnvFile(testProducts, liveProducts, testWebhook, liveWebhook);
    
    // Summary
    console.log('\n✅ Stripe setup completed successfully!');
    console.log('\n📋 Next steps:');
    console.log('1. Copy environment variables from stripe-env-vars.env to your .env files');
    console.log('2. Update Render service environment variables with Stripe keys');
    console.log('3. Test the checkout flow with test card: 4242 4242 4242 4242');
    console.log('4. Configure tax settings in Stripe Dashboard if needed');
    console.log('5. Set up invoice templates and email notifications');
    console.log('6. When ready for production, run this script with live keys');
    
    console.log('\n🔑 Important URLs:');
    console.log(`- Test Dashboard: https://dashboard.stripe.com/test`);
    console.log(`- Live Dashboard: https://dashboard.stripe.com`);
    console.log(`- Webhook Testing: stripe listen --forward-to ${API_URL_STAGING}/api/webhooks/stripe`);
}

// Run the script
main().catch(console.error);