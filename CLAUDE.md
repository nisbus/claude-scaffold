# Claude Code Project Scaffolding Instructions

## Purpose
This document provides instructions for Claude Code to scaffold new projects or retrofit existing projects with a complete modern stack including authentication, payments, and deployment infrastructure.

## Important: Example Files Available
The `scaffold/examples/` directory contains working examples for all major components:
- **Erlang Database**: Production-ready connection pooling with Neon support (`erlang/`)
- **Render Configuration**: Complete yaml examples (`render/`)
- **GitHub Actions**: Full CI/CD workflows (`github/`)
- **Auth0 Setup**: Complete configuration script (`auth0/`)
- **Stripe Setup**: Full payment integration (`stripe/`)

Use these examples as templates when generating code.

## Initialization Command
When the user runs `claude-code scaffold new [project-name]` or `claude-code scaffold existing`, follow this workflow.

## Pre-flight Checklist

### 1. Tool Verification
Check and install required tools:
```bash
# Check for required CLIs
which render || echo "❌ Render CLI not found"
which gh || echo "❌ GitHub CLI not found"  
which stripe || echo "❌ Stripe CLI not found"
which auth0 || echo "❌ Auth0 CLI not found"
```

### 2. MCP Server Verification
Verify MCP servers are configured:
- Check for `render-mcp` in settings
- Check for `github-mcp` in settings
- Check for `stripe-mcp` in settings
- Check for `playwright-mcp` in settings (for Auth0 automation)

### 3. Authentication Status
```bash
# Check authentication status
render whoami
gh auth status
stripe whoami
auth0 whoami
```

## Scaffolding Workflow

### Phase 1: Project Analysis

#### For New Projects:
1. Ask for project details:
   - Project name
   - Description
   - Backend language preference (Erlang/Node/Python)
   - Database preference (Render PostgreSQL/Neon)
   - Authentication model (Basic/SaaS/Marketplace/Organization)
   - Pricing model (Simple/Tiered/Usage-based/Seat-based)

#### For Existing Projects:
1. Analyze project structure:
   ```bash
   # Detect frontend framework
   [ -f "package.json" ] && grep -E "next|react|vue|angular" package.json
   
   # Detect backend language
   [ -f "rebar.config" ] && echo "Erlang detected"
   [ -f "requirements.txt" ] && echo "Python detected"
   [ -f "server/package.json" ] && echo "Node.js detected"
   
   # Detect existing authentication
   grep -r "auth0\|Auth0" --include="*.js" --include="*.ts" --include="*.tsx"
   
   # Detect existing payments
   grep -r "stripe\|Stripe" --include="*.js" --include="*.ts" --include="*.tsx"
   ```

2. Analyze for user roles and features:
   ```bash
   # Look for role definitions
   grep -r "role\|permission\|admin\|user" --include="*.js" --include="*.ts"
   
   # Look for feature flags or tiers
   grep -r "plan\|tier\|subscription\|premium\|free" --include="*.js" --include="*.ts"
   ```

3. Present findings and ask for confirmation:
   ```
   Based on analysis:
   - Frontend: [detected framework]
   - Backend: [detected language]
   - User roles found: [list]
   - Suggested Auth0 roles: [mapped roles]
   - Suggested subscription tiers: [based on features]
   
   Proceed with these settings? (y/n)
   ```

### Phase 2: Service Setup

#### 2.1 GitHub Repository
```bash
# For new projects, create repository
gh repo create [project-name] --public/--private

# Setup branches
git checkout -b staging
git push -u origin staging
git checkout -b main
git push -u origin main
```

#### 2.2 Render Services
Using Render MCP:
1. Create PostgreSQL database:
   ```javascript
   // Determine cost-effective option
   if (userPreference === 'cost-effective') {
     // Single database with schemas
     createPostgres({
       name: `${projectName}-db`,
       plan: 'basic_256mb',
       region: 'oregon'
     });
     // Create schemas via SQL
     runSQL('CREATE SCHEMA staging; CREATE SCHEMA production;');
   } else {
     // Separate databases
     createPostgres({
       name: `${projectName}-db-staging`,
       plan: 'free',
       region: 'oregon'
     });
     createPostgres({
       name: `${projectName}-db-prod`,
       plan: 'basic_256mb',
       region: 'oregon'
     });
   }
   ```

2. Create backend services:
   ```javascript
   // Staging backend
   createWebService({
     name: `${projectName}-api-staging`,
     runtime: backendLanguage,
     branch: 'staging',
     buildCommand: getBuildCommand(backendLanguage),
     startCommand: getStartCommand(backendLanguage),
     envVars: stagingEnvVars
   });
   
   // Production backend
   createWebService({
     name: `${projectName}-api`,
     runtime: backendLanguage,
     branch: 'main',
     buildCommand: getBuildCommand(backendLanguage),
     startCommand: getStartCommand(backendLanguage),
     envVars: productionEnvVars
   });
   ```

3. Create frontend services:
   ```javascript
   // Staging frontend
   createStaticSite({
     name: `${projectName}-frontend-staging`,
     branch: 'staging',
     buildCommand: 'npm install && npm run build',
     publishPath: './dist'
   });
   
   // Production frontend
   createStaticSite({
     name: `${projectName}-frontend`,
     branch: 'main',
     buildCommand: 'npm install && npm run build',
     publishPath: './dist'
   });
   ```

#### 2.3 Auth0 Setup
1. Use Playwright automation for Auth0 CLI login:
   ```javascript
   // scaffold/scripts/auth0-auto-login.js
   const { chromium } = require('playwright');
   
   async function autoLogin() {
     // Start auth0 login process
     const loginProcess = spawn('auth0', ['login']);
     
     // Wait for browser to open
     await sleep(2000);
     
     // Use Playwright to click authorize
     const browser = await chromium.connectOverCDP('http://localhost:9222');
     const page = browser.pages()[0];
     
     // Click the authorize button
     await page.click('button:has-text("Authorize")');
     
     // Wait for completion
     await loginProcess;
   }
   ```

2. Create Auth0 applications:
   ```bash
   # Create staging application
   auth0 apps create \
     --name "${PROJECT_NAME} Staging" \
     --type spa \
     --callbacks "https://${PROJECT_NAME}-frontend-staging.onrender.com/auth/callback" \
     --logout-urls "https://${PROJECT_NAME}-frontend-staging.onrender.com" \
     --web-origins "https://${PROJECT_NAME}-frontend-staging.onrender.com"
   
   # Create production application
   auth0 apps create \
     --name "${PROJECT_NAME} Production" \
     --type spa \
     --callbacks "https://${PROJECT_NAME}-frontend.onrender.com/auth/callback" \
     --logout-urls "https://${PROJECT_NAME}-frontend.onrender.com" \
     --web-origins "https://${PROJECT_NAME}-frontend.onrender.com"
   ```

3. Setup roles and permissions:
   ```bash
   # Create roles based on selected model
   auth0 roles create --name admin --description "Administrator role"
   auth0 roles create --name user --description "Standard user role"
   
   # Add permissions
   auth0 roles permissions add admin --permissions "read:all" "write:all" "delete:all"
   auth0 roles permissions add user --permissions "read:own" "write:own"
   ```

#### 2.4 Stripe Setup
Using Stripe MCP:
1. Create products and prices:
   ```javascript
   // Based on selected pricing model
   const products = getPricingTiers(pricingModel);
   
   for (const product of products) {
     const stripeProduct = await createProduct({
       name: product.name,
       description: product.description
     });
     
     if (product.price > 0) {
       await createPrice({
         product: stripeProduct.id,
         unit_amount: product.price * 100, // cents
         currency: 'usd',
         recurring: { interval: 'month' }
       });
     }
   }
   ```

2. Setup webhooks:
   ```javascript
   // Staging webhook
   stripe.webhooks.create({
     url: `https://${projectName}-api-staging.onrender.com/api/webhooks/stripe`,
     enabled_events: ['customer.subscription.created', 'customer.subscription.updated', 'customer.subscription.deleted']
   });
   
   // Production webhook
   stripe.webhooks.create({
     url: `https://${projectName}-api.onrender.com/api/webhooks/stripe`,
     enabled_events: ['customer.subscription.created', 'customer.subscription.updated', 'customer.subscription.deleted']
   });
   ```

### Phase 3: Code Generation

#### 3.1 Backend Generation

##### Erlang (Cowboy)
Generate OTP application structure using examples from `scaffold/examples/erlang/`:

**CRITICAL for Erlang Database Connections:**
1. Use `scaffold/examples/erlang/db_connection_pool.erl` as the base for database pooling
2. Use `scaffold/examples/erlang/db_worker.erl` for the worker implementation
3. These handle both Neon and standard PostgreSQL connections properly
4. Key features:
   - Automatic SSL configuration for cloud databases
   - Neon endpoint parameter handling
   - Connection retry logic
   - Proper error handling and reconnection

```
backend/
├── src/
│   ├── backend_app.erl
│   ├── backend_sup.erl
│   ├── backend_handler.erl
│   ├── backend_auth.erl
│   ├── backend_db.erl        # Based on db_connection_pool.erl
│   ├── backend_db_worker.erl # Based on db_worker.erl
│   └── backend_stripe.erl
├── priv/
│   └── backend.routes
├── rebar.config              # Use poolboy, epgsql dependencies
└── config/
    └── sys.config
```

**Database URL Parsing for Erlang:**
The examples handle DATABASE_URL in format:
`postgresql://user:password@host:port/database?params`

For Neon specifically, the connection requires:
- SSL enabled
- Endpoint parameter in password field
- Example: `endpoint=ep-sweet-bush-123456;actualpassword`

##### Node.js (Express)
Generate Express structure:
```
backend/
├── src/
│   ├── index.js
│   ├── routes/
│   ├── middleware/
│   ├── services/
│   └── models/
├── package.json
└── .env
```

##### Python (FastAPI)
Generate FastAPI structure:
```
backend/
├── app/
│   ├── main.py
│   ├── routers/
│   ├── models/
│   ├── services/
│   └── middleware/
├── requirements.txt
└── .env
```

#### 3.2 Frontend Generation
For new projects, generate Next.js structure:
```
frontend/
├── app/
│   ├── layout.tsx
│   ├── page.tsx
│   ├── auth/
│   ├── dashboard/
│   └── billing/
├── components/
│   ├── auth/
│   └── stripe/
├── lib/
│   ├── auth.ts
│   └── api.ts
├── public/
│   └── _redirects     # Critical for SPA routing
└── package.json
```

**IMPORTANT: SPA Routing Configuration**
For Auth0 callback and client-side routing to work properly, you MUST configure redirects:

1. **In render.yaml (Preferred method):**
```yaml
services:
  - type: static
    name: myapp-frontend
    routes:
      - type: rewrite
        source: /*
        destination: /index.html
```

2. **Alternative: _redirects file in public directory:**
```
/*    /index.html   200
```

This ensures Auth0 callbacks and all client-side routes work correctly. Without this, users will get 404 errors when accessing routes directly or after authentication.

### Phase 4: CI/CD Setup

Generate GitHub Actions workflows:
```yaml
# .github/workflows/deploy-staging.yml
name: Deploy to Staging
on:
  push:
    branches: [staging]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Run tests
        run: |
          npm test
          npm run lint
  
  deploy:
    needs: test
    runs-on: ubuntu-latest
    steps:
      - name: Deploy to Render
        run: |
          curl -X POST "https://api.render.com/v1/services/${{ secrets.RENDER_SERVICE_ID }}/deploys" \
            -H "Authorization: Bearer ${{ secrets.RENDER_API_KEY }}"
  
  e2e-tests:
    needs: deploy
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Run E2E tests
        run: npm run test:e2e

# .github/workflows/deploy-production.yml
name: Deploy to Production
on:
  push:
    branches: [main]
# Similar structure with production-specific steps
```

### Phase 5: Environment Configuration

#### 5.1 Update Render Environment Variables
```javascript
// Using Render MCP
await updateEnvironmentVariables({
  serviceId: stagingBackendId,
  envVars: [
    { key: 'DATABASE_URL', value: dbUrl },
    { key: 'AUTH0_DOMAIN', value: auth0Domain },
    { key: 'AUTH0_CLIENT_SECRET', value: auth0Secret },
    { key: 'STRIPE_SECRET_KEY', value: stripeTestKey },
    { key: 'STRIPE_WEBHOOK_SECRET', value: stripeWebhookSecret }
  ]
});
```

#### 5.2 Create Local .env Files
```bash
# Generate .env.local
cat > .env.local << EOF
# Database
DATABASE_URL=${DATABASE_URL}

# Auth0
NEXT_PUBLIC_AUTH0_DOMAIN=${AUTH0_DOMAIN}
NEXT_PUBLIC_AUTH0_CLIENT_ID=${AUTH0_CLIENT_ID}
AUTH0_CLIENT_SECRET=${AUTH0_CLIENT_SECRET}

# Stripe
NEXT_PUBLIC_STRIPE_PUBLISHABLE_KEY=${STRIPE_PUB_KEY}
STRIPE_SECRET_KEY=${STRIPE_SECRET_KEY}
EOF

# Add to .gitignore
echo ".env.local" >> .gitignore
```

#### 5.3 Setup GitHub Secrets
```bash
gh secret set RENDER_API_KEY --body "${RENDER_API_KEY}"
gh secret set RENDER_SERVICE_ID_STAGING --body "${STAGING_SERVICE_ID}"
gh secret set RENDER_SERVICE_ID_PROD --body "${PROD_SERVICE_ID}"
gh secret set AUTH0_TEST_USER --body "${AUTH0_TEST_USER}"
gh secret set AUTH0_TEST_PASSWORD --body "${AUTH0_TEST_PASSWORD}"
```

### Phase 6: Database Initialization

```sql
-- Create tables based on detected/selected schema
CREATE TABLE IF NOT EXISTS users (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  email VARCHAR(255) UNIQUE NOT NULL,
  auth0_id VARCHAR(255) UNIQUE,
  role VARCHAR(50),
  stripe_customer_id VARCHAR(255),
  subscription_status VARCHAR(50),
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW()
);

-- Add any project-specific tables
```

### Phase 7: Testing & Validation

1. **Service Health Checks**
   ```bash
   # Check all services are running
   curl https://${PROJECT_NAME}-api-staging.onrender.com/health
   curl https://${PROJECT_NAME}-frontend-staging.onrender.com
   ```

2. **Auth0 Test**
   ```javascript
   // Test auth flow
   const testUser = await auth0.createUser({
     email: 'test@example.com',
     password: 'Test123!',
     connection: 'Username-Password-Authentication'
   });
   ```

3. **Stripe Test**
   ```bash
   # Test webhook
   stripe trigger customer.subscription.created
   ```

4. **Database Connection Test**
   ```javascript
   // Test database connection
   const client = new Client({ connectionString: DATABASE_URL });
   await client.connect();
   await client.query('SELECT NOW()');
   ```

### Phase 8: Documentation Generation

Generate project-specific documentation:
```markdown
# ${PROJECT_NAME}

## Setup
1. Clone the repository
2. Install dependencies: `npm install`
3. Copy `.env.example` to `.env.local`
4. Run locally: `npm run dev`

## Deployment
- Staging: Push to `staging` branch
- Production: Merge PR from staging to main

## Services
- Frontend Staging: https://${PROJECT_NAME}-frontend-staging.onrender.com
- Frontend Production: https://${PROJECT_NAME}-frontend.onrender.com
- API Staging: https://${PROJECT_NAME}-api-staging.onrender.com
- API Production: https://${PROJECT_NAME}-api.onrender.com

## Authentication
Using Auth0 with roles: ${SELECTED_ROLES}

## Payments
Using Stripe with tiers: ${SELECTED_TIERS}
```

## Error Handling

### Common Issues and Solutions

1. **MCP Server Not Found**
   ```
   Error: render-mcp not configured
   Solution: Add to ~/.claude/mcp_settings.json and restart Claude Code
   ```

2. **Authentication Failed**
   ```
   Error: Auth0 CLI not authenticated
   Solution: Run auth0-auto-login.js script with Playwright
   ```

3. **Database Connection Failed**
   ```
   Error: Cannot connect to database
   Solution: Check DATABASE_URL and SSL settings
   ```

## Interactive Prompts

### Backend Language Selection
```
Select backend language:
1. Erlang (Cowboy) - High concurrency, fault-tolerant [DEFAULT]
2. Node.js (Express) - JavaScript everywhere
3. Python (FastAPI) - Modern, fast, type-hints

Choice [1]: 
```

### Database Provider Selection
```
Select database provider:
1. Render PostgreSQL - Integrated, managed [DEFAULT]
2. Neon - Serverless, branching

Choice [1]:
```

### Database Strategy (Render)
```
Select database strategy:
1. Single database with schemas - Cost-effective [DEFAULT]
2. Separate databases - Better isolation

Choice [1]:
```

### Authentication Model
```
Select authentication model:
1. Basic (user, admin) [DEFAULT]
2. SaaS Standard (free, premium, enterprise, admin)
3. Marketplace (buyer, seller, admin)
4. Organization (owner, admin, member, viewer)

Choice [1]:
```

### Pricing Model
```
Select pricing model:
1. Simple (Free, Pro) [DEFAULT]
2. Tiered (Free, Starter, Pro, Enterprise)
3. Usage-based (Pay as you go)
4. Seat-based (Per user)

Choice [1]:
```

## Completion Checklist

After scaffolding, verify:
- [ ] All services deployed and healthy
- [ ] Authentication working (test login)
- [ ] Payment flow working (test subscription)
- [ ] Database connected and seeded
- [ ] CI/CD pipelines running
- [ ] Environment variables set
- [ ] Documentation generated
- [ ] Local development working

## Next Steps for User

1. Review generated code and customize
2. Update branding and styling
3. Add business logic
4. Configure custom domain
5. Set up monitoring
6. Implement backup strategy

## Maintenance Commands

```bash
# Check service status
claude-code scaffold status

# Update environment variables
claude-code scaffold update-env

# Rotate secrets
claude-code scaffold rotate-secrets

# Backup database
claude-code scaffold backup-db
```