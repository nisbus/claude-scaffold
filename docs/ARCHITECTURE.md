# Claude Code Scaffolding Architecture

## Overview
This document defines the architecture template for scaffolding new projects and retrofitting existing projects with a complete modern stack including:
- Frontend (Next.js/React)
- Backend (Erlang/Node.js/Python)
- Database (Render PostgreSQL or Neon)
- Authentication (Auth0)
- Payments (Stripe)
- Deployment (Render)
- CI/CD (GitHub Actions)

## Architecture Patterns

### 1. Service Architecture
```
┌─────────────────────────────────────────────────────────────┐
│                         Production                          │
├─────────────────────────────────────────────────────────────┤
│  Frontend (Render)  │  Backend (Render)  │  DB (Render/Neon)│
│  - Next.js/React    │  - Erlang/Node/Py  │  - PostgreSQL    │
│  - Static Site      │  - REST API        │  - PostGIS       │
│  - CDN              │  - WebSockets      │  - TimescaleDB   │
└─────────────────────────────────────────────────────────────┘
                              │
                              │ PR Merge
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                          Staging                            │
├─────────────────────────────────────────────────────────────┤
│  Frontend (Render)  │  Backend (Render)  │  DB (Render/Neon)│
│  - Same stack       │  - Same stack      │  - Separate DB   │
│  - Test env vars    │  - Test env vars   │    or Schema     │
└─────────────────────────────────────────────────────────────┘
```

### 2. Authentication Flow (Auth0)
- Single tenant with two applications (staging/production)
- Role-based access control (RBAC)
- JWT tokens for API authentication
- Session management with refresh tokens

### 3. Payment Processing (Stripe)
- Single account with test/live modes
- Subscription-based billing
- Webhook integration for payment events
- Customer portal for self-service

### 4. Database Strategy
**Default: Render PostgreSQL**
- Option 1: Separate databases for staging/production (higher cost, better isolation)
- Option 2: Single database with separate schemas (lower cost, shared resources)
- Extensions: PostGIS, TimescaleDB (if supported)

**Alternative: Neon**
- Serverless PostgreSQL
- Branching for development
- Auto-scaling

### 5. CI/CD Pipeline
```yaml
Workflow:
  1. Push to staging branch
     → Run unit tests
     → Deploy to staging
     → Run E2E tests
     → Create PR to main
  
  2. Merge PR to main
     → Run unit tests
     → Deploy to production
     → Run smoke tests
     → Monitor deployment
```

## Frontend Configuration

### SPA Routing Support
For single-page applications (SPAs) like React, Vue, or Angular, client-side routing requires special configuration:

**Method 1: render.yaml (Recommended)**
```yaml
services:
  - type: static
    name: myapp-frontend
    routes:
      - type: rewrite
        source: /*
        destination: /index.html
```

**Method 2: _redirects file**
Place a `_redirects` file in your public directory:
```
/*    /index.html   200
```

This configuration ensures that all routes are handled by your SPA's router, allowing proper navigation and direct URL access.

## Backend Language Templates

### Erlang (Default)
- Framework: Cowboy
- Structure: OTP application
- Key libraries:
  - `cowboy` - HTTP/WebSocket server
  - `jiffy` - JSON parsing
  - `epgsql` - PostgreSQL client
  - `jwt` - JWT token handling

### Node.js
- Framework: Express.js
- Structure: MVC pattern
- Key libraries:
  - `express` - Web framework
  - `pg` - PostgreSQL client
  - `socket.io` - WebSockets
  - `express-jwt` - JWT middleware

### Python
- Framework: FastAPI
- Structure: Clean architecture
- Key libraries:
  - `fastapi` - Web framework
  - `sqlalchemy` - ORM
  - `alembic` - Migrations
  - `python-jose` - JWT tokens

## Environment Variables Structure

### Frontend (.env)
```env
# Auth0
NEXT_PUBLIC_AUTH0_DOMAIN=
NEXT_PUBLIC_AUTH0_CLIENT_ID=
NEXT_PUBLIC_AUTH0_AUDIENCE=

# Stripe
NEXT_PUBLIC_STRIPE_PUBLISHABLE_KEY=

# API
NEXT_PUBLIC_API_URL=
```

### Backend (.env)
```env
# Database
DATABASE_URL=

# Auth0
AUTH0_DOMAIN=
AUTH0_CLIENT_SECRET=
AUTH0_AUDIENCE=

# Stripe
STRIPE_SECRET_KEY=
STRIPE_WEBHOOK_SECRET=

# Email
SMTP_HOST=
SMTP_PORT=
SMTP_USER=
SMTP_PASS=
```

## Default Role & Permission Structure

### Option 1: Basic
```json
{
  "roles": [
    {
      "name": "admin",
      "permissions": ["manage:all"]
    },
    {
      "name": "user",
      "permissions": ["read:own_profile", "update:own_profile"]
    }
  ]
}
```

### Option 2: SaaS Standard
```json
{
  "roles": [
    {
      "name": "admin",
      "permissions": ["manage:all"]
    },
    {
      "name": "enterprise_user",
      "permissions": ["access:all_features", "priority:support"]
    },
    {
      "name": "premium_user",
      "permissions": ["access:premium_features"]
    },
    {
      "name": "free_user",
      "permissions": ["access:basic_features"]
    }
  ]
}
```

## Default Subscription Tiers

### Option 1: Simple SaaS
```json
{
  "products": [
    {
      "name": "Free",
      "price": 0,
      "features": ["Basic features", "Community support"]
    },
    {
      "name": "Pro",
      "price": 29,
      "interval": "month",
      "features": ["All features", "Priority support", "Advanced analytics"]
    }
  ]
}
```

### Option 2: Tiered SaaS
```json
{
  "products": [
    {
      "name": "Free",
      "price": 0,
      "features": ["Up to 3 users", "Basic features"]
    },
    {
      "name": "Starter",
      "price": 9,
      "interval": "month",
      "features": ["Up to 10 users", "Core features", "Email support"]
    },
    {
      "name": "Professional",
      "price": 29,
      "interval": "month",
      "features": ["Unlimited users", "All features", "Priority support"]
    },
    {
      "name": "Enterprise",
      "price": "custom",
      "features": ["Custom features", "SLA", "Dedicated support"]
    }
  ]
}
```

## Scaffolding Usage

### New Project
In Claude Code (after registration):
```
/scaffold new [project-name]
```

Or natural language:
```
"Please scaffold a new project called [project-name]"
```

### Existing Project
In Claude Code:
```
/scaffold existing
```

Or natural language:
```
"Please scaffold this existing project with authentication and payments"
```

### Configuration Options
Claude will interactively prompt for:
- Backend language: Erlang (default), Node.js, or Python
- Database provider: Render PostgreSQL (default) or Neon
- Authentication roles: Basic, SaaS, Marketplace, or Organization
- Pricing model: Simple, Tiered, Usage-based, or Seat-based
- Service selections: Auth0, Stripe, GitHub Actions (all optional)

## Required Tools & Services

### CLI Tools
- `render` - Render CLI
- `gh` - GitHub CLI
- `stripe` - Stripe CLI
- `auth0` - Auth0 CLI
- `git` - Version control

### MCP Servers
- `render-mcp` - Required
- `github-mcp` - Required
- `stripe-mcp` - Required
- `neon-mcp` - Optional (if using Neon)
- `playwright-mcp` - Required (for Auth0 automation)

### API Keys Required
- Render API key
- GitHub personal access token
- Stripe API keys (test and live)
- Auth0 management API token
- Neon API key (if using Neon)

## Deployment Strategy

### Staging
1. Auto-deploy on push to `staging` branch
2. Run full test suite
3. Manual approval for promotion

### Production
1. Deploy only from `main` branch
2. Require PR from staging
3. Run smoke tests post-deployment
4. Automatic rollback on failure

## Security Considerations

1. **Secrets Management**
   - Never commit secrets to repository
   - Use Render environment variables
   - Use GitHub Actions secrets
   - Rotate keys regularly

2. **Database Security**
   - Use connection pooling
   - Enable SSL/TLS
   - Implement row-level security
   - Regular backups

3. **API Security**
   - JWT validation on all endpoints
   - Rate limiting
   - CORS configuration
   - Input validation

4. **Frontend Security**
   - Content Security Policy
   - XSS protection
   - Secure cookie handling
   - HTTPS only

## Monitoring & Observability

1. **Application Monitoring**
   - Error tracking (Sentry/Rollbar)
   - Performance monitoring
   - User analytics

2. **Infrastructure Monitoring**
   - Render metrics
   - Database performance
   - API response times

3. **Business Metrics**
   - Stripe subscription metrics
   - User engagement
   - Feature usage

## Disaster Recovery

1. **Backup Strategy**
   - Daily database backups
   - Point-in-time recovery
   - Cross-region backups

2. **Rollback Procedures**
   - Git revert for code
   - Database migration rollback
   - Render deployment rollback

3. **Incident Response**
   - Automated alerts
   - On-call rotation
   - Post-mortem process