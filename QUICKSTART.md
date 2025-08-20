# 🚀 Claude Code Scaffolding - Quick Start Guide

## Immediate Usage - Two Methods

### Method 1: Using the Shell Script (Automated)

```bash
# For new projects
./scaffold/scripts/scaffold-init.sh new my-app

# For existing projects  
cd existing-project
/path/to/scaffold/scripts/scaffold-init.sh existing
```

The script will:
1. Check and install required tools
2. Configure MCP servers
3. Authenticate services
4. Prompt for configuration choices
5. Launch Claude Code with scaffolding instructions

### Method 2: Direct Claude Code Command (Manual)

Simply tell Claude Code:
```
Please scaffold a new project called "my-app" using the instructions in scaffold/CLAUDE.md
```

Or for existing projects:
```
Please add authentication, payments, and deployment to this existing project using scaffold/CLAUDE.md
```

## Essential Setup (One-Time)

### 1. Set Environment Variables
```bash
export RENDER_API_KEY="your-render-api-key"
export GITHUB_TOKEN="your-github-token"
export STRIPE_SECRET_KEY="your-stripe-secret-key"
export NEON_API_KEY="your-neon-api-key"  # Optional - only if using Neon
export AUTH0_DOMAIN="your-domain.auth0.com"
export AUTH0_CLIENT_ID="your-client-id"
export AUTH0_CLIENT_SECRET="your-client-secret"
```

### 2. Ensure MCP Servers are Configured
Add to `~/.claude/mcp_settings.json`:
```json
{
  "mcpServers": {
    "render": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-render"],
      "env": {
        "RENDER_API_KEY": "${RENDER_API_KEY}"
      }
    },
    "github": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-github"],
      "env": {
        "GITHUB_PERSONAL_ACCESS_TOKEN": "${GITHUB_TOKEN}"
      }
    },
    "stripe": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-stripe"],
      "env": {
        "STRIPE_API_KEY": "${STRIPE_SECRET_KEY}"
      }
    },
    "neon": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-neon"],
      "env": {
        "NEON_API_KEY": "${NEON_API_KEY}"
      }
    },
    "playwright": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-playwright"]
    },
    "auth0": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-auth0"],
      "env": {
        "AUTH0_DOMAIN": "${AUTH0_DOMAIN}",
        "AUTH0_CLIENT_ID": "${AUTH0_CLIENT_ID}",
        "AUTH0_CLIENT_SECRET": "${AUTH0_CLIENT_SECRET}"
      }
    }
  }
}
```

Then restart Claude Code.

## Configuration Choices

You'll be prompted for:

1. **Backend Language**
   - Erlang (Cowboy) - Default, high concurrency
   - Node.js (Express)
   - Python (FastAPI)

2. **Database**
   - Render PostgreSQL - Default
   - Neon (optional)

3. **Auth Model**
   - Basic (user/admin) - Default
   - SaaS (free/premium/enterprise)
   - Marketplace (buyer/seller)
   - Organization (team-based)

4. **Pricing Model**
   - Simple (Free/Pro) - Default
   - Tiered (multiple levels)
   - Usage-based
   - Seat-based

## What Gets Created

### Services
- **GitHub**: Repository with staging/main branches
- **Render**: Frontend + Backend services (staging & production)
- **Database**: PostgreSQL with schemas/tables
- **Auth0**: Two applications (staging & production)
- **Stripe**: Products, prices, and webhooks

### Code
- Backend API (language of choice)
- Frontend (Next.js/React)
- Authentication components
- Payment integration
- GitHub Actions workflows

### Configuration
- Environment variables (Render & local)
- GitHub secrets for CI/CD
- Database migrations
- API endpoints

## Verification

After scaffolding completes:

```bash
# Check service status
./scaffold/scripts/scaffold-init.sh status

# Or manually verify:
curl https://my-app-api-staging.onrender.com/health
curl https://my-app-frontend-staging.onrender.com
```

## Common Commands

### For Claude Code:
```
# Scaffold new project
claude-code scaffold new my-app

# Add to existing project
claude-code scaffold existing

# Check status
claude-code scaffold status

# Update environment variables
claude-code scaffold update-env
```

### Direct Service Commands:
```bash
# Deploy to staging
git push origin staging

# Deploy to production
git push origin main

# View logs
render logs --service my-app-api --follow

# Run tests
npm test
npm run test:e2e
```

## Troubleshooting

### Issue: MCP servers not found
**Solution**: Add to `~/.claude/mcp_settings.json` and restart Claude Code

### Issue: Authentication failed
**Solution**: 
```bash
# For Auth0
node scaffold/scripts/auth0-auto-login.js

# For others
render login
gh auth login
stripe login
```

### Issue: Services not created
**Solution**: Check API keys and re-run with Claude Code

## Example Full Flow

```bash
# 1. Start scaffolding
./scaffold/scripts/scaffold-init.sh new awesome-saas

# 2. Answer prompts
#    Backend: Erlang [Enter]
#    Database: Render PostgreSQL [Enter]
#    DB Strategy: Single with schemas [Enter]
#    Auth Model: SaaS Standard [2]
#    Pricing: Tiered [2]

# 3. Wait for Claude Code to complete

# 4. Verify
cd awesome-saas
npm run dev  # Test locally

# 5. Deploy
git add .
git commit -m "Initial scaffold"
git push origin staging

# 6. Access services
# Frontend: https://awesome-saas-frontend-staging.onrender.com
# API: https://awesome-saas-api-staging.onrender.com
```

## Next Steps

1. **Customize** your application logic
2. **Test** authentication and payments
3. **Configure** custom domains
4. **Monitor** service health
5. **Scale** as needed

---

Need help? Check the full [README](README.md) or [Architecture](docs/ARCHITECTURE.md) docs.