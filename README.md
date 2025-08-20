# 🚀 Claude Code Scaffold: From Idea to Production in Hours

**The fastest way to build production-ready applications with Claude Code.** 

Stop wrestling with boilerplate, authentication setup, payment integration, and deployment configuration. Start building features that matter.

## 💡 The Problem with Modern Development

Building a SaaS application today means:
- ⏱️ **Weeks of setup** before writing your first feature
- 🔐 **Authentication complexity** with roles, permissions, and JWT handling
- 💳 **Payment integration headaches** with subscriptions and webhooks
- 🚀 **Deployment infrastructure** with staging, production, and CI/CD
- 📊 **Database design** and migration management
- 🔧 **Configuration management** across multiple services

**What if you could skip all of that and focus on your unique value proposition?**

## ✨ The Claude Code Scaffold Solution

**One command. Complete application. Production ready.**

```bash
# Install once
curl -sSL https://raw.githubusercontent.com/nisbus/claude-scaffold/main/scripts/install-scaffold-system.sh | bash

# Build anything
/scaffold new my-saas-app
```

Claude Code handles everything:
- 🏗️ **Full-stack architecture** (Frontend + Backend + Database)
- 🔐 **Authentication & authorization** (Auth0 with roles)
- 💳 **Payment processing** (Stripe with subscriptions)
- 🚀 **Multi-environment deployment** (Staging + Production)
- ⚡ **CI/CD pipelines** (GitHub Actions)
- 📱 **Modern frameworks** (Next.js, React, your choice of backend)

## 🎯 Who This Is For

**Experienced developers who value their time.** You know how to build software, but you're tired of:
- Setting up the same infrastructure repeatedly
- Fighting with OAuth flows and JWT tokens
- Configuring Stripe webhooks for the hundredth time
- Writing deployment scripts instead of features

**This is not another tutorial or template.** It's a complete automation system that leverages Claude's intelligence to build exactly what you need.

## 🏆 Real Results

From our testing and community feedback:

- ⚡ **2 hours**: Idea to deployed SaaS application
- 🎨 **Zero boilerplate**: Every line of code serves your business logic
- 🔒 **Enterprise-grade**: Security, scalability, and monitoring built-in
- 💰 **Cost-effective**: Optimized for development and production efficiency

## 🤝 Built by the Community, for the Community

**We need your expertise!** This project thrives on contributions from developers who understand what makes development truly productive.

### 🛠️ Current Stack Support
- **Frontend**: Next.js, React
- **Backend**: Erlang/OTP, Node.js, Python
- **Database**: PostgreSQL (Render, Neon)
- **Auth**: Auth0
- **Payments**: Stripe
- **Deployment**: Render

### 🎯 We're Looking For
- **Go/Gin** templates and examples
- **Django/FastAPI** advanced patterns
- **Vue.js/Nuxt** frontend alternatives
- **Ruby on Rails** backend support
- **Supabase** auth integration
- **AWS/GCP** deployment options
- **Docker/Kubernetes** deployment strategies
- **Your favorite stack combination**

**See something missing? [Contribute a template!](#contributing)** Even 30 minutes of your expertise can save thousands of hours for the community.

---

## 🏗️ Architecture & Flow

### Scaffold Command Flow

```mermaid
graph TD
    A[User: /scaffold new my-app] --> B[Claude Code]
    B --> C[Load scaffold.json]
    C --> D[Execute scaffold-execute.sh]
    
    D --> E[Create Project Directory]
    E --> F[Copy Templates]
    F --> G[Setup .claude/mcp_settings.json]
    G --> H[Generate Project CLAUDE.md]
    H --> I[Initialize Git Repo]
    
    I --> J[Claude Reads CLAUDE.md]
    J --> K[Follow Scaffolding Workflow]
    
    K --> L[Setup Services]
    L --> M[Render: Create DB & Services]
    L --> N[Auth0: Configure Authentication]
    L --> O[Stripe: Setup Payments]
    L --> P[GitHub: Create Repo & Actions]
    
    M --> Q[Deploy Application]
    N --> Q
    O --> Q
    P --> Q
    
    Q --> R[Project Ready!]
```

### Directory Structure After Scaffolding

```
my-app/
├── .claude/                      # Claude-specific configs
│   ├── mcp_settings.json        # MCP server configurations
│   └── project.json             # Project metadata
├── .scaffold/                   # Scaffolding reference
│   ├── CLAUDE.md               # Original scaffold instructions
│   ├── examples/               # Example configurations
│   ├── templates/              # Template files
│   └── scripts/                # Utility scripts
├── frontend/                    # Frontend application
│   ├── app/                    # Next.js app directory
│   ├── components/             # React components
│   ├── lib/                    # Utilities
│   └── public/                 # Static assets
├── backend/                     # Backend API
│   ├── src/                    # Source code
│   ├── config/                 # Configuration
│   └── package.json           # Dependencies
├── .github/                     # GitHub configuration
│   └── workflows/              # CI/CD pipelines
├── docs/                        # Documentation
│   └── ARCHITECTURE.md        # Architecture overview
├── CLAUDE.md                   # Project-specific Claude instructions
└── README.md                   # Project documentation
```

### How It Works

1. **Command Registration**: Run `scripts/register-scaffold-command.sh` to register the `/scaffold` command with Claude Code
2. **Command Invocation**: Use `/scaffold new my-app` or `/scaffold existing` in Claude Code
3. **Project Setup**: The scaffold script creates the project structure and copies all necessary files
4. **MCP Configuration**: Local `.claude/mcp_settings.json` is created with all required MCP servers
5. **Claude Instructions**: A project-specific `CLAUDE.md` is generated to guide Claude through the setup
6. **Service Initialization**: Claude follows the workflow to set up all services (Render, Auth0, Stripe, GitHub)
7. **Deployment**: Applications are deployed to staging and production environments

## 🚀 Features

- **Full-Stack Setup**: Frontend (Next.js/React) + Backend (Erlang/Node.js/Python)
- **Authentication**: Auth0 integration with role-based access control
- **Payments**: Stripe subscription management
- **Database**: Render PostgreSQL or Neon with automatic schema setup
- **Deployment**: Render services with staging and production environments
- **CI/CD**: GitHub Actions workflows for automated testing and deployment
- **Infrastructure as Code**: All services configured programmatically

## ⏱️ Time to Production

With Claude Code Scaffold, you can go from idea to deployed application in:
- **Initial Setup**: 5 minutes (one-time)
- **Project Creation**: 2 minutes
- **Service Configuration**: 15 minutes
- **Basic Features**: 30-60 minutes
- **Deployment**: 10 minutes

**Total: Under 2 hours for a production-ready application!**

## 📋 Prerequisites

### Required API Keys
- **Render API Key**: Get from [Render Dashboard](https://dashboard.render.com/account/api-keys)
- **GitHub Personal Access Token**: Create at [GitHub Settings](https://github.com/settings/tokens)
- **Stripe API Keys**: Get from [Stripe Dashboard](https://dashboard.stripe.com/apikeys)
- **Auth0 Credentials**: Get from [Auth0 Dashboard](https://manage.auth0.com)
- **Neon API Key** (optional): Get from [Neon Console](https://console.neon.tech/api-keys)

### CLI Tools (verified during setup)
- `render` - Render CLI
- `gh` - GitHub CLI
- `stripe` - Stripe CLI
- `auth0` - Auth0 CLI

### MCP Servers (configured automatically)
- `render-mcp` - Render integration
- `github-mcp` - GitHub integration
- `stripe-mcp` - Stripe integration
- `playwright-mcp` - Browser automation (for Auth0)
- `neon-mcp` - Neon integration (optional)

## 📖 Complete Workflow Story

**[Read the full story: From Idea to Production](docs/WORKFLOW_STORY.md)** - Follow Sarah as she builds "PlantCare", a SaaS application, from concept to deployment in just 2 hours. Includes:
- Step-by-step walkthrough with real commands
- Best practices for prompting Claude
- Common workflows and pro tips
- Complete example from database design to production deployment

## 🎯 Quick Start

### System Installation (One-time)

Install the scaffold system globally for your user:

```bash
# Install directly from GitHub
curl -sSL https://raw.githubusercontent.com/nisbus/claude-scaffold/main/scripts/install-scaffold-system.sh | bash

# Or clone and install locally
git clone https://github.com/nisbus/claude-scaffold.git
cd claude-scaffold
./scripts/install-scaffold-system.sh

# Restart Claude Code for changes to take effect
```

This installs the scaffold system to `~/.claude-scaffold` and registers the `/scaffold` command globally.

### New Project

In Claude Code, use the registered command:
```
/scaffold new my-awesome-app
```

Or ask Claude naturally:
```
"Please scaffold a new project called my-awesome-app"
```

### Existing Project

Navigate to your project directory, then in Claude Code:
```
/scaffold existing
```

Or ask Claude:
```
"Please scaffold this existing project with auth, payments, and deployment"
```

## 🔧 Configuration Options

### Backend Languages
- **Erlang** (default) - Cowboy framework, OTP, high concurrency
- **Node.js** - Express.js, familiar JavaScript
- **Python** - FastAPI, modern async Python

### Database Providers
- **Render PostgreSQL** (default) - Integrated, managed
  - Single database with schemas (cost-effective)
  - Separate databases (better isolation)
- **Neon** - Serverless, branching, auto-scaling

### Authentication Models
- **Basic** - Simple user/admin roles
- **SaaS Standard** - Free/Premium/Enterprise tiers
- **Marketplace** - Buyer/Seller roles
- **Organization** - Team-based permissions

### Pricing Models
- **Simple** - Free + Pro tiers
- **Tiered** - Multiple subscription levels
- **Usage-based** - Pay-as-you-go
- **Seat-based** - Per-user pricing

## 📁 Project Structure

### Generated Structure
```
my-app/
├── frontend/               # Next.js/React frontend
│   ├── app/               # App router pages
│   ├── components/        # React components
│   │   ├── auth/         # Auth0 components
│   │   └── stripe/       # Stripe components
│   └── lib/              # Utilities
├── backend/               # Backend service
│   ├── src/              # Source code
│   ├── config/           # Configuration
│   └── tests/            # Test files
├── .github/
│   └── workflows/        # CI/CD pipelines
│       ├── deploy-staging.yml
│       └── deploy-production.yml
├── .env.local            # Local environment variables
├── .scaffold-config.json # Scaffolding configuration
└── README.md            # Project documentation
```

## 🔄 Workflow

### 1. Initialization
```bash
./scripts/scaffold-init.sh new my-app
```
- Checks prerequisites
- Installs missing tools
- Configures MCP servers
- Authenticates services

### 2. Configuration
Interactive prompts for:
- Backend language selection
- Database provider choice
- Authentication model
- Pricing structure

### 3. Service Creation
Claude Code will:
- Create GitHub repository
- Setup Render services (frontend + backend)
- Configure Auth0 applications
- Create Stripe products and webhooks
- Initialize database schema

### 4. Code Generation
- Backend API scaffolding
- Frontend components
- Authentication flows
- Payment integration
- GitHub Actions workflows

### 5. Validation
Automatic verification of:
- Service health checks
- Authentication flow
- Payment processing
- Database connectivity

## 🤖 Claude Code Integration

### Manual Command
When prompted, Claude Code will execute:
```
/scaffold new [project-name]
```

### Claude Code Instructions
The system provides Claude Code with:
- Step-by-step scaffolding workflow
- Service configuration templates
- Code generation patterns
- Validation procedures

### Interactive Flow
1. Claude Code reads `scaffold/CLAUDE.md` for instructions
2. Uses MCP servers to create services
3. Generates code based on templates
4. Configures environment variables
5. Sets up CI/CD pipelines

## 🔐 Security Best Practices

### Environment Variables
- Never commit secrets to repository
- Use Render environment variables for services
- Use GitHub Actions secrets for CI/CD
- Local `.env.local` added to `.gitignore`

### Authentication
- JWT validation on all API endpoints
- Role-based access control
- Secure session management
- Regular token rotation

### Database
- SSL/TLS connections required
- Connection pooling
- Row-level security
- Regular automated backups

## 🧪 Testing

### Local Development
```bash
# Install dependencies
npm install

# Run locally
npm run dev

# Run tests
npm test
```

### E2E Testing
```bash
# Install Playwright
npm run test:install

# Run E2E tests
npm run test:e2e

# Run specific test suites
npm run test:auth
npm run test:subscription
```

## 📊 Monitoring

### Service Health
- Render dashboard for service metrics
- Database performance monitoring
- API response time tracking

### Business Metrics
- Stripe subscription analytics
- User engagement tracking
- Feature usage statistics

## 🚨 Troubleshooting

### Common Issues

#### MCP Server Not Found
```bash
# Add to ~/.claude/mcp_settings.json
# Restart Claude Code
```

#### Auth0 Login Failed
```bash
# Use automated login
node scaffold/scripts/auth0-auto-login.js

# Or manual login
auth0 login
```

#### Database Connection Failed
```bash
# Check DATABASE_URL
# Verify SSL settings
# Check network connectivity
```

## 📚 Documentation

### Core Documents
- `scaffold/docs/ARCHITECTURE.md` - System architecture
- `scaffold/CLAUDE.md` - Claude Code instructions
- `scaffold/templates/` - Code templates

### Template Files
- `erlang/` - Erlang/OTP templates
- `node/` - Node.js/Express templates
- `python/` - Python/FastAPI templates
- `frontend/` - Next.js/React templates
- `github/` - GitHub Actions workflows

## 🔄 Maintenance

### Update Environment Variables
```bash
/scaffold update-env
```

### Rotate Secrets
```bash
/scaffold rotate-secrets
```

### Database Backup
```bash
/scaffold backup-db
```

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests
5. Submit a pull request

## 📝 License

MIT License - See LICENSE file for details

## 🆘 Support

- **Documentation**: Read this README and docs/
- **Issues**: Report bugs on GitHub Issues
- **Questions**: Ask in discussions

## 🎉 Success Checklist

After scaffolding, verify:
- [ ] All services deployed and healthy
- [ ] Authentication working (test login)
- [ ] Payment flow working (test subscription)
- [ ] Database connected and seeded
- [ ] CI/CD pipelines running
- [ ] Environment variables set
- [ ] Documentation generated
- [ ] Local development working

## 🔧 System Management

### Update Scaffold System
```bash
# Update to latest version
~/.claude-scaffold/update-scaffold.sh

# Or reinstall
curl -sSL https://raw.githubusercontent.com/nisbus/claude-scaffold/main/scripts/install-scaffold-system.sh | bash
```

### Uninstall
```bash
# Remove scaffold system
~/.claude-scaffold/uninstall-scaffold.sh
```

### System Paths
- **Installation**: `~/.claude-scaffold/`
- **Claude Config**: `~/.claude/commands/scaffold.json`
- **Project Templates**: `~/.claude-scaffold/templates/`
- **Examples**: `~/.claude-scaffold/examples/`

## 🤝 Contributing

We believe the best scaffolding system is built by the community of developers who use it daily. **Your contributions make everyone more productive.**

### 🎯 High-Impact Contributions

**New Language/Framework Templates** (45-60 minutes):
- Copy existing template structure from `templates/`
- Adapt for your favorite stack (Go/Gin, Django, Rails, etc.)
- Test with the scaffold system
- Submit PR with documentation

**Service Integrations** (30-45 minutes):
- Add support for new auth providers (Supabase, Firebase)
- Integration with new payment systems (PayPal, Paddle)
- Cloud deployment options (AWS, GCP, Azure)

**MCP Server Integrations** (60+ minutes):
- Create MCP servers for new services
- Improve existing server capabilities
- Add new automation workflows

### 📋 Quick Contribution Guide

1. **Fork & Clone**
   ```bash
   git clone https://github.com/yourusername/claude-scaffold.git
   cd claude-scaffold
   ```

2. **Create Your Template**
   ```bash
   # Copy existing template
   cp -r templates/node templates/your-stack
   # Modify for your framework
   ```

3. **Test Your Template**
   ```bash
   ./scripts/install-scaffold-system.sh
   /scaffold new test-project
   # Verify it works with your template
   ```

4. **Submit PR**
   - Clear description of what you're adding
   - Example usage or screenshots
   - Any special configuration needed

### 🌟 Recognition

Contributors get:
- **Credit** in the main README and documentation
- **Maintainer status** for templates you create
- **Community recognition** for helping thousands of developers

### 💡 Ideas We'd Love to See

**Backend Languages:**
- Go with Gin/Echo/Fiber
- Rust with Axum/Actix
- Ruby on Rails
- Java with Spring Boot
- C# with ASP.NET Core
- PHP with Laravel/Symfony

**Frontend Frameworks:**
- Vue.js with Nuxt
- Svelte with SvelteKit
- Angular
- Solid.js
- Alpine.js with HTMX

**Database Options:**
- MongoDB templates
- Supabase integration
- PlanetScale configuration
- Redis patterns
- GraphQL schemas

**Deployment Platforms:**
- Vercel deployment automation
- AWS CDK templates
- Google Cloud Run
- Digital Ocean Apps
- Railway deployment
- Fly.io configurations

**Authentication Providers:**
- Supabase Auth
- Firebase Auth
- AWS Cognito
- Clerk integration
- Magic links

### 🔥 Featured Contributors

*Be the first to contribute and get featured here!*

## ⚠️ Technical Notes

### SPA Routing Configuration
For single-page applications with client-side routing and Auth0 authentication, configure URL rewrites:

**render.yaml (Recommended)**:
```yaml
services:
  - type: static
    name: your-app-frontend
    routes:
      - type: rewrite
        source: /*
        destination: /index.html
```

**_redirects file**:
Place in your public directory:
```
/*    /index.html   200
```

## 🚀 Next Steps

1. **Customize** - Update branding, styling, and business logic
2. **Configure** - Set up custom domains and SSL
3. **Monitor** - Configure alerting and logging
4. **Scale** - Optimize performance and costs
5. **Secure** - Implement additional security measures

---

**Built with ❤️ by the community, for rapid application development**