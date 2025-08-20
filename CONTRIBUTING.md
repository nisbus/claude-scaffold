# Contributing to Claude Code Scaffold

Thank you for your interest in making Claude Code Scaffold better! This project thrives on community contributions from experienced developers who understand what makes development truly productive.

## 🎯 Why Contribute?

Every template you create saves **hundreds of hours** for developers in the community. Your expertise with your favorite stack becomes a force multiplier for the entire ecosystem.

## 🚀 Quick Start for Contributors

### 1. Set Up Development Environment

```bash
# Fork the repository on GitHub, then:
git clone https://github.com/yourusername/claude-scaffold.git
cd claude-scaffold

# Install the scaffold system for testing
./scripts/install-scaffold-system.sh

# Test the current system
/scaffold new test-project
```

### 2. Understand the Structure

```
claude-scaffold/
├── templates/           # Framework templates
│   ├── erlang/         # Erlang/OTP backend
│   ├── node/           # Node.js backend  
│   ├── python/         # Python backend
│   └── frontend/       # Frontend components
├── examples/           # Complete working examples
│   ├── auth0/         # Auth0 setup scripts
│   ├── stripe/        # Stripe integration
│   ├── render/        # Render deployment
│   └── github/        # CI/CD workflows
├── scripts/           # Scaffold automation
└── docs/             # Documentation
```

## 🛠️ Types of Contributions

### 1. New Framework Templates (High Impact)

**Time Investment**: 45-60 minutes  
**Community Impact**: Hundreds of saved hours per template

#### Backend Templates

Create a new backend template:

```bash
# Copy existing template as starting point
cp -r templates/node templates/go
cd templates/go

# Modify files for your framework
# - Update package.json → go.mod
# - Convert JavaScript → Go
# - Adapt authentication middleware
# - Update database connection patterns
```

**Key files to customize**:
- **Dependency management**: `go.mod`, `package.json`, `requirements.txt`
- **Application structure**: Main entry point, routing, middleware
- **Database integration**: Connection pooling, migrations
- **Authentication**: JWT handling, Auth0 integration
- **Environment configuration**: Config loading, validation

#### Frontend Templates

Create framework alternatives:

```bash
cp -r templates/frontend/nextjs templates/frontend/nuxt
# Adapt for Vue.js/Nuxt patterns
```

### 2. Service Integrations (Medium Impact)

**Time Investment**: 30-45 minutes  
**Examples**:
- Supabase authentication
- Firebase integration  
- PayPal payments
- AWS deployment
- MongoDB connections

### 3. Complete Examples (High Impact)

**Time Investment**: 60-90 minutes  
Create working examples in `examples/`:

```bash
examples/
├── supabase/          # Complete Supabase setup
├── vercel/           # Vercel deployment
├── django/           # Django backend example
└── vue/              # Vue.js frontend example
```

### 4. MCP Server Development (Advanced)

**Time Investment**: 2+ hours  
Create new MCP servers for services not yet supported.

## 📋 Template Creation Guide

### Backend Template Checklist

- [ ] **Project structure** matches framework conventions
- [ ] **Dependency management** with lock files
- [ ] **Environment configuration** with validation
- [ ] **Database connection** with pooling and SSL
- [ ] **Authentication middleware** for JWT tokens
- [ ] **CORS configuration** for frontend integration
- [ ] **Error handling** and logging
- [ ] **Health check endpoint** for deployment monitoring
- [ ] **Docker configuration** (optional but recommended)
- [ ] **Test setup** with basic examples

### Frontend Template Checklist

- [ ] **Modern framework setup** with TypeScript support
- [ ] **Authentication integration** (Auth0 SDK)
- [ ] **API client configuration** with error handling  
- [ ] **Component examples** (login, dashboard, profile)
- [ ] **Routing setup** with protected routes
- [ ] **State management** (Redux, Vuex, etc.)
- [ ] **Styling framework** (Tailwind recommended)
- [ ] **Build configuration** for production
- [ ] **Environment variables** handling

### Documentation Requirements

Each template needs:

1. **README.md** with:
   - Framework-specific setup instructions
   - Key architectural decisions
   - Environment variables explanation
   - Development workflow

2. **Comments in code** explaining:
   - Framework-specific patterns
   - Integration points with other services
   - Common customization points

## 🧪 Testing Your Contribution

### 1. Functional Testing

```bash
# Test new project creation
/scaffold new test-my-template

# Verify structure
cd test-my-template
ls -la

# Check that all files are present
# Test that the development server starts
# Verify authentication flows work
```

### 2. Integration Testing

```bash
# Test with different configurations
/scaffold new test-minimal
/scaffold new test-full-features

# Verify all service integrations work
# Test deployment configurations
# Check CI/CD pipeline setup
```

### 3. Documentation Testing

- [ ] Follow your own README instructions
- [ ] Test all code examples work
- [ ] Verify environment setup steps
- [ ] Check that beginners can follow along

## 🎨 Code Style Guidelines

### General Principles
- **Consistency** with existing templates
- **Clarity** over cleverness
- **Production-ready** patterns, not just demos
- **Security-first** approach
- **Documentation** for framework-specific decisions

### File Naming
- Use framework conventions (e.g., `main.go`, `app.py`, `server.js`)
- Include version numbers when relevant
- Use descriptive names for configuration files

### Comments
```javascript
// ✅ Good: Explain WHY, not WHAT
// JWT middleware configured for Auth0 integration
// Handles token validation and user context injection

// ❌ Bad: Explains obvious code
// This function validates JWT tokens
```

## 🔄 Pull Request Process

### 1. Before Submitting

- [ ] Test your template thoroughly
- [ ] Update documentation
- [ ] Add examples to the examples directory
- [ ] Verify no secrets or API keys are included

### 2. PR Title Format

```
Add Go/Gin backend template with PostgreSQL integration
Update Django template for async support
Fix Node.js authentication middleware bug
```

### 3. PR Description Template

```markdown
## What This Adds

Brief description of the new template/feature.

## Framework Details

- **Language**: Go 1.21+
- **Framework**: Gin v1.9+
- **Database**: PostgreSQL with pgx driver
- **Key Libraries**: jwt-go, godotenv, etc.

## Testing

- [ ] Creates new projects successfully
- [ ] Development server starts
- [ ] Authentication flow works
- [ ] Database connection established
- [ ] All examples in README work

## Documentation

- [ ] README.md updated
- [ ] Code comments added
- [ ] Architecture decisions explained

## Screenshots (if applicable)

Include screenshots of the working application.
```

### 4. Review Process

1. **Automated checks** verify basic functionality
2. **Community review** for code quality and patterns
3. **Maintainer review** for integration with existing system
4. **Testing** with the scaffold system
5. **Merge** and **celebration**! 🎉

## 🌟 Recognition

### Contributor Levels

**Template Creator**: Created and maintain one or more templates
- GitHub profile linked in README
- Maintainer badge on relevant issues
- Direct input on template evolution

**Core Contributor**: Multiple templates or major features
- Featured in project documentation
- Priority review of your future PRs
- Invitation to contributor Discord

**Framework Specialist**: Deep expertise in specific technology
- Technical leadership for that framework
- Mentorship of new contributors
- Speaking opportunities at community events

### Hall of Fame

Contributors are credited in:
- Main README.md
- Template documentation
- Community showcase
- Conference presentations about the project

## 🤝 Community Guidelines

### Be Helpful
- Welcome newcomers to your framework
- Share knowledge generously
- Provide constructive feedback

### Be Respectful  
- Different frameworks serve different needs
- No flame wars about technology choices
- Focus on making everyone more productive

### Be Collaborative
- Credit others' work and ideas
- Work together on complex features
- Share lessons learned

## 💡 Contribution Ideas by Experience Level

### Beginner Friendly (30-60 minutes)
- Add missing environment variables to templates
- Improve README documentation
- Fix broken links or typos
- Add code comments for clarity

### Intermediate (1-3 hours)
- Create new framework template
- Add service integration
- Improve error handling
- Add comprehensive examples

### Advanced (3+ hours)
- Create new MCP server
- Design new scaffold workflows
- Build deployment automation
- Architect complex integrations

## 📞 Getting Help

**Stuck on something?** We're here to help:

- 🐛 **Bug reports**: Open a GitHub issue
- 💡 **Ideas**: Start a GitHub discussion
- 🤔 **Questions**: Comment on relevant issues
- 📧 **Direct contact**: Tag maintainers in issues

Remember: **Every contribution matters**. Even small improvements make the entire community more productive.

---

**Ready to contribute? The community can't wait to see what you'll build!** 🚀