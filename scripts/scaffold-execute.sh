#!/bin/bash

# Claude Code Scaffold Execution Script
# This script is called when /scaffold command is invoked

set -e

# Get scaffold directory
SCAFFOLD_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Parse arguments
ACTION="${1:-new}"
PROJECT_NAME="${2:-my-app}"
CURRENT_DIR="$(pwd)"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

print_color() {
    local color=$1
    shift
    echo -e "${color}$@${NC}"
}

# Function to copy templates
copy_templates() {
    local target_dir=$1
    
    # Copy template directories
    print_color $BLUE "Copying templates..."
    cp -r "$SCAFFOLD_DIR/templates" "$target_dir/"
    cp -r "$SCAFFOLD_DIR/examples" "$target_dir/"
    cp -r "$SCAFFOLD_DIR/scripts" "$target_dir/.scaffold/"
    
    # Copy essential docs
    cp "$SCAFFOLD_DIR/docs/ARCHITECTURE.md" "$target_dir/docs/"
}

# Function to setup MCP configuration
setup_mcp_config() {
    local target_dir=$1
    
    print_color $BLUE "Setting up MCP configuration..."
    mkdir -p "$target_dir/.claude"
    
    # Copy and customize MCP settings
    cp "$SCAFFOLD_DIR/config/mcp_settings.json" "$target_dir/.claude/"
    
    # Add project-specific configuration
    cat > "$target_dir/.claude/project.json" << EOF
{
  "project": {
    "name": "$PROJECT_NAME",
    "type": "scaffolded",
    "created": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
    "scaffold_version": "1.0.0"
  },
  "services": {
    "render": {
      "staging": "${PROJECT_NAME}-staging",
      "production": "${PROJECT_NAME}"
    },
    "github": {
      "repo": "${PROJECT_NAME}"
    },
    "auth0": {
      "tenant": "${PROJECT_NAME}"
    }
  }
}
EOF
}

# Function to generate project CLAUDE.md
generate_claude_md() {
    local target_dir=$1
    
    print_color $BLUE "Generating project CLAUDE.md..."
    
    cat > "$target_dir/CLAUDE.md" << 'EOF'
# Project Instructions for Claude Code

This project was scaffolded with the Claude Code Scaffold System.

## Available MCP Servers

This project is configured with the following MCP servers:
- **render**: For deployment and infrastructure management
- **github**: For repository and CI/CD management
- **stripe**: For payment processing
- **neon**: For database management
- **playwright**: For Auth0 automation

## Project Structure

```
.
├── .claude/              # Claude-specific configurations
│   ├── mcp_settings.json # MCP server configurations
│   └── project.json      # Project metadata
├── .scaffold/            # Scaffolding scripts and tools
├── frontend/             # Frontend application
├── backend/              # Backend API
├── .github/              # GitHub Actions workflows
├── docs/                 # Documentation
└── CLAUDE.md            # This file
```

## Common Tasks

### Deploy to Staging
Ask: "Deploy the current branch to staging"

### Run Tests
Ask: "Run the test suite and fix any failures"

### Update Environment Variables
Ask: "Update the environment variables for [staging/production]"

### Database Migrations
Ask: "Create a migration to [describe change]"

### Add New Feature
Ask: "Add a new feature for [describe feature]"

## Service URLs

- **Frontend Staging**: https://${PROJECT_NAME}-frontend-staging.onrender.com
- **Frontend Production**: https://${PROJECT_NAME}-frontend.onrender.com
- **API Staging**: https://${PROJECT_NAME}-api-staging.onrender.com
- **API Production**: https://${PROJECT_NAME}-api.onrender.com

## Important Notes

1. Always check `.scaffold/CLAUDE.md` for detailed scaffolding instructions
2. Environment variables are managed through Render Dashboard
3. Secrets should never be committed to the repository
4. Use staging environment for testing before production deployment
EOF
    
    # Append original scaffolding instructions reference
    echo "" >> "$target_dir/CLAUDE.md"
    echo "## Scaffolding Reference" >> "$target_dir/CLAUDE.md"
    echo "" >> "$target_dir/CLAUDE.md"
    echo "For detailed scaffolding workflows and instructions, see:" >> "$target_dir/CLAUDE.md"
    echo "- \`.scaffold/CLAUDE.md\` - Original scaffolding instructions" >> "$target_dir/CLAUDE.md"
    echo "- \`.scaffold/examples/\` - Example configurations" >> "$target_dir/CLAUDE.md"
    echo "- \`.scaffold/templates/\` - Template files" >> "$target_dir/CLAUDE.md"
}

# Main execution
main() {
    print_color $BLUE "🚀 Claude Code Scaffold Execution"
    print_color $BLUE "=================================="
    
    if [ "$ACTION" = "new" ]; then
        # Create new project
        PROJECT_DIR="$CURRENT_DIR/$PROJECT_NAME"
        
        if [ -d "$PROJECT_DIR" ]; then
            print_color $RED "Error: Directory $PROJECT_DIR already exists"
            exit 1
        fi
        
        print_color $GREEN "Creating new project: $PROJECT_NAME"
        mkdir -p "$PROJECT_DIR"
        cd "$PROJECT_DIR"
        
        # Create directory structure
        mkdir -p frontend backend docs .github/workflows
        
        # Copy templates and configs
        copy_templates "$PROJECT_DIR"
        setup_mcp_config "$PROJECT_DIR"
        generate_claude_md "$PROJECT_DIR"
        
        # Copy the main CLAUDE.md for reference
        cp "$SCAFFOLD_DIR/CLAUDE.md" "$PROJECT_DIR/.scaffold/CLAUDE.md"
        
        # Initialize git
        git init
        
        print_color $GREEN "✅ Project scaffolded successfully!"
        print_color $YELLOW "Next steps:"
        echo "1. cd $PROJECT_NAME"
        echo "2. Open Claude Code and follow the instructions in CLAUDE.md"
        echo "3. Claude will guide you through service setup"
        
    elif [ "$ACTION" = "existing" ]; then
        # Scaffold existing project
        PROJECT_DIR="$CURRENT_DIR"
        PROJECT_NAME="$(basename "$PROJECT_DIR")"
        
        print_color $GREEN "Scaffolding existing project: $PROJECT_NAME"
        
        # Create scaffold directory
        mkdir -p ".scaffold"
        
        # Copy templates and configs
        copy_templates "$PROJECT_DIR"
        setup_mcp_config "$PROJECT_DIR"
        
        # Check if CLAUDE.md exists and back it up
        if [ -f "CLAUDE.md" ]; then
            print_color $YELLOW "Backing up existing CLAUDE.md to CLAUDE.md.backup"
            cp CLAUDE.md CLAUDE.md.backup
        fi
        
        generate_claude_md "$PROJECT_DIR"
        
        # Copy the main CLAUDE.md for reference
        cp "$SCAFFOLD_DIR/CLAUDE.md" "$PROJECT_DIR/.scaffold/CLAUDE.md"
        
        print_color $GREEN "✅ Existing project scaffolded successfully!"
        print_color $YELLOW "Next steps:"
        echo "1. Review the generated CLAUDE.md"
        echo "2. Open Claude Code and follow the scaffolding workflow"
        
    else
        print_color $RED "Unknown action: $ACTION"
        echo "Usage: scaffold [new|existing] [project-name]"
        exit 1
    fi
}

# Run main function
main
