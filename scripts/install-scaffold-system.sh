#!/bin/bash

# Claude Code Scaffold System Installation
# This script installs the scaffold system globally for the current user

set -e

# Colors for output
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

# Configuration
SCAFFOLD_HOME="$HOME/.claude-scaffold"
CLAUDE_CONFIG_DIR="$HOME/.claude"
SCAFFOLD_REPO_URL="https://github.com/nisbus/claude-scaffold.git"

print_color $BLUE "🚀 Installing Claude Code Scaffold System"
print_color $BLUE "========================================"

# Check if Git is available
if ! command -v git &> /dev/null; then
    print_color $RED "❌ Git is required but not installed"
    exit 1
fi

# Check if Claude config directory exists
if [ ! -d "$CLAUDE_CONFIG_DIR" ]; then
    print_color $YELLOW "Creating Claude config directory at $CLAUDE_CONFIG_DIR"
    mkdir -p "$CLAUDE_CONFIG_DIR"
fi

# Create scaffold home directory
if [ -d "$SCAFFOLD_HOME" ]; then
    print_color $YELLOW "Existing installation found. Updating..."
    cd "$SCAFFOLD_HOME"
    git pull origin main
else
    print_color $BLUE "Cloning scaffold repository to $SCAFFOLD_HOME"
    git clone "$SCAFFOLD_REPO_URL" "$SCAFFOLD_HOME"
fi

# Create custom commands directory if it doesn't exist
COMMANDS_DIR="$CLAUDE_CONFIG_DIR/commands"
if [ ! -d "$COMMANDS_DIR" ]; then
    print_color $YELLOW "Creating custom commands directory"
    mkdir -p "$COMMANDS_DIR"
fi

# Create the scaffold command definition with system-wide paths
print_color $BLUE "Registering /scaffold command..."

cat > "$COMMANDS_DIR/scaffold.json" << EOF
{
  "name": "scaffold",
  "description": "Scaffold a new full-stack project with auth, payments, and deployment",
  "version": "1.0.0",
  "command": {
    "type": "script",
    "path": "$SCAFFOLD_HOME/scripts/scaffold-execute.sh",
    "workingDirectory": "current"
  },
  "parameters": {
    "action": {
      "type": "string",
      "enum": ["new", "existing"],
      "description": "Create new project or scaffold existing",
      "required": true
    },
    "projectName": {
      "type": "string",
      "description": "Name of the project to scaffold",
      "required": false
    }
  },
  "instructions": {
    "claudeMd": "$SCAFFOLD_HOME/CLAUDE.md",
    "workflow": [
      {
        "step": 1,
        "action": "Parse command parameters",
        "description": "Determine if scaffolding new or existing project"
      },
      {
        "step": 2,
        "action": "Create project directory",
        "description": "Set up project structure and copy templates"
      },
      {
        "step": 3,
        "action": "Copy MCP configurations",
        "description": "Set up .claude/mcp_settings.json for the project"
      },
      {
        "step": 4,
        "action": "Generate CLAUDE.md",
        "description": "Create project-specific Claude instructions"
      },
      {
        "step": 5,
        "action": "Run initialization",
        "description": "Execute scaffolding workflow from CLAUDE.md"
      }
    ]
  },
  "mcp_servers": {
    "required": ["render", "github", "stripe", "neon", "playwright"],
    "config_path": "$SCAFFOLD_HOME/config/mcp_settings.json"
  }
}
EOF

# Update the execution script to use system-wide paths
print_color $BLUE "Updating execution script..."

cat > "$SCAFFOLD_HOME/scripts/scaffold-execute.sh" << 'EXEC_EOF'
#!/bin/bash

# Claude Code Scaffold Execution Script (System-wide Installation)
# This script is called when /scaffold command is invoked

set -e

# Get scaffold directory from system installation
SCAFFOLD_HOME="$HOME/.claude-scaffold"
if [ ! -d "$SCAFFOLD_HOME" ]; then
    echo "❌ Scaffold system not found. Please run install-scaffold-system.sh"
    exit 1
fi

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

# Function to copy templates from system installation
copy_templates() {
    local target_dir=$1
    
    print_color $BLUE "Copying templates from system installation..."
    cp -r "$SCAFFOLD_HOME/templates" "$target_dir/"
    cp -r "$SCAFFOLD_HOME/examples" "$target_dir/"
    
    # Create .scaffold directory for project-specific scaffold tools
    mkdir -p "$target_dir/.scaffold"
    cp -r "$SCAFFOLD_HOME/scripts" "$target_dir/.scaffold/"
    
    # Copy essential docs
    mkdir -p "$target_dir/docs"
    cp "$SCAFFOLD_HOME/docs/ARCHITECTURE.md" "$target_dir/docs/"
}

# Function to setup MCP configuration from system installation
setup_mcp_config() {
    local target_dir=$1
    
    print_color $BLUE "Setting up MCP configuration..."
    mkdir -p "$target_dir/.claude"
    
    # Copy and customize MCP settings from system installation
    cp "$SCAFFOLD_HOME/config/mcp_settings.json" "$target_dir/.claude/"
    
    # Add project-specific configuration
    cat > "$target_dir/.claude/project.json" << EOF
{
  "project": {
    "name": "$PROJECT_NAME",
    "type": "scaffolded",
    "created": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
    "scaffold_version": "1.0.0",
    "scaffold_home": "$SCAFFOLD_HOME"
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
    
    cat > "$target_dir/CLAUDE.md" << EOF
# Project Instructions for Claude Code

This project was scaffolded with the Claude Code Scaffold System.
Scaffold system installed at: \`$SCAFFOLD_HOME\`

## Available MCP Servers

This project is configured with the following MCP servers:
- **render**: For deployment and infrastructure management
- **github**: For repository and CI/CD management
- **stripe**: For payment processing
- **neon**: For database management
- **playwright**: For Auth0 automation

## Project Structure

\`\`\`
.
├── .claude/              # Claude-specific configurations
│   ├── mcp_settings.json # MCP server configurations
│   └── project.json      # Project metadata
├── .scaffold/            # Project-specific scaffolding tools
├── frontend/             # Frontend application
├── backend/              # Backend API
├── .github/              # GitHub Actions workflows
├── docs/                 # Documentation
└── CLAUDE.md            # This file
\`\`\`

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

1. System scaffold installation: \`$SCAFFOLD_HOME\`
2. Scaffold reference materials in \`.scaffold/\` directory
3. Environment variables are managed through Render Dashboard
4. Secrets should never be committed to the repository
5. Use staging environment for testing before production deployment

## Scaffolding Reference

For detailed scaffolding workflows and instructions, see:
- \`.scaffold/CLAUDE.md\` - Original scaffolding instructions
- \`.scaffold/examples/\` - Example configurations
- \`.scaffold/templates/\` - Template files
- System installation: \`$SCAFFOLD_HOME\`

## Updating Scaffold System

To update the scaffold system to the latest version:
\`\`\`bash
cd $SCAFFOLD_HOME
git pull origin main
\`\`\`

Or reinstall:
\`\`\`bash
curl -sSL https://raw.githubusercontent.com/nisbus/claude-scaffold/main/scripts/install-scaffold-system.sh | bash
\`\`\`
EOF
}

# Main execution
main() {
    print_color $BLUE "🚀 Claude Code Scaffold Execution"
    print_color $BLUE "=================================="
    print_color $BLUE "Using system installation: $SCAFFOLD_HOME"
    
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
        
        # Copy templates and configs from system installation
        copy_templates "$PROJECT_DIR"
        setup_mcp_config "$PROJECT_DIR"
        generate_claude_md "$PROJECT_DIR"
        
        # Copy the main CLAUDE.md for reference
        cp "$SCAFFOLD_HOME/CLAUDE.md" "$PROJECT_DIR/.scaffold/CLAUDE.md"
        
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
        print_color $BLUE "Using system installation: $SCAFFOLD_HOME"
        
        # Create scaffold directory
        mkdir -p ".scaffold"
        
        # Copy templates and configs from system installation
        copy_templates "$PROJECT_DIR"
        setup_mcp_config "$PROJECT_DIR"
        
        # Check if CLAUDE.md exists and back it up
        if [ -f "CLAUDE.md" ]; then
            print_color $YELLOW "Backing up existing CLAUDE.md to CLAUDE.md.backup"
            cp CLAUDE.md CLAUDE.md.backup
        fi
        
        generate_claude_md "$PROJECT_DIR"
        
        # Copy the main CLAUDE.md for reference
        cp "$SCAFFOLD_HOME/CLAUDE.md" "$PROJECT_DIR/.scaffold/CLAUDE.md"
        
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
EXEC_EOF

# Make the execution script executable
chmod +x "$SCAFFOLD_HOME/scripts/scaffold-execute.sh"

print_color $GREEN "✅ Execution script updated"

# Create command aliases
cat > "$CLAUDE_CONFIG_DIR/aliases.json" << EOF
{
  "scaffold": {
    "command": "/scaffold",
    "description": "Shortcut for scaffolding projects"
  }
}
EOF

print_color $GREEN "✅ Command alias created"

# Create update script for easy maintenance
cat > "$SCAFFOLD_HOME/update-scaffold.sh" << 'UPDATE_EOF'
#!/bin/bash

# Update Claude Code Scaffold System
echo "🔄 Updating Claude Code Scaffold System..."

cd "$HOME/.claude-scaffold"
git pull origin main

echo "✅ Scaffold system updated!"
echo "💡 Restart Claude Code to use any new features"
UPDATE_EOF

chmod +x "$SCAFFOLD_HOME/update-scaffold.sh"

# Create uninstall script
cat > "$SCAFFOLD_HOME/uninstall-scaffold.sh" << 'UNINSTALL_EOF'
#!/bin/bash

# Uninstall Claude Code Scaffold System
echo "🗑️  Uninstalling Claude Code Scaffold System..."

# Remove command definition
rm -f "$HOME/.claude/commands/scaffold.json"

# Remove aliases
rm -f "$HOME/.claude/aliases.json"

# Remove scaffold home (ask for confirmation)
read -p "Remove all scaffold files from $HOME/.claude-scaffold? (y/N): " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    rm -rf "$HOME/.claude-scaffold"
    echo "✅ Scaffold system completely removed"
else
    echo "✅ Command removed, files kept at $HOME/.claude-scaffold"
fi

echo "💡 Restart Claude Code for changes to take effect"
UNINSTALL_EOF

chmod +x "$SCAFFOLD_HOME/uninstall-scaffold.sh"

# Final instructions
print_color $GREEN "\n✅ Claude Code Scaffold System installed successfully!"
print_color $BLUE "\n📍 Installation Details:"
echo "  System Home: $SCAFFOLD_HOME"
echo "  Claude Config: $CLAUDE_CONFIG_DIR"
echo ""
print_color $BLUE "📋 Available Commands:"
echo "  Update:    $SCAFFOLD_HOME/update-scaffold.sh"
echo "  Uninstall: $SCAFFOLD_HOME/uninstall-scaffold.sh"
echo ""
print_color $BLUE "📋 Usage in Claude Code:"
echo "  /scaffold new my-project    - Create a new scaffolded project"
echo "  /scaffold existing          - Scaffold the current directory"
echo ""
echo "Or simply ask Claude:"
echo "  'Please scaffold a new project called my-app'"
echo "  'Scaffold this existing project'"
echo ""
print_color $YELLOW "⚠️  Important: Restart Claude Code for the command to take effect"
print_color $GREEN "\nHappy scaffolding! 🚀"