#!/bin/bash

# Claude Code Scaffolding Initialization Script
# This script handles the complete scaffolding workflow

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
SCAFFOLD_DIR="$(dirname "$(dirname "$(realpath "$0")")")"
CLAUDE_CONFIG_FILE="$HOME/.claude/scaffold_config.json"
PROJECT_NAME=""
PROJECT_TYPE=""
BACKEND_LANG="erlang"
DB_PROVIDER="render"
AUTH_MODEL="basic"
PRICING_MODEL="simple"

# Function to print colored output
print_color() {
    local color=$1
    shift
    echo -e "${color}$@${NC}"
}

# Function to check if a command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Function to prompt for user input with default
prompt_with_default() {
    local prompt=$1
    local default=$2
    local response
    
    read -p "$prompt [$default]: " response
    echo "${response:-$default}"
}

# Function to select from options
select_option() {
    local prompt=$1
    shift
    local options=("$@")
    local PS3="$prompt: "
    local selected
    
    select selected in "${options[@]}"; do
        if [[ -n "$selected" ]]; then
            echo "$selected"
            break
        fi
    done
}

# Check prerequisites
check_prerequisites() {
    print_color $BLUE "🔍 Checking prerequisites..."
    
    local missing_tools=()
    
    # Check required CLIs
    if ! command_exists "render"; then
        missing_tools+=("render")
        print_color $RED "  ❌ Render CLI not found"
    else
        print_color $GREEN "  ✅ Render CLI found"
    fi
    
    if ! command_exists "gh"; then
        missing_tools+=("gh")
        print_color $RED "  ❌ GitHub CLI not found"
    else
        print_color $GREEN "  ✅ GitHub CLI found"
    fi
    
    if ! command_exists "stripe"; then
        missing_tools+=("stripe")
        print_color $RED "  ❌ Stripe CLI not found"
    else
        print_color $GREEN "  ✅ Stripe CLI found"
    fi
    
    if ! command_exists "auth0"; then
        missing_tools+=("auth0")
        print_color $RED "  ❌ Auth0 CLI not found"
    else
        print_color $GREEN "  ✅ Auth0 CLI found"
    fi
    
    if [ ${#missing_tools[@]} -gt 0 ]; then
        print_color $YELLOW "\n📦 Installing missing tools..."
        install_missing_tools "${missing_tools[@]}"
    fi
    
    # Check MCP servers
    check_mcp_servers
    
    # Check authentication
    check_authentication
}

# Install missing tools
install_missing_tools() {
    local tools=("$@")
    
    for tool in "${tools[@]}"; do
        case $tool in
            "render")
                print_color $YELLOW "  Installing Render CLI..."
                if [[ "$OSTYPE" == "darwin"* ]]; then
                    brew install render
                else
                    curl -L https://render.com/install.sh | sh
                fi
                ;;
            "gh")
                print_color $YELLOW "  Installing GitHub CLI..."
                if [[ "$OSTYPE" == "darwin"* ]]; then
                    brew install gh
                else
                    curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | sudo dd of=/usr/share/keyrings/githubcli-archive-keyring.gpg
                    echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | sudo tee /etc/apt/sources.list.d/github-cli.list > /dev/null
                    sudo apt update && sudo apt install gh
                fi
                ;;
            "stripe")
                print_color $YELLOW "  Installing Stripe CLI..."
                if [[ "$OSTYPE" == "darwin"* ]]; then
                    brew install stripe/stripe-cli/stripe
                else
                    curl -L https://github.com/stripe/stripe-cli/releases/latest/download/stripe_linux_x86_64.tar.gz | tar xz
                    sudo mv stripe /usr/local/bin
                fi
                ;;
            "auth0")
                print_color $YELLOW "  Installing Auth0 CLI..."
                if [[ "$OSTYPE" == "darwin"* ]]; then
                    brew tap auth0/auth0-cli && brew install auth0
                else
                    curl -L https://github.com/auth0/auth0-cli/releases/latest/download/auth0-cli_Linux_x86_64.tar.gz | tar xz
                    sudo mv auth0 /usr/local/bin
                fi
                ;;
        esac
    done
}

# Check MCP servers configuration
check_mcp_servers() {
    print_color $BLUE "\n🔌 Checking MCP servers..."
    
    local mcp_config="$HOME/.claude/mcp_settings.json"
    
    if [ ! -f "$mcp_config" ]; then
        print_color $YELLOW "  MCP configuration not found. Creating..."
        mkdir -p "$HOME/.claude"
        cp "$SCAFFOLD_DIR/config/mcp_settings.json" "$mcp_config"
        print_color $YELLOW "  ⚠️  Please restart Claude Code to load MCP servers"
        read -p "Press Enter after restarting Claude Code..."
    else
        # Check for required MCP servers
        local required_servers=("render" "github" "stripe" "playwright")
        local missing_servers=()
        
        for server in "${required_servers[@]}"; do
            if ! grep -q "\"$server\"" "$mcp_config"; then
                missing_servers+=("$server")
                print_color $RED "  ❌ $server MCP server not configured"
            else
                print_color $GREEN "  ✅ $server MCP server configured"
            fi
        done
        
        if [ ${#missing_servers[@]} -gt 0 ]; then
            print_color $YELLOW "\n  Adding missing MCP servers..."
            # Update MCP configuration
            node "$SCAFFOLD_DIR/scripts/update-mcp-config.js" "${missing_servers[@]}"
            print_color $YELLOW "  ⚠️  Please restart Claude Code to load new MCP servers"
            read -p "Press Enter after restarting Claude Code..."
        fi
    fi
}

# Check authentication status
check_authentication() {
    print_color $BLUE "\n🔐 Checking authentication..."
    
    # Check Render auth
    if ! render whoami &>/dev/null; then
        print_color $YELLOW "  Render CLI not authenticated"
        print_color $YELLOW "  Please enter your Render API key:"
        read -s RENDER_API_KEY
        echo "export RENDER_API_KEY=$RENDER_API_KEY" >> ~/.bashrc
        export RENDER_API_KEY
    else
        print_color $GREEN "  ✅ Render authenticated"
    fi
    
    # Check GitHub auth
    if ! gh auth status &>/dev/null; then
        print_color $YELLOW "  GitHub CLI not authenticated"
        gh auth login
    else
        print_color $GREEN "  ✅ GitHub authenticated"
    fi
    
    # Check Stripe auth
    if ! stripe whoami &>/dev/null; then
        print_color $YELLOW "  Stripe CLI not authenticated"
        stripe login
    else
        print_color $GREEN "  ✅ Stripe authenticated"
    fi
    
    # Check Auth0 auth - use Playwright automation
    if ! auth0 whoami &>/dev/null; then
        print_color $YELLOW "  Auth0 CLI not authenticated"
        print_color $YELLOW "  Starting automated Auth0 login..."
        node "$SCAFFOLD_DIR/scripts/auth0-auto-login.js"
    else
        print_color $GREEN "  ✅ Auth0 authenticated"
    fi
}

# Detect project type
detect_project_type() {
    if [ -z "$(ls -A)" ]; then
        PROJECT_TYPE="new"
        print_color $BLUE "📁 Empty directory detected - Creating new project"
    else
        PROJECT_TYPE="existing"
        print_color $BLUE "📂 Existing project detected - Adding scaffolding"
        analyze_existing_project
    fi
}

# Analyze existing project
analyze_existing_project() {
    print_color $BLUE "\n🔍 Analyzing existing project..."
    
    # Detect frontend framework
    if [ -f "package.json" ]; then
        if grep -q "next" package.json; then
            print_color $GREEN "  ✅ Next.js frontend detected"
        elif grep -q "react" package.json; then
            print_color $GREEN "  ✅ React frontend detected"
        fi
    fi
    
    # Detect backend language
    if [ -f "rebar.config" ] || [ -f "src/*.erl" ]; then
        BACKEND_LANG="erlang"
        print_color $GREEN "  ✅ Erlang backend detected"
    elif [ -f "requirements.txt" ] || [ -f "*.py" ]; then
        BACKEND_LANG="python"
        print_color $GREEN "  ✅ Python backend detected"
    elif [ -f "server/package.json" ]; then
        BACKEND_LANG="node"
        print_color $GREEN "  ✅ Node.js backend detected"
    fi
    
    # Detect existing auth
    if grep -r "auth0\|Auth0" --include="*.js" --include="*.ts" --include="*.tsx" . 2>/dev/null; then
        print_color $YELLOW "  ⚠️  Auth0 integration detected - will update configuration"
    fi
    
    # Detect existing payments
    if grep -r "stripe\|Stripe" --include="*.js" --include="*.ts" --include="*.tsx" . 2>/dev/null; then
        print_color $YELLOW "  ⚠️  Stripe integration detected - will update configuration"
    fi
    
    # Analyze for user roles
    print_color $BLUE "\n  🔍 Analyzing user roles..."
    local roles=$(grep -r "role\|permission\|admin\|user" --include="*.js" --include="*.ts" . 2>/dev/null | head -5)
    if [ -n "$roles" ]; then
        print_color $GREEN "  Found potential roles in code"
        echo "$roles" | head -5
    fi
}

# Get project configuration
get_project_config() {
    print_color $BLUE "\n⚙️  Project Configuration"
    
    if [ "$PROJECT_TYPE" = "new" ]; then
        PROJECT_NAME=$(prompt_with_default "Project name" "my-app")
    else
        PROJECT_NAME=$(basename "$PWD")
    fi
    
    # Backend language selection
    print_color $BLUE "\n🔧 Backend Language"
    echo "1) Erlang (Cowboy) - High concurrency, fault-tolerant [DEFAULT]"
    echo "2) Node.js (Express) - JavaScript everywhere"
    echo "3) Python (FastAPI) - Modern, fast, type-hints"
    read -p "Choice [1]: " backend_choice
    case $backend_choice in
        2) BACKEND_LANG="node" ;;
        3) BACKEND_LANG="python" ;;
        *) BACKEND_LANG="erlang" ;;
    esac
    
    # Database provider selection
    print_color $BLUE "\n💾 Database Provider"
    echo "1) Render PostgreSQL - Integrated, managed [DEFAULT]"
    echo "2) Neon - Serverless, branching"
    read -p "Choice [1]: " db_choice
    case $db_choice in
        2) DB_PROVIDER="neon" ;;
        *) DB_PROVIDER="render" ;;
    esac
    
    if [ "$DB_PROVIDER" = "render" ]; then
        print_color $BLUE "\n💰 Database Strategy"
        echo "1) Single database with schemas - Cost-effective [DEFAULT]"
        echo "2) Separate databases - Better isolation"
        read -p "Choice [1]: " db_strategy
        case $db_strategy in
            2) DB_STRATEGY="separate" ;;
            *) DB_STRATEGY="schemas" ;;
        esac
    fi
    
    # Authentication model
    print_color $BLUE "\n🔐 Authentication Model"
    echo "1) Basic (user, admin) [DEFAULT]"
    echo "2) SaaS Standard (free, premium, enterprise, admin)"
    echo "3) Marketplace (buyer, seller, admin)"
    echo "4) Organization (owner, admin, member, viewer)"
    read -p "Choice [1]: " auth_choice
    case $auth_choice in
        2) AUTH_MODEL="saas" ;;
        3) AUTH_MODEL="marketplace" ;;
        4) AUTH_MODEL="organization" ;;
        *) AUTH_MODEL="basic" ;;
    esac
    
    # Pricing model
    print_color $BLUE "\n💳 Pricing Model"
    echo "1) Simple (Free, Pro) [DEFAULT]"
    echo "2) Tiered (Free, Starter, Pro, Enterprise)"
    echo "3) Usage-based (Pay as you go)"
    echo "4) Seat-based (Per user)"
    read -p "Choice [1]: " pricing_choice
    case $pricing_choice in
        2) PRICING_MODEL="tiered" ;;
        3) PRICING_MODEL="usage" ;;
        4) PRICING_MODEL="seat" ;;
        *) PRICING_MODEL="simple" ;;
    esac
    
    # Summary
    print_color $GREEN "\n📋 Configuration Summary:"
    echo "  Project Name: $PROJECT_NAME"
    echo "  Project Type: $PROJECT_TYPE"
    echo "  Backend Language: $BACKEND_LANG"
    echo "  Database Provider: $DB_PROVIDER"
    [ "$DB_PROVIDER" = "render" ] && echo "  Database Strategy: $DB_STRATEGY"
    echo "  Authentication Model: $AUTH_MODEL"
    echo "  Pricing Model: $PRICING_MODEL"
    
    read -p "\nProceed with this configuration? (y/n): " confirm
    if [ "$confirm" != "y" ]; then
        print_color $RED "Scaffolding cancelled"
        exit 1
    fi
}

# Save configuration
save_configuration() {
    local config_file=".scaffold-config.json"
    cat > "$config_file" << EOF
{
  "projectName": "$PROJECT_NAME",
  "projectType": "$PROJECT_TYPE",
  "backendLanguage": "$BACKEND_LANG",
  "databaseProvider": "$DB_PROVIDER",
  "databaseStrategy": "${DB_STRATEGY:-separate}",
  "authenticationModel": "$AUTH_MODEL",
  "pricingModel": "$PRICING_MODEL",
  "createdAt": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")"
}
EOF
    print_color $GREEN "✅ Configuration saved to $config_file"
}

# Execute scaffolding with Claude Code
execute_scaffolding() {
    print_color $BLUE "\n🚀 Starting scaffolding with Claude Code..."
    
    # Create a prompt file for Claude Code
    local prompt_file=".scaffold-prompt.md"
    cat > "$prompt_file" << EOF
# Scaffold Project: $PROJECT_NAME

Please execute the scaffolding workflow using the following configuration:

## Configuration
- Project Name: $PROJECT_NAME
- Project Type: $PROJECT_TYPE
- Backend Language: $BACKEND_LANG
- Database Provider: $DB_PROVIDER
- Database Strategy: ${DB_STRATEGY:-separate}
- Authentication Model: $AUTH_MODEL
- Pricing Model: $PRICING_MODEL

## Instructions
1. Follow the scaffolding workflow in scaffold/CLAUDE.md
2. Create all necessary services in Render
3. Setup Auth0 applications and roles
4. Configure Stripe products and webhooks
5. Generate appropriate code templates
6. Setup GitHub Actions workflows
7. Configure all environment variables
8. Initialize the database
9. Run validation tests

## Available Tools
- Render MCP is configured
- GitHub MCP is configured
- Stripe MCP is configured
- Playwright MCP is configured for Auth0 automation

Please proceed with the scaffolding and provide status updates as you progress.
EOF
    
    # Check if Claude Code is already running with --resume
    if pgrep -f "claude-code" > /dev/null; then
        print_color $YELLOW "Claude Code is already running. Please paste the following in the conversation:"
        print_color $BLUE "---"
        cat "$prompt_file"
        print_color $BLUE "---"
    else
        # Start Claude Code with the prompt
        print_color $GREEN "Starting Claude Code with scaffolding instructions..."
        
        # Check if we should resume a previous session
        if [ -f ".claude-session" ]; then
            read -p "Resume previous Claude Code session? (y/n): " resume
            if [ "$resume" = "y" ]; then
                claude-code --resume
            else
                claude-code --prompt "@$prompt_file"
            fi
        else
            claude-code --prompt "@$prompt_file"
        fi
    fi
}

# Validate scaffolding
validate_scaffolding() {
    print_color $BLUE "\n✅ Validating scaffolding..."
    
    local validation_passed=true
    
    # Check services
    print_color $BLUE "  Checking Render services..."
    if render services list | grep -q "$PROJECT_NAME"; then
        print_color $GREEN "    ✅ Render services created"
    else
        print_color $RED "    ❌ Render services not found"
        validation_passed=false
    fi
    
    # Check Auth0
    print_color $BLUE "  Checking Auth0 applications..."
    if auth0 apps list | grep -q "$PROJECT_NAME"; then
        print_color $GREEN "    ✅ Auth0 applications created"
    else
        print_color $RED "    ❌ Auth0 applications not found"
        validation_passed=false
    fi
    
    # Check Stripe
    print_color $BLUE "  Checking Stripe products..."
    if stripe products list | grep -q "$PROJECT_NAME"; then
        print_color $GREEN "    ✅ Stripe products created"
    else
        print_color $RED "    ❌ Stripe products not found"
        validation_passed=false
    fi
    
    # Check GitHub
    print_color $BLUE "  Checking GitHub repository..."
    if gh repo view "$PROJECT_NAME" &>/dev/null; then
        print_color $GREEN "    ✅ GitHub repository exists"
    else
        print_color $RED "    ❌ GitHub repository not found"
        validation_passed=false
    fi
    
    # Check local files
    print_color $BLUE "  Checking generated files..."
    local required_files=(".env.local" ".github/workflows/deploy-staging.yml" ".github/workflows/deploy-production.yml")
    for file in "${required_files[@]}"; do
        if [ -f "$file" ]; then
            print_color $GREEN "    ✅ $file exists"
        else
            print_color $RED "    ❌ $file not found"
            validation_passed=false
        fi
    done
    
    if [ "$validation_passed" = true ]; then
        print_color $GREEN "\n🎉 Scaffolding completed successfully!"
        print_color $BLUE "\n📚 Next steps:"
        echo "  1. Review generated code and customize"
        echo "  2. Update branding and styling"
        echo "  3. Add business logic"
        echo "  4. Configure custom domain"
        echo "  5. Set up monitoring"
        
        print_color $BLUE "\n🔗 Service URLs:"
        echo "  Frontend Staging: https://$PROJECT_NAME-frontend-staging.onrender.com"
        echo "  Frontend Production: https://$PROJECT_NAME-frontend.onrender.com"
        echo "  API Staging: https://$PROJECT_NAME-api-staging.onrender.com"
        echo "  API Production: https://$PROJECT_NAME-api.onrender.com"
    else
        print_color $RED "\n⚠️  Scaffolding validation failed. Please check the errors above."
    fi
}

# Main execution
main() {
    print_color $BLUE "🚀 Claude Code Scaffolding System v1.0"
    print_color $BLUE "======================================"
    
    # Parse command line arguments
    case "${1:-}" in
        new)
            PROJECT_TYPE="new"
            PROJECT_NAME="${2:-}"
            if [ -z "$PROJECT_NAME" ]; then
                print_color $RED "Error: Project name required for 'new' command"
                echo "Usage: $0 new <project-name>"
                exit 1
            fi
            mkdir -p "$PROJECT_NAME"
            cd "$PROJECT_NAME"
            ;;
        existing)
            PROJECT_TYPE="existing"
            ;;
        status)
            validate_scaffolding
            exit 0
            ;;
        help|--help|-h)
            echo "Usage: $0 [command] [options]"
            echo ""
            echo "Commands:"
            echo "  new <name>    Create a new project with scaffolding"
            echo "  existing      Add scaffolding to existing project"
            echo "  status        Check status of scaffolded services"
            echo "  help          Show this help message"
            exit 0
            ;;
        *)
            detect_project_type
            ;;
    esac
    
    # Run scaffolding workflow
    check_prerequisites
    get_project_config
    save_configuration
    execute_scaffolding
    
    # Wait for user to complete Claude Code scaffolding
    print_color $YELLOW "\n⏳ Waiting for Claude Code to complete scaffolding..."
    print_color $YELLOW "Press Enter when scaffolding is complete..."
    read
    
    validate_scaffolding
}

# Run main function
main "$@"