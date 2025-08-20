#!/bin/bash

# Auth0 Setup Script
# Complete Auth0 configuration for staging and production

set -e

# Configuration
PROJECT_NAME="${1:-myapp}"
AUTH0_DOMAIN="${AUTH0_DOMAIN:-}"
FRONTEND_URL_PROD="${2:-https://$PROJECT_NAME.onrender.com}"
FRONTEND_URL_STAGING="${3:-https://$PROJECT_NAME-staging.onrender.com}"
API_IDENTIFIER="${4:-https://api.$PROJECT_NAME.com}"

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

# Check Auth0 CLI is installed and authenticated
check_auth0() {
    if ! command -v auth0 &> /dev/null; then
        print_color $RED "Auth0 CLI not found. Please install it first."
        exit 1
    fi
    
    if ! auth0 whoami &> /dev/null; then
        print_color $YELLOW "Auth0 CLI not authenticated. Running login..."
        auth0 login
    fi
    
    # Get tenant domain
    AUTH0_DOMAIN=$(auth0 tenants list --json | jq -r '.[0].domain')
    print_color $GREEN "Using Auth0 domain: $AUTH0_DOMAIN"
}

# Create Auth0 API
create_api() {
    print_color $BLUE "Creating Auth0 API..."
    
    auth0 apis create \
        --name "$PROJECT_NAME API" \
        --identifier "$API_IDENTIFIER" \
        --scopes "read:all write:all delete:all admin:all" \
        --token-lifetime 86400 \
        --offline-access true \
        --json > auth0-api.json
    
    API_ID=$(jq -r '.id' auth0-api.json)
    print_color $GREEN "✅ API created with ID: $API_ID"
}

# Create Auth0 Applications
create_applications() {
    # Production Application
    print_color $BLUE "Creating Production Application..."
    
    auth0 apps create \
        --name "$PROJECT_NAME Production" \
        --type spa \
        --auth-method none \
        --callbacks "$FRONTEND_URL_PROD/auth/callback,$FRONTEND_URL_PROD/api/auth/callback" \
        --logout-urls "$FRONTEND_URL_PROD" \
        --origins "$FRONTEND_URL_PROD" \
        --web-origins "$FRONTEND_URL_PROD" \
        --json > auth0-app-prod.json
    
    PROD_CLIENT_ID=$(jq -r '.client_id' auth0-app-prod.json)
    print_color $GREEN "✅ Production app created with Client ID: $PROD_CLIENT_ID"
    
    # Staging Application
    print_color $BLUE "Creating Staging Application..."
    
    auth0 apps create \
        --name "$PROJECT_NAME Staging" \
        --type spa \
        --auth-method none \
        --callbacks "$FRONTEND_URL_STAGING/auth/callback,$FRONTEND_URL_STAGING/api/auth/callback" \
        --logout-urls "$FRONTEND_URL_STAGING" \
        --origins "$FRONTEND_URL_STAGING" \
        --web-origins "$FRONTEND_URL_STAGING" \
        --json > auth0-app-staging.json
    
    STAGING_CLIENT_ID=$(jq -r '.client_id' auth0-app-staging.json)
    print_color $GREEN "✅ Staging app created with Client ID: $STAGING_CLIENT_ID"
    
    # Machine-to-Machine Application (for backend)
    print_color $BLUE "Creating M2M Application..."
    
    auth0 apps create \
        --name "$PROJECT_NAME Backend" \
        --type m2m \
        --description "Backend service for API access" \
        --json > auth0-app-m2m.json
    
    M2M_CLIENT_ID=$(jq -r '.client_id' auth0-app-m2m.json)
    M2M_CLIENT_SECRET=$(jq -r '.client_secret' auth0-app-m2m.json)
    print_color $GREEN "✅ M2M app created"
    
    # Grant M2M app access to API
    auth0 apps update $M2M_CLIENT_ID \
        --grants "client_credentials" \
        --scopes "read:all write:all delete:all admin:all"
}

# Create Auth0 Roles
create_roles() {
    print_color $BLUE "Creating Auth0 Roles..."
    
    # Admin Role
    auth0 roles create \
        --name "admin" \
        --description "Administrator with full access" \
        --json > auth0-role-admin.json
    
    ADMIN_ROLE_ID=$(jq -r '.id' auth0-role-admin.json)
    
    # User Role
    auth0 roles create \
        --name "user" \
        --description "Standard user with limited access" \
        --json > auth0-role-user.json
    
    USER_ROLE_ID=$(jq -r '.id' auth0-role-user.json)
    
    # Premium User Role (for SaaS)
    auth0 roles create \
        --name "premium_user" \
        --description "Premium user with enhanced features" \
        --json > auth0-role-premium.json
    
    PREMIUM_ROLE_ID=$(jq -r '.id' auth0-role-premium.json)
    
    print_color $GREEN "✅ Roles created"
}

# Create Auth0 Rules/Actions
create_actions() {
    print_color $BLUE "Creating Auth0 Actions..."
    
    # Create action to add roles to tokens
    cat > add-roles-action.js << 'EOF'
exports.onExecutePostLogin = async (event, api) => {
    const namespace = 'https://myapp.com';
    const assignedRoles = (event.authorization || {}).roles || [];
    
    // Add roles to ID token
    api.idToken.setCustomClaim(`${namespace}/roles`, assignedRoles);
    
    // Add roles to Access token
    api.accessToken.setCustomClaim(`${namespace}/roles`, assignedRoles);
    
    // Add user metadata
    api.idToken.setCustomClaim(`${namespace}/user_metadata`, event.user.user_metadata);
    api.accessToken.setCustomClaim(`${namespace}/user_metadata`, event.user.user_metadata);
};
EOF
    
    # Deploy the action
    auth0 actions create \
        --name "Add Roles to Token" \
        --trigger "post-login" \
        --code @add-roles-action.js \
        --json > auth0-action-roles.json
    
    # Create action to sync with Stripe
    cat > stripe-sync-action.js << 'EOF'
exports.onExecutePostLogin = async (event, api) => {
    // Only sync on first login
    if (event.stats.logins_count !== 1) {
        return;
    }
    
    // Create Stripe customer
    const stripe = require('stripe')(event.secrets.STRIPE_SECRET_KEY);
    
    try {
        const customer = await stripe.customers.create({
            email: event.user.email,
            name: event.user.name,
            metadata: {
                auth0_id: event.user.user_id
            }
        });
        
        // Store Stripe customer ID in user metadata
        api.user.setUserMetadata('stripe_customer_id', customer.id);
    } catch (error) {
        console.error('Failed to create Stripe customer:', error);
    }
};
EOF
    
    auth0 actions create \
        --name "Sync with Stripe" \
        --trigger "post-login" \
        --code @stripe-sync-action.js \
        --dependencies "stripe@latest" \
        --secret "STRIPE_SECRET_KEY=$STRIPE_SECRET_KEY" \
        --json > auth0-action-stripe.json
    
    print_color $GREEN "✅ Actions created"
}

# Create Auth0 Database Connection
create_database_connection() {
    print_color $BLUE "Creating Database Connection..."
    
    auth0 connections create \
        --name "Username-Password-Authentication" \
        --strategy "auth0" \
        --enabled-clients "$PROD_CLIENT_ID,$STAGING_CLIENT_ID" \
        --json > auth0-connection.json
    
    print_color $GREEN "✅ Database connection created"
}

# Create test users
create_test_users() {
    print_color $BLUE "Creating test users..."
    
    # Admin user
    auth0 users create \
        --email "admin@$PROJECT_NAME.com" \
        --password "Admin123!@#" \
        --connection "Username-Password-Authentication" \
        --name "Admin User" \
        --json > auth0-user-admin.json
    
    ADMIN_USER_ID=$(jq -r '.user_id' auth0-user-admin.json)
    
    # Assign admin role
    auth0 users roles assign $ADMIN_USER_ID --roles $ADMIN_ROLE_ID
    
    # Regular user
    auth0 users create \
        --email "user@$PROJECT_NAME.com" \
        --password "User123!@#" \
        --connection "Username-Password-Authentication" \
        --name "Test User" \
        --json > auth0-user-test.json
    
    TEST_USER_ID=$(jq -r '.user_id' auth0-user-test.json)
    
    # Assign user role
    auth0 users roles assign $TEST_USER_ID --roles $USER_ROLE_ID
    
    print_color $GREEN "✅ Test users created"
}

# Configure Auth0 tenant settings
configure_tenant() {
    print_color $BLUE "Configuring tenant settings..."
    
    # Update tenant settings
    auth0 tenants update \
        --friendly-name "$PROJECT_NAME" \
        --support-email "support@$PROJECT_NAME.com" \
        --support-url "https://$PROJECT_NAME.com/support" \
        --session-lifetime 168 \
        --idle-session-lifetime 72
    
    # Configure email templates
    auth0 email-templates update "verify_email" \
        --enabled true \
        --from "noreply@$PROJECT_NAME.com" \
        --subject "Verify your email for $PROJECT_NAME" \
        --body "<html><body><h1>Welcome to $PROJECT_NAME!</h1><p>Please verify your email by clicking <a href='{{ url }}'>here</a>.</p></body></html>"
    
    auth0 email-templates update "reset_email" \
        --enabled true \
        --from "noreply@$PROJECT_NAME.com" \
        --subject "Reset your password for $PROJECT_NAME" \
        --body "<html><body><h1>Password Reset</h1><p>Click <a href='{{ url }}'>here</a> to reset your password.</p></body></html>"
    
    print_color $GREEN "✅ Tenant configured"
}

# Generate environment variables file
generate_env_file() {
    print_color $BLUE "Generating environment variables..."
    
    cat > auth0-env-vars.sh << EOF
# Auth0 Environment Variables
# Source this file or copy to your .env

# Production
export AUTH0_DOMAIN="$AUTH0_DOMAIN"
export AUTH0_CLIENT_ID_PROD="$PROD_CLIENT_ID"
export AUTH0_AUDIENCE="$API_IDENTIFIER"
export AUTH0_CALLBACK_URL_PROD="$FRONTEND_URL_PROD/auth/callback"

# Staging
export AUTH0_CLIENT_ID_STAGING="$STAGING_CLIENT_ID"
export AUTH0_CALLBACK_URL_STAGING="$FRONTEND_URL_STAGING/auth/callback"

# Backend (M2M)
export AUTH0_M2M_CLIENT_ID="$M2M_CLIENT_ID"
export AUTH0_M2M_CLIENT_SECRET="$M2M_CLIENT_SECRET"

# Test Users
export AUTH0_TEST_USER="user@$PROJECT_NAME.com"
export AUTH0_TEST_PASSWORD="User123!@#"
export AUTH0_TEST_ADMIN="admin@$PROJECT_NAME.com"
export AUTH0_TEST_ADMIN_PASSWORD="Admin123!@#"
EOF
    
    print_color $GREEN "✅ Environment variables saved to auth0-env-vars.sh"
}

# Main execution
main() {
    print_color $BLUE "🔐 Auth0 Setup for $PROJECT_NAME"
    print_color $BLUE "=================================="
    
    check_auth0
    create_api
    create_applications
    create_roles
    create_actions
    create_database_connection
    create_test_users
    configure_tenant
    generate_env_file
    
    print_color $GREEN "\n✅ Auth0 setup completed successfully!"
    print_color $YELLOW "\n📋 Next steps:"
    echo "1. Source the environment variables: source auth0-env-vars.sh"
    echo "2. Update your application configuration with the generated credentials"
    echo "3. Test the authentication flow with the test users"
    echo "4. Customize the Auth0 Universal Login page in the dashboard"
    echo "5. Configure social login providers if needed"
    
    print_color $BLUE "\n🔑 Credentials saved in:"
    echo "- auth0-api.json"
    echo "- auth0-app-prod.json"
    echo "- auth0-app-staging.json"
    echo "- auth0-app-m2m.json"
    echo "- auth0-env-vars.sh"
}

# Run main function
main "$@"