# TaskFlow SaaS - Team Management Platform

## Overview
A comprehensive team management and task tracking SaaS application built with Erlang/OTP backend and Next.js frontend.

## Tech Stack
- **Backend**: Erlang/OTP with Cowboy
- **Frontend**: Next.js 14 with TypeScript
- **Database**: PostgreSQL (Render or Neon)
- **Authentication**: Auth0 with role-based access
- **Payments**: Stripe with tiered subscriptions
- **Deployment**: Render (staging + production)

## Features

### Core Functionality
- **Project Management**: Create, organize, and track projects
- **Task Management**: Assign tasks, set deadlines, track progress
- **Team Collaboration**: Team chat, file sharing, comments
- **Time Tracking**: Built-in time tracking for tasks and projects
- **Reporting**: Analytics dashboard, time reports, productivity insights

### User Roles
- **Owner**: Full access, billing management
- **Admin**: Team management, project oversight
- **Member**: Task management, collaboration
- **Viewer**: Read-only access to assigned projects

### Subscription Tiers
- **Free**: Up to 3 team members, 5 projects
- **Pro ($19/month)**: Up to 25 team members, unlimited projects
- **Business ($49/month)**: Unlimited team members, advanced analytics
- **Enterprise ($99/month)**: SSO, custom integrations, priority support

## Database Schema

### Users Table
```sql
CREATE TABLE users (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  email VARCHAR(255) UNIQUE NOT NULL,
  auth0_id VARCHAR(255) UNIQUE,
  role VARCHAR(50) DEFAULT 'member',
  stripe_customer_id VARCHAR(255),
  created_at TIMESTAMP DEFAULT NOW()
);
```

### Organizations Table
```sql
CREATE TABLE organizations (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  name VARCHAR(255) NOT NULL,
  owner_id UUID REFERENCES users(id),
  subscription_tier VARCHAR(50) DEFAULT 'free',
  stripe_subscription_id VARCHAR(255),
  created_at TIMESTAMP DEFAULT NOW()
);
```

### Projects Table
```sql
CREATE TABLE projects (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  org_id UUID REFERENCES organizations(id),
  name VARCHAR(255) NOT NULL,
  description TEXT,
  status VARCHAR(50) DEFAULT 'active',
  created_by UUID REFERENCES users(id),
  created_at TIMESTAMP DEFAULT NOW()
);
```

### Tasks Table
```sql
CREATE TABLE tasks (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  project_id UUID REFERENCES projects(id),
  title VARCHAR(255) NOT NULL,
  description TEXT,
  assigned_to UUID REFERENCES users(id),
  status VARCHAR(50) DEFAULT 'todo',
  priority VARCHAR(20) DEFAULT 'medium',
  due_date TIMESTAMP,
  created_at TIMESTAMP DEFAULT NOW()
);
```

## API Endpoints

### Authentication
- `POST /api/auth/callback` - Auth0 callback
- `GET /api/auth/user` - Get current user
- `POST /api/auth/logout` - Logout user

### Organizations
- `GET /api/organizations` - List user's organizations
- `POST /api/organizations` - Create new organization
- `PUT /api/organizations/:id` - Update organization
- `DELETE /api/organizations/:id` - Delete organization

### Projects
- `GET /api/projects` - List projects in organization
- `POST /api/projects` - Create new project
- `PUT /api/projects/:id` - Update project
- `DELETE /api/projects/:id` - Delete project

### Tasks
- `GET /api/tasks` - List tasks in project
- `POST /api/tasks` - Create new task
- `PUT /api/tasks/:id` - Update task
- `DELETE /api/tasks/:id` - Delete task

### Billing
- `GET /api/billing/subscription` - Get subscription details
- `POST /api/billing/create-checkout` - Create Stripe checkout
- `POST /api/billing/webhook` - Stripe webhook handler

## Frontend Components

### Pages
- `/` - Landing page with pricing
- `/dashboard` - Main dashboard
- `/projects/:id` - Project detail view
- `/tasks` - Task management
- `/team` - Team management
- `/billing` - Subscription management
- `/settings` - Organization settings

### Components
- `ProjectCard` - Project overview component
- `TaskList` - Task management interface
- `TeamMemberList` - Team member management
- `SubscriptionManager` - Billing and subscription
- `TimeTracker` - Time tracking widget

## Deployment Configuration

### Environment Variables
```bash
# Database
DATABASE_URL=postgresql://...

# Auth0
NEXT_PUBLIC_AUTH0_DOMAIN=taskflow.auth0.com
NEXT_PUBLIC_AUTH0_CLIENT_ID=...
AUTH0_CLIENT_SECRET=...

# Stripe
NEXT_PUBLIC_STRIPE_PUBLISHABLE_KEY=pk_test_...
STRIPE_SECRET_KEY=sk_test_...
STRIPE_WEBHOOK_SECRET=whsec_...

# App
NEXT_PUBLIC_APP_URL=https://taskflow-saas.onrender.com
API_URL=https://taskflow-api.onrender.com
```

### Render Services
- **Frontend**: `taskflow-frontend` (static site)
- **Backend**: `taskflow-api` (web service)
- **Database**: `taskflow-db` (PostgreSQL)

## Success Metrics
- User sign-ups and activation rate
- Team adoption and collaboration metrics
- Task completion rates
- Subscription conversion and retention
- Average revenue per user (ARPU)

## Development Timeline
- **Setup & Scaffolding**: 15 minutes
- **Backend API Development**: 2-3 hours
- **Frontend Implementation**: 3-4 hours
- **Authentication Integration**: 1 hour
- **Payment Integration**: 1 hour
- **Testing & Deployment**: 1 hour

**Total Development Time**: 8-10 hours from scaffold to production