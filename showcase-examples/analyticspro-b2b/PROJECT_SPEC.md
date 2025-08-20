# AnalyticsPro B2B - Enterprise Analytics Platform

## Overview
A comprehensive B2B analytics platform that helps enterprises track, analyze, and visualize their business metrics with advanced reporting, real-time dashboards, and API analytics.

## Tech Stack
- **Backend**: Python with FastAPI
- **Frontend**: Next.js 14 with TypeScript
- **Database**: PostgreSQL with TimescaleDB extension (time-series data)
- **Authentication**: Auth0 with enterprise SSO support
- **Payments**: Stripe with usage-based billing
- **Cache**: Redis for real-time analytics
- **Queue**: Celery for background processing
- **Deployment**: Render (staging + production)

## Features

### Core Functionality
- **Real-time Analytics**: Live dashboards with WebSocket updates
- **Custom Metrics**: Define and track custom business metrics
- **API Analytics**: Monitor API usage, performance, and errors
- **Data Visualization**: Interactive charts, graphs, and reports
- **Alerting System**: Custom alerts based on metric thresholds
- **Data Export**: CSV, PDF, and API data exports

### Enterprise Features
- **SSO Integration**: SAML, OIDC, and Active Directory
- **White-label Solution**: Custom branding and domains
- **Multi-tenant Architecture**: Isolated data per organization
- **Advanced Security**: Role-based access, audit logs, SOC2 compliance
- **Custom Integrations**: Webhook support, API connectors

### User Roles
- **Owner**: Full platform access, billing management
- **Admin**: User management, configuration, reporting
- **Analyst**: Dashboard creation, data analysis
- **Viewer**: Read-only access to assigned dashboards

### Pricing Tiers
- **Starter ($99/month)**: Up to 100K events/month, 5 users
- **Professional ($299/month)**: Up to 1M events/month, 25 users
- **Enterprise ($999/month)**: Up to 10M events/month, unlimited users
- **Custom**: Usage-based pricing for high-volume customers

## Database Schema

### Organizations Table
```sql
CREATE TABLE organizations (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  name VARCHAR(255) NOT NULL,
  domain VARCHAR(255) UNIQUE,
  subscription_tier VARCHAR(50) DEFAULT 'starter',
  monthly_event_limit INTEGER DEFAULT 100000,
  events_used_this_month INTEGER DEFAULT 0,
  billing_cycle_start DATE,
  stripe_subscription_id VARCHAR(255),
  custom_branding JSONB,
  sso_config JSONB,
  created_at TIMESTAMP DEFAULT NOW()
);
```

### Events Table (TimescaleDB)
```sql
CREATE TABLE events (
  id UUID DEFAULT gen_random_uuid(),
  org_id UUID REFERENCES organizations(id),
  event_name VARCHAR(255) NOT NULL,
  properties JSONB,
  user_id VARCHAR(255),
  session_id VARCHAR(255),
  timestamp TIMESTAMPTZ DEFAULT NOW(),
  ip_address INET,
  user_agent TEXT,
  
  PRIMARY KEY (id, timestamp)
) PARTITION BY RANGE (timestamp);

-- Convert to hypertable for time-series optimization
SELECT create_hypertable('events', 'timestamp');
```

### Dashboards Table
```sql
CREATE TABLE dashboards (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  org_id UUID REFERENCES organizations(id),
  name VARCHAR(255) NOT NULL,
  description TEXT,
  config JSONB NOT NULL, -- Chart configurations
  created_by UUID REFERENCES users(id),
  is_public BOOLEAN DEFAULT false,
  tags VARCHAR(100)[],
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW()
);
```

### Alerts Table
```sql
CREATE TABLE alerts (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  org_id UUID REFERENCES organizations(id),
  name VARCHAR(255) NOT NULL,
  metric_query TEXT NOT NULL,
  condition_type VARCHAR(50), -- 'above', 'below', 'equals'
  threshold_value DECIMAL,
  notification_channels JSONB, -- email, slack, webhook
  is_active BOOLEAN DEFAULT true,
  last_triggered_at TIMESTAMP,
  created_by UUID REFERENCES users(id),
  created_at TIMESTAMP DEFAULT NOW()
);
```

### API Keys Table
```sql
CREATE TABLE api_keys (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  org_id UUID REFERENCES organizations(id),
  key_hash VARCHAR(255) UNIQUE NOT NULL,
  name VARCHAR(255) NOT NULL,
  permissions VARCHAR(100)[], -- 'read', 'write', 'admin'
  last_used_at TIMESTAMP,
  expires_at TIMESTAMP,
  created_by UUID REFERENCES users(id),
  created_at TIMESTAMP DEFAULT NOW()
);
```

## API Endpoints

### Authentication & Organizations
- `POST /api/auth/callback` - Auth0 callback
- `GET /api/auth/user` - Get current user
- `GET /api/organizations` - List user's organizations
- `POST /api/organizations` - Create new organization
- `PUT /api/organizations/:id` - Update organization

### Event Tracking
- `POST /api/events` - Track single event
- `POST /api/events/batch` - Track multiple events
- `GET /api/events` - Query events with filters
- `GET /api/events/stats` - Get event statistics

### Analytics & Metrics
- `GET /api/analytics/metrics` - Get metric values
- `POST /api/analytics/query` - Execute custom analytics query
- `GET /api/analytics/funnel` - Funnel analysis
- `GET /api/analytics/retention` - Retention analysis
- `GET /api/analytics/segmentation` - User segmentation

### Dashboards
- `GET /api/dashboards` - List dashboards
- `POST /api/dashboards` - Create dashboard
- `PUT /api/dashboards/:id` - Update dashboard
- `DELETE /api/dashboards/:id` - Delete dashboard
- `GET /api/dashboards/:id/data` - Get dashboard data

### Alerts
- `GET /api/alerts` - List alerts
- `POST /api/alerts` - Create alert
- `PUT /api/alerts/:id` - Update alert
- `DELETE /api/alerts/:id` - Delete alert
- `POST /api/alerts/:id/test` - Test alert

### Billing & Usage
- `GET /api/billing/usage` - Get current usage
- `GET /api/billing/subscription` - Get subscription details
- `POST /api/billing/upgrade` - Upgrade subscription
- `GET /api/billing/invoices` - List invoices

## FastAPI Application Structure

### Main Application
```python
from fastapi import FastAPI, Depends, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from fastapi.middleware.trustedhost import TrustedHostMiddleware
from contextlib import asynccontextmanager
import asyncpg
import redis.asyncio as redis

@asynccontextmanager
async def lifespan(app: FastAPI):
    # Startup
    app.state.db_pool = await asyncpg.create_pool(DATABASE_URL)
    app.state.redis = await redis.from_url(REDIS_URL)
    
    yield
    
    # Shutdown
    await app.state.db_pool.close()
    await app.state.redis.close()

app = FastAPI(
    title="AnalyticsPro API",
    description="Enterprise Analytics Platform",
    version="1.0.0",
    lifespan=lifespan
)

app.add_middleware(
    CORSMiddleware,
    allow_origins=["https://analyticspro.com"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Include routers
app.include_router(auth_router, prefix="/api/auth", tags=["auth"])
app.include_router(events_router, prefix="/api/events", tags=["events"])
app.include_router(analytics_router, prefix="/api/analytics", tags=["analytics"])
app.include_router(dashboards_router, prefix="/api/dashboards", tags=["dashboards"])
app.include_router(alerts_router, prefix="/api/alerts", tags=["alerts"])
```

### Event Tracking Service
```python
from typing import List, Dict, Any
from datetime import datetime
import asyncpg
import asyncio

class EventService:
    def __init__(self, db_pool: asyncpg.Pool, redis_client):
        self.db_pool = db_pool
        self.redis = redis_client
    
    async def track_event(
        self,
        org_id: str,
        event_name: str,
        properties: Dict[str, Any],
        user_id: str = None,
        session_id: str = None,
        timestamp: datetime = None
    ) -> bool:
        """Track a single event"""
        
        # Check event quota
        if not await self._check_event_quota(org_id):
            raise HTTPException(403, "Monthly event limit reached")
        
        # Insert event
        async with self.db_pool.acquire() as conn:
            await conn.execute("""
                INSERT INTO events (org_id, event_name, properties, user_id, session_id, timestamp)
                VALUES ($1, $2, $3, $4, $5, $6)
            """, org_id, event_name, properties, user_id, session_id, timestamp or datetime.utcnow())
        
        # Update usage counter
        await self._increment_usage(org_id)
        
        # Real-time updates
        await self._publish_event_update(org_id, event_name)
        
        return True
    
    async def track_events_batch(self, org_id: str, events: List[Dict]) -> int:
        """Track multiple events efficiently"""
        
        if not await self._check_event_quota(org_id, len(events)):
            raise HTTPException(403, "Batch would exceed monthly event limit")
        
        # Batch insert
        async with self.db_pool.acquire() as conn:
            await conn.executemany("""
                INSERT INTO events (org_id, event_name, properties, user_id, session_id, timestamp)
                VALUES ($1, $2, $3, $4, $5, $6)
            """, [
                (org_id, event['name'], event.get('properties', {}), 
                 event.get('user_id'), event.get('session_id'), 
                 event.get('timestamp', datetime.utcnow()))
                for event in events
            ])
        
        await self._increment_usage(org_id, len(events))
        return len(events)
    
    async def query_events(
        self,
        org_id: str,
        event_names: List[str] = None,
        start_time: datetime = None,
        end_time: datetime = None,
        filters: Dict = None,
        limit: int = 1000
    ) -> List[Dict]:
        """Query events with filters"""
        
        query = "SELECT * FROM events WHERE org_id = $1"
        params = [org_id]
        
        if event_names:
            query += f" AND event_name = ANY($2)"
            params.append(event_names)
        
        if start_time:
            query += f" AND timestamp >= ${len(params) + 1}"
            params.append(start_time)
        
        if end_time:
            query += f" AND timestamp <= ${len(params) + 1}"
            params.append(end_time)
        
        # Add property filters
        if filters:
            for key, value in filters.items():
                query += f" AND properties->>${len(params) + 1} = ${len(params) + 2}"
                params.extend([key, str(value)])
        
        query += f" ORDER BY timestamp DESC LIMIT ${len(params) + 1}"
        params.append(limit)
        
        async with self.db_pool.acquire() as conn:
            rows = await conn.fetch(query, *params)
            return [dict(row) for row in rows]
```

## Frontend Components

### Real-time Dashboard
```typescript
// components/RealTimeDashboard.tsx
import { useEffect, useState } from 'react';
import { Chart } from 'react-chartjs-2';
import { useWebSocket } from '../hooks/useWebSocket';

interface DashboardProps {
  dashboardId: string;
  organizationId: string;
}

export const RealTimeDashboard: React.FC<DashboardProps> = ({
  dashboardId,
  organizationId
}) => {
  const [dashboardData, setDashboardData] = useState(null);
  const [isLoading, setIsLoading] = useState(true);
  
  // WebSocket for real-time updates
  const { lastMessage, connectionStatus } = useWebSocket(
    `wss://api.analyticspro.com/ws/dashboard/${dashboardId}`
  );
  
  useEffect(() => {
    fetchDashboardData();
  }, [dashboardId]);
  
  useEffect(() => {
    if (lastMessage) {
      const update = JSON.parse(lastMessage.data);
      updateDashboardData(update);
    }
  }, [lastMessage]);
  
  const fetchDashboardData = async () => {
    try {
      const response = await fetch(`/api/dashboards/${dashboardId}/data`);
      const data = await response.json();
      setDashboardData(data);
    } catch (error) {
      console.error('Failed to fetch dashboard:', error);
    } finally {
      setIsLoading(false);
    }
  };
  
  const updateDashboardData = (update: any) => {
    setDashboardData(prev => ({
      ...prev,
      metrics: {
        ...prev?.metrics,
        [update.metric]: update.value
      }
    }));
  };
  
  if (isLoading) return <DashboardSkeleton />;
  
  return (
    <div className="dashboard-container">
      <div className="dashboard-header">
        <h1>{dashboardData.name}</h1>
        <div className="connection-status">
          <span className={`status-indicator ${connectionStatus}`} />
          {connectionStatus === 'connected' ? 'Live' : 'Offline'}
        </div>
      </div>
      
      <div className="dashboard-grid">
        {dashboardData.widgets.map(widget => (
          <DashboardWidget 
            key={widget.id} 
            widget={widget}
            data={dashboardData.metrics[widget.metric]}
          />
        ))}
      </div>
    </div>
  );
};
```

## Deployment Configuration

### Environment Variables
```bash
# Database
DATABASE_URL=postgresql://...
REDIS_URL=redis://...

# Auth0
AUTH0_DOMAIN=analyticspro.auth0.com
AUTH0_CLIENT_ID=...
AUTH0_CLIENT_SECRET=...
AUTH0_AUDIENCE=https://api.analyticspro.com

# Stripe
STRIPE_SECRET_KEY=sk_live_...
STRIPE_WEBHOOK_SECRET=whsec_...

# Application
API_URL=https://analyticspro-api.onrender.com
FRONTEND_URL=https://analyticspro.com
ENVIRONMENT=production

# Background Tasks
CELERY_BROKER_URL=redis://...
CELERY_RESULT_BACKEND=redis://...

# Monitoring
SENTRY_DSN=...
```

### Render Services
- **Frontend**: `analyticspro-frontend` (static site)
- **Backend**: `analyticspro-api` (web service)
- **Worker**: `analyticspro-worker` (background service)
- **Database**: `analyticspro-db` (PostgreSQL with TimescaleDB)
- **Redis**: `analyticspro-redis` (Redis)

## Success Metrics
- Monthly recurring revenue (MRR)
- Customer acquisition cost (CAC)
- Event volume and processing latency
- Dashboard engagement metrics
- Customer retention rate
- Average revenue per user (ARPU)

## Competitive Advantages
- **Real-time Processing**: Sub-second analytics updates
- **Enterprise-Ready**: SOC2, GDPR compliance built-in
- **Flexible Pricing**: Usage-based model scales with growth
- **White-label Solution**: Complete customization options
- **Advanced Analytics**: ML-powered insights and predictions

## Development Timeline
- **Setup & Scaffolding**: 15 minutes
- **Backend API Development**: 6-8 hours
- **Frontend Dashboard**: 5-6 hours
- **Real-time Features**: 3-4 hours
- **Enterprise Features**: 4-5 hours
- **Testing & Deployment**: 2 hours

**Total Development Time**: 20-25 hours from scaffold to production