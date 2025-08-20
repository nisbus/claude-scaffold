# Community Platform - Organization Management & Forums

## Overview
A comprehensive community platform for organizations to manage members, facilitate discussions, share resources, and build engagement with seat-based pricing and advanced moderation tools.

## Tech Stack
- **Backend**: Erlang/OTP with Cowboy
- **Frontend**: Next.js 14 with TypeScript
- **Database**: PostgreSQL (Render or Neon)
- **Authentication**: Auth0 with organization-based roles
- **Payments**: Stripe with seat-based billing
- **Real-time**: WebSockets for live chat and notifications
- **Storage**: S3-compatible for file uploads
- **Deployment**: Render (staging + production)

## Features

### Core Functionality
- **Organization Management**: Multi-tenant community spaces
- **Member Management**: Invite, onboard, and manage community members
- **Discussion Forums**: Threaded discussions with categories
- **Resource Library**: Document sharing and knowledge base
- **Event Management**: Community events and calendar
- **Messaging**: Direct messages and group chats

### Organization Features
- **Custom Branding**: Logo, colors, and domain customization
- **Role Hierarchy**: Owner, Admin, Moderator, Member, Guest
- **Permission System**: Granular access control
- **Analytics Dashboard**: Community engagement metrics
- **Integration Hub**: Slack, Discord, and webhook integrations

### User Roles & Permissions
- **Owner**: Full platform access, billing, organization settings
- **Admin**: Member management, content moderation, analytics
- **Moderator**: Content moderation, user management in assigned areas
- **Member**: Post, comment, access private areas
- **Guest**: Read-only access to public content

### Subscription Tiers
- **Community ($29/month)**: Up to 50 members, basic features
- **Organization ($99/month)**: Up to 250 members, advanced features
- **Enterprise ($299/month)**: Up to 1000 members, full customization
- **Custom**: Unlimited members, white-label, dedicated support

## Database Schema

### Organizations Table
```sql
CREATE TABLE organizations (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  name VARCHAR(255) NOT NULL,
  slug VARCHAR(100) UNIQUE NOT NULL,
  description TEXT,
  logo_url VARCHAR(500),
  custom_domain VARCHAR(255),
  branding JSONB, -- Colors, fonts, custom CSS
  subscription_tier VARCHAR(50) DEFAULT 'community',
  member_limit INTEGER DEFAULT 50,
  current_member_count INTEGER DEFAULT 0,
  stripe_subscription_id VARCHAR(255),
  settings JSONB, -- Privacy, features, integrations
  created_at TIMESTAMP DEFAULT NOW()
);
```

### Memberships Table
```sql
CREATE TABLE memberships (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  org_id UUID REFERENCES organizations(id),
  user_id UUID REFERENCES users(id),
  role VARCHAR(50) DEFAULT 'member',
  status VARCHAR(50) DEFAULT 'active', -- active, pending, suspended
  permissions JSONB, -- Custom permissions override
  joined_at TIMESTAMP DEFAULT NOW(),
  invited_by UUID REFERENCES users(id),
  invitation_token VARCHAR(255),
  invitation_expires_at TIMESTAMP,
  
  UNIQUE(org_id, user_id)
);
```

### Forums Table
```sql
CREATE TABLE forums (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  org_id UUID REFERENCES organizations(id),
  name VARCHAR(255) NOT NULL,
  description TEXT,
  slug VARCHAR(100) NOT NULL,
  parent_id UUID REFERENCES forums(id), -- For sub-forums
  is_private BOOLEAN DEFAULT false,
  required_role VARCHAR(50) DEFAULT 'member',
  sort_order INTEGER DEFAULT 0,
  topic_count INTEGER DEFAULT 0,
  post_count INTEGER DEFAULT 0,
  last_post_at TIMESTAMP,
  created_at TIMESTAMP DEFAULT NOW(),
  
  UNIQUE(org_id, slug)
);
```

### Topics Table
```sql
CREATE TABLE topics (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  forum_id UUID REFERENCES forums(id),
  org_id UUID REFERENCES organizations(id),
  title VARCHAR(255) NOT NULL,
  slug VARCHAR(255) NOT NULL,
  content TEXT NOT NULL,
  author_id UUID REFERENCES users(id),
  is_pinned BOOLEAN DEFAULT false,
  is_locked BOOLEAN DEFAULT false,
  is_announcement BOOLEAN DEFAULT false,
  view_count INTEGER DEFAULT 0,
  reply_count INTEGER DEFAULT 0,
  last_reply_at TIMESTAMP,
  last_reply_by UUID REFERENCES users(id),
  tags VARCHAR(100)[],
  created_at TIMESTAMP DEFAULT NOW(),
  
  UNIQUE(forum_id, slug)
);
```

### Posts Table
```sql
CREATE TABLE posts (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  topic_id UUID REFERENCES topics(id),
  org_id UUID REFERENCES organizations(id),
  content TEXT NOT NULL,
  author_id UUID REFERENCES users(id),
  parent_id UUID REFERENCES posts(id), -- For threaded replies
  is_solution BOOLEAN DEFAULT false, -- Marked as solution
  attachments JSONB, -- File attachments
  reactions JSONB, -- Emoji reactions
  edit_count INTEGER DEFAULT 0,
  last_edited_at TIMESTAMP,
  last_edited_by UUID REFERENCES users(id),
  created_at TIMESTAMP DEFAULT NOW()
);
```

### Events Table
```sql
CREATE TABLE events (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  org_id UUID REFERENCES organizations(id),
  title VARCHAR(255) NOT NULL,
  description TEXT,
  event_type VARCHAR(50), -- meeting, webinar, social, etc.
  start_time TIMESTAMP NOT NULL,
  end_time TIMESTAMP NOT NULL,
  timezone VARCHAR(100),
  location VARCHAR(255), -- Physical or virtual
  meeting_url VARCHAR(500),
  max_attendees INTEGER,
  registration_required BOOLEAN DEFAULT false,
  is_public BOOLEAN DEFAULT true,
  created_by UUID REFERENCES users(id),
  created_at TIMESTAMP DEFAULT NOW()
);
```

### Resources Table
```sql
CREATE TABLE resources (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  org_id UUID REFERENCES organizations(id),
  title VARCHAR(255) NOT NULL,
  description TEXT,
  file_url VARCHAR(500),
  file_size INTEGER,
  file_type VARCHAR(100),
  category VARCHAR(100),
  tags VARCHAR(100)[],
  is_featured BOOLEAN DEFAULT false,
  download_count INTEGER DEFAULT 0,
  uploaded_by UUID REFERENCES users(id),
  required_role VARCHAR(50) DEFAULT 'member',
  created_at TIMESTAMP DEFAULT NOW()
);
```

## Erlang/OTP Backend Structure

### Application Supervisor
```erlang
-module(community_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% Database connection pool
    DbPoolSpec = #{
        id => community_db_pool,
        start => {community_db, start_pool, []},
        restart => permanent,
        shutdown => 5000,
        type => worker
    },
    
    %% WebSocket connection manager
    WsManagerSpec = #{
        id => community_ws_manager,
        start => {community_ws_manager, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker
    },
    
    %% Real-time notification system
    NotificationSpec = #{
        id => community_notifications,
        start => {community_notifications, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker
    },
    
    %% Forum post indexer (for search)
    IndexerSpec = #{
        id => community_indexer,
        start => {community_indexer, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker
    },
    
    Children = [DbPoolSpec, WsManagerSpec, NotificationSpec, IndexerSpec],
    {ok, {{one_for_all, 3, 10}, Children}}.
```

### Real-time WebSocket Handler
```erlang
-module(community_ws_handler).
-behaviour(cowboy_websocket).

-export([init/2, websocket_init/1, websocket_handle/2, 
         websocket_info/2, terminate/3]).

-record(state, {
    user_id :: binary(),
    org_id :: binary(),
    subscriptions :: [binary()]
}).

init(Req, _State) ->
    %% Verify JWT token and extract user info
    case community_auth:verify_ws_token(Req) of
        {ok, UserId, OrgId} ->
            {cowboy_websocket, Req, #state{
                user_id = UserId,
                org_id = OrgId,
                subscriptions = []
            }};
        {error, _} ->
            {ok, cowboy_req:reply(401, Req), #state{}}
    end.

websocket_init(State = #state{user_id = UserId, org_id = OrgId}) ->
    %% Register connection with manager
    community_ws_manager:register_connection(UserId, OrgId, self()),
    
    %% Subscribe to organization-wide events
    gproc:reg({p, l, {org_events, OrgId}}),
    gproc:reg({p, l, {user_events, UserId}}),
    
    {[], State}.

websocket_handle({text, Msg}, State) ->
    case jsx:decode(Msg, [return_maps]) of
        #{<<"type">> := <<"subscribe">>, <<"channel">> := Channel} ->
            handle_subscribe(Channel, State);
        #{<<"type">> := <<"unsubscribe">>, <<"channel">> := Channel} ->
            handle_unsubscribe(Channel, State);
        #{<<"type">> := <<"typing">>, <<"topic_id">> := TopicId} ->
            handle_typing(TopicId, State);
        _ ->
            {[], State}
    end.

websocket_info({notification, Event}, State) ->
    %% Send real-time notification to client
    Message = jsx:encode(Event),
    {[{text, Message}], State};

websocket_info({typing, TopicId, UserId, UserName}, State) ->
    %% Broadcast typing indicator
    Event = #{
        type => <<"typing">>,
        topic_id => TopicId,
        user_id => UserId,
        user_name => UserName
    },
    {[{text, jsx:encode(Event)}], State};

websocket_info(_Info, State) ->
    {[], State}.

terminate(_Reason, _Req, #state{user_id = UserId, org_id = OrgId}) ->
    community_ws_manager:unregister_connection(UserId, OrgId, self()),
    ok.

handle_subscribe(Channel, State = #state{subscriptions = Subs, org_id = OrgId}) ->
    %% Subscribe to specific channels (forum, topic, etc.)
    case binary:split(Channel, <<":">>, [global]) of
        [<<"forum">>, ForumId] ->
            gproc:reg({p, l, {forum_events, OrgId, ForumId}}),
            {[], State#state{subscriptions = [Channel | Subs]}};
        [<<"topic">>, TopicId] ->
            gproc:reg({p, l, {topic_events, OrgId, TopicId}}),
            {[], State#state{subscriptions = [Channel | Subs]}};
        _ ->
            {[], State}
    end.
```

### Forum Handler
```erlang
-module(community_forum_handler).
-behaviour(cowboy_handler).

-export([init/2, allowed_methods/2, content_types_provided/2, 
         content_types_accepted/2]).
-export([handle_get/2, handle_post/2, handle_put/2, handle_delete/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_get}], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, handle_post}], Req, State}.

%% GET /api/forums - List organization forums
handle_get(Req0, State) ->
    case community_auth:verify_token(Req0) of
        {ok, UserId, OrgId} ->
            %% Check if requesting specific forum
            case cowboy_req:binding(id, Req0) of
                undefined ->
                    %% List all forums user can access
                    Forums = community_db:get_accessible_forums(UserId, OrgId),
                    community_response:json(Req0, State, #{forums => Forums});
                ForumId ->
                    %% Get specific forum with topics
                    case community_auth:can_access_forum(UserId, OrgId, ForumId) of
                        true ->
                            Forum = community_db:get_forum_with_topics(ForumId, UserId),
                            community_response:json(Req0, State, #{forum => Forum});
                        false ->
                            community_response:error(Req0, State, 403, <<"Access denied">>)
                    end
            end;
        {error, _} ->
            community_response:error(Req0, State, 401, <<"Unauthorized">>)
    end.

%% POST /api/forums - Create new forum
handle_post(Req0, State) ->
    case community_auth:verify_token(Req0) of
        {ok, UserId, OrgId} ->
            case community_auth:has_permission(UserId, OrgId, create_forum) of
                true ->
                    {ok, Body, Req1} = cowboy_req:read_body(Req0),
                    case jsx:decode(Body, [return_maps]) of
                        #{<<"name">> := Name} = ForumData ->
                            ForumParams = #{
                                org_id => OrgId,
                                name => Name,
                                description => maps:get(<<"description">>, ForumData, <<"">>),
                                slug => community_utils:slugify(Name),
                                parent_id => maps:get(<<"parent_id">>, ForumData, null),
                                is_private => maps:get(<<"is_private">>, ForumData, false),
                                required_role => maps:get(<<"required_role">>, ForumData, <<"member">>),
                                created_by => UserId
                            },
                            case community_db:create_forum(ForumParams) of
                                {ok, Forum} ->
                                    %% Broadcast forum creation
                                    community_notifications:broadcast_org_event(OrgId, #{
                                        type => <<"forum_created">>,
                                        forum => Forum,
                                        created_by => UserId
                                    }),
                                    community_response:json(Req1, State, #{forum => Forum});
                                {error, Reason} ->
                                    community_response:error(Req1, State, 400, Reason)
                            end;
                        _ ->
                            community_response:error(Req1, State, 400, <<"Invalid request body">>)
                    end;
                false ->
                    community_response:error(Req0, State, 403, <<"Insufficient permissions">>)
            end;
        {error, _} ->
            community_response:error(Req0, State, 401, <<"Unauthorized">>)
    end.
```

## Frontend Components

### Organization Dashboard
```typescript
// components/OrganizationDashboard.tsx
import { useState, useEffect } from 'react';
import { useAuth0 } from '@auth0/nextjs-auth0/client';
import { ForumList } from './ForumList';
import { RecentActivity } from './RecentActivity';
import { MemberStats } from './MemberStats';
import { EventCalendar } from './EventCalendar';

interface DashboardProps {
  organizationId: string;
}

export const OrganizationDashboard: React.FC<DashboardProps> = ({
  organizationId
}) => {
  const { user } = useAuth0();
  const [dashboardData, setDashboardData] = useState(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    fetchDashboardData();
  }, [organizationId]);

  const fetchDashboardData = async () => {
    try {
      const response = await fetch(`/api/organizations/${organizationId}/dashboard`);
      const data = await response.json();
      setDashboardData(data);
    } catch (error) {
      console.error('Failed to fetch dashboard:', error);
    } finally {
      setLoading(false);
    }
  };

  if (loading) return <DashboardSkeleton />;

  return (
    <div className="min-h-screen bg-gray-50">
      {/* Dashboard Header */}
      <div className="bg-white shadow">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-6">
          <div className="flex items-center justify-between">
            <div>
              <h1 className="text-3xl font-bold text-gray-900">
                {dashboardData.organization.name}
              </h1>
              <p className="text-gray-600">
                {dashboardData.stats.member_count} members • 
                {dashboardData.stats.active_discussions} active discussions
              </p>
            </div>
            <div className="flex space-x-4">
              <button className="bg-blue-600 text-white px-4 py-2 rounded-md hover:bg-blue-700">
                Invite Members
              </button>
              <button className="bg-green-600 text-white px-4 py-2 rounded-md hover:bg-green-700">
                Create Event
              </button>
            </div>
          </div>
        </div>
      </div>

      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">
          {/* Main Content */}
          <div className="lg:col-span-2 space-y-8">
            {/* Quick Stats */}
            <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
              <div className="bg-white p-6 rounded-lg shadow">
                <div className="text-2xl font-bold text-blue-600">
                  {dashboardData.stats.total_posts}
                </div>
                <div className="text-sm text-gray-600">Total Posts</div>
              </div>
              <div className="bg-white p-6 rounded-lg shadow">
                <div className="text-2xl font-bold text-green-600">
                  {dashboardData.stats.active_members}
                </div>
                <div className="text-sm text-gray-600">Active Members</div>
              </div>
              <div className="bg-white p-6 rounded-lg shadow">
                <div className="text-2xl font-bold text-purple-600">
                  {dashboardData.stats.upcoming_events}
                </div>
                <div className="text-sm text-gray-600">Upcoming Events</div>
              </div>
              <div className="bg-white p-6 rounded-lg shadow">
                <div className="text-2xl font-bold text-orange-600">
                  {dashboardData.stats.shared_resources}
                </div>
                <div className="text-sm text-gray-600">Resources</div>
              </div>
            </div>

            {/* Forums */}
            <div className="bg-white rounded-lg shadow">
              <div className="px-6 py-4 border-b">
                <h2 className="text-lg font-medium">Community Forums</h2>
              </div>
              <ForumList forums={dashboardData.forums} />
            </div>

            {/* Recent Activity */}
            <div className="bg-white rounded-lg shadow">
              <div className="px-6 py-4 border-b">
                <h2 className="text-lg font-medium">Recent Activity</h2>
              </div>
              <RecentActivity activities={dashboardData.recent_activity} />
            </div>
          </div>

          {/* Sidebar */}
          <div className="space-y-8">
            {/* Member Stats */}
            <div className="bg-white rounded-lg shadow">
              <div className="px-6 py-4 border-b">
                <h3 className="text-lg font-medium">Member Overview</h3>
              </div>
              <MemberStats stats={dashboardData.member_stats} />
            </div>

            {/* Upcoming Events */}
            <div className="bg-white rounded-lg shadow">
              <div className="px-6 py-4 border-b">
                <h3 className="text-lg font-medium">Upcoming Events</h3>
              </div>
              <EventCalendar events={dashboardData.upcoming_events} />
            </div>

            {/* Quick Actions */}
            <div className="bg-white rounded-lg shadow p-6">
              <h3 className="text-lg font-medium mb-4">Quick Actions</h3>
              <div className="space-y-3">
                <button className="w-full text-left p-3 rounded-md hover:bg-gray-50 border">
                  Create New Forum
                </button>
                <button className="w-full text-left p-3 rounded-md hover:bg-gray-50 border">
                  Schedule Event
                </button>
                <button className="w-full text-left p-3 rounded-md hover:bg-gray-50 border">
                  Upload Resource
                </button>
                <button className="w-full text-left p-3 rounded-md hover:bg-gray-50 border">
                  Send Announcement
                </button>
              </div>
            </div>
          </div>
        </div>
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

# Auth0
NEXT_PUBLIC_AUTH0_DOMAIN=community.auth0.com
NEXT_PUBLIC_AUTH0_CLIENT_ID=...
AUTH0_CLIENT_SECRET=...

# Stripe
NEXT_PUBLIC_STRIPE_PUBLISHABLE_KEY=pk_test_...
STRIPE_SECRET_KEY=sk_test_...
STRIPE_WEBHOOK_SECRET=whsec_...

# File Storage
AWS_ACCESS_KEY_ID=...
AWS_SECRET_ACCESS_KEY=...
AWS_S3_BUCKET=community-files

# Real-time
WEBSOCKET_PORT=8081

# App
NEXT_PUBLIC_APP_URL=https://community-platform.onrender.com
API_URL=https://community-api.onrender.com
```

### Render Services
- **Frontend**: `community-frontend` (static site)
- **Backend**: `community-api` (web service) 
- **WebSocket**: `community-ws` (web service on different port)
- **Database**: `community-db` (PostgreSQL)

## Success Metrics
- Monthly recurring revenue (MRR) per organization
- Member engagement and retention rates
- Forum activity and post frequency
- Event attendance rates
- Resource usage and downloads
- Customer satisfaction scores

## Development Timeline
- **Setup & Scaffolding**: 15 minutes
- **Backend API Development**: 5-6 hours
- **Real-time WebSocket System**: 3-4 hours
- **Frontend Community Features**: 4-5 hours
- **Organization Management**: 2-3 hours
- **Testing & Deployment**: 1-2 hours

**Total Development Time**: 15-20 hours from scaffold to production