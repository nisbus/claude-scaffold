# Multi-stage Dockerfile for Erlang/OTP applications
# Optimized for Render deployment

# Build stage
FROM erlang:26-alpine AS builder

# Install build dependencies
RUN apk add --no-cache git make gcc musl-dev

# Set working directory
WORKDIR /build

# Copy rebar config and lock file first (for better caching)
COPY rebar.config rebar.lock ./

# Download dependencies
RUN rebar3 deps

# Copy source code
COPY . .

# Compile the application
RUN rebar3 compile

# Create release
RUN rebar3 as prod release

# Runtime stage
FROM alpine:3.18

# Install runtime dependencies
RUN apk add --no-cache \
    libstdc++ \
    openssl \
    ncurses-libs \
    ca-certificates \
    && update-ca-certificates

# Create app user
RUN addgroup -g 1000 app && \
    adduser -u 1000 -G app -s /bin/sh -D app

# Set working directory
WORKDIR /app

# Copy release from builder
COPY --from=builder --chown=app:app /build/_build/prod/rel/myapp ./

# Switch to app user
USER app

# Expose port (default Cowboy port)
EXPOSE 8080

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD wget --no-verbose --tries=1 --spider http://localhost:8080/health || exit 1

# Start the application
CMD ["bin/myapp", "foreground"]