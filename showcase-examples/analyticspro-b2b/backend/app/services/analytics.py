from typing import List, Dict, Any, Optional
from datetime import datetime, timedelta
import asyncpg
import json
from fastapi import HTTPException
import asyncio
from ..models.analytics import MetricQuery, FunnelQuery, RetentionQuery

class AnalyticsService:
    def __init__(self, db_pool: asyncpg.Pool, redis_client):
        self.db_pool = db_pool
        self.redis = redis_client
    
    async def get_metric_value(
        self,
        org_id: str,
        metric_name: str,
        start_time: datetime,
        end_time: datetime,
        granularity: str = "hour",
        filters: Dict[str, Any] = None
    ) -> List[Dict]:
        """
        Calculate metric values over time with specified granularity
        """
        # Generate cache key
        cache_key = f"metric:{org_id}:{metric_name}:{start_time.isoformat()}:{end_time.isoformat()}:{granularity}"
        
        # Try cache first
        cached_result = await self.redis.get(cache_key)
        if cached_result:
            return json.loads(cached_result)
        
        # Build time series query based on granularity
        time_bucket = self._get_time_bucket(granularity)
        
        query = f"""
        SELECT 
            time_bucket('{time_bucket}', timestamp) AS time_bucket,
            COUNT(*) as value
        FROM events 
        WHERE org_id = $1 
            AND event_name = $2 
            AND timestamp >= $3 
            AND timestamp <= $4
        """
        
        params = [org_id, metric_name, start_time, end_time]
        
        # Add property filters
        if filters:
            for key, value in filters.items():
                query += f" AND properties->>${len(params) + 1} = ${len(params) + 2}"
                params.extend([key, str(value)])
        
        query += " GROUP BY time_bucket ORDER BY time_bucket"
        
        async with self.db_pool.acquire() as conn:
            rows = await conn.fetch(query, *params)
            result = [
                {
                    "timestamp": row["time_bucket"].isoformat(),
                    "value": row["value"]
                }
                for row in rows
            ]
        
        # Cache for 5 minutes
        await self.redis.setex(cache_key, 300, json.dumps(result))
        
        return result
    
    async def execute_custom_query(
        self,
        org_id: str,
        query: MetricQuery
    ) -> Dict[str, Any]:
        """
        Execute custom analytics query with aggregations
        """
        try:
            # Build SQL query based on metric query
            sql_query = self._build_sql_query(org_id, query)
            
            async with self.db_pool.acquire() as conn:
                if query.group_by:
                    # Grouped results
                    rows = await conn.fetch(sql_query["query"], *sql_query["params"])
                    result = {
                        "type": "grouped",
                        "data": [dict(row) for row in rows]
                    }
                else:
                    # Single value
                    row = await conn.fetchrow(sql_query["query"], *sql_query["params"])
                    result = {
                        "type": "single",
                        "value": row["value"] if row else 0
                    }
            
            return result
            
        except Exception as e:
            raise HTTPException(400, f"Query execution failed: {str(e)}")
    
    async def analyze_funnel(
        self,
        org_id: str,
        funnel_query: FunnelQuery
    ) -> Dict[str, Any]:
        """
        Perform funnel analysis on sequential events
        """
        steps = funnel_query.steps
        time_window = funnel_query.time_window_hours
        
        # Build funnel analysis query
        funnel_cte = []
        for i, step in enumerate(steps):
            step_alias = f"step_{i}"
            funnel_cte.append(f"""
            {step_alias} AS (
                SELECT DISTINCT user_id, timestamp
                FROM events
                WHERE org_id = $1 
                    AND event_name = '{step.event_name}'
                    AND timestamp >= $2 
                    AND timestamp <= $3
                {self._build_property_filters(step.filters) if step.filters else ''}
            )
            """)
        
        # Build sequential join query
        join_query = "SELECT s0.user_id FROM step_0 s0"
        for i in range(1, len(steps)):
            join_query += f"""
            LEFT JOIN step_{i} s{i} ON s0.user_id = s{i}.user_id 
                AND s{i}.timestamp > s0.timestamp 
                AND s{i}.timestamp <= s0.timestamp + INTERVAL '{time_window} hours'
            """
        
        # Calculate conversion at each step
        conversion_query = f"""
        WITH {', '.join(funnel_cte)}
        SELECT 
            COUNT(DISTINCT s0.user_id) as step_0_users,
            {', '.join([f"COUNT(DISTINCT s{i}.user_id) as step_{i}_users" for i in range(1, len(steps))])}
        FROM step_0 s0
        {' '.join([f"LEFT JOIN step_{i} s{i} ON s0.user_id = s{i}.user_id AND s{i}.timestamp > s0.timestamp AND s{i}.timestamp <= s0.timestamp + INTERVAL '{time_window} hours'" for i in range(1, len(steps))])}
        """
        
        async with self.db_pool.acquire() as conn:
            row = await conn.fetchrow(
                conversion_query,
                org_id,
                funnel_query.start_time,
                funnel_query.end_time
            )
            
            # Calculate conversion rates
            results = []
            total_users = row[f"step_0_users"]
            
            for i in range(len(steps)):
                users_at_step = row[f"step_{i}_users"]
                conversion_rate = (users_at_step / total_users * 100) if total_users > 0 else 0
                
                results.append({
                    "step": i,
                    "event_name": steps[i].event_name,
                    "users": users_at_step,
                    "conversion_rate": round(conversion_rate, 2),
                    "drop_off": round(100 - conversion_rate, 2) if i > 0 else 0
                })
        
        return {
            "funnel_steps": results,
            "total_users": total_users,
            "overall_conversion": round((results[-1]["users"] / total_users * 100), 2) if total_users > 0 else 0
        }
    
    async def analyze_retention(
        self,
        org_id: str,
        retention_query: RetentionQuery
    ) -> Dict[str, Any]:
        """
        Perform cohort retention analysis
        """
        # Cohort analysis query
        query = """
        WITH user_cohorts AS (
            SELECT 
                user_id,
                DATE_TRUNC($4, MIN(timestamp)) as cohort_month
            FROM events
            WHERE org_id = $1 
                AND event_name = $2
                AND timestamp >= $5
                AND timestamp <= $6
            GROUP BY user_id
        ),
        user_activities AS (
            SELECT DISTINCT
                e.user_id,
                DATE_TRUNC($4, e.timestamp) as activity_month
            FROM events e
            JOIN user_cohorts uc ON e.user_id = uc.user_id
            WHERE e.org_id = $1 
                AND e.event_name = $3
                AND e.timestamp >= uc.cohort_month
        ),
        cohort_data AS (
            SELECT 
                uc.cohort_month,
                ua.activity_month,
                COUNT(DISTINCT uc.user_id) as cohort_size,
                COUNT(DISTINCT ua.user_id) as retained_users
            FROM user_cohorts uc
            LEFT JOIN user_activities ua ON uc.user_id = ua.user_id
            GROUP BY uc.cohort_month, ua.activity_month
        )
        SELECT 
            cohort_month,
            activity_month,
            cohort_size,
            retained_users,
            ROUND(retained_users::DECIMAL / cohort_size * 100, 2) as retention_rate,
            EXTRACT(EPOCH FROM (activity_month - cohort_month)) / (60*60*24*30) as period_number
        FROM cohort_data
        WHERE activity_month IS NOT NULL
        ORDER BY cohort_month, activity_month
        """
        
        async with self.db_pool.acquire() as conn:
            rows = await conn.fetch(
                query,
                org_id,
                retention_query.initial_event,
                retention_query.return_event,
                retention_query.period,  # 'month', 'week', 'day'
                retention_query.start_time,
                retention_query.end_time
            )
            
            # Structure retention data by cohort
            cohorts = {}
            for row in rows:
                cohort_key = row["cohort_month"].isoformat()
                if cohort_key not in cohorts:
                    cohorts[cohort_key] = {
                        "cohort_month": cohort_key,
                        "cohort_size": row["cohort_size"],
                        "retention_data": []
                    }
                
                cohorts[cohort_key]["retention_data"].append({
                    "period": int(row["period_number"]),
                    "retained_users": row["retained_users"],
                    "retention_rate": float(row["retention_rate"])
                })
        
        return {
            "cohorts": list(cohorts.values()),
            "analysis_period": retention_query.period,
            "total_cohorts": len(cohorts)
        }
    
    async def get_segmentation_analysis(
        self,
        org_id: str,
        event_name: str,
        segment_property: str,
        start_time: datetime,
        end_time: datetime
    ) -> Dict[str, Any]:
        """
        Analyze user segmentation based on event properties
        """
        query = """
        SELECT 
            properties->>$4 as segment_value,
            COUNT(*) as event_count,
            COUNT(DISTINCT user_id) as unique_users,
            AVG(CASE WHEN properties->>$5 IS NOT NULL 
                THEN (properties->>$5)::NUMERIC ELSE NULL END) as avg_value
        FROM events
        WHERE org_id = $1 
            AND event_name = $2
            AND timestamp >= $3
            AND timestamp <= $6
            AND properties->>$4 IS NOT NULL
        GROUP BY properties->>$4
        ORDER BY event_count DESC
        """
        
        async with self.db_pool.acquire() as conn:
            rows = await conn.fetch(
                query,
                org_id,
                event_name,
                start_time,
                segment_property,
                'value',  # Common numeric property name
                end_time
            )
            
            segments = []
            total_events = sum(row["event_count"] for row in rows)
            total_users = len(set(row["unique_users"] for row in rows))
            
            for row in rows:
                segments.append({
                    "segment_value": row["segment_value"],
                    "event_count": row["event_count"],
                    "unique_users": row["unique_users"],
                    "event_percentage": round(row["event_count"] / total_events * 100, 2),
                    "avg_value": float(row["avg_value"]) if row["avg_value"] else None
                })
        
        return {
            "segments": segments,
            "total_events": total_events,
            "total_users": total_users,
            "segment_property": segment_property
        }
    
    def _get_time_bucket(self, granularity: str) -> str:
        """Get TimescaleDB time bucket interval"""
        intervals = {
            "minute": "1 minute",
            "hour": "1 hour", 
            "day": "1 day",
            "week": "1 week",
            "month": "1 month"
        }
        return intervals.get(granularity, "1 hour")
    
    def _build_sql_query(self, org_id: str, query: MetricQuery) -> Dict:
        """Build SQL query from metric query object"""
        
        select_clause = "SELECT "
        if query.aggregation == "count":
            select_clause += "COUNT(*) as value"
        elif query.aggregation == "count_distinct":
            select_clause += f"COUNT(DISTINCT {query.field or 'user_id'}) as value"
        elif query.aggregation == "sum":
            select_clause += f"SUM((properties->>{query.field})::NUMERIC) as value"
        elif query.aggregation == "avg":
            select_clause += f"AVG((properties->>{query.field})::NUMERIC) as value"
        
        if query.group_by:
            select_clause += f", properties->>'{query.group_by}' as group_key"
        
        where_clause = "WHERE org_id = $1"
        params = [org_id]
        
        if query.event_names:
            where_clause += f" AND event_name = ANY($2)"
            params.append(query.event_names)
        
        if query.start_time:
            where_clause += f" AND timestamp >= ${len(params) + 1}"
            params.append(query.start_time)
        
        if query.end_time:
            where_clause += f" AND timestamp <= ${len(params) + 1}"
            params.append(query.end_time)
        
        # Add property filters
        if query.filters:
            for key, value in query.filters.items():
                where_clause += f" AND properties->>${len(params) + 1} = ${len(params) + 2}"
                params.extend([key, str(value)])
        
        group_clause = ""
        if query.group_by:
            group_clause = f"GROUP BY properties->>'{query.group_by}' ORDER BY value DESC"
        
        sql_query = f"{select_clause} FROM events {where_clause} {group_clause}"
        
        return {
            "query": sql_query,
            "params": params
        }
    
    def _build_property_filters(self, filters: Dict[str, Any]) -> str:
        """Build property filter SQL clause"""
        filter_clauses = []
        for key, value in filters.items():
            filter_clauses.append(f"AND properties->>'{key}' = '{value}'")
        return " ".join(filter_clauses)