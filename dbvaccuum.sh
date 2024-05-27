#!/bin/bash

# Database credentials
DATABASE="atlas_dev"
PORT=5434
HOST="localhost"

# Fetch all schema and table names from the database
TABLES=$(psql -h $HOST -p $PORT -d $DATABASE -t -c "SELECT schemaname, tablename FROM pg_tables WHERE schemaname != 'pg_catalog' AND schemaname != 'information_schema';")

# Loop through each table and perform autovacuum if necessary
while IFS='|' read -r schemaname tablename; do
    # Check if autovacuum is required for the table
    IS_AUTOVACUUM_REQUIRED=$(psql -h $HOST -p $PORT -d $DATABASE -t -c "SELECT 'autovacuum' FROM pg_stat_all_tables WHERE schemaname = '$schemaname' AND relname = '$tablename' AND last_autovacuum < NOW() - INTERVAL '1 day';")

    # Check the size of dead rows for the table
    DEAD_ROWS_SIZE=$(psql -h $HOST -p $PORT -d $DATABASE -t -c "SELECT n_dead_tup * (SELECT current_setting('block_size')::int / 1024)::bigint AS dead_rows_size FROM pg_stat_user_tables WHERE schemaname = '$schemaname' AND relname = '$tablename';")

    if [ "$IS_AUTOVACUUM_REQUIRED" = "autovacuum" ] && [ "$DEAD_ROWS_SIZE" -gt 1073741824 ]; then
        echo "Performing autovacuum on table: $schemaname.$tablename"
        psql -h $HOST -p $PORT -d $DATABASE -c "VACUUM VERBOSE $schemaname.$tablename;"
        echo "Autovacuum completed for table: $schemaname.$tablename"
    else
        echo "Autovacuum is not required for table: $schemaname.$tablename"
    fi
done <<< "$TABLES"

