#!/bin/bash
# atlas_db_backup.sh
# Script to backup specific tables from Atlas databases (INSERT statements only)

# Log function for terminal output only
log_message() {
    echo "$(date +"%Y-%m-%d %H:%M:%S") - $1"
}

# Error handling function
handle_error() {
    log_message "ERROR: $1"
    echo "ERROR: $1" >&2
    rm -f "$LOCK_FILE"
    exit 1
}

# Configuration
DRIVER_OFFER_DB_NAME="atlas_dev"
DRIVER_OFFER_SCHEMA="atlas_driver_offer_bpp"
DRIVER_OFFER_PG_USER="atlas_driver_offer_bpp_user"
DRIVER_OFFER_PG_PASSWORD="atlas"
DRIVER_OFFER_PG_HOST="localhost"
DRIVER_OFFER_PG_PORT="5434"

APP_DB_NAME="atlas_dev"
APP_SCHEMA="atlas_app"
APP_PG_USER="atlas_app_user"
APP_PG_PASSWORD="atlas"
APP_PG_HOST="localhost"
APP_PG_PORT="5434"

# Backup settings
BACKUP_DIR="$HOME/Table_backup"
DATE=$(date +%d-%m-%Y)  # dd-mm-yyyy format
RETENTION_DAYS=7
LOCK_FILE="/tmp/atlas_backup_script.lock"

# List of tables to backup from each schema
DRIVER_OFFER_TABLES="table1 table2 location"  # Replace with actual config tables
APP_TABLES="table4 table5 table6"  # Includes 'location'

# Check if script is already running
[ -f "$LOCK_FILE" ] && handle_error "Another backup process is running"

# Create lock file and set trap
touch "$LOCK_FILE" || handle_error "Cannot create lock file"
trap 'rm -f "$LOCK_FILE"; exit' EXIT INT TERM

# Create backup directory
mkdir -p "$BACKUP_DIR" || handle_error "Failed to create backup directory"

log_message "Starting Atlas database INSERT backup process"

# Function to backup tables as INSERT statements
backup_tables_as_inserts() {
    local db_name=$1
    local schema=$2
    local db_user=$3
    local db_password=$4
    local db_host=$5
    local db_port=$6
    local tables=$7

    export PGPASSWORD="$db_password"

    # Check database connectivity
    if ! pg_isready -q -U "$db_user" -h "$db_host" -p "$db_port" -d "$db_name"; then
        log_message "ERROR: Cannot connect to database $db_name at $db_host:$db_port. Skipping backup."
        return
    fi

    # Backup each table as INSERT statements
    for table in $tables; do
        # Check if table exists
        table_exists=$(psql -U "$db_user" -h "$db_host" -p "$db_port" -d "$db_name" -tAc \
            "SELECT EXISTS (SELECT 1 FROM information_schema.tables WHERE table_schema = '$schema' AND table_name = '$table');")

        if [[ "$table_exists" != "t" ]]; then
            log_message "Skipping $schema.$table (table does not exist)"
            continue
        fi

        backup_file="$BACKUP_DIR/${schema}_${table}_${DATE}_insert_only.sql"
        compressed_backup_file="${backup_file}.gz"

        log_message "Backing up INSERT statements for table $schema.$table from $db_name database"

        if pg_dump -U "$db_user" -h "$db_host" -p "$db_port" \
            -n "$schema" -t "$schema.$table" --data-only --inserts "$db_name" > "$backup_file" 2>/dev/null; then
            log_message "Successfully backed up INSERT statements for $schema.$table. Compressing..."

            # Compress the backup file
            gzip -f "$backup_file"

            log_message "Backup compressed: $compressed_backup_file"
        else
            log_message "ERROR: Failed to backup INSERT statements for $schema.$table. Skipping."
            continue  # Skip and move to next table
        fi
    done

    unset PGPASSWORD
}

# Perform backups
backup_tables_as_inserts "$DRIVER_OFFER_DB_NAME" "$DRIVER_OFFER_SCHEMA" "$DRIVER_OFFER_PG_USER" \
    "$DRIVER_OFFER_PG_PASSWORD" "$DRIVER_OFFER_PG_HOST" "$DRIVER_OFFER_PG_PORT" "$DRIVER_OFFER_TABLES"

backup_tables_as_inserts "$APP_DB_NAME" "$APP_SCHEMA" "$APP_PG_USER" \
    "$APP_PG_PASSWORD" "$APP_PG_HOST" "$APP_PG_PORT" "$APP_TABLES"

# Clean up old backups
log_message "Cleaning up INSERT backups older than $RETENTION_DAYS days"
find "$BACKUP_DIR" -type f -mtime +"$RETENTION_DAYS" -name "*_insert_only.sql" -exec rm -f {} \; 2>/dev/null

# Calculate total size
TOTAL_SIZE=$(du -sh "$BACKUP_DIR" | cut -f1 2>/dev/null || echo "Unknown")
log_message "Backup completed successfully. Total backup size: $TOTAL_SIZE"

echo "Backup process completed at $(date)"
exit 0