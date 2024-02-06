import psycopg2
import os

def execute_sql_commands(conn, commands):
    with conn.cursor() as cur:
        for command in commands:
            try:
                cur.execute(command)
                conn.commit()
            except Exception as e:
                print(f"Error executing command '{command}': {e}")
                conn.rollback()

def main():
    # Database connection parameters
    db_params = {
        'host': os.environ.get('DB_HOST'),
        'user': os.environ.get('DB_USER'),
        'password': os.environ.get('DB_PASSWORD'),
        'dbname': os.environ.get('DB_NAME')
    }

    conn = None
    try:
        # Connect to the database
        conn = psycopg2.connect(**db_params)

        # SQL commands to be executed
        db_commands = [
            "DROP DATABASE IF EXISTS atlas_app_sandbox",
            "DROP DATABASE IF EXISTS atlas_bap_dashboard_sandbox",
            "DROP DATABASE IF EXISTS atlas_bpp_dashboard_sandbox",
            "DROP DATABASE IF EXISTS atlas_driver_offer_bpp_sandbox",
            "CREATE DATABASE atlas_app_sandbox WITH TEMPLATE atlas_app_v2 OWNER cloud",
            "CREATE DATABASE atlas_bap_dashboard_sandbox WITH TEMPLATE atlas_bap_dashboard_v2 OWNER cloud",
            "CREATE DATABASE atlas_bpp_dashboard_sandbox WITH TEMPLATE atlas_bpp_dashboard_v2 OWNER cloud",
            "CREATE DATABASE atlas_driver_offer_bpp_sandbox WITH TEMPLATE atlas_driver_offer_bpp_v2 OWNER cloud"
        ]

        terminate_commands = [
            "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname = 'atlas_app_v2' AND pid != pg_backend_pid() AND leader_pid IS NULL",
            "SELECT  pg_terminate_backend(pid) FROM  pg_stat_activity WHERE  datname =  'atlas_bap_dashboard_v2' AND  pid != pg_backend_pid() AND  leader_pid  IS NULL",
            "SELECT  pg_terminate_backend(pid) FROM  pg_stat_activity WHERE  datname =  'atlas_bpp_dashboard_v2' AND  pid != pg_backend_pid() AND  leader_pid  IS NULL",
            "SELECT  pg_terminate_backend(pid) FROM  pg_stat_activity WHERE  datname =  'atlas_driver_offer_bpp_v2' AND  pid != pg_backend_pid() AND  leader_pid  IS NULL",
            "SELECT  pg_terminate_backend(pid) FROM  pg_stat_activity WHERE  datname =  'atlas_mock_registry_v2' AND  pid != pg_backend_pid() AND  leader_pid  IS NULL"
            # Add similar commands for other databases
        ]

        update_commands = [
            "UPDATE merchant SET subscriber_id = REPLACE(subscriber_id, '/dev', '') WHERE subscriber_id LIKE '%/dev%'",
            "UPDATE merchant SET registry_url = REPLACE(registry_url, '/dev', '') WHERE registry_url LIKE '%/dev%'",
            # Add similar UPDATE statements for other fields and tables
        ]

        # Execute the commands
        execute_sql_commands(conn, db_commands)
        execute_sql_commands(conn, terminate_commands)
        execute_sql_commands(conn, update_commands)

    except psycopg2.DatabaseError as e:
        print(f"Database error: {e}")
    except Exception as e:
        print(f"An error occurred: {e}")
    finally:
        if conn is not None:
            conn.close()

if __name__ == "__main__":
    main()
