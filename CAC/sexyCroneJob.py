import psycopg2
import requests
import json

# Database connection details
host = "localhost"
database = "atlas_dev"
port = "5434"
user = "akhilesh.b"
password = ""
table = "system_configs"
tenantAndSchema = [("atlas_driver_offer_bpp_v2", "atlas_driver_offer_bpp")]

# Connect to the database
conn = psycopg2.connect(host=host, database=database, user=user, password=password,port=port,
        connect_timeout=3,
        keepalives=1,
        keepalives_idle=5,
        keepalives_interval=2,
        keepalives_count=2)
cur = conn.cursor()

for (tenant, schema) in tenantAndSchema:
    # API endpoint URL and method
    api_url = "https://api.sandbox.beckn.juspay.in/cac/config"
    method = "GET"
    headers = {'Content-Type': 'application/json', 'Authorization': 'Bearer 12345678', 'x-tenant': f'{tenant}'}
    # Make the API call
    # (Replace with your actual API call implementation using requests or similar library)
    try:
        response = requests.get(api_url, headers=headers)
        response.raise_for_status()  # Raise an exception for non-2xx status codes
    except requests.exceptions.RequestException as e:
        print("Error making API call:", e)
        conn.close()
        exit(1)

    # Parse the API response (assuming JSON)
    api_data = response.json()
    api_data = json.dumps(api_data)

    # Prepare SQL statement to insert data into the table
    sql = f"""
    UPDATE {schema}.{table}
    SET config_value = %s
    WHERE id = %s;
    """

    # Execute the SQL statement for each response item
        # Construct data to insert based on your column structure
    # insert_data = json.dumps(api_data)  # Example assuming "id" in response
    print(api_data)
    cur.execute(sql,(api_data, tenant))

# Commit the changes and close the connection
conn.commit()
conn.close()

print("Data stored in the 'system_configs' table successfully!")
