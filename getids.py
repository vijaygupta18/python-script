import psycopg2
import os

# Database connection details
host = "localhost"
username = "postgres"
dbname = "atlas_dev"
password = "root"
port = 5434
schemaName = "atlas_app"

def fetch_data_from_table(table_name):
    try:
        connection_string = f"host='{host}' dbname='{dbname}' user='{username}' password='{password}' port={port}"
        with psycopg2.connect(connection_string) as conn:
            with conn.cursor() as cursor:
                cursor.execute(f"SELECT * FROM {schemaName}.{table_name} LIMIT 1;")
                result = cursor.fetchone()
                if result:
                    table_id = result[0]
                    return table_id
                return None
    except Exception as e:
        print(f"Error: {e}")
        return None

def convert_to_camel_case(snake_case_str):
    words = snake_case_str.split('_')
    camel_case_str = ''.join(word.title() for word in words)
    return camel_case_str

def save_to_txt(data):
    try:
        with open('table_data.txt', 'w') as txtfile:
            for table_name, table_id in data.items():
                res = table_name
                table_name = convert_to_camel_case(table_name)
                line_of_code = f'{res} <- Storage.Queries.{table_name}.findById(Id "{table_id}")'
                line_of_code += f'\nlogInfo $ "{table_name} are: " <> show {res}'
                print(line_of_code)
                txtfile.write(line_of_code + '\n')
        print("Data saved to 'table_data.txt'")
    except Exception as e:
        print(f"Error while saving to TXT: {e}")

if __name__ == "__main__":
    os.environ['PGPASSWORD'] = password

    # Fetch table names from the database
    connection_string = f"host='{host}' dbname='{dbname}' user='{username}' password='{password}' port={port}"
    with psycopg2.connect(connection_string) as conn:
        with conn.cursor() as cursor:
            cursor.execute(f"SELECT table_name FROM information_schema.tables WHERE table_schema = '{schemaName}';")
            table_names = [table[0] for table in cursor.fetchall()]


    ids_data = {table_name: fetch_data_from_table(table_name) for table_name in table_names}
    if ids_data:
        save_to_txt(ids_data)
