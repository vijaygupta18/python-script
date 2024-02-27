import psycopg2
from psycopg2 import sql
import requests
import time
# Replace these values with your AWS RDS credentials
host = 'localhost'
port = '5434' # 5434 for local
dbname = 'atlas_dev'
user = 'akhilesh.b' # postgres for local
password = ''
column_name_to_get = 'fare_policy_id'


# Other Misc vars
schema_name = '' # Let this be empty
table_names = ['fare_policy_progressive_details_per_extra_km_rate_section']
env = 'local'
app = 'dobpp'
cac_tgt_url = 'http://localhost:8080'
tenant = 'test'
columns_to_remove = ['fare_policy_id']

def rm_sq(s):
   res = ""
   for ch in s:
      if ch != "'":
         res += ch
   return res

def convertToCamelCase(col_names):
  new_col_names = []
  for col_name in col_names:
    new_col_name = ''
    i = 0
    while i < len(col_name):
      if col_name[i] == '_':
          new_col_name += col_name[i + 1].upper()
          i += 2
      else:
          new_col_name += col_name[i]
          i += 1
    new_col_names.append(new_col_name)
  return new_col_names



def main(table_name):
  if app == 'dobpp':
    if env == 'master': fare_policy_id = '1e7b7ab9-3b9b-4d3e-a47c-11e7d2a9ff98'
    elif env == 'prod': fare_policy_id = 'f067bccf-5b34-fb51-a5a3-9d6fa6baac26'
    else: fare_policy_id = 'aa8c2b6b-620f-b872-0fa3-3837933e0114' # For local replace this !!!!!!!!!
    schema_name = 'atlas_driver_offer_bpp'
  else:
    if env == 'master': fare_policy_id = 'b30daaf7-77d2-17c8-00d9-baf7ad0f5719'
    elif env == 'prod': fare_policy_id = 'f067bccf-5b34-fb51-a5a3-9d6fa6baac26'
    else: fare_policy_id = 'aa8c2b6b-620f-b872-0fa3-3837933e0114' # For local replace this !!!!!!!!!
    schema_name = 'atlas_app'
  try:
      # Establish a connection to the RDS instance
      connection = psycopg2.connect(
          host=host,
          dbname=dbname,
          user=user,
          password=password,
          port=port,
          connect_timeout=3,
          keepalives=1,
          keepalives_idle=5,
          keepalives_interval=2,
          keepalives_count=2
      )

      # Create a cursor object to interact with the database
      cursor = connection.cursor()
      query = sql.SQL(f"SELECT * FROM {schema_name}.{table_name} WHERE fare_policy_id = '{fare_policy_id}';")
      cursor.execute(query)

      # Fetch the results
      results = cursor.fetchall()

      column_names = convertToCamelCase([desc[0] for desc in cursor.description])
      print("col names :", column_names)
      
      # Creating the list
      combined_dicts = []
      for value_tuple in results:
          combined_dict = {}
          for column, value in zip(column_names, value_tuple):
              combined_dict[column] = value
          combined_dicts.append(combined_dict)
      # Process the results
      print("\n\nresults :", results)
      print("\n", combined_dicts)
      columns_to_remove1 = convertToCamelCase(columns_to_remove)
      # Remove the unwanted columns 
      for d in combined_dicts:
        for column in columns_to_remove1:
            d.pop(column, None)

      print(combined_dicts)
      # Adding to CAC
      query = sql.SQL(f"SELECT DISTINCT {column_name_to_get} FROM {schema_name}.{table_name};")
      cursor.execute(query)
      results1 = cursor.fetchall()
      # Extract single values from tuples
      distinct_values = [result[0] for result in results1]

      print("\n\n\nDistinct Values:", distinct_values)
      return 

      headers = {'Content-Type': 'application/json', 'Authorization': 'Bearer 12345678', 'x-tenant': f'{tenant}'}
      new_table_name = convertToCamelCase([table_name])[0]
      url = f'{cac_tgt_url}/default-config/{new_table_name}:{fare_policy_id}'
      data = {"value":(str(combined_dicts)),"schema":{"type":"string","pattern":".*"}}
      response = requests.put(url, json=data, headers=headers)
      if response.status_code == 200:
          print(f"Successfully added data {data}! Yaaayyyyyy")
      else:
          print(f"Error: {response.status_code}! Sad Broooooo")
      time.sleep(1)

  except psycopg2.Error as e:
      print(f"Error: {e}")
  finally:
      # Close the cursor and connection
      if cursor:
          cursor.close()
      if connection:
          connection.close()


if __name__ == '__main__':
  for tn in table_names:
    
    main(tn)