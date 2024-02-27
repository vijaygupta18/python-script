#######################################################################
# This script is used to add data from a table in RDS to CAC.
# author: Ratnadeep Bhattacharya
# date: 2024-01-22
#######################################################################

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


# Other Misc vars
schema_name = '' # Let this be empty
table_names = ["driver_pool_config", "go_home_config", "driver_intelligent_pool_config", "transporter_config"]
env = 'local'
app = 'dobpp'
cac_tgt_url = 'http://localhost:8080'
tenant = 'test'
where_column = 'merchant_operating_city_id'

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
    if env == 'master': merchantOpCityId = '1e7b7ab9-3b9b-4d3e-a47c-11e7d2a9ff98'
    elif env == 'prod': merchantOpCityId = 'f067bccf-5b34-fb51-a5a3-9d6fa6baac26'
    else: merchantOpCityId = 'a174b699-a3f3-b362-22a1-602951dbe8a2' # For local replace this !!!!!!!!!
    schema_name = 'atlas_driver_offer_bpp'
  else:
    if env == 'master': merchantOpCityId = 'b30daaf7-77d2-17c8-00d9-baf7ad0f5719'
    elif env == 'prod': merchantOpCityId = 'f067bccf-5b34-fb51-a5a3-9d6fa6baac26'
    else: merchantOpCityId = 'a174b699-a3f3-b362-22a1-602951dbe8a2' # For local replace this !!!!!!!!!
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
      query = sql.SQL(f"SELECT * FROM {schema_name}.{table_name} WHERE {where_column} = '{merchantOpCityId}';")
      limit_value = 1
      cursor.execute(query, [limit_value])

      # Fetch the results
      results = cursor.fetchall()

      column_names = convertToCamelCase([desc[0] for desc in cursor.description])
      print("col names :", column_names)

      # Process the results
      print("\n\nresults :", results)

      # Adding to CAC
      n = len(results)
      m = len(column_names)


      headers = {'Content-Type': 'application/json', 'Authorization': 'Bearer 12345678', 'x-tenant': f'{tenant}'}
      new_table_name = convertToCamelCase([table_name])[0]
      for j in range (0, m):
        print("column name :", column_names[j])
        url = f'{cac_tgt_url}/default-config/{new_table_name}:{column_names[j]}'
        try:
          # try:
          #   data = {"value":int(results[0][j]),"schema":{"type":"number"}} TODO: Uncomment this after fix is given by PP.
          # except:
          if(type(results[0][j]) == int):
            data = {"value":int(results[0][j]),"schema":{"type":"number"}}
          elif (type(results[0][j]) == bool):
            data = {"value":bool(results[0][j]),"schema":{"type":"boolean"}}
          elif (type(results[0][j]) == float):
            data = {"value":int(results[0][j]),"schema":{"type":"number"}}
          else:
            if(":" in str(results[0][j])) and ("{" in str(results[0][j])):
              print("adding this value", results[0][j])
              data = {"value":(str(results[0][j])),"schema":{"type":"string","pattern":".*"}}
            else:
              data = {"value":rm_sq(str(results[0][j])),"schema":{"type":"string","pattern":".*"}}
        except:
            print("Skipping column since cannot parse :-\ :", column_names[j])

        # print("Url is :", url)
        # print("Data is :", data)

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