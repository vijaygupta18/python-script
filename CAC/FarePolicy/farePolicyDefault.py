#######################################################################
# This script is used to add data from a table in RDS to CAC.
# author: Ratnadeep Bhattacharya
# date: 2024-01-22
#######################################################################

import psycopg2
from psycopg2 import sql
import requests
import time
import json
import decimal
# Replace these values with your AWS RDS credentials
host = 'localhost'
port = '5434' # 5434 for local
dbname = 'atlas_dev'
user = 'akhilesh.b' # postgres for local
password = ''


# Other Misc vars
schema_name = '' # Let this be empty
table_names = ["fare_policy_rental_details"]
env = 'local'
app = 'dobpp'
cac_tgt_url = 'http://localhost:8080'
tenant = 'test'
where_column = 'fare_policy_id'

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
    else: merchantOpCityId = '71b52524-e773-03dc-5853-290132ce6fd5' # For local replace this !!!!!!!!!
    schema_name = 'atlas_driver_offer_bpp'
  else:
    if env == 'master': merchantOpCityId = 'b30daaf7-77d2-17c8-00d9-baf7ad0f5719'
    elif env == 'prod': merchantOpCityId = 'f067bccf-5b34-fb51-a5a3-9d6fa6baac26'
    else: merchantOpCityId = 'dc66fddb-e248-c537-b214-a1219bdf482c' # For local replace this !!!!!!!!!
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

      snk_cols = [desc[0] for desc in cursor.description] # col names in snake case
      column_names = convertToCamelCase(snk_cols)
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
        print ("result is : ", results[0][j])
        try:
          if (results[0][j] == None or results[0][j] == "None"): #TODO: Test This Module.
            cursor.execute(f"SELECT data_type FROM information_schema.columns WHERE table_name LIKE '{table_name}' AND column_name = '{snk_cols[j]}'")
            typ = cursor.fetchone()[0]
            print("The Type Of Maybe Field is : ", typ)
            if typ.lower() == 'numeric' or typ.lower() == 'integer' or typ.lower() == 'bigint':
               typ = 'number'
            elif typ.lower() == 'decimal' or typ.lower() == 'real':
               typ = 'number'
            else :
               typ = 'string'
            if typ != 'string':
              data = {"value":None,"schema":{"type":["null",f"{typ}"]}}
            else:
               print(f"WARNING:- Could not decode type of the maybe column {column_names[j]} hence putting as null as string type. Check This !!!!!!! ")
               data = {"value":None,"schema":{"type":["string","null"],"pattern":".*"}}
          elif (type(results[0][j]) == int):
            data = {"value":int(results[0][j]),"schema":{"type":["null","number"]}}
          elif (type(results[0][j]) == bool):
            data = {"value":bool(results[0][j]),"schema":{"type":["null","boolean"]}}
          elif (type(results[0][j]) == float):
            data = {"value":float(results[0][j]),"schema":{"type":["null","number"]}}
          elif (type(results[0][j]) == decimal.Decimal):
            data = {"value":float(results[0][j]),"schema":{"type":["null","number"]}}
          else:
            if(":" in str(results[0][j])) and ("{" in str(results[0][j]) and "[" not in results[0][j]):
              print("adding this value", results[0][j])
              data = {"value":(results[0][j]),"schema":{"type":["null","object"]}}
              #UNCOMMENT THE BELOW CODE ONCE COMPATIBILITY FOR ARRAYS IS ADDED.
            elif type(results[0][j]) == list:
              print("adding this value (Array)", results[0][j])
              data = {"value":(results[0][j]),"schema":{"type":["null","array"]}}
            else:
              data = {"value":rm_sq(str(results[0][j])),"schema":{"type":["string","null"],"pattern":".*"}}
        except Exception as e:
            print(f"WARNING: Skipping column since cannot parse :-\ : {column_names[j]} with error : {e} !!!!!!!!!!")
            continue

        # print("Url is :", url)
        # print("Data is :", data)

        response = requests.put(url, json=data, headers=headers)
        if response.status_code == 200:
            print(f"Successfully added data {data}! Yaaayyyyyy")
        else:
            print(f"Error: {response.status_code}! Sad Broooooo")
            return
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