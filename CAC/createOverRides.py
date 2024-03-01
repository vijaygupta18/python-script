#######################################################################
# This script is used to add overrides from a table in RDS to CAC.
# author: Ratnadeep Bhattacharya
# date: 2024-01-30
#######################################################################

import psycopg2
from psycopg2 import sql
import requests
import time
import sys

# Replace these values with your AWS RDS credentials
host = 'localhost'
port = '5434' # 5434 for local
dbname = 'atlas_dev'
user = 'ratnadeep.b' # postgres for local
password = ''


# Other Misc vars
schema_name = '' # Let this be empty
table_names = ['transporter_config'] # NOTE: This works only for one table at a time!!!! (I have used list here as this part was copied from sqlToCac.py  Lol :-P )
env = 'dev'
app = 'dobpp'
cac_tgt_url = 'http://localhost:8080'
tenant = 'test'
MAX_INT = 2147483000

class TestFailed(Exception):
    def __init__(self, m):
        self.message = m
    def __str__(self):
        return self.message
    
def rm_sq(s):
   res = ""
   for ch in s:
      if ch != "'":
         res += ch
   return res

def convert_type(res):
  new_res = []
  for r in res:
    new_res.append(r[0])
  return new_res

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

def convertOneToCamelCase(col_name):
   return convertToCamelCase([col_name])[0]
   
def get_default_configs(headers, table_name):
  api_url = f'{cac_tgt_url}/default-config'
  response = requests.get(api_url, headers=headers)
  if response.status_code != 200:
    raise TestFailed(f"Error: {response.status_code}! while Fetching default cfgs Sad Broooooo with resp {response}")
  response = response.json()
  print("Default Configs = ", response)
  table_name += ":"
  table_name = convertOneToCamelCase(table_name)
  processed_resp = dict()
  for r in response:
     if table_name in r['key']:
        processed_resp[r['key']] = r['value']
  return processed_resp

def solution(ind, n, stack, context, dist_overrides, overrides, table_name, cursor, sn, limit_value, column_names):
  if (ind == n):
    query_str = f"SELECT * FROM {sn}.{table_name} WHERE "
    n = len(stack)
    for j in range(n):
      if (overrides[j][1] == '=='):
        if stack[j] != None:
          try:
            query_str += f"{overrides[j][0]} = {str(int(stack[j]))} AND "
          except:
            query_str += f"{overrides[j][0]} = '{stack[j]}' AND "
        else:
          query_str += f"{overrides[j][0]} IS NULL AND "
      elif (overrides[j][1] == 'slot'):
        query_str += f"{overrides[j][0]} >= {stack[j][0]} AND {overrides[j][0]} < {stack[j][1]} AND "
      else:
         raise TestFailed("Error: Invalid operator for override!")
    ln = len(query_str)
    query_str = query_str[:ln - 5] + ';'
    print("Query = ", query_str)
    query = sql.SQL(query_str)
    cursor.execute(query, [limit_value])
    res = cursor.fetchall()
    print("rowcount = ", cursor.rowcount)
    if cursor.rowcount > 1:
      raise TestFailed(f"Error: Did not return unique override! : \n\n {res}")
    if cursor.rowcount == 0:
      print("NOTE:-  No override found for this condition !!!!")
      return
    res = res[0]
    print("Data to be overridden = ", res)
    print ("len res:", len(res))
    print ("len column_names:", column_names)
    m = len(res)
    override_data = {}
    for i in range(m):
      if(type(res[i]) == int):
        override_data[convertOneToCamelCase(table_name) + ":" + column_names[i]] = int(res[i])
      elif (type(res[i]) == bool):
        override_data[convertOneToCamelCase(table_name) + ":" + column_names[i]] = bool(res[i])
      elif (type(res[i]) == float):
        override_data[convertOneToCamelCase(table_name) + ":" + column_names[i]] = float(res[i])
      else:
        if(":" in str(res[i])) and ("{" in str(res[i])):
          override_data[convertOneToCamelCase(table_name) + ":" + column_names[i]] = (str(res[i])) 
        elif type(res[0][j]) == list:
          print("adding this value (Array)", res[i])
          override_data[convertOneToCamelCase(table_name) + ":" + column_names[i]] = res[i]
        else:
          override_data[convertOneToCamelCase(table_name) + ":" + column_names[i]] = rm_sq(str(res[i])) # None is getting ignored 
    headers = {'Content-Type': 'application/json', 'Authorization': 'Bearer 12345678', 'x-tenant': f'{tenant}'}
    def_cfgs = get_default_configs(headers, table_name)
    diffed_data = {}
    if len(def_cfgs) != len(override_data):
      print("lengths: ", len(def_cfgs), len(override_data))
      raise TestFailed("Error: Default config and override config length mismatch!")
    if def_cfgs.keys() != override_data.keys():
      raise TestFailed("Error: Default config and override config keys mismatch!")

    for k in override_data.keys():
      if override_data[k] != def_cfgs[k]:
        diffed_data[k] = override_data[k]

    print("Diff config data :", diffed_data)
    if diffed_data == {}:
      print(" ** NOTE:-  No override required for this condition !!!! as no diff found !!!!! ")
      return
    data = {"override":diffed_data,"context":{"and":context}}
    url = f'{cac_tgt_url}/context'

    # Logger block start....
    print("Url is :", url)
    print("Data is :", data)
    print("Headers are :", headers)
    # Logger block end....

    response = requests.put(url, json=data, headers=headers)
    if response.status_code == 200:
        print(f"Successfully added override {data}! Yaaayyyyyy")
    else:
        print(f"Error: {response.status_code}! Sad Broooooo Disappointed and resp {response}\n\n DATA = {data}")
    time.sleep(1)
    return
    
        
  for i in range(len(dist_overrides[ind])):
    flg = True
    print("overrides[ind][0] outside :", overrides[ind][0])
    if (overrides[ind][1] == '=='):
      print("overrides[ind][0] :", overrides[ind][0])
      if dist_overrides[ind][i] != None and dist_overrides[ind][i] != "None": 
        val = overrides[ind][0]
        if(overrides[ind][0] == 'id'):
          val = 'fare_policy_id'
        context.append({"==":[{"var":convertOneToCamelCase(val)}, str(dist_overrides[ind][i])]}) # None is getting ignored by cac
      else: flg = False
      stack.append(dist_overrides[ind][i])
      solution(ind + 1, n, stack, context, dist_overrides, overrides, table_name, cursor, sn, limit_value, column_names)
    elif (overrides[ind][1] == 'slot'):
      if i + 1 < len(dist_overrides[ind]):
        context.append({"<=":[str(dist_overrides[ind][i]), {"var":convertOneToCamelCase(overrides[ind][0])}, str(dist_overrides[ind][i + 1] - 1)]})
        stack.append((dist_overrides[ind][i], dist_overrides[ind][i + 1]))
        solution(ind + 1, n, stack, context, dist_overrides, overrides, table_name, cursor, sn, limit_value, column_names)
      else:
        context.append({"<=":[str(dist_overrides[ind][i]), {"var":convertOneToCamelCase(overrides[ind][0])}, str(MAX_INT)]}) 
        stack.append((dist_overrides[ind][i], MAX_INT)) 
        solution(ind + 1, n, stack, context, dist_overrides, overrides, table_name, cursor, sn, limit_value, column_names)
    else:
      raise TestFailed("Error: Invalid operator for override!")
    stack.pop()
    if flg: context.pop()


def main(table_name):
  if app == 'dobpp':
    schema_name = 'atlas_driver_offer_bpp'
  else:
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
      n = len(sys.argv)
      if n % 2 == 0:
        raise TestFailed("Error: Invalid number of arguments!")
      overrides = [(sys.argv[i], sys.argv[i + 1]) for i in range(1, n - 1, 2)]
      print("Overrides = ", overrides)
      dist_overrides = []
      for override in overrides:
        query = sql.SQL(f"SELECT distinct {override[0]} FROM {schema_name}.{table_name} order by {override[0]};")
        limit_value = 1000
        cursor.execute(query, [limit_value])
        results = convert_type(cursor.fetchall())
        dist_overrides.append(results)
      print("Distinct Overrides = ", dist_overrides)
      n = len(dist_overrides)
      col_query = sql.SQL(f"SELECT * FROM {schema_name}.{table_name} LIMIT 0;")
      cursor.execute(col_query)
      column_names = convertToCamelCase([desc[0] for desc in cursor.description])
      solution(0, n, [], [], dist_overrides, overrides, table_name, cursor, schema_name, limit_value, column_names)


  except psycopg2.Error as e:
      print(f"Error: {e}")
  finally:
      # Close the cursor and connection
      print("Closing cursor and connection...")
      if cursor:
          cursor.close()
      if connection:
          connection.close()


if __name__ == '__main__':
  for tn in table_names:
    main(tn)