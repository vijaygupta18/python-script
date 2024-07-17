import json 

rider_table_path = "rider-tables.json"
driver_table_path = "driver-tables.json"

# query to get all tables 
# SELECT table_name
# FROM information_schema.tables
# WHERE table_schema = 'atlas_app';



def config_value_json(table_list, table_names):
    config_value ={
    "enableKVForWriteAlso": table_list,
    "enableKVForRead": table_names,
    "useCAC": [],
    "useCACForFrontend": "false",
    "readFromMasterDb":[]
    }
    return config_value

def update_query(schema,table_list, table_names):
    return f"""UPDATE {schema}.system_configs SET config_value = '{config_value_json(table_list,table_names)}' WHERE id = 'kv_configs';"""

def get_rider_update_query():
    with open(rider_table_path, 'r') as f:
        data = json.load(f)
        tables = []
        for table in data:
            tables.append(table['table_name'])
        table_list = [{"nameOfTable": str(table), "percentEnable": 100} for table in tables]
        table_names = tables
        return update_query("atlas_app",table_list, table_names)
    
def get_driver_update_query():
    with open(driver_table_path, 'r') as f:
        data = json.load(f)
        tables = []
        for table in data:
            tables.append(table['table_name'])
        table_list = [{"nameOfTable": table, "percentEnable": 100} for table in tables]
        table_names = tables
        return update_query("atlas_driver_offer_bpp",table_list, table_names)
    
rider_query = get_rider_update_query()
driver_query = get_driver_update_query()
with open("rider_query.sql", "w") as f:
    f.write(rider_query.replace("'", "\""))
with open("driver_query.sql", "w") as f:
    f.write(driver_query.replace("'", "\""))
# print(get_driver_update_query())

    
    