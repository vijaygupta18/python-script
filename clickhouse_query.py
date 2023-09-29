import re

import subprocess
# Run all the services and build the project in local so you can run all the migration then run this script this will take the dump.
# Define your PostgreSQL connection details
host = 'localhost'
port = '5434'
user = 'postgres'
password = 'root'
database = 'atlas_dev'

# Generate the pg_dump command
pg_dump_command = f'pg_dump -h {host} -p {port} -U {user} -d {database} --schema-only'

# Run the pg_dump command and capture the output
output = subprocess.check_output(pg_dump_command, shell=True)

# Write the output to a SQL file
with open('create_tables.sql', 'wb') as f:
    f.write(output)

print('SQL queries generated and saved to create_tables.sql')

fileData = open('create_tables.sql', 'r')
print(fileData)

#======================================================================================
# Clean all the queries to get only create queries in sql.

fileData = open("create_tables.sql", 'r')
file_path = 'clear_tables.sql'
newFile = open(file_path, 'a+')
f = False
for i in fileData:
    # print(i)
    if "CREATE TABLE" in i and ("atlas_app" in i or "atlas_driver_offer_bpp" in i):
        print(i)
        f = True;
        newFile.write(i)
    elif f:
        print(i)
        newFile.write(i)
        if ";" in i:
            f = False

#===================================================================================================

# Now convert to the clickhouse query from the sql query.
def getClickHouseType(sqlType):
    sql_to_clickhouse_types = {
        'character': 'String',
        'varchar': 'String',
        'text': 'String',
        'bigint': 'Int64',
        'integer': 'Int64',  # Mapping 'integer' to ClickHouse 'Int64'
        'smallint': 'Int64',  # Mapping 'smallint' to ClickHouse 'Int64'
        'real': 'Float64',  # Mapping 'real' to ClickHouse 'Float64'
        'double': 'Float64',
        'timestamp': 'DateTime',
        'date': 'DateTime',
        'time': 'DateTime',
        }
    if sqlType in sql_to_clickhouse_types.keys():
        return sql_to_clickhouse_types[sqlType]
    else:
        return "String"
    
def getColumnNameAndType(query):
    columns = []
    f = False
    l ,r = -1, -1
    for i in range(len(query)):
        if(query[i]=='(' and l ==-1):
            l = i
        if(query[i]==')'):
            r = i
    newQuery = query[l+1:r].split('\n')
    for i in range(len(newQuery)):
        newQuery[i] = newQuery[i].strip()
        newQuery[i] = newQuery[i].split(' ')
    for i in newQuery:
        if(len(i)>1):
            columns.append([i[0], getClickHouseType(i[1])])
    return (columns)

def getSchemaNameAndTableName(query):
    queries = query.split('\n');
    schemanName = "default_schema"
    tableName = "defualt_table"
    for que in queries:
        if "TABLE" in que:
            schemanName = que.split(' ')[2].split('.')[0]
            tableName = que.split(' ')[2].split('.')[1]
            break
    return (schemanName, tableName)

def materializedTable(columns, tableName,schemaName):
    materializedQuery = f"CREATE MATERIALIZED VIEW {schemaName}.{tableName} ON CLUSTER `{{cluster}}` TO {schemaName}.{tableName}\n(\n"
    for column_name,type in columns:
        materializedQuery += f"\t`{column_name}` {type},\n"
    materializedQuery.strip(',\n')
    materializedQuery += ")\n"
    materializedQuery += "\tAS SELECT\n"
    for column_name,type in columns:
        if type == 'String':
            materializedQuery += f"\tifNull(JSONExtractString(message,'{column_name}'),'') as {column_name},\n"
        elif type == 'Int64':
            materializedQuery += f"\tifNull(JSONExtractInt(message,'{column_name}'), 0) as {column_name},\n"
        elif type == 'Float64':
            materializedQuery += f"\tifNull(JSONExtractFloat(message,'{column_name}'),0.0) as {column_name},\n"
        else:
            materializedQuery += f"\ttoDateTime(JSONExtractInt(message,'{column_name}')) as {column_name},\n"
    materializedQuery.strip(',\n')
    materializedQuery += '\n'
    materializedQuery += f"\tFROM {schemaName}.{tableName}_queue\n\twhere JSONExtractString(message,'tag') = "
    value = ''.join(word.capitalize() for word in tableName.split('_'))
    materializedQuery += f"'{value}Object'"
    materializedQuery += '\n\n\n'
    # print(materializedQuery)
    return materializedQuery

# Define the file path with the SQL queries and clickhouse queries
file_path = 'clear_tables.sql'
clickhouse_queries = open('clickhouse_query.sql','w')
# Read the SQL queries from the file
with open(file_path, 'r') as file:
    sql_queries = file.read()


# Split the content into individual queries
queries = sql_queries.split(';')
# Process each query
for query in queries:
    if query.strip():  # Check if the query is not empty
        print(f"Processing query: {query};")
        # Extract column names and types from the SQL query
        columns = getColumnNameAndType(query)
        (schemaName, tableName) = getSchemaNameAndTableName(query)
        # Generate ClickHouse CREATE TABLE statement
        clickhouse_create_table = f"""CREATE TABLE {schemaName}_helper.{tableName}_shard ON CLUSTER `{{cluster}}`
    (\n"""
            # Iterate over the columns and append them to the table creation string
        for column_name, data_type in columns:
            if data_type == "DateTime":
                clickhouse_create_table += f"    `{column_name}` {data_type} DEFAULT now(),\n"
            else:
                clickhouse_create_table += f"    `{column_name}` Nullable ({data_type}),\n"

        # Remove the trailing comma and newline
        clickhouse_create_table = clickhouse_create_table.rstrip(',\n')

        # Complete the table creation string
        clickhouse_create_table += "\n\t)\n"

        clickhouse_create_table += "\nENGINE = ReplicatedCollapsingMergeTree('/clickhouse/{cluster}/tables/{shard}/{database}/{table}', '{replica}', sign)\n"
        clickhouse_create_table += "SETTINGS index_granularity = 8192\n\n"
        clickhouse_create_table += f"CREATE TABLE {schemaName}.{tableName} ON CLUSTER `{{cluster}}` AS {schemaName}_helper.{tableName}_shard\n"
        clickhouse_create_table += f"ENGINE = Distributed(`{{cluster}}`, {schemaName}_helper, {tableName}_shard, xxHash32(id))\n\n"
        clickhouse_create_table += materializedTable(columns, tableName, schemaName)
        clickhouse_queries.write(clickhouse_create_table)