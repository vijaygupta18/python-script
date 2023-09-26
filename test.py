import re
# Define the file path with the SQL queries
file_path = 'clear_tables.sql'
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
    print(columns)

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

# Read the SQL queries from the file
with open(file_path, 'r') as file:
    sql_queries = file.read()

# Split the content into individual queries
queries = sql_queries.split(';')

# Process each query
for query in queries:
    if query.strip():  # Check if the query is not empty
        # print(f"Processing query: {query};")
        getColumnNameAndType(query)
        # print(query)
        # columns = re.findall(r'\((.*?)\)', query)
        # print(columns)
        # Perform further processing on the query here
