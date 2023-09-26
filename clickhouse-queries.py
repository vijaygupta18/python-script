import subprocess

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