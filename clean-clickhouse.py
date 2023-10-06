fileData = open("create_tables.sql", 'r')
file_path = 'clear_tables.sql'
newFile = open(file_path, 'a+')
primaryKey = {}
f = False
count = 0
tableName = ""
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
    if "ALTER TABLE ONLY" in i:
        tableName = i.split('.')[1].strip('\n')
    elif "PRIMARY KEY" in i:
        primaryKey[tableName] = '('+i.split('(')[1].strip('\n').strip(';')
print(primaryKey)