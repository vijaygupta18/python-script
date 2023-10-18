import re

import subprocess

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


def generateSessionFile(columns, tableName):
    data = """{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}\n
"""
    data += f"""module {tableName}.Session where


import qualified Data.HashMap.Strict as HM
import NestedRecord
import Codec.Serialise.Class
import Generics.Deriving.Monoid
import Data.Text (Text)
import Data.Aeson
import GHC.Generics
import Data.Default.Class
import Data.Monoid
import Data.Default.Instances.Containers
import Control.Lens
import Data.Aeson.Lens
import Data.Time.LocalTime
import Data.Text.Strict.Lens
import Data.Time.Clock.POSIX
import Data.Bits
import Data.Time.Clock
import Data.Scientific hiding (scientific)
import Data.Ratio
import Data.Text.Manipulate
import qualified Data.Text as T
import Generics.Deriving.Semigroup
import Sessionizer.Orphans
import qualified Data.Aeson.KeyMap as KM
import Codec.Serialise.Encoding (encodeWord32, encodeWord8)
import Codec.Serialise.Decoding (decodeWord32, decodeWord8)
import Control.Monad
import Data.Word (Word32)\n\n"""

    data += """data Session =
  Session
    { """
    for i in range(len(columns)):
        data += f"""_{columns[i][0]} :: Last {columns[i][1]}\n"""
        if(i!=len(columns)-1):
            data += "\t\t, "
        else :
            data += "\t\t, _tag :: First Text\n"
    data += """\t} deriving (Generic, Show)
$(makeLenses ''Session)

deriving instance Default Session
instance Monoid Session where
  mempty = def
instance Semigroup Session where
  a <> b = Session {
    """
    for i in range(len(columns)):
        data += f"_{columns[i][0]} = _{columns[i][0]} a <> _{columns[i][0]} b,\n"
        if i != len(columns)-1:
            data += "  , "
        else:
            data += "\t, _tag = _tag a <> _tag b\n"
    data += "  }\n\n"
    data += """
opts :: Options
opts =
  defaultOptions
    { fieldLabelModifier = over packed (toSnake . T.tail) }
instance FromJSON Session where
  parseJSON = genericParseJSON opts

instance ToJSON Session where
  toJSON = genericToJSON opts"""
    return data



pathWrite = '/home/akhilesh/Desktop/projects/atlas-sessionizer/app/BPP/PaymentOrder/Session.hs'
filePathSession = open(pathWrite, 'w')
file_path = 'clear_tables.sql'
# Read the SQL queries from the file
with open(file_path, 'r') as file:
    sql_queries = file.read()
myTable = "payment_order"

# Split the content into individual queries
queries = sql_queries.split(';')
# Process each query
for query in queries:
    if query.strip():  # Check if the query is not empty
        # print(f"Processing query: {query};")
        (schemaName, tableName) = getSchemaNameAndTableName(query)
        if tableName == myTable:
            print( tableName, myTable)
            components = tableName.split('_')
            tableName = ''.join(x.title() for x in components) 
            # Extract column names and types from the SQL query
            columns = getColumnNameAndType(query)
            # Generate the session file
            sessionFile = generateSessionFile(columns, tableName)
            # Write the session file
            filePathSession.write(sessionFile)
            break
        else :
            print( tableName, myTable)

        