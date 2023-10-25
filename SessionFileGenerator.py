import re

import subprocess

# Now convert to the clickhouse query from the sql query.
def getNewType(sqlType):
    sql_to_clickhouse_types = {
        'character': 'Text',
        'varchar': 'Text',
        'text': 'Text',
        'bigint': 'Integer',
        'integer': 'Integer',  # Mapping 'integer' to ClickHouse 'Integer'
        'smallint': 'Integer',  # Mapping 'smallint' to ClickHouse 'Integer'
        'real': 'Double',  # Mapping 'real' to ClickHouse 'Double'
        'numeric(30,2)' : 'Double',
        'double': 'Double',
        'timestamp': 'DateTime',
        'date': 'DateTime',
        'time': 'DateTime',
        'boolean': 'String'
        }
    if sqlType in sql_to_clickhouse_types.keys():
        return sql_to_clickhouse_types[sqlType]
    else:
        return "Text"

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
            columns.append([i[0], getNewType(i[1]), i[1]])
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
import           NestedRecord
import           Codec.Serialise.Class
import           Generics.Deriving.Monoid
import           Data.Text (Text)
import           Data.Aeson
import           GHC.Generics
import           Data.Default.Class
import           Data.Monoid
import           Data.Default.Instances.Containers
import           Control.Lens
import           Data.Aeson.Lens
import           Data.Time.LocalTime
import           Data.Text.Strict.Lens
import           Data.Time.Clock.POSIX
import           Data.Bits
import           Data.Time.Clock
import           Data.Scientific hiding (scientific)
import           Data.Ratio
import           Data.Text.Manipulate
import qualified Data.Text as T
import           Generics.Deriving.Semigroup
import           Sessionizer.Orphans
import qualified Data.Aeson.KeyMap as KM
import           Codec.Serialise.Encoding (encodeWord32, encodeWord8)
import           Codec.Serialise.Decoding (decodeWord32, decodeWord8)
import           Control.Monad
import           Data.Vector (Vector)
import           Data.Word (Word32)\n\n"""

    data += """data Session =
  Session
    { """
    for i in range(len(columns)):
        if(columns[i][1] == "DateTime"):
            data += f"""_{columns[i][0]} :: Last Int\n"""
        else:
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
        data += f"_{columns[i][0]} = _{columns[i][0]} a <> _{columns[i][0]} b\n"
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



def generateSessionizerFile (columns, tableName, bapOrBPP) :
    data = """{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}




"""
    data += f"""module {tableName}.Sessionizer where\n\n

import           Debug.Trace
import           Data.Time.Format.Internal
import           Data.Attoparsec.Text
import qualified Data.Text as T
import           Control.Applicative
import           Data.Aeson
import           Control.Lens.Combinators
import           Control.Lens.Operators
import           Data.Aeson.Lens
import           Data.Monoid
import           Data.Text (Text)
import           Data.Time.Format
import           Data.Time.Clock.POSIX
import           Data.String (IsString)
import           Data.List (elem, foldl) 
import           GHC.Generics hiding (to, from)
import qualified Data.Text.Lazy as TL
import           Data.ByteString (ByteString)
import           Data.Maybe (fromMaybe)
import           Text.Regex (mkRegex,subRegex)
import           Text.Regex.TDFA hiding(empty)
import           {tableName}.Session(Session(Session))
import           Data.Aeson.Text (encodeToLazyText)
import           Data.Time.Clock (nominalDiffTimeToSeconds)
import           Utils(timeComponent, MyType(..), myDefaultValue)
import           Data.Vector(Vector)


"""

    data += f"""logToSession{bapOrBPP}{tableName} :: Object -> MyType
logToSession{bapOrBPP}{tableName} value =
    let session = Just $ value &\n\t\tSession <$>\n"""
    for i in range(len(columns)):
        columnName = ''.join(x.title() for x in columns[i][0].split('_'))
        data += f"""\t\t\ttransform{columnName} <*>\n"""
    data += "\t\t\ttransformTag\n"
    data += """\tin myDefaultValue {"""
    data += f'''_{tableName[0].lower()+tableName[1:]}Session = Last session'''
    data += "}\n\n\n"

    # dataTypeDict = {}
    # for i in range(len(columns)):
    #     dataTypeDict[columns[i][0]] = getNewType(columns[i][1])
    data += """transformTag :: Object -> First Text
transformTag l  = First $ preview (ix "tag"._String.meaningFul) l\n\n"""
    for i in columns:
        col = ''.join(x.title() for x in i[0].split('_'))
        newCol = (''.join(x.title() for x in i[0].split('_'))[0]).lower() + (''.join(x.title() for x in i[0].split('_'))[1:])
        # print(col, newCol)
        if (i[1]) == "Double":
            data += f'''transform{col} :: Object -> Last {(i[1])}\n'''
            data +=f'''transform{col} l  = Last $ preview (ix "contents".key "{newCol}"._Double) l\n\n'''
        elif (i[1]) == "DateTime":
            data += f'''transform{col} :: Object -> Last Int\n'''
            data +=f'''transform{col} l  = Last $ preview (ix "contents".key "{newCol}"._String. to timeComponent._Right ) l\n\n'''

        elif (i[1]) == "Integer":
            data += f'''transform{col} :: Object -> Last {(i[1])}\n'''
            data +=f'''transform{col} l  = Last $ preview (ix "contents".key "{newCol}"._Integral) l\n\n'''
        elif (i[2]== "boolean"):
            data += f'''transform{col} :: Object -> Last {(i[1])}\n'''
            data +=f'''transform{col} l  = Last (convertToString <$> preview (ix "contents".key "{newCol}") l)\n\n'''
        else:
            data += f'''transform{col} :: Object -> Last {(i[1])}\n'''
            data +=f'''transform{col} l  = Last $ preview (ix "contents".key "{newCol}"._String.meaningFul) l\n\n'''
    
    data += """\n\n\n\n{-# INLINE timeComponentInMilliseconds #-}
timeComponentInMilliseconds :: Text -> Either String Int
timeComponentInMilliseconds time =
  let
    timeParser = do
      day <-
        zip "Ymd" <$> some digit `sepBy1` char '-'
      (space <|> char 'T')
      time <-
        zip "HMS" <$> some digit `sepBy1` char  ':'
      _ <- option '.' (char '.')  
      mil <-
        option 0 (decimal)
      takeLazyText
      maybe
        (traceShow ("Unable to form time " <> show time) empty)
        (pure . (\\a -> (a*1000 +(toEnum mil))) . truncate . nominalDiffTimeToSeconds. utcTimeToPOSIXSeconds)
        (buildTime defaultTimeLocale (day ++ time))
  in parseOnly timeParser time

check :: Text -> Int 
check time = T.length $ head $ T.splitOn "-" time 

{-# INLINE meaningFul #-}
meaningFul :: (Eq s, IsString s) => Traversal' s s
meaningFul =
    filtered (not . (`elem` discard))
    where
    discard = ["null", "UNKNOWN", "unknown", ""]

-- stringToInt :: String -> Int
-- stringToInt value read value :: Int

{-# INLINE jsonText #-}
jsonText :: Value -> Text 
jsonText = TL.toStrict . encodeToLazyText 

convertToString :: Value -> String
convertToString (Bool a) = show a
convertToString (String a) = show a
convertToString a = show a"""
    return data

        





pathWrite = '/Users/akhilesh.b/Desktop/atlas-sessionizer/app/BPP/PaymentOrder/Session.hs'
pathWrite1 = '/Users/akhilesh.b/Desktop/atlas-sessionizer/app/BPP/PaymentOrder/Sessionizer.hs'
bapOrBpp = 'BPP'
myTable = "payment_order"
schma = 'atlas_driver_offer_bpp'

filePathSession = open(pathWrite, 'w')
filePathSessionizer = open(pathWrite1, 'w')
file_path = 'clear_tables.sql'
# Read the SQL queries from the file
with open(file_path, 'r') as file:
    sql_queries = file.read()

# Split the content into individual queries
queries = sql_queries.split(';')
# Process each query
for query in queries:
    if query.strip():  # Check if the query is not empty
        # print(f"Processing query: {query};")
        (schemaName, tableName) = getSchemaNameAndTableName(query)
        if tableName == myTable and schma == schemaName:
            print( tableName, myTable)
            components = tableName.split('_')
            tableName = ''.join(x.title() for x in components) 
            # Extract column names and types from the SQL query
            columns = getColumnNameAndType(query)
            print(columns)
            # Generate the session file
            sessionFile = generateSessionFile(columns, tableName)
            # Write the session file
            filePathSession.write(sessionFile)
            # Generate the sessionizer file
            sessionizerFile = generateSessionizerFile(columns, tableName, bapOrBpp)
            # Write the sessionizer file
            filePathSessionizer.write(sessionizerFile)
            break
        # else :
            # print( tableName, myTable)

        