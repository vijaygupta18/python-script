import re
import os
def overwriteFile(file_path, new_contents):
    with open(file_path, 'w') as file:
        file.write(new_contents)
        file.close()

def getStaticData():
  copyRightString = "{-\n"
  copyRightString += "  Copyright 2022-23, Juspay India Pvt Ltd\n\n"

  copyRightString += "  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License\n\n"

  copyRightString += "  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program\n\n"

  copyRightString += "  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY\n\n"

  copyRightString += "  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of\n\n"

  copyRightString += "  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.\n"
  copyRightString += "-}\n"

  languageExtensions = ''
  languageExtensions += "{-# LANGUAGE DerivingStrategies #-}\n"
  languageExtensions += "{-# LANGUAGE TemplateHaskell #-}\n"
  languageExtensions += "{-# OPTIONS_GHC -Wno-orphans #-}\n\n\n"
  return copyRightString + languageExtensions

def createInstance(dataType) :
  instance = ""
  instance += "instance FromField " + dataType + " where\n"
  instance += "  fromField = fromFieldEnum\n\n"

  instance += "instance HasSqlValueSyntax be String => HasSqlValueSyntax be " + dataType + " where\n"
  instance += "\tsqlValueSyntax = autoSqlValueSyntax\n\n"

  instance += "instainstance BeamSqlBackend be => B.HasSqlEqualityCheck be " + dataType + "\n\n"

  instance += "instance FromBackendRow Postgres " + dataType + "\n\n"
  return instance 

def getImports(fileData,instanceList):
  commonImports = ""
  specificImports = ""
  allData = fileData.split("\n")
  for line in allData:
    if(line.startswith("module")):
      specificImports += line + "\n\n"
    if(line.startswith("import")):
      specificImports += line + "\n"
  
  commonImports += "import qualified Data.Aeson as A\n"
  commonImports += "import Data.ByteString.Internal (ByteString, unpackChars)\n"
  commonImports += "import qualified Data.HashMap.Internal as HM\n"
  commonImports += "import qualified Data.Map.Strict as M\n"
  commonImports += "import Data.Serialize\n"
  commonImports += "import qualified Data.Time as Time\n"
  commonImports += "import qualified Database.Beam as B\n"
  commonImports += "import Database.Beam.Backend\n"
  commonImports += "import Database.Beam.MySQL ()\n"
  commonImports += "import Database.Beam.Postgres\n"
  commonImports += "\t( Postgres,\n"
  commonImports += "\t\tResultError (ConversionFailed, UnexpectedNull),\n"
  commonImports += "\t)\n"
  commonImports += "import Database.PostgreSQL.Simple.FromField (FromField, fromField)\n"
  commonImports += "import qualified Database.PostgreSQL.Simple.FromField as DPSF\n"
  commonImports += "import GHC.Generics (Generic)\n"
  commonImports += "import Kernel.Prelude hiding (Generic)\n"
  commonImports += "import Kernel.Types.Common hiding (id)\n"
  commonImports += "import Lib.UtilsTH\n"
  commonImports += "import Sequelize\n\n"
  commonImports += "fromFieldEnum ::\n"
  commonImports += "  (Typeable a, Read a) =>"
  commonImports += "  DPSF.Field ->\n"
  commonImports += "  Maybe ByteString ->\n"
  commonImports += "  DPSF.Conversion a\n"
  commonImports += "fromFieldEnum f mbValue = case mbValue of\n"
  commonImports += "  Nothing -> DPSF.returnError UnexpectedNull f mempty\n"
  commonImports += "  Just value' ->\n"
  commonImports += "    case (readMaybe (unpackChars value')) of\n"
  commonImports += "      Just val -> pure val\n"
  commonImports += '      _ -> DPSF.returnError ConversionFailed f ' + "Could not 'read' value for 'Rule'.\n\n"
  allInstances=""
  for instance in instanceList:
    allInstances += createInstance(instance)
  return specificImports+commonImports+'\n\n' + allInstances




def camelToSnakeCase(text):
    converted = re.sub(r'(?<!^)(?=[A-Z])', '_', text)
    converted = converted.lower()
    return converted

def extractDataList(fileData,filePath):
    patternField = r'mkPersist\s+[\s\S]*?\|\]'
    extractFields = re.findall(patternField, fileData)
    if(len(extractFields)==0):
        print("No content found in File:",filePath)
        return []
    extractFields = extractFields[0].split('\n')
    return extractFields

def getNewFileData(fileData,filePath,fileName):
  modifiedData =''
  dataList = extractDataList(fileData,filePath)
  if(len(dataList)==0):
    return ''
  dataList= [word for word in dataList if word !='']
  lastIndex = -1
  derivingList = []

  for i in range(len(dataList)):
    dataList[i] = dataList[i].strip()
    dataList[i] = dataList[i].split(' ')
    if(dataList[i][0]=="Primary"):
      lastIndex = i
    if((dataList[i][0].lower())=="deriving" ):
      derivingList = dataList[i][1:]
  
  derivingData = "\tderiving (" + ', '.join(str(elem) for elem in derivingList) +", B.Beamable)\n" 
  modifiedData += "data "+dataList[3][0] + ' f = ' + dataList[3][0:-1][0] +"\n \t{\n" 
  anotherSchema = dataList[3][0]+"Mod :: " + dataList[3][0] + " (B.FieldModification (B.TableField " + dataList[3][0] + "))\n"
  anotherSchema += dataList[3][0]+"Mod = \n B.tableModification\n\t{\n"
  dataTypeList=["Text", "Int", "Bool","UTCTime", "Double"]
  instanceList = []

  for i in range(4, lastIndex):
    modifiedData += "\t\t\t" + dataList[i][0]+ " :: B.C f "
    anotherSchema += "\t\t" + dataList[i][0] + ' = B.fieldNamed "' +camelToSnakeCase(dataList[i][0]) +'",\n'
    if(len(dataList[i])>2) :
      if(dataList[i][-1]=='Maybe'):
        if 'TId' in dataList[i][-2]:
          modifiedData += '(Maybe Text),\n'
        else:
          if(dataList[i][-2] not in dataTypeList):
            instanceList.append(dataList[i][-2])
          modifiedData += '(Maybe '+ dataList[i][-2]+'),\n'
    else:
      if 'TId' in dataList[i][-1]:
        modifiedData += 'Text,\n'
      else:
        if(dataList[i][-1] not in dataTypeList):
            instanceList.append(dataList[i][-1])
        modifiedData += dataList[i][-1]+',\n'
        
  modifiedData += "\t}\n"
  modifiedData = modifiedData.replace("UTCTime", "Time.LocalTime")
  modifiedData += derivingData
  modifiedData += "\ninstance B.Table " + dataList[3][0] + " where\n\t"
  modifiedData += "data PrimaryKey " + dataList[3][0] +" f\n\t\t"
  modifiedData += "= Id (B.C f Text)\n\t\t"
  modifiedData += derivingData 
  modifiedData += "\tprimaryKey = Id . id\n\n"
  modifiedData += "instance ModelMeta "+ dataList[3][0] + " where\n"
  modifiedData += "\tmodelFieldModification = " + dataList[3][0]+"Mod\n"
  modifiedData += '\tmodelTableName = "'+fileName.lower()+'"\n' # Get the information about the table name.
  modifiedData += "\tmkExprWithDefault _ = B.insertExpressions []\n"

  newDataType = dataList[3][0][0:-1]

  modifiedData += "\ntype "+ newDataType + " = " + dataList[3][0] + " Identity\n"
  modifiedData += "\ninstance FromJSON " + newDataType +" where\n"
  modifiedData += "\tparseJSON = A.genericParseJSON A.defaultOptions\n"
  modifiedData += "\ninstance ToJSON " + newDataType + " where\n"
  modifiedData += "\ttoJSON = A.genericToJSON A.defaultOptions\n"
  modifiedData += "\nderiving stock instance Show " + newDataType +"\n\n"
  modifiedData += anotherSchema +"\t}\n"
  modifiedData += ""
  modifiedData = modifiedData.replace("\t", "  ")
  importData = getStaticData() + getImports(fileData,instanceList)
  modifiedData = importData + modifiedData 
  modifiedData += "$(enableKVPG ''" + dataList[3][0] + " ['id] [])"
  return modifiedData
  # print(modifiedData)
  # print(instanceList)


# filePath = '/Users/akhilesh.b/Desktop/nammayatri/Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Storage/Tabular/Booking.hs'
# with open(filePath, 'r') as file:
#     filename=os.path.basename(filePath)
#     filename = filename.split('.')[0]
#     fileContents = file.read()
#     newFileData=getNewFileData(fileContents,filePath,filename)
#     # print(newFilseData)
#     overwriteFile(filePath, newFileData)



path = '/Users/vijay.gupta/Desktop/py/Tabular'
for filename in os.listdir(path):
  file_path = os.path.join(path, filename)
  if os.path.isfile(file_path):
    with open(file_path, 'r') as file:
      filename=os.path.basename(file_path)
      fileExtension = filename.split('.')[1]
      filename = filename.split('.')[0]
      if(fileExtension!='hs'):
        continue
      file_contents = file.read()
      newFileData = getNewFileData(file_contents,file_path,filename)
      if(newFileData==''):
        continue
      overwriteFile('/Users/vijay.gupta/Desktop/py/Beam/'+filename+'.hs', newFileData)
