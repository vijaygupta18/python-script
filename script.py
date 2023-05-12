import re
import os

def camelToSnakeCase(text):
    converted = re.sub(r'(?<!^)(?=[A-Z])', '_', text)
    converted = converted.lower()
    return converted

def extractDataList(fileData,filePath):
    patternField = r'mkPersist\s+[\s\S]*?\|\]'
    extractFields = re.findall(patternField, fileData)
    if(len(extractFields)==0):
        print("No content found in File:",filePath)
        return
    extractFields = extractFields[0].split('\n')
    return extractFields

def getNewFileData(fileData,filePath):
  modifiedData = ''
  dataList = extractDataList(fileData,filePath)
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

  for i in range(4, lastIndex):
    modifiedData += "\t\t\t" + dataList[i][0]+ " :: B.C f "
    anotherSchema += "\t\t" + dataList[i][0] + ' = B.fieldNamed "' +camelToSnakeCase(dataList[i][0]) +'",\n'
    if(len(dataList[i])>2) :
      if(dataList[i][-1]=='Maybe'):
        if 'TId' in dataList[i][-2]:
          modifiedData += '(Maybe Text),\n'
        else:
          modifiedData += '(Maybe '+ dataList[i][-2]+'),\n'
    else:
      if 'TId' in dataList[i][-1]:
        modifiedData += 'Text,\n'
      else:  
        modifiedData += dataList[i][-1]+',\n'
        
  modifiedData += "\t}\n"
  modifiedData = modifiedData.replace("UTCTime", "Time.LocalTime")
  modifiedData += derivingData
  modifiedData += "instance B.Table " + dataList[3][0] + " where\n\t"
  modifiedData += "data PrimaryKey " + dataList[3][0] +" f\n\t\t"
  modifiedData += "= Id (B.C f Text)\n\t\t"
  modifiedData += derivingData 
  modifiedData += "\tprimaryKey = Id . id\n"
  modifiedData += "instance ModelMeta "+ dataList[3][0] + " where\n"
  modifiedData += "\tmodelFieldModification = " + dataList[3][0]+"Mod\n"
  modifiedData += "\tmodelTableName = booking\n" # Get the information about the table name.
  modifiedData += "\tmkExprWithDefault _ = B.insertExpressions []\n"

  newDataType = "type " + dataList[3][0][0:-1]

  modifiedData += newDataType + " = " + dataList[3][0] + " Identity\n"
  modifiedData += "instance FromJSON " + newDataType +" where\n"
  modifiedData += "\tparseJSON = A.genericParseJSON A.defaultOptions\n"
  modifiedData += "instance ToJSON " + newDataType + " where\n"
  modifiedData += "\ttoJSON = A.genericToJSON A.defaultOptions\n"
  modifiedData += "deriving stock instance Show " + newDataType +"\n"
  modifiedData += anotherSchema +"}\n"
  modifiedData += ""
  modifiedData = modifiedData.replace("\t", "  ")
  print(modifiedData)



filePath = '/Users/vijay.gupta/Desktop/nammayatri/Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Storage/Tabular/Booking.hs'
with open(filePath, 'r') as file:
    fileContents = file.read()
    getNewFileData(fileContents,filePath)
print("=====================")
# path = '/Users/akhilesh.b/Desktop/practice/folder/'
# for filename in os.listdir(path):
#     file_path = os.path.join(path, filename)
#     if os.path.isfile(file_path):
#         # print(file_path)
#         with open('/Users/vijay.gupta/Desktop/nammayatri/Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Storage/Tabular/Booking.hs', 'r') as file:
#             file_contents = file.read()
#         fun(file_contents)
#         print("=====================")
