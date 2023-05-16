import re
import os

def getInstance(instanceList):
  instance='\n'
  for x in instanceList:
    instance+="instance IsString "+ x+" where\n"
    instance+="\tfromString = show\n\n"
  return instance  


def append_text_to_second_last_line(file_path, text):
    with open(file_path, 'r+') as file:
        lines = file.readlines()

        if len(lines) >= 2:
            lines[-2] = lines[-2].rstrip('\n') + text + '\n'

        file.seek(0)
        file.writelines(lines)


def extractDataList(fileData,filePath):
    patternField = r'mkPersist\s+[\s\S]*?\|\]'
    extractFields = re.findall(patternField, fileData)
    if(len(extractFields)==0):
        print("No content found in File:",filePath)
        return []
    extractFields = extractFields[0].split('\n')
    return extractFields

def getNewFileData(fileData,filePath,fileName):
  modifiedData ='\n'
  instanceList = []
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
  

  modifiedData+= "default" + fileName + " :: " + fileName+'\n'
  modifiedData+= "default" + fileName+' = \n'
  modifiedData+= '\t'+fileName+'T\n\t\t{\n'
  dataTypeList=["Text", "Int", "Bool","UTCTime", "Double"] 
  for i in range(4, lastIndex):
    if(len(dataList[i])>2) :
      if(dataList[i][-1]=='Maybe'):
        if 'TId' in dataList[i][-2]:
          test='Move'
        else:
          if(dataList[i][-2] not in dataTypeList):
            instanceList.append(dataList[i][-2])
    else:
      if 'TId' in dataList[i][-1]:
        test='Move'
      else:
        if(dataList[i][-1] not in dataTypeList):
            instanceList.append(dataList[i][-1])

    if "Maybe" in str(dataList[i]):
        modifiedData+='\t\t\t'+dataList[i][0]+" = "+'Nothing,\n'
    else:
        if dataList[i][1]=='UTCTime':
            modifiedData+='\t\t\t'+dataList[i][0]+" = "+'defaultDate,\n'
        if dataList[i][1]=='Bool':
          modifiedData+='\t\t\t'+dataList[i][0]+" = "+'False,\n'
        if dataList[i][1]=='Int':
          modifiedData+='\t\t\t'+dataList[i][0]+" = "+'0,\n'
        else:    
            modifiedData+='\t\t\t'+dataList[i][0]+" = "+'"",\n'
  modifiedData=modifiedData[:-2]+'\n'
  modifiedData += "\t\t}\n"
  modifiedData+='instance Serialize '+fileName+' where\n'

  modifiedData+='\tput = error "undefined"\n'
  modifiedData+='\tget = error "undefined"\n'
  modifiedData=getInstance(instanceList)+modifiedData
  modifiedData = modifiedData.replace("\t", "  ")
  return modifiedData



filePath = '/Users/vijay.gupta/Desktop/nammayatri/Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Storage/Tabular/RegistrationToken.hs'
with open(filePath, 'r') as file:
    filename=os.path.basename(filePath)
    filename = filename.split('.')[0]
    fileContents = file.read()
    newFileData=getNewFileData(fileContents,filePath,filename)
    # append_text_to_second_last_line('/Users/vijay.gupta/Desktop/nammayatri/Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Storage/Beam/Booking.hs',newFileData)
    print(newFileData)



# path = '/Users/vijay.gupta/Desktop/nammayatri/Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Storage/Tabular'
# for filename in os.listdir(path):
#   file_path = os.path.join(path, filename)
#   if os.path.isfile(file_path):
#     with open(file_path, 'r') as file:
#       filename=os.path.basename(file_path)
#       fileExtension = filename.split('.')[1]
#       filename = filename.split('.')[0]
#       if(fileExtension!='hs'):
#         continue
#       file_contents = file.read()
#       newFileData = getNewFileData(file_contents,file_path,filename)
#       if(newFileData==''):
#         continue
#       append_text_to_second_last_line('/Users/vijay.gupta/Desktop/nammayatri/Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Storage/Beam/'+filename+'.hs', newFileData)
