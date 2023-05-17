import re
import os

# get the line number at which import statements ends
def getImportEndLines(file_path):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    import_end_lines = []
    for i, line in enumerate(lines):
        if line.startswith('import ') or line.startswith('from '):
            import_end_lines.append(i + 1)
    return import_end_lines

# get all capital lines of a text
def getFileNameLetters(text):
    capital_letters = re.findall(r'[A-Z]', text)
    return ''.join(capital_letters)

# add imports after a given line
def appendImports(file_path, line_number, line_to_append):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    lines.insert(line_number, line_to_append + '\n')
    with open(file_path, 'w') as file:
        file.writelines(lines)

# add imports at top 
def addImports(fileName,filePath):
    imports=''
    imports+="import qualified Lib.Mesh as Mesh\n"
    imports+="import qualified Sequelize as Se\n"
    imports+="import qualified Storage.Beam."+fileName+" as Beam"+ getFileNameLetters(fileName)+'\n'
    imports+="import qualified EulerHS.KVConnector.Flow as KV\n"
    imports+="import EulerHS.KVConnector.Types\n"
    imports+="import qualified EulerHS.Language as L\n"
    imports+="import qualified EulerHS.Extra.EulerDB as Extra\n"
    lines=getImportEndLines(filePath)
    endLine=lines[-1]
    appendImports(filePath, endLine,imports)

# to append the data in end of file
def appendEnd(file_path, text):
    with open(file_path, 'a') as file:
        file.write(text)

# to extract the required data
def extractDataList(fileData,fileName):
    pattern = rf"{fileName} = {fileName}\n  {{(.*?)\}}"
    match = re.search(pattern, fileData, re.DOTALL)
    data=[]
    if match:
        lines = match.group(1).strip().split('\n')
        for x in lines:
            data.append(x.strip())
        return data
    return []    

def getNewFileData(fileData,filePath,fileName):

  addImports(fileName,filePath)

  modifiedData ='\n'
  dataList = extractDataList(fileData,fileName)
  if(len(dataList)==0):
    return ''
  toDomain='\n'
  toDomain += "transformBeam"+fileName+"ToDomain" + " :: " +'Beam'+getFileNameLetters(fileName)+'.'+fileName+' -> '+ fileName+ '\n'
  toDomain += "transformBeam"+fileName+"ToDomain " +'Beam'+getFileNameLetters(fileName)+'.'+ fileName+'T {..} = do\n'
  toDomain += '\t'+fileName+'\n\t\t{\n'

  beamName='Beam'+getFileNameLetters(fileName)
  modifiedData+= "transformDomain"+fileName+"ToBeam" + " :: " + fileName+' -> Beam'+getFileNameLetters(fileName)+'.'+fileName+'\n'
  modifiedData+= "transformDomain"+fileName+"ToBeam " + fileName+' {..} = \n'
  modifiedData+= '\t'+'Beam'+getFileNameLetters(fileName)+'.default'+fileName+'\n\t\t{\n'


  for x in dataList:
    x=x.split('::')
    if len(x)==1:
        continue
    if 'Id ' in x[1]:
        if "Maybe" in x[1]:
            modifiedData+= '\t\t\t'+beamName+'.'+x[0].strip()+' = '+'getId <$> '+x[0].strip()+',\n'
            toDomain+='\t\t\t'+x[0].strip()+' = Id <$> '+x[0].strip()+',\n'
        else:
            modifiedData+= '\t\t\t'+beamName+'.'+x[0].strip()+' = '+'getId '+x[0].strip()+',\n'
            toDomain+='\t\t\t'+x[0].strip()+' = Id '+x[0].strip()+',\n'
    else:
        modifiedData+= '\t\t\t'+beamName+'.'+x[0].strip()+' = '+x[0].strip()+',\n'
        toDomain+='\t\t\t'+x[0].strip()+' = '+x[0].strip()+',\n'
    
  modifiedData=modifiedData[:-2]+'\n'
  modifiedData += "\t\t}\n"
  toDomain=toDomain[:-2]+'\n'
  toDomain += "\t\t}\n"
  modifiedData=toDomain + modifiedData
  modifiedData = modifiedData.replace("\t", "  ")
  return modifiedData



filePath = '/Users/vijay.gupta/Desktop/nammayatri/Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Types/Exophone.hs'
with open(filePath, 'r') as file:
    filename=os.path.basename(filePath)
    filename = filename.split('.')[0]
    fileContents = file.read()
    newFilePath = '/Users/vijay.gupta/Desktop/nammayatri/Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Storage/Queries/Exophone.hs'
    newFileData=getNewFileData(fileContents,newFilePath,filename)
    print(newFileData)
    # appendEnd(newFilePath,newFileData)
    print('Done')



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
