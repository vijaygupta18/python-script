import os
import re

def getFileNameLetters(text):
    capital_letters = re.findall(r'[A-Z]', text)
    return ''.join(capital_letters)

def add_new_line(file_path,filename):
    with open(file_path, 'r') as file:
        lines = file.readlines()

    modified_lines = []
    for line in lines:
        modified_lines.append(line)
        if 'dbConf <- L.getOption KBT.PsqlDbCfg' in line:
            newline = "  let modelName = Se.modelTableName @Beam"+getFileNameLetters(filename)+"."+filename+"T\n"
            newline+= "  updatedMeshConfig <- setMeshConfig modelName\n"
            modified_lines.append(newline)
    with open(file_path, 'w') as file:
        file.writelines(modified_lines)




path = '/Users/vijay.gupta/Desktop/nammayatri/Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Storage/Queries/SearchRequest'
for filename in os.listdir(path):
  file_path = os.path.join(path, filename)
  if os.path.isfile(file_path):
    with open(file_path, 'r') as file:
      filename=os.path.basename(file_path)
      fileExtension = filename.split('.')[1]
      filename = filename.split('.')[0]
      if(fileExtension!='hs'):
        continue
      add_new_line(file_path,filename)