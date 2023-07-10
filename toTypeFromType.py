import os
import re

# get the line number at which import statements ends
def getImportEndLines(file_path):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    import_end_lines = []
    for i, line in enumerate(lines):
        if line.startswith('import ') or line.startswith('from '):
            import_end_lines.append(i + 1)
    return import_end_lines

# add imports after a given line
def appendImports(file_path, line_number, line_to_append):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    lines.insert(line_number, line_to_append + '\n')
    with open(file_path, 'w') as file:
        file.writelines(lines)

# add imports at top 
def addImports(filePath):
    imports=''
    imports+="import Lib.Utils (FromTType' (fromTType'), ToTType' (toTType'), createWithKV, findAllWithKV, findOneWithKV, getMasterDBConfig, updateWithKV, findAllWithOptionsKV, deleteWithKV)\n"
    lines=getImportEndLines(filePath)
    endLine=lines[-1]
    appendImports(filePath, endLine,imports)

def getFileNameLetters(text):
    capital_letters = re.findall(r'[A-Z]', text)
    return ''.join(capital_letters)


def tyepConversion(file_path,fileName):
    beamLine = "transformBeam"+fileName+"ToDomain :: "
    domainLine = "transformDomain"+fileName+"ToBeam :: "
    beamReplacement = "instance FromTType' Beam"+ getFileNameLetters(fileName) +"."+fileName +" "+fileName+" where\n"
    beamReplacement += "  fromTType' Beam"+ getFileNameLetters(fileName) +"."+fileName+"T {..} = do\n"
    domainReplacement = "instance ToTType' Beam"+ getFileNameLetters(fileName) +"."+fileName +" "+fileName+" where\n"
    domainReplacement += "  toTType' "+fileName+" {..} = do\n"

    with open(file_path, 'r') as file:
        lines = file.readlines()
    for i, line in enumerate(lines):
        if re.search(beamLine, line):
            lines.remove(line)
            lines[i] = beamReplacement
            lines[i+1]= "    pure $ Just "+ lines[i+1].strip()+"\n"
            j = i+2
            while True:
                lines[j] = '  ' + lines[j]
                print(lines[j])
                if '}' in  lines[j]: break
                j += 1
        if re.search(domainLine, line):
            lines.remove(line)
            lines[i] = domainReplacement
            j = i+1 
            while True:
                lines[j] = '  ' + lines[j]
                print(lines[j])
                if '}' in  lines[j]: break
                j += 1
        if re.search('module Storage.Queries.',line):
            lines[i-1] =  "{-# OPTIONS_GHC -Wno-orphans #-}\n\n"
    with open(file_path, 'w') as file:
        file.writelines(lines)

def dbConf (file_path,fileName):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    for i, line in enumerate(lines):
        if re.search('dbConf <- L.getOption', line):
            lines[i] = '  dbConf <- getMasterDBConfig\n'
    with open(file_path, 'w') as file:
        file.writelines(lines)

def removeMeshConfig (file_path,fileName):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    for i, line in enumerate(lines):
        if re.search('let modelName = ', line):
            lines.remove(line)
            lines.remove(lines[i])
    with open(file_path, 'w') as file:
        file.writelines(lines)

def removeCase (file_path,fileName):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    filtered_lines = [line for line in lines if "case" not in line]

    with open(file_path, 'w') as file:
        file.writelines(filtered_lines)

def removeNothing (file_path,fileName):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    filtered_lines = [line for line in lines if "Nothing ->" not in line]
    with open(filePath, "w") as file:
        file.writelines(filtered_lines)

def removeLeft (file_path,fileName):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    filtered_lines = [line for line in lines if "Left" not in line]
    with open(file_path, 'w') as file:
        file.writelines(filtered_lines)
    
def removeRight (file_path,fileName):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    filtered_lines = [line for line in lines if "Right" not in line]
    with open(file_path, 'w') as file:
        file.writelines(filtered_lines)

def removeUnderscore (file_path,fileName):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    filtered_lines = [line for line in lines if "_" not in line]
    with open(file_path, 'w') as file:
        file.writelines(filtered_lines)

def removeDo (file_path,fileName):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    for i, line in enumerate(lines):
        if "Just dbCOnf' -> do" in line or "Just dbConf' -> do" in line:
            lines.remove(line)
    with open(file_path, 'w') as file:
        file.writelines(lines)

def createWithKV (file_path,fileName):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    for i, line in enumerate(lines):
        if re.search('KV.createWoReturingKVConnector',line):
            lineData = line.split(' ')
            lines[i] = "  createWithKV dbConf " + lineData[-1][:-2] + '\n'
    with open(file_path, 'w') as file:
        file.writelines(lines)


def findOneWithKV (file_path,fileName):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    for i, line in enumerate(lines):
        if 'KV.findWithKVConnector' in line:
            search_text = "updatedMeshConfig"
            last_index = line.rfind(search_text)
            ending_index = last_index + len(search_text)
            lines[i] = "  findOneWithKV dbConf " + line[ending_index:] + '\n'
    with open(file_path, 'w') as file:
        file.writelines(lines)

def findAllWithKV (file_path,fileName):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    for i, line in enumerate(lines):
        if 'KV.findAllWithKVConnector' in line:
            search_text = "updatedMeshConfig"
            last_index = line.rfind(search_text)
            ending_index = last_index + len(search_text)
            lines[i] = "  findAllWithKV dbConf " + line[ending_index:] + '\n'
    with open(file_path, 'w') as file:
        file.writelines(lines)

def findAllWithOptions (file_path,fileName):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    for i, line in enumerate(lines): 
        if 'KV.findAllWithOptionsKVConnector' in line:
            search_text = "updatedMeshConfig"
            last_index = line.rfind(search_text)
            ending_index = last_index + len(search_text)
            lines[i] = "  findAllWithOptionsKV dbConf " + line[ending_index:] + '\n'
    with open(file_path, 'w') as file:
        file.writelines(lines)

def deleteWithKV (file_path,fileName):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    for i, line in enumerate(lines): 
        if 'KV.deleteWithKVConnector' in line:
            search_text = "updatedMeshConfig"
            last_index = line.rfind(search_text)
            ending_index = last_index + len(search_text)
            lines[i] = "  deleteWithKV "  + '\n'
    with open(file_path, 'w') as file:
        file.writelines(lines)

def updateWithKv (file_path,fileName):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    for i, line in enumerate(lines): 
        if 'KV.updateWoReturningWithKVConnector' in line:
            lines[i] = "  updateWithKV " + '\n'
    with open(file_path, 'w') as file:
        file.writelines(lines)

def removeOther(file_path,fileName):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    for i, line in enumerate(lines):
        if 'updatedMeshConfig' in line:
            lines[i].replace('updatedMeshConfig','')
        if " dbConf' " in line:
            lines[i].replace("dbConf' ","dbConf")
        if "dbCOnf" in line:
            lines[i].replace("dbCOnf","dbConf")
        if 'm (MeshResult ())' in line:
            lines[i].replace('m (MeshResult ())','m ()') 
    with open(file_path, 'w') as file:
        file.writelines(lines)

def replaceData (filePath,fileName):
    tyepConversion(filePath,fileName)
    dbConf(filePath,fileName)
    createWithKV(filePath,fileName)
    findOneWithKV(filePath,fileName)
    findAllWithKV(filePath,fileName)
    findAllWithOptions(filePath,fileName)
    updateWithKv(filePath,fileName)
    deleteWithKV(filePath,fileName)
    removeMeshConfig(filePath,fileName)
    removeCase(filePath,fileName)
    removeLeft(filePath,fileName)
    removeRight(filePath,fileName)
    # removeUnderscore(filePath,fileName)
    removeDo(filePath,fileName)
    removeNothing(filePath,fileName)
    removeOther(filePath,fileName)


filePath = '/Users/vijay.gupta/Desktop/nammayatri/Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Storage/Queries/DriverStats.hs'
with open(filePath, 'r') as file:
    filename=os.path.basename(filePath)
    filename = filename.split('.')[0]
    addImports(filePath)
    replaceData(filePath,filename)
    # print(file.read())
    print('Done')



