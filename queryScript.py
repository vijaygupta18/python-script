import re
import os
def overwriteFile(file_path, new_contents):
    with open(file_path, 'w') as file:
        file.write(new_contents)
        file.close()

def getFindByIdList(fileContents):
    
    extractFields = re.findall(pattern, fileContents)
    return extractFields

def getNewFileData(fileContents,filePath,filename):
    return getFindByIdList(fileContents)


filePath = '/Users/vijay.gupta/Desktop/nammayatri/Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Storage/Queries/Booking.hs'
with open(filePath, 'r') as file:
    filename=os.path.basename(filePath)
    filename = filename.split('.')[0]
    fileContents = file.read()
    newFileData=getNewFileData(fileContents,filePath,filename)
    print(newFileData)
    # print(newFilseData)
    # overwriteFile(filePath, newFileData)