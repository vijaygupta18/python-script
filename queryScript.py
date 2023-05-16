import re
import os
def overwriteFile(file_path, new_contents):
    with open(file_path, 'w') as file:
        file.write(new_contents)
        file.close()




def getFindByIdList(fileContents):
    pattern = r"\n\s*\n" 
    extractFields = re.split(pattern, fileContents)
    extractFields = [x.replace('Transactionable','MonadFlow') for x in extractFields if 'Transactionable' in x]
    return extractFields

# findById (Id driverId) = do
#   dbConf <- L.getOption Extra.EulerPsqlDbCfg
#   case dbConf of
#     Just dbCOnf' -> T.trace (T.unpack driverId) $ either (\x -> T.trace (show x) Nothing) (transformVechileNewToVechile <$>) <$> KV.findWithKVConnector dbCOnf' VN.meshConfig [Se.Is VN.driverId $ Se.Eq driverId]
#     Nothing -> T.trace "Rahull Nothing" $ pure Nothing

def getNewFileData(fileContents,filePath,filename):
    dataList = getFindByIdList(fileContents)
    # print(dataList)
    for i in dataList:
        print()
        print(i)
    modifiedData=''


filePath = '/Users/vijay.gupta/Desktop/nammayatri/Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Storage/Queries/Booking.hs'
with open(filePath, 'r') as file:
    filename=os.path.basename(filePath)
    filename = filename.split('.')[0]
    fileContents = file.read()
    newFileData=getNewFileData(fileContents,filePath,filename)
    # print(newFileData)
    # print(newFilseData)
    # overwriteFile(filePath, newFileData)