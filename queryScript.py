import re
import os


def getImports(fileContents):
    commonImports = ""
    specificImports = ""
    allData = fileContents.split("\n")
    for line in allData:
        if(line.startswith("module")):
            specificImports += line + "\n\n"
        if(line.startswith("import")):
            specificImports += line + "\n"
    commonImports += "import qualified Data.Text as T\n"
    commonImports += "import qualified Debug.Trace as T\n"
    commonImports += "import qualified EulerHS.Extra.EulerDB as Extra\n"
    commonImports += "import qualified EulerHS.KVConnector.Flow as KV\n"
    commonImports += "import qualified EulerHS.Language as L\n"
    commonImports += "import Sequelize as Se\n"
    commonImports += "import qualified Storage.Tabular.VechileNew as VN\n"
    return specificImports+commonImports



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
    print(getImports(fileContents))
    # for i in dataList:
    #     print()
    #     print(i)
    modifiedData=''


filePath = '/Users/akhilesh.b/Desktop/nammayatri/Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Storage/Queries/Vehicle.hs'
with open(filePath, 'r') as file:
    filename=os.path.basename(filePath)
    filename = filename.split('.')[0]
    fileContents = file.read()
    newFileData=getNewFileData(fileContents,filePath,filename)
    # print(newFileData)
    # print(newFilseData)
    # overwriteFile(filePath, newFileData)