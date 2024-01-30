import os

beamTypePath = '/Users/akhilesh.b/Desktop/nammayatri/Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Storage/Beam/Coins/CoinsConfig.hs'
domainTypePath = ''

beamDirectoryPath = '/Users/akhilesh.b/Desktop/nammayatri/Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Storage/Beam'

# Dictionary of beam types
beamTypeDic = {}

# Dictionary for special Types
specialTypeDic = {}

# Repo Paths 
repoPath = []


def modifiedStrip(str,value):
    if str.startswith(value):
        return str[len(value):]
    if str.endswith(value):
        return str[:-len(value)]

# Generate the dictionary of beam types
def getBeamType (path):
    with open (path, 'r') as beamTypeFile:
        for line in beamTypeFile:
            try :
                if  "::" in line:
                    if "--" in line:
                        line = line.split('--')[0]
                    splittext = line.split('::')
                    beamTypeDic[splittext[0].strip(',').strip().strip('{').strip(' ')] = modifiedStrip(splittext[1].strip(),"B.C f").strip('}').strip(',').strip()
            except:
                print("Error in line : ", line) 
    print("Printing the akhilesh", path, '\n\n\n')
    print(beamTypeDic)

def checkAll(path):
    for file in os.listdir(path):
        filePath = os.path.join(path, file)
        if os.path.isdir(filePath):
            checkAll(filePath)
        else:
            getBeamType(filePath)

checkAll(beamDirectoryPath)