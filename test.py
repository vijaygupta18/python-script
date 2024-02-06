pathOfFile = "/home/akhilesh/Desktop/projects/nammayatri/Backend/dynamic-offer-driver-app-exe.log"
print(pathOfFile)
def processFile():
    # Read the file and process it
    with open(pathOfFile, 'r') as f:
        for line in f:
            # Do something with the line
            if "Latency for redisFindAll" in line:
                value = line.find("Latency for redisFindAll")
                print(line[value:-3])

processFile()
