import json

def getRequestBodyData(line):
    for x in line:
        if "body" in x:
            body = x.strip()
            body = body.replace('"', '')
            body = body.replace("body", "")
            body = body.replace("=", "")
            body = body.replace(" ", "")
            body = body.split(",")
            if len(body) == 1:
                return {"body": ""}
            data_dict = {}
            for item in body:
                key, value = item.split(':')
                data_dict[key] = value
            json_data = {"body": data_dict}
            # print(json_data)
            return json_data
            
def getRawPathInfo(line):
    for x in line:
        if "rawPathInfo" in x:
            rawPathInfo = x.strip()
            rawPathInfo = rawPathInfo.replace('"', '')
            rawPathInfo = rawPathInfo.split("=")
            # to get the value of rawPathInfo as JSON
            json_data = {"rawPathInfo": rawPathInfo[1].strip()}
            # print(json_data)
            return json_data

def getHeaders(line):
    for x in line:
        if "requestHeaders" in x:
            requestHeaders = x.strip()
            requestHeaders = requestHeaders.split("=")
            requestHeaders = requestHeaders[1].strip()

            #eval function to convert string to list
            print(requestHeaders)
            data_list = eval(requestHeaders)
            data_dict = dict(data_list)

            # Convert dictionary to JSON
            json_data = {"requestHeaders": data_dict}
                
            # print(json_data)
            return json_data



def getRawQueryString(line):
    for x in line:
        if "rawQueryString" in x:
            rawQueryString = x.strip()
            rawQueryString = rawQueryString.split("=")
            rawQueryString = rawQueryString[1].replace('"', '').strip()

            # to remove the rawQueryString if it is empty or has only ? in it
            if len (rawQueryString) <=1:
                rawQueryString = ""

            # to get the value of rawQueryString as JSON
            json_data = {"rawQueryString": rawQueryString}
            # print(json_data)
            return json_data



def getResponseBody(line):
    line = line.replace("\\", "")
    line = line.replace('"}"}"}',"")
    line = line[1:]
    line_data = json.loads(line)
    # Creating the response dictionary
    response_dict = {"response": line_data}
    # print(response_dict)
    return response_dict

def getRequestMethod(line):
    for x in line:
        if "requestMethod" in x:
            requestMethod = x.strip()
            requestMethod = requestMethod.split("requestMethod")
            requestMethod = requestMethod[1].replace('"', '').replace("=","").strip()
            json_data = {"requestMethod": requestMethod}
            return json_data

def getAPIdata(line):
    oldLine = line
    if "ResponseART" in oldLine:
        response = line.split("response = ")
        response = response[1].strip()
        line = line.strip().split(', ')
        line = [string.replace("\\n", "").replace("\\", "").strip() for string in line]
        line = [string.replace("{", "") for string in line]
        line = [string.replace("}", "").replace("'","") for string in line]
        print(line,"\n***************")
        #this line will contain the data for api as requestId then path then rawquery then headers then body then response
        # print(response)
        requestMethod = getRequestMethod(line)
        headers = getHeaders(line)
        rawPathInfo = getRawPathInfo(line)
        rawQueryString = getRawQueryString(line)
        requestBody = getRequestBodyData(line)
        responseBody = getResponseBody(response)

        # we will use the above data to create a json object and write into a file
        apiData= [requestMethod,headers, rawPathInfo,rawQueryString,requestBody,responseBody]
        return apiData
        


def processAPIdatainLogFile(input_file_path,output_file_path):
    APIdata = []
    counter = 0
    with open(input_file_path, 'r') as file:
        for line in file:
            if "ResponseART" in line:
                apiData = getAPIdata(line)
                counter+=1
                APIdata.append(json.dumps({"API"+str(counter): apiData}))

    with open(output_file_path, 'w') as output_file:
        for line in APIdata:
            output_file.write(line + ',\n')
        output_file.write('\n\n')


def process_log_file(input_file_path, output_file_path):
    requestIds = {}
    with open(input_file_path, 'r') as file:
        for line in file:
            if "ResponseART" in line:
                line = line.strip().split(' ')
                data1 = line[0]
                data2 = line[1]
                if "randomRequestId" in data1:
                    if data1 not in requestIds:
                        requestIds[data1] = []
                    requestIds[data1].append(line)
                if "randomRequestId" in data2:
                    data2 = data2.replace("\\", "").replace("\"", "")
                    if data2 not in requestIds:
                        requestIds[data2] = []
                    requestIds[data2].append(line)

    with open(output_file_path, 'w') as output_file:
        for key, value in requestIds.items():
            for line in value:
                output_file.write(' '.join(line) + '\n')
            output_file.write('\n\n')

    


# Define file paths
input_file_path = "/home/kv/projects/nammayatri/dynamic-offer-ART.log"
output_file_path = "/home/kv/projects/nammayatri/output_data.log"
output_file_path2 = "/home/kv/projects/nammayatri/output_data2.json"

process_log_file(input_file_path, output_file_path)
# processAPIdatainLogFile(input_file_path, output_file_path2)
print("Data processed successfully!")


