import time
import requests
import json
import re
token = ""
authId = ""

def replace_between_texts(original_string, start_text, end_text, new_text):
    """
    Replace everything between start_text and end_text in the original_string with new_text.
    Returns the modified string including start_text and end_text.
    """
    pattern = re.compile(re.escape(start_text) + '(.*?)' + re.escape(end_text), re.DOTALL)
    return re.sub(pattern, start_text + new_text + end_text, original_string)

def handleResponseTokens(response):
    response = response.json()
    global token
    global authId
    if "token" in response:
        token = response["token"]
    if "authId" in response:
        authId = response["authId"]

def handleUrl(host, urlPath):
    if "verify" in urlPath and authId != "":
        return host+replace_between_texts(urlPath, "auth/", "/verify", authId)
    else: return host+urlPath

def callAPI(json_filePath):
    with open(json_filePath, 'r') as file:
        api_data = json.load(file)
        for api_name, api_details in api_data.items():
            host = "http://" + dict(api_details["requestHeaders"])['Host']
            urlPath = api_details["rawPathInfo"]
            handledUrl = handleUrl(host,urlPath) + api_details["rawQueryString"]
            newheaders=dict(api_details["requestHeaders"])
            if len (api_details["requestBody"]) >1: 
                requestBody = json.loads(api_details["requestBody"]) 
            else : 
                requestBody = api_details["requestBody"] 
            print(f"Calling API: {handledUrl}..............")
            if token != "":
                newheaders['token'] = token
            response = requests.request(
                method=api_details["requestMethod"],
                url=handledUrl,
                headers=newheaders,
                json=requestBody
            )
            if response.status_code == 200:
                handleResponseTokens(response)
                print(f"Response: {response.json()}")
            else :
                print(f"ErrorCode: {response.status_code}")
                print(f"Error: {response.text}")
            print("Sleeping for 1 seconds............")
            time.sleep(1)

callAPI ("/home/kv/projects/nammayatri/output_data.json")