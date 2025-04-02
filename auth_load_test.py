import requests
import json
from concurrent.futures import ThreadPoolExecutor

# API details
# url = "https://api.sandbox.beckn.juspay.in/dev/dobpp/ui/auth"

url = "http://localhost:8016/ui/auth"
headers = {
    'Content-Type': 'application/json',
    'token': '{{customer_token}}'
}

# Function to generate unique mobile numbers
def generate_mobile_number(base_number, offset):
    return str(base_number + offset)

# Function to call the API multiple times
def call_api_multiple_times(thread_id, base_mobile_number, num_calls):
    for i in range(num_calls):
        try:
            # Generate a unique mobile number for each call
            # mobile_number = generate_mobile_number(base_mobile_number, thread_id * num_calls + i)
            mobile_number = "6666666666"
            payload = json.dumps({
                "mobileNumber": mobile_number,
                "mobileCountryCode": "+91",
                "merchantId": "favorit0-0000-0000-0000-00000favorit",
                # "merchantId": "7f7896dd-787e-4a0b-8675-e9e6fe93bb8f",
                # "merchantOperatingCity": "Bangalore",
                "merchantOperatingCity" : "Kochi"
            })

            # Make the API request
            response = requests.post(url, headers=headers, data=payload)
            print(f"Thread {thread_id}, Call {i+1}: Mobile {mobile_number}, Status {response.status_code}, Response {response.text}")
        except Exception as e:
            print(f"Thread {thread_id}, Call {i+1}: Error occurred: {e}")

# Number of threads and calls per thread
num_threads = 1
calls_per_thread = 1
base_mobile_number = 1111111111  # Starting mobile number

# Using ThreadPoolExecutor for multi-threading
if __name__ == "__main__":
    with ThreadPoolExecutor(max_workers=num_threads) as executor:
        for thread_id in range(num_threads):
            executor.submit(call_api_multiple_times, thread_id, base_mobile_number, calls_per_thread)


# import requests
# from concurrent.futures import ThreadPoolExecutor
# import time

# # API details
# url = "http://localhost:8013/v2/profile"
# payload = "{\n    \"firstName\" : \"User-Cust\"\n}"
# headers = {
#     'Content-Type': 'application/json;charset=utf-8',
#     'token': '79e25f2b-8ac3-4473-8419-5a327755b035'
# }

# # Function to call the API multiple times
# def call_api_multiple_times(thread_id, num_calls):
#     for i in range(num_calls):
#         try:
#             #  lets add delay of .10s 
#             time.sleep(0.10)
#             response = requests.post(url, headers=headers, data=payload)
#             print(f"Thread {thread_id}, Call {i+1}: Status {response.status_code}, Response {response.text}")
#         except Exception as e:
#             print(f"Thread {thread_id}, Call {i+1}: Error occurred: {e}")

# # Number of threads and calls per thread
# num_threads = 10
# calls_per_thread = 200

# # Using ThreadPoolExecutor for multi-threading
# if __name__ == "__main__":
#     with ThreadPoolExecutor(max_workers=num_threads) as executor:
#         for thread_id in range(num_threads):
#             executor.submit(call_api_multiple_times, thread_id, calls_per_thread)

