import requests
import time

BASE_URL = "http://localhost:8016/ui"
AUTH_URL = f"{BASE_URL}/auth"
VERIFY_URL = f"{BASE_URL}/auth/{{authId}}/verify"
SET_ACTIVITY_URL = f"{BASE_URL}/driver/setActivity"
PROFILE_URL = f"{BASE_URL}/driver/profile"
LOCATION_URL = "http://localhost:8081/ui/driver/location"
NEARBY_RIDE_REQUEST_URL = f"{BASE_URL}/driver/nearbyRideRequest"
OFFER_QUOTE_URL = f"{BASE_URL}/driver/searchRequest/quote/offer"
RIDE_LIST_URL = f"{BASE_URL}/driver/ride/list"
RIDE_START_URL = f"{BASE_URL}/driver/ride/{{rideId}}/start"
RIDE_END_URL = f"{BASE_URL}/driver/ride/{{rideId}}/end"
RIDE_SEARCH_URL = "http://localhost:8013/v2/rideSearch"
RIDE_SEARCH_RESULTS_URL = "http://localhost:8013/v2/rideSearch/{{searchId}}/results"
CUSTOMER_AUTH_URL = "http://localhost:8013/v2/auth"
CUSTOMER_VERIFY_URL = "http://localhost:8013/v2/auth/{{authId}}/verify"

def measure_latency(func):
    def wrapper(*args, **kwargs):
        start_time = time.time()
        response = func(*args, **kwargs)
        end_time = time.time()
        latency = end_time - start_time
        print(f"{func.__name__} latency: {latency:.4f} seconds")
        return response
    return wrapper

@measure_latency
def login():
    payload = {
        "mobileNumber": "6666666666",
        "mobileCountryCode": "+91",
        "merchantId": "favorit0-0000-0000-0000-00000favorit"
    }
    response = requests.post(AUTH_URL, json=payload)
    return response.json()

@measure_latency
def verify(authId):
    payload = {
        "otp": "7891",
        "deviceToken": "8e83b5dc-99a0-4306-b90d-2345f3050972"
    }
    url = VERIFY_URL.format(authId=authId)
    response = requests.post(url, json=payload)
    return response.json()

@measure_latency
def enable_driver(token):
    headers = {"token": token}
    url = SET_ACTIVITY_URL + "?active=true&mode=ONLINE"
    response = requests.post(url, headers=headers)
    return response.json()

@measure_latency
def get_profile(token):
    headers = {"token": token}
    response = requests.get(PROFILE_URL, headers=headers)
    return response.json()

@measure_latency
def update_location(token, lat, lon, time):
    headers = {
        "token": token,
        "Content-Type": "application/json;charset=utf-8"
    }
    payload = [
        {
            "pt": {
                "lat": lat,
                "lon": lon
            },
            "ts": time
        }
    ]
    response = requests.post(LOCATION_URL, json=payload, headers=headers)
    return response.json()

@measure_latency
def get_nearby_ride_requests(token):
    headers = {"token": token}
    response = requests.get(NEARBY_RIDE_REQUEST_URL, headers=headers)
    return response.json()

@measure_latency
def offer_quote(token, search_request_id):
    headers = {
        "token": token,
        "Content-Type": "application/json;charset=utf-8"
    }
    payload = {
        "searchRequestId": search_request_id
    }
    response = requests.post(OFFER_QUOTE_URL, json=payload, headers=headers)
    return response.json()

@measure_latency
def get_ride_list(token):
    headers = {"token": token}
    response = requests.get(RIDE_LIST_URL + "?onlyActive=true&limit=10", headers=headers)
    return response.json()

@measure_latency
def start_ride(token, ride_id, otp, lat, lon):
    headers = {
        "token": token,
        "Content-Type": "application/json;charset=utf-8"
    }
    payload = {
        "rideOtp": otp,
        "point": {
            "lat": lat,
            "lon": lon
        }
    }
    url = RIDE_START_URL.format(rideId=ride_id)
    response = requests.post(url, json=payload, headers=headers)
    return response.json()

@measure_latency
def end_ride(token, ride_id, lat, lon):
    headers = {
        "token": token,
        "Content-Type": "application/json;charset=utf-8"
    }
    payload = {
        "point": {
            "lat": lat,
            "lon": lon
        }
    }
    url = RIDE_END_URL.format(rideId=ride_id)
    response = requests.post(url, json=payload, headers=headers)
    return response.json()

@measure_latency
def customer_login():
    payload = {
        "mobileNumber": "8565017450",
        "mobileCountryCode": "+91",
        "merchantId": "NAMMA_YATRI"
    }
    headers = {
        "Content-Type": "application/json;charset=utf-8"
    }
    response = requests.post(CUSTOMER_AUTH_URL, headers=headers, json=payload)
    return response.json()

@measure_latency
def customer_verify(authId, device_token):
    payload = {
        "otp": "1865",
        "deviceToken": device_token
    }
    headers = {
        "Content-Type": "application/json;charset=utf-8"
    }
    url = CUSTOMER_VERIFY_URL.format(authId=authId)
    response = requests.post(url, headers=headers, json=payload)
    return response.json()

@measure_latency
def ride_search(token, origin_lat, origin_lon, dest_lat, dest_lon):
    headers = {
        "Content-Type": "application/json;charset=utf-8",
        "token": token
    }
    payload = {
        "fareProductType": "ONE_WAY",
        "contents": {
            "origin": {
                "address": {
                    "area": "8th Block Koramangala",
                    "areaCode": "560047",
                    "building": "Juspay Buildings",
                    "city": "Bangalore",
                    "country": "India",
                    "door": "#444",
                    "street": "18th Main",
                    "state": "Karnataka"
                },
                "gps": {
                    "lat": origin_lat,
                    "lon": origin_lon
                }
            },
            "destination": {
                "address": {
                    "area": "6th Block Koramangala",
                    "areaCode": "560047",
                    "building": "Juspay Apartments",
                    "city": "Bangalore",
                    "country": "India",
                    "door": "#444",
                    "street": "18th Main",
                    "state": "Karnataka"
                },
                "gps": {
                    "lat": dest_lat,
                    "lon": dest_lon
                }
            },
            "searchTypes": ["ON_DEMAND", "PUBLIC_TRANSPORT"]
        }
    }
    response = requests.post(RIDE_SEARCH_URL, json=payload, headers=headers)
    return response.json()

@measure_latency
def ride_search_results(token, search_id):
    headers = {
        "token": token
    }
    url = RIDE_SEARCH_RESULTS_URL.format(searchId=search_id)
    response = requests.get(url, headers=headers)
    return response.json()

# def main():
    # auth_response = login()
    # auth_id = auth_response.get('authId')
    
    # verify_response = verify(auth_id)
    # token = verify_response.get('token')
    
    # enable_driver(token)
    # profile_response = get_profile(token)
    
    # org_id = profile_response['organization']['id']
    # vt = profile_response['linkedVehicle']['variant']
    # dm = profile_response['mode']
    
    # current_time = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
    # update_location(token, 12.846907, 77.556936, current_time)
    
    # ride_requests_response = get_nearby_ride_requests(token)
    # search_request_id = ride_requests_response['searchRequestsForDriver'][0]['searchRequestId']
    
    # offer_quote(token, search_request_id)
    # ride_list_response = get_ride_list(token)
    
    # ride_id = ride_list_response['list'][0]['id']
    # ride_otp = ride_list_response['list'][0]['rideOtp']
    
    # start_ride(token, ride_id, ride_otp, 12.959849, 77.611269)
    
    # customer_auth_response = customer_login()
    # customer_auth_id = customer_auth_response.get('authId')
    
    # random_device_token = "some_random_device_token"  # Generate or get a random device token
    # customer_verify_response = customer_verify(customer_auth_id, random_device_token)
    # customer_token = customer_verify_response.get('token')
    
    # ride_search_response = ride_search(customer_token, 12.846907, 77.556936, 12.846907, 77.566936)
    # search_id = ride_search_response.get('searchId')
    
    # ride_search_results_response = ride_search_results(customer_token, search_id)
    
    # end_ride(token, ride_id, 12.94005, 77.62264)

def get_driver_token():
    auth_response = login()
    auth_id = auth_response.get('authId')
    verify_response = verify(auth_id)
    token = verify_response.get('token')
    enable_driver(token)
    return token

def get_customer_token():
    customer_auth_response = customer_login()
    customer_auth_id = customer_auth_response.get('authId')
    random_device = "some_random_device_token"
    customer_verify_response = customer_verify(customer_auth_id, random_device)
    customer_token = customer_verify_response.get('token')
    return customer_token



def ride_search_and_results(token, customer_token):
    current_time = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
    now = time.time()
    update_location(token, 12.846907, 77.556936, current_time)
    ride_search_response = ride_search(customer_token, 12.846907, 77.556936, 12.846907, 77.566936)
    ride_search_end_time = time.time()
    print(f"Ride search latency: {ride_search_end_time - now:.4f} seconds")
    search_id = ride_search_response.get('searchId')
    now = time.time()
    ride_search_results_response = {}
    while ride_search_results_response.get('estimate') is None:
        ride_search_results_response = ride_search_results(customer_token, search_id)
    search_results_end_time = time.time()
    print(f"Ride search results latency: {search_results_end_time - now:.4f} seconds")
    return ride_search_results_response


def main():
    driver_token = get_driver_token()
    customer_token = get_customer_token()
    ride_search_and_results(driver_token, customer_token)

if __name__ == "__main__":
    main()
