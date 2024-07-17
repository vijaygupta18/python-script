import threading
import requests
import datetime
import json

# Define the number of threads
NUM_THREADS = 100
NUM_REQUESTS = 10

def get_current_timestamp():
    return datetime.datetime.now().isoformat()

# Define the API endpoints and headers
driver_location_url = 'http://localhost:8081/ui/driver/location'
ride_search_url = 'http://localhost:8013/v2/rideSearch'

driver_location_headers = {
    'Content-Type': 'application/json;charset=utf-8',
    'token': 'bb6da79f-3e1b-4691-a4dd-0873f3013efd',
    'mId': 'favorit0-0000-0000-0000-00000favorit',
    'vt': 'AUTO_RICKSHAW',
    'dm': 'ONLINE'
}

ride_search_headers = {
    'Content-Type': 'application/json;charset=utf-8',
    'token': 'b7d8de2d-8cdb-4940-9c7f-535bbf0b2e39'
}


driver_location_data = [
    {
        "pt": {
            "lat": 12.95247991,
            "lon": 77.6050944
        },
        "ts": get_current_timestamp() + "+00:00",
    }
]

ride_search_data = {
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
                "lat": 12.952479909463571,
                "lon": 77.60509448873273
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
                "lat": 12.94840464059951,
                "lon": 77.58996589788522
            }
        }
    }
}


def call_driver_location():
    for _ in range(NUM_REQUESTS):
        response = requests.post(driver_location_url, headers=driver_location_headers, json=driver_location_data)
        print(f"Ride Search API response status: {response.json()}")
        print(f"Driver Location API response status: {response.status_code}")

def call_ride_search():
    for _ in range(NUM_REQUESTS):
        response = requests.post(ride_search_url, headers=ride_search_headers, json=ride_search_data)
        print(f"Ride Search API response status: {response.status_code}")

if __name__ == "__mainn__":
    threads = []

    # Create threads for driver location API
    for _ in range(NUM_THREADS // 2):
        thread = threading.Thread(target=call_driver_location)
        threads.append(thread)
        thread.start()

    # Create threads for ride search API
    for _ in range(NUM_THREADS // 2):
        thread = threading.Thread(target=call_ride_search)
        threads.append(thread)
        thread.start()

    # Wait for all threads to complete
    for thread in threads:
        thread.join()

    print("Load testing complete")

call_driver_location()