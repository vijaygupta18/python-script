import requests

# Define your URLs
FIRST_URL = "https://api.sandbox.beckn.juspay.in/context/list"
SECOND_URL = "https://api.sandbox.beckn.juspay.in/context"

# Define your headers
headers = {
    'Authorization': 'Bearer 12345678',
    'X-Tenant': 'atlas_driver_offer_bpp_v2'
}

def get_context_ids(url):
    response = requests.get(url, headers=headers)
    if response.status_code == 200:
        data = response.json()
        return [context["id"] for context in data]
    else:
        print(f"Failed to fetch data: {response.status_code}")
        return []

def delete_context(id):
    response = requests.delete(f"{SECOND_URL}/{id}", headers=headers)
    if response.status_code == 200:
        print(f"Successfully deleted context with id: {id}")
    else:
        print(f"Failed to delete context with id: {id}, Status Code: {response.status_code}")

def main():
    context_ids = get_context_ids(FIRST_URL)
    print(context_ids)
    for context_id in context_ids:
        delete_context(context_id)

if __name__ == "__main__":
    main()