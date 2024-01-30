import json

def compare_jsons(json_list, reference_json):
    differing_key_values = {}

    for idx, json_data in enumerate(json_list):
        for key, value in reference_json.items():
            if key not in json_data or json_data[key] != value:
                differing_key_values.setdefault(idx, {})[key] = json_data.get(key)

    return differing_key_values



# Example usage:
json_list = [
    {"name": "John", "age": 30, "city": "New York"},
    {"name": "Alice", "age": 25, "city": "San Francisco"},
    {"name": "Bob", "age": 35, "city": "Los Angeles"}
]

reference_json = {"name": "John", "age": 30}

context = []

differences = compare_jsons(json_list, reference_json)

if differences:
    print("Differences found:")
    for idx, diff in differences.items():
        print(f"JSON at index {idx}: {diff}")
else:
    print("No differences found.")