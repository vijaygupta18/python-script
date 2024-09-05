import json
import csv

json_file_path = "/Users/vijay.gupta/Downloads/response.json"
csv_file_path = "/Users/vijay.gupta/Downloads/response.csv"

with open(json_file_path, 'r') as json_file:
    data = json.load(json_file)

with open(csv_file_path, 'w', newline='') as csv_file:
    writer = csv.writer(csv_file)

    headers = data[0].keys()
    writer.writerow(headers)

    for item in data:
        writer.writerow(item.values())

print("CSV file has been created successfully.")
