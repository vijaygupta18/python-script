def process_log_file(input_file_path, output_file_path):
    requestIds = {}
    with open(input_file_path, 'r') as file:
        for line in file:
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
output_file_path = "/home/kv/projects/nammayatri/output_data.txt"

process_log_file(input_file_path, output_file_path)
print("Data processed successfully!")
