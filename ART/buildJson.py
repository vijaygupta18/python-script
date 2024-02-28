

def processAPIdatainLogFile(input_file_path, output_file_path):
    APIdata = []
    counter = 0
    with open(input_file_path, 'r') as file:
        for line in file:
            print(line)
            json_dict = eval(line)
            APIdata.append(json_dict)
            

    with open(output_file_path, 'w') as output_file:
        output_file.write("{")
        for line in APIdata:
            counter += 1
            api = "API" + str(counter)
            output_file.write('"'+api+'"' + ":" + line + ",\n")

        output_file.write("}")
            
    print("Data written to file: ", output_file_path)
input_file_path = "/home/kv/projects/nammayatri/data.log"
output_file_path = "/home/kv/projects/nammayatri/output_data2.json"
processAPIdatainLogFile(input_file_path, output_file_path)
