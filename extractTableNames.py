import os

def extract_model_table_names(root_dir, output_file):
    table_names = ''
    with open(output_file, 'w') as output:
        for foldername, _, filenames in os.walk(root_dir):
            for filename in filenames:
                file_path = os.path.join(foldername, filename)
                with open(file_path, 'r') as file:
                    for line in file:
                        if 'modelTableName =' in line:
                            table_name = line.split('=')[1].strip().replace('"', '')
                            table_names+=table_name+' '
                           
        output.write(str(table_names))
    print(table_names)

if __name__ == "__main__":
    root_directory = "/Users/vijay.gupta/Desktop/nammayatri/Backend/app/rider-platform/rider-app/Main/src/Storage/Beam"  # Replace with the root directory you want to search
    output_file = "table.txt"  # Output file to save the extracted lines
    extract_model_table_names(root_directory, output_file)
