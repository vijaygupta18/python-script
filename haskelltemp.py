import os
import re

def camelToSnakeCase(text):
    converted = re.sub(r'(?<!^)(?=[A-Z])', '_', text)
    converted = converted.lower()
    return converted

def modify_file(input_file, fileName, tableName):

    directory = "atlas_driver_offer_bpp" if "dynamic" in str(input_file) else "atlas_app"
    start_marker = "primaryKey = Id"
    start_marker1 = "instance Serialize"
    end_marker = "TMod ::"
    end_marker1 = "$(enableKVPG"
    lines_to_keep = []
    should_keep = True

    with open(input_file, 'r') as file:
        for line in file:
            if end_marker in line:
                should_keep = True
            if end_marker1 in line:
                should_keep = True
            if start_marker1 in line:
                should_keep = False
            if "modelTableName" in line:
                tableName = line.split('=')[1].strip().replace('"','')
            if should_keep:
                lines_to_keep.append(line)
            if start_marker in line:
                should_keep = False
                line = "\ntype "+fileName+" = "+fileName+"T "+"Identity\n\n"
                lines_to_keep.append(line)
    new_line = '$(mkTableInstances '+"''"+fileName+'T '+ '"' +tableName+ '" ' +'"'+directory+'")'     
    with open(input_file, 'w') as file:
        file.writelines(lines_to_keep)
        file.write('\n'+new_line + '\n')

def process_files_in_folder(folder_path):
    for root, _, files in os.walk(folder_path):
        for filename in files:
            if filename.endswith('.hs'):  # Process only files with the '.hs' extension
                input_file_path = os.path.join(root, filename)
                file_name = os.path.splitext(filename)[0]
                table_name = camelToSnakeCase(file_name)
                print(f"Processing file: {input_file_path}")
                modify_file(input_file_path, file_name, table_name)
                print(f"Modified file: {input_file_path}")

if __name__ == "__main__":
    input_folder_path = "/Users/vijay.gupta/Desktop/nammayatri/Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Storage/Beam"
    process_files_in_folder(input_folder_path)

