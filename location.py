from geopy.distance import geodesic
import json
from datetime import datetime

# Define the coordinates of the two points
file_path = "/Users/vijay.gupta/Downloads/query_result_2024-05-28T03_26_18.215032Z.json"
lat_lon = []
data = []
speed = 0
# lets read a file which have all the lat longs as json in a list
with open(file_path, 'r') as f:
    data = json.load(f)

driver_eda = {}

# let's get distances in meters from first to 5th point
for i in range(len(data)):
    if data[i]["Rid"] not in driver_eda:
        driver_eda[data[i]["Rid"]] = []
    driver_eda[data[i]["Rid"]].append({ "Lat": data[i]["Lat"], "Lon": data[i]["Lon"],"Ts": data[i]["Ts"],"Speed": data[i]["Speed"]})


range_frame = 5

results = {}


def parse_timestamp(timestamp):
    # Define possible formats
    formats = [
        "%Y-%m-%dT%H:%M:%S.%f",
        "%Y-%m-%dT%H:%M:%S"
    ]
    for fmt in formats:
        try:
            return datetime.strptime(timestamp, fmt)
        except ValueError:
            continue
    raise ValueError(f"Timestamp {timestamp} is not in a recognized format")


def diff_between_two_times(time1, time2):
    # 2024-05-13T06:10:09.217
    t1 = parse_timestamp(time1)
    t2 = parse_timestamp(time2)
    return (t2 - t1).total_seconds()

def get_distance(value, j):
    distance = 0
    for k in range(j, len(value)-1):
        distance += geodesic((value[k]["Lat"], value[k]["Lon"]), (value[k+1]["Lat"], value[k+1]["Lon"])).meters
    return distance
def get1():
    print("Driver ID", "Range", "Avg_speed", "ETA", "Timestamp")
    for key, value in driver_eda.items():
        results[key] = []
        speed = 0
        for i in range(0, len(value)):
            speed = 0
            speed_lts = 0
            distance_range = 0
            time_range = 0
            if i + range_frame < len(value)-1:
                for j in range(i, i + range_frame):
                    distance = geodesic((value[j]["Lat"], value[j]["Lon"]), (value[j+1]["Lat"], value[j+1]["Lon"])).meters
                    time = diff_between_two_times(value[j]["Ts"], value[j+1]["Ts"])
                    distance_range += distance
                    time_range += time
                    # print("Distance", distance, "Time", time)
                    speed_lts += value[j]["Speed"]
                    if time == 0:
                        continue
                    speed += distance/time
                    # print(key, i, speed, distance, time)
                avg_speed = speed/(range_frame-1)
                avg_speed_lts = speed_lts/range_frame
                avg_speed_v2 = distance_range/time_range
                if avg_speed == 0 or avg_speed_lts == 0 or avg_speed_v2 == 0:
                    print("Avg speed is zero")
                    speed = 0
                    continue
                distance_total = get_distance(value,j+1)
                eta_v2 = distance_total/avg_speed_v2
                eta_lts = distance_total/avg_speed_lts
                eta = distance_total/avg_speed
                actual_time = diff_between_two_times(value[j+1]["Ts"], value[-1]["Ts"])
                results[key].append({(str(i) + "-" + str(i+range_frame)): {"Avg_speed": avg_speed, "ETA": eta, "Timestamp": actual_time, "Distance Remaning": distance_total, "Distance Travelled": distance_range,"Time_range":time_range,"Avg_Speed_v2":avg_speed_v2,"ETA_V2":eta_v2}, "Avg_speed_lts": avg_speed_lts,"ETA_lts": eta_lts})
                speed = 0
            else:
                for j in range(i, len(value)-1):
                    distance = geodesic((value[j]["Lat"], value[j]["Lon"]), (value[j+1]["Lat"], value[j+1]["Lon"])).meters
                    time = diff_between_two_times(value[j]["Ts"], value[j+1]["Ts"])
                    distance_range += distance
                    time_range += time
                    speed_lts += value[j]["Speed"]
                    if time == 0:
                        continue
                    speed += distance/time
                if len(value) == 1:
                    continue
                avg_speed = speed/(len(value)-1)
                avg_speed_lts = speed_lts/(len(value)-1)
                if time_range == 0:
                    continue
                avg_speed_v2 = distance_range/time_range
                if avg_speed == 0 or avg_speed_lts == 0:
                    speed = 0
                    continue
                
                distance_total = get_distance(value,j+1)
                eta_v2 = distance_total/avg_speed_v2
                eta = distance_total/avg_speed
                ets_lts = distance_total/avg_speed_lts
                print("Distance", distance_total, "Speed", avg_speed, "ETA", eta, "Timestamp", actual_time)
                actual_time = diff_between_two_times(value[j+1]["Ts"], value[-1]["Ts"])
                results[key].append({(str(i) + "-" + str(len(value)-1)): {"Avg_speed": avg_speed, "ETA": eta, "Timestamp": actual_time, "Distance Remaning": distance_total, "Distance Travelled": distance_range,"Time_range":time_range,"Avg_Speed_v2":avg_speed_v2,"ETA_V2":eta_v2}, "Avg_speed_lts": avg_speed_lts,"ETA_lts": ets_lts})
                speed = 0
    print ("Completed")

    with open("results.json", 'w') as f:
        json.dump(results, f)
    print("Results saved in results.json") 

def get2():

    print("Driver ID", "Range", "Avg_speed", "ETA", "Timestamp")
    distance_frame = 80
    new_results = {}
    for key, value in driver_eda.items():
        new_results[key] = []
        distance = 0
        frame_index = 0
        avg_speed_new = 0
        lts_avg_speed = 0
        lts_speed = 0
        for i in range(0, len(value)-1):
            distance_new = geodesic((value[i]["Lat"], value[i]["Lon"]), (value[i+1]["Lat"], value[i+1]["Lon"])).meters
            time_new = diff_between_two_times(value[i]["Ts"], value[i+1]["Ts"])
            lts_speed += value[i]["Speed"]
            if time_new !=0:
                avg_speed_new += distance_new/time_new
            distance += distance_new
            if distance >= distance_frame:
                time = diff_between_two_times(value[frame_index]["Ts"], value[i]["Ts"])
                
                if i != frame_index:
                    avg_speed_new = avg_speed_new/(i-frame_index)
                    lts_avg_speed = lts_speed/(i-frame_index)


                if time == 0:
                    distance = 0
                    frame_index = i + 1
                    avg_speed_new = 0
                    continue
                speed = distance/ time
                eta = get_distance(value, i+1)/speed
                eta_new = get_distance(value, i+1)/avg_speed_new
                eta_lts = get_distance(value, i+1)/lts_avg_speed
                actual_time = diff_between_two_times(value[i+1]["Ts"], value[-1]["Ts"])
                initial_latLon = (value[frame_index]["Lat"], value[frame_index]["Lon"])
                final_latLon = (value[i]["Lat"], value[i]["Lon"])
                new_results[key].append({(str(frame_index) + "-" + str(i)): {"Avg_speed": speed, "ETA": eta, "Timestamp": actual_time, "Distance Remaning": get_distance(value, i),"Avg_Speed_v2":avg_speed_new,"ETA_V2":eta_new}, "Avg_speed_lts": lts_avg_speed,"ETA_lts": eta_lts, "Initial_LatLon": initial_latLon, "Final_LatLon": final_latLon})
                frame_index = i
                distance = 0
                avg_speed_new = 0
                lts_speed = 0
                lts_avg_speed = 0
                print("Driver ID", key, "Range", i, "Avg_speed", speed, "ETA", eta, "Timestamp", actual_time)
        print("Driver ID", key, "Completed")


    print("Completed")
    with open("results_distance.json", 'w') as f:
        json.dump(new_results, f)

    print("Results saved in results_distance.json")
            
                
# print("calling get1")
# get1()
print("calling get2")
get2()