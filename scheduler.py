import redis
import json
import uuid
from faker import Faker
from datetime import datetime, timedelta, timezone
import time

# Initialize Redis connection and Faker
r = redis.Redis(host='localhost', port=6379, db=0)
fake = Faker()

# Function to get current time in the desired format with nanosecond precision
def get_current_time_with_ns():
    current_time = datetime.now(timezone.utc)
    nanoseconds = int(time.time_ns() % 1_000_000_000)
    return current_time.strftime(f"%Y-%m-%dT%H:%M:%S.{nanoseconds:09d}Z")

# Function to generate random job data
def generate_job_data():
    created_at = get_current_time_with_ns()
    scheduled_at = (datetime.now(timezone.utc) + timedelta(seconds=fake.random_int(min=1, max=60)))
    scheduled_at_ns = scheduled_at.strftime(f"%Y-%m-%dT%H:%M:%S.{int(time.time_ns() % 1_000_000_000):09d}Z")

    job_data = {
        "createdAt": created_at,
        "currErrors": 0,
        "id": str(uuid.uuid4()),
        "maxErrors": 5,
        "parentJobId": str(uuid.uuid4()),
        "scheduledAt": scheduled_at_ns,
        "shardId": fake.random_int(min=1, max=10),
        "status": "Pending",
        "storedJobInfo": ({
            "storedJobContent": json.dumps({
                "estimatedRideDistance": fake.random_int(min=1000, max=10000),
                "searchTryId": str(uuid.uuid4())
            }),
            "storedJobType": "SendSearchRequestToDriver"
        }),
        "updatedAt": created_at
    }
    k = str(uuid.uuid4())
    v = json.dumps(job_data)
    return {k: v}

# Push 1000 random job data to Redis stream
while True:
    job_data = generate_job_data()
    # time.sleep(0.1)
    r.xadd('Available_Jobs', job_data)
    print(f"Pushed job with ID {job_data.keys()} to stream Available_Jobs with job data: {job_data}")

print("1000 jobs pushed to Redis stream successfully.")
