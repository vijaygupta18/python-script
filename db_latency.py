import psycopg2
from psycopg2 import pool
import threading
import random
import time

# Database connection parameters for the read replica
db_params = {
    'dbname': '',
    'user': 'postgres',  # Replace with your username
    'host': '',  # Replace with your read replica host (endpoint)
    'password': '',  # Replace with your password
    'port': '5432'      # Replace with your DB port (usually 5432)
}
# Initialize the connection pool
print("Initializing connection pool.")
connection_pool = pool.SimpleConnectionPool(1, 10, **db_params)
print("Connection pool initialized.")

def read_data():
    try:
        # Get a connection from the pool
        conn = connection_pool.getconn()
        print(f"Thread {threading.current_thread().name} got a connection from the pool.")
        cursor = conn.cursor()
        print(f"Thread {threading.current_thread().name} got a cursor.")
        query_count = 0
        total_latency = 0

        while query_count < 30:  # Measure latency for 10 queries
            start_time = time.time()
            cursor.execute("SELECT * FROM sample_data")
            cursor.fetchall()  # Fetch all results
            end_time = time.time()
            
            query_latency = end_time - start_time
            total_latency += query_latency
            query_count += 1
            
            print(f"Thread {threading.current_thread().name} executed query {query_count} with latency: {query_latency:.6f} seconds.")

        # Calculate and print the average latency for the 10 queries
        avg_latency = total_latency / query_count
        print(f"Thread {threading.current_thread().name} average latency over 10 queries: {avg_latency:.6f} seconds.")

        cursor.close()
        connection_pool.putconn(conn)
    except Exception as e:
        print(f"Error in thread {threading.current_thread().name}: {e}")

# Create threads to simulate read load
threads = []
num_threads = 2  # Number of concurrent threads
print(f"Starting {num_threads} threads to read data.")
for _ in range(num_threads):
    thread = threading.Thread(target=read_data)
    threads.append(thread)
    thread.start()
    print(f"Thread {thread.name} started.")

# Wait for all threads to finish
for thread in threads:
    thread.join()

# Close the connection pool
connection_pool.closeall()

print("Read load test with latency measurement completed.")
