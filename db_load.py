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
        while True:  # Infinite loop to keep reading
            print(f"Thread {threading.current_thread().name} executing a read query.")
            cursor.execute("SELECT * FROM sample_data")
            print(f"Thread {threading.current_thread().name} executed a read query.")
            cursor.fetchall()  # Fetch all results
            print(f"Thread {threading.current_thread().name} executed a read query.")
            time.sleep(1)  # Add a small delay to avoid overwhelming the database

        cursor.close()
        connection_pool.putconn(conn)
    except Exception as e:
        print(f"Error in thread {threading.current_thread().name}: {e}")

# Create threads to simulate read load
threads = []
num_threads = 5  # Number of concurrent threads
print(f"Starting {num_threads} threads to read data.")
for _ in range(num_threads):
    thread = threading.Thread(target=read_data)
    threads.append(thread)
    thread.start()
    print(f"Thread {thread.name} started.")

# Wait for all threads to finish (they won't, due to the infinite loop)
try:
    while True:
        time.sleep(1)  # Keep the main thread alive
except KeyboardInterrupt:
    print("Interrupt received, stopping threads.")

# Close the connection pool and join threads
for thread in threads:
    thread.join()

# Close the connection pool
connection_pool.closeall()

print("Read load test completed.")
