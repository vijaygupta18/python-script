import threading
import subprocess

# Configure your Redis connection here
REDIS_HOST = 'localhost'
REDIS_PORT = 6379

# Number of threads to run in parallel
NUM_THREADS = 100

def generate_load():
    while True:
        # Performing KEYS * operation
        subprocess.run(['redis-cli', '-h', REDIS_HOST, '-p', str(REDIS_PORT), 'KEYS', '*'], capture_output=True, text=True)
        print(f"Thread {threading.current_thread().name} performed KEYS *")

if __name__ == "__main__":
    threads = []
    for i in range(NUM_THREADS):
        thread = threading.Thread(target=generate_load)
        threads.append(thread)
        thread.start()

    for thread in threads:
        thread.join()
