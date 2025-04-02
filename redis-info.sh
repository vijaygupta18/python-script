
# Replace this with your Redis cluster endpoint
REDIS_HOST="redis.fdmlwb.clustercfg.aps1.cache.amazonaws.com"
REDIS_PORT=6379
PATTERN="*bpp_rt_*"

# Temporary file to store results
RESULT_FILE="/tmp/keys_info.txt"

# Clear the result file
> "$RESULT_FILE"

# Step 1: Fetch the slot-to-node mapping
echo "Fetching cluster slot-to-node mapping..."
SLOT_TO_NODE=$(redis-cli -c -h "$REDIS_HOST" -p "$REDIS_PORT" cluster slots | \
  awk '
  BEGIN { OFS=":" }
  $1 ~ /^[0-9]+$/ { start = $1; next }
  $2 ~ /^[0-9]+$/ { end = $2; next }
  $0 ~ /^[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+/ {
    node = $1; port = $(NF-1);
    for (slot = start; slot <= end; slot++) {
      print slot, node ":" port;
    }
  }
  ')

# Step 2: Scan for keys matching the pattern
echo "Scanning for keys matching pattern '$PATTERN'..."
redis-cli -c -h "$REDIS_HOST" -p "$REDIS_PORT" --scan --pattern "$PATTERN" | while read -r KEY; do
  # Get the slot for the key
  SLOT=$(redis-cli -c -h "$REDIS_HOST" -p "$REDIS_PORT" cluster keyslot "$KEY")

  # Find the node handling this slot
  NODE=$(echo "$SLOT_TO_NODE" | awk -v slot="$SLOT" '$1 == slot { print $2 }')

  # Append the information to the result file
  echo "$KEY | Slot: $SLOT | Node: $NODE" >> "$RESULT_FILE"
done

# Step 3: Display the results
echo "Results:"
cat "$RESULT_FILE"
