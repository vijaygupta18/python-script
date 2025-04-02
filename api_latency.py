import pandas as pd
import numpy as np

# Load the datasets
file1 = "/Users/vijaygupta/Downloads/Response99new.csv"
file2 = "/Users/vijaygupta/Downloads/Response99old.csv"

# Load data
df1 = pd.read_csv(file1)
df2 = pd.read_csv(file2)

# Remove leading/trailing spaces in column names
df1.columns = df1.columns.str.strip()
df2.columns = df2.columns.str.strip()

# Get all unique API columns across both datasets
api_columns = set(df1.columns) | set(df2.columns)
api_columns.discard("Time")  # Exclude the 'Time' column

# Ensure both dataframes have the same set of columns by adding missing ones with NaN values
for col in api_columns:
    if col not in df1:
        df1[col] = np.nan
    if col not in df2:
        df2[col] = np.nan

# Convert response times to numeric values
df1 = df1.astype({col: "float64" for col in api_columns}, errors="ignore")
df2 = df2.astype({col: "float64" for col in api_columns}, errors="ignore")

# Compute statistics for both datasets
stats1 = df1[list(api_columns)].agg(["min", "mean", "std"]).transpose()
stats2 = df2[list(api_columns)].agg(["min", "mean", "std"]).transpose()

freq1 = df1[list(api_columns)].count().rename("Count_Old")
freq2 = df2[list(api_columns)].count().rename("Count_New")

# Rename columns
stats1.columns = ["Min_New", "Avg_New", "Std Dev_New"]
stats2.columns = ["Min_Old", "Avg_Old", "Std Dev_Old"]

# Merge results for comparison
comparison = stats1.join(stats2)
comparison = comparison.join(freq1)
comparison = comparison.join(freq2)

# Calculate percentage change safely (handling NaN values)
comparison["Min Change (%)"] = ((comparison["Min_New"] - comparison["Min_Old"]) / comparison["Min_Old"]) * 100
comparison["Avg Change (%)"] = ((comparison["Avg_New"] - comparison["Avg_Old"]) / comparison["Avg_Old"]) * 100
comparison["Std Dev Change (%)"] = ((comparison["Std Dev_New"] - comparison["Std Dev_Old"]) / comparison["Std Dev_Old"]) * 100

# Replace infinities and NaNs in percentage change with 0 (meaning no change)
comparison = comparison.replace([np.inf, -np.inf], np.nan).fillna(0)

# Sort by Avg_New in descending order
comparison = comparison.sort_values(by="Avg_New", ascending=False)

# Save the results to a CSV file
comparison.to_csv("/Users/vijaygupta/Downloads/API_Latency_Comparison_Sortedtp99.csv", index=True)

# Display the results
print("API Latency Comparison Table Generated. Check API_Latency_Comparison_Sorted.csv")