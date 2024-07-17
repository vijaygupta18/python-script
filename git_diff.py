# To run this script, you need to install requests and colorama
# You can install them using pip
# pip install requests colorama
# You can run this script using python3 git_diff.py
# put your start commit and end commit hash in the script
# This script will get the diff between two commits in a github repository
# It will then print the changes in the sql files and create a release_notes.txt and release_queries.sql file
# The release_notes.txt will contain the commit messages and the author of the commit
# The release_queries.sql will contain the sql queries that were changed in the commits


import requests
from colorama import init, Fore, Style

def get_github_commit_diff(repo_owner, repo_name, start_commit, end_commit, token=None):
    url = f"https://api.github.com/repos/{repo_owner}/{repo_name}/compare/{start_commit}...{end_commit}"
    headers = {
        "Accept": "application/vnd.github.v3+json"
    }
    if token:
        headers["Authorization"] = f"token {token}"
    response = requests.get(url, headers=headers)
    response.raise_for_status()
    data = response.json()
    return data
commits = []
queries = {}
linecheck ={}

def print_sql_file_changes(diff_info):
    print(f"{Fore.CYAN}Status: {diff_info['status']}")
    print(f"{Fore.CYAN}Total commits: {diff_info['total_commits']}")
    print(f"{Fore.YELLOW}Commits:")
    for commit in diff_info['commits']:
        print(f"{Fore.GREEN}Commit: {commit['sha']}")
        print(f"{Fore.BLUE}Author: {commit['commit']['author']['name']}")
        print(f"{Fore.BLUE}Date: {commit['commit']['author']['date']}")
        print(f"{Fore.RED}Message: {commit['commit']['message']}")
        print(f"{Fore.MAGENTA}{'='*80}")
        commits.append((commit['commit']['message'], commit['commit']['author']['name']))
    
    for file in diff_info['files']:
        if file['filename'].endswith('.sql'):
            print(f"{Fore.BLUE}File: {file['filename']}")
            print(f"{Fore.GREEN}Changes: {file['changes']}")
            print(f"{Fore.GREEN}Additions: {file['additions']}")
            print(f"{Fore.RED}Deletions: {file['deletions']}")
            print(f"{Fore.CYAN}Status: {file['status']}")
            queries[file['filename']] = []
            if 'patch' in file:
                for line in file['patch'].split('\n'):
                    if "No newline at end of file" in line:
                        continue
                    if line.startswith('+') and len(line.strip()) > 2 and line[1:] not in linecheck:
                        queries[file['filename']].append(line[1:])
                        linecheck[line[1:]] = 1
                        print(f"{Fore.GREEN}{line}")
                    elif line.startswith('-') and len(line.strip()) > 2 and line[1:] not in linecheck:
                        queries[file['filename']].append("--"+line) 
                        linecheck[line[1:]] = 1
                        print(f"{Fore.RED}{line}")
                    else:
                        print(line)
                        queries[file['filename']].append(line)
            else:
                print(f"   No patch available")
            print(f"{Fore.MAGENTA}{'='*80}")


def print_queries():
    messages = "-- :green-signal: ATLAS MASTER RELEASE BACKEND:green-signal:\n"
    for query in queries:
        messages += f"\n\n-- {query}\n"
        for line in queries[query]:
            messages += f"  {line}\n"
    with open('release_queries.sql', 'w') as f:
        f.write(messages)

def print_release_info():
    messages = ":green-signal: ATLAS MASTER RELEASE BACKEND:green-signal:\n"
    for commit in commits:
        messages += f"- {commit[0]} : @{commit[1]}\n"
    with open('release_notes.txt', 'w') as f:
        f.write(messages)
    print(messages)
if __name__ == "__main__":
    repo_owner = "nammayatri"
    repo_name = "nammayatri"
    # start_commit = input("Please enter your start commit hash: ")
    # end_commit = input("Please enter your end commit hash: ")
    start_commit = "eac87ac1de164f7ebd0248b9a8919ab5f18527df"
    end_commit = "bc0fd2"

    diff_info = get_github_commit_diff(repo_owner, repo_name, start_commit, end_commit, None)
    print_sql_file_changes(diff_info)
    print_release_info()
    print_queries()