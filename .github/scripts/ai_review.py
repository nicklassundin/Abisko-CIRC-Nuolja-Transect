import requests

def get_diff(repo_path):
    import subprocess
    result = subprocess.run(['git', 'diff'], cwd=repo_path, capture_output=True, text=True)
    return result.stdout

def send_diff_to_ai(diff):
    url = "https://api.openai.com/v1/engines/davinci-codex/completions"
    headers = {"Authorization": f"Bearer {YOUR_API_KEY}"}
    data = {
            "prompt": f"Review the following code diff:\n{diff}",
            "max_tokens": 150
            }
    response = requests.post(url, headers=headers, json=data)
    return response.json()
if __name__ == "__main__":
    repo_path = "../../"
    diff = get_diff(repo_path)
    review = send_diff_to_ai(diff)
    print(review)

