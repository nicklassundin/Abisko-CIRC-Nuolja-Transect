import os
import requests


def get_diff(repo_path):
    import subprocess
    result = subprocess.run(['git', 'diff'], cwd=repo_path, capture_output=True, text=True)
    return result.stdout

def send_diff_to_ai(diff):
    github_token = os.getenv('GITHUB_TOKEN')
    openai_api_key = os.getenv('OPENAI_API_KEY')
    url = "https://api.openai.com/v1/engines/davinci-codex/completions"
    headers = {"Authorization": f"Bearer {openai_api_key}"}
    data = {
            "prompt": f"Review the following code diff:\n{diff}",
            "max_tokens": 150
            }
    response = requests.post(url, headers=headers, json=data)
    return response.json()
if __name__ == "__main__":
    repo_path = "../../"
    diff = get_diff(repo_path)
    if diff:
        ai_response = send_diff_to_ai(diff)
        feedback = ai_response['choices'][0]['text'].strip()
        with open('ai_review_feedback.txt', 'w') as f:
        f.write(feedback)
    else:
        with open('ai_review_feedback.txt', 'w') as f:
        f.write("No changes to review.")

