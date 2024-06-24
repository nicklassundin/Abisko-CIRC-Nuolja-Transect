import os
import requests
import random
import tiktoken


def get_diff(repo_path):
    import subprocess
    result = subprocess.run(['git', 'diff'], cwd=repo_path, capture_output=True, text=True)
    return result.stdout

def get_random_file(repo_path):
    """Get a random file from the repository.     """
    files = []
    for root, _, filenames in os.walk(repo_path):
        for filename in filenames:
            if filename.endswith('.py'):  # Adjust the file extension as needed
                files.append(os.path.join(root, filename))
    if not files:
        return None
    return random.choice(files)

def send_to_ai(data, prompt):
    github_token = os.getenv('GITHUB_TOKEN')
    openai_api_key = os.getenv('OPENAI_API_KEY')
    url = "https://api.openai.com/v1/chat/completions"
    headers = {"Authorization": f"Bearer {openai_api_key}"}
    data = {
            "model": "gpt-3.5-turbo",
            "messages": [{"role": "system", "content": "You are a code reviewer."},
                         {"role": "user", "content": f"Review the following Python code:\n{data}"}],
            "max_tokens": 150,
            "temperature": 0.5
    }
    response = requests.post(url, headers=headers, json=data)
    return response.json()

def token_calc(data, prompt):
    encoding = tiktoken.encoding_for_model("gpt-3.5-turbo")

    github_token = os.getenv('GITHUB_TOKEN')
    openai_api_key = os.getenv('OPENAI_API_KEY')
    url = "https://api.openai.com/v1/chat/completions"
    headers = {"Authorization": f"Bearer {openai_api_key}"}
    data = {
            "model": "gpt-3.5-turbo",
            "messages": [{"role": "system", "content": "You are a code reviewer."},
                         {"role": "user", "content": f"Review the following Python code:\n{data}"}],
            "max_tokens": 150,
            "temperature": 0.5
    }
    token_1 = encoding.encode(data['messages'][0])
    token_2 = encoding.encode(data['messages'][1])

    result = {
            "tokens": [token_1, token_2],
            "length": len(token_1)+len(token_2),
            "data": data
    }
    response = requests.post(url, headers=headers, json=result)
    return response.json()

def send_code_to_ai(code):
    return send_to_ai(code, "Review the following code and suggest one or two improvements:")
def calc_token_to_ai(code):
    return token_calc(code, "Review the following code and suggest one or two improvements:")

def send_diff_to_ai(diff):
    return send_to_ai(diff, "Review the following code diff:")
def calc_token_diff_to_ai(diff):
    return token_calc(diff, "Review the following code diff:")

if __name__ == "__main__":
    repo_path = "../../"
    diff = get_diff(repo_path)
    random = get_random_file(repo_path)
    if diff:
        ai_response = calc_token_diff_to_ai(diff) 
        feedback = ai_response 
        with open('ai_review_feedback.txt', 'w') as f:
            f.write(feedback)
    else:
        ai_response = calc_token_to_ai(random)
        if 'choices' in ai_response:
            feedback = ai_response
        else:
            feedback = "AI response did not contain 'choices'. Response: " + str(ai_response)
        with open('ai_review_feedback.txt', 'w') as f:
            f.write(feedback)

# if __name__ == "__main__":
#     repo_path = "../../"
#     diff = get_diff(repo_path)
#     random = get_random_file(repo_path)
#     if diff:
#         ai_response = send_diff_to_ai(diff)
#         feedback = ai_response['choices'][0]['text'].strip()
#         with open('ai_review_feedback.txt', 'w') as f:
#             f.write(feedback)
#     else:
#         ai_response = send_code_to_ai(random)
#         if 'choices' in ai_response:
#             feedback = ai_response['choices'][0]['text'].strip()
#         else:
#             feedback = "AI response did not contain 'choices'. Response: " + str(ai_response)
#         with open('ai_review_feedback.txt', 'w') as f:
#             f.write(feedback)

