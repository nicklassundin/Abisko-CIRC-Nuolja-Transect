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

def token_calc(data):
     encoding = tiktoken.encoding_for_model("gpt-4")
     tokens = encoding.encode(data)
     return tokens

def calc_token_to_ai(code):
    tokens = token_calc(code)
    return {
            "tokens": tokens,
            "length": len(tokens)                            
    }

def calc_token_diff_to_ai(code):
    tokens = token_calc(code)
    return {
            "tokens": tokens,
            "length": len(tokens)                            
    }

if __name__ == "__main__":
    repo_path = "../../"
    diff = get_diff(repo_path)
    random = get_random_file(repo_path)
    if diff:
        ai_response = calc_token_diff_to_ai(diff) 
        feedback = str(ai_response.length)
        with open('ai_review_feedback.txt', 'w') as f:
            f.write(feedback)
    else:
        ai_response = calc_token_to_ai(random)
        feedback = str(ai_response.length)
        with open('ai_review_feedback.txt', 'w') as f:
            f.write(feedback)
