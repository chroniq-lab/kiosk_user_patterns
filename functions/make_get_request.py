# https://consumer-docs.amplifydata.io/docs/code-examples
# Date: 1 July 2025 (updated 10 months ago)

import random
import requests
import time


def make_get_request(url, params = None, attempt_count = 1, max_attempts = 5,API_KEY = None):
    """call API using GET request, including retry logic"""
    headers = {'X-API-KEY': API_KEY,
               'Content-Type': 'application/json'}

    response = requests.get(url, params=params, headers=headers)

    if response.status_code == 429 and attempt_count <= max_attempts:
        sleep_time = (2 ** attempt_count) + random.uniform(0, 2)
        print(f'Ratelimit reached, retrying after {sleep_time:.1f} seconds')
        time.sleep(sleep_time)
        return make_get_request(url=url, params=params, attempt_count=attempt_count+1)

    if response.status_code == 400:
        raise Exception(f'Request failed with {response.status_code}')

    if not (response.status_code >= 200 and response.status_code < 300):
        raise Exception(f'Request failed with {response.status_code} and {response.json()}')

    return response