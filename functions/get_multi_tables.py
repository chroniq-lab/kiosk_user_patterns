# https://docs.deweydata.io/docs/api-multi-table-datasets
# Date: 1 July 2025

import random
import requests
import time
import os
import pandas as pd

class Multitable:
    """
    A helper class to interact with multi-table products through APIs.
    """
    def __init__(self, apikey: str, pp_: str):
        """
        Initialize the Multitable class with an API key and the base URL for the multi-table product.

        :param apikey: Your API key created on Dewey
        :param pp_: The base URL for the multi-table product
        """
        self.apikey = apikey
        self.pp_ = pp_
    
    def make_get_request(self, url: str, params: dict = None, attempt_count: int = 1, max_attempts: int = 5):
        """
        Call API using GET request, including retry logic for rate limiting.

        :param url: API endpoint.
        :param params: Parameters for the API call.
        :param attempt_count: Current retry attempt.
        :return: Response object from the GET request.
        """

        headers = {'X-API-KEY': self.apikey,
                   'Content-Type': 'application/json',
                   'accept': 'application/json'}

        response = requests.get(url, params=params, headers=headers)

        if response.status_code == 429 and attempt_count <= max_attempts:
            sleep_time = (2 ** attempt_count) + random.uniform(0, 2)
            print(f'Ratelimit reached, retrying after {sleep_time:.1f} seconds')
            time.sleep(sleep_time)
            return self.make_get_request(url, params=params, attempt_count=attempt_count+1)

        if response.status_code == 400:
            raise Exception(f'Request failed with {response.status_code}')

        if not (response.status_code >= 200 and response.status_code < 300):
            raise Exception(f'Request failed with {response.status_code} and {response.json()}')

        return response
    
    
    def get_table_list(self):
        """
        Get a list of tables available for download from a multi-table product.
        """
        results = self.make_get_request(url=self.pp_+'/multi-table-product-metadata')
        table_options = [table_dict['table_name'] for table_dict in results.json()['items']]
        return table_options
    
    
    def get_mt_meta(self, tables: list = 'all', df: bool = False):
        """
        Get metadata for a multi-tables.

        :param tables: List of tables to get metadata for. Call get_table_list() to get a list of available tables.
        :param df: Return metadata as a pandas DataFrame.
        """
        if tables == 'all':
            tables = self.get_table_list()

        metadata_list = []

        for table in tables:
            results = self.make_get_request(url=f'{self.pp_}/metadata?table_name={table}')
            meta_data = results.json()

            partition = meta_data.get('partition_column')
            min_partition_key = meta_data.get('min_partition_key')
            max_partition_key = meta_data.get('max_partition_key')
            total_files = meta_data.get('total_files')
            total_size = meta_data.get('total_size')

            metadata_list.append({
                "table_name": table,
                "partition": partition,
                "min_partition_key": min_partition_key,
                "max_partition_key": max_partition_key,
                "total_files": total_files,
                "total_size": total_size

            })
        
        if df:
            return pd.DataFrame(metadata_list)
        else:
            return metadata_list

    
    
    def get_multi_tables(self, table_list: list, directory: str, skip_exists: bool = True, date_start: str = None, date_end: str = None):
        """
        Download all files from a list of tables from a multi-table product.
        
        :param table_list: List of tables to download. Call get_table_list() to get a list of available tables.
        :param directory: Directory to save downloaded files
        :param date_start: Start date for partitioned tables if applicable
        :param date_end: End date for partitioned tables if applicable
        """
        def determine_partition_params(metadata, date_start, date_end):
            """
            Determine partition parameters based on metadata and provided dates.
            """
            partition = metadata.get('partition')
            if partition:
                beg_date = date_start or metadata.get('min_partition_key')
                end_date = date_end or metadata.get('max_partition_key')
                return {'partition_key_before': end_date, 'partition_key_after': beg_date}
            return {}


        for table_name in table_list:
            table_dir = os.path.join(directory, table_name)
            os.makedirs(table_dir, exist_ok=True)
        
        # loop through all API result pages, keeping track of number of downloaded files
        for table_name in table_list:
            page = 1
            download_count = 0

            metadata = self.get_mt_meta(tables=[table_name])[0]
            partition_params = determine_partition_params(metadata, date_start, date_end)


            while True:
                params = {'page': page, 'table_name': table_name, **partition_params}

                # get results from API endpoint, using API key for authentication, for a specific page
                results = self.make_get_request(
                    url=self.pp_,
                    params=params
                )
                response_json = results.json()

                # for each result page, loop through download links and save to your computer
                for link_data in response_json['download_links']:
                        original_file_name = link_data['file_name']
                        file_name = f"{download_count + 1:03d}_{original_file_name}"
                        
                        file_path = os.path.join(directory, table_name, file_name)

                        if skip_exists and os.path.exists(file_path):
                            print(f"File {file_name} already exists. Skipping download.")
                            continue
                        
                        else:
                            print(f"Downloading file {file_name} to folder {table_name}...")
                            data = self.make_get_request(link_data['link'])

                            with open(file_path, 'wb') as file:
                                file.write(data.content)
                        
                        download_count += 1
                
                # only continue if there are more result pages to process
                total_pages = response_json['total_pages']
                if page >= total_pages:
                    break
                page += 1
            
            print(f"Successfully downloaded {download_count} files to folder {table_name}.")