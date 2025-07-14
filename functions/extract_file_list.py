# :param apikey: API Key.
# :param dataset_path: API endpoint or Dataset (or Folder) ID.
# :param start_page: Start page of file list. Default is 1.
# :param end_page: End page of file list. Default is Inf.
# :param start_date: Data start date character for files in the form of '2021-07-01'. Default is None ("1000-01-01"), which indicates no limit.
# :param end_date: Data end date character for files in the form of '2023-08-21'. Default is None ('9999-12-31'), which indicates no limit.
# :param print_info: Print file list information. Default is True.
# :return: A DataFrame object contains files information.

def extract_file_list(api_key, pp_,sd=None, ed=None):
    # https://docs.deweydata.io/docs/api-access


    meta_data = ddp.get_meta(apikey = api_key, dataset_path = pp_) # returns a dataframe with meta information
    file_list = ddp.get_file_list(apikey = api_key, dataset_path = pp_,start_date=sd, end_date=ed)

    return file_list, meta_data


