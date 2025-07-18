def compare_file_sizes(dest_dir, files_csv):
    err_list = pd.DataFrame(columns=['file_name', 'file_path', 'expected_size', 'actual_size'])
    for fname in os.listdir(dest_dir):
        fpath = os.path.join(dest_dir, fname)
        if os.path.isfile(fpath):
            file_info = files_csv[files_csv['file_name'] == fname]
            if not file_info.empty:
                expected_size = file_info.iloc[0]['file_size_bytes']
                actual_size = os.path.getsize(fpath)
                if expected_size - actual_size >= 10_000_000:
                    df = pd.DataFrame([{
                        'file_name': fname,
                        'file_path': fpath,
                        'expected_size': expected_size,
                        'actual_size': actual_size
                    }])

                    print(f"File size mismatch for {fname}: expected {expected_size}, got {actual_size}")
                    err_list = pd.concat([err_list, df], ignore_index=True)
    return err_list