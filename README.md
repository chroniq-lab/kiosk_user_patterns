# Kiosk User Patterns, 2024-2025


## Getting started with this repository

### Software
0. Download and install [VSCode](https://code.visualstudio.com/) and [RStudio](https://posit.co/download/rstudio-desktop/)
1. Install the Python, Pylance, and Python Debugger extensions. Also install the R and R Syntax extensions.This can be done using the 'Extensions' button on the navigation bar on the left.   
2. Install GitHub copilot and GitHub copilot Chat extensions.     
3. Clone the repository using instructions on Slack. Go to **#team** >> **Git and GitHub Setup** page. Please follow the instructions carefully. Do remember to clone the repository on a local folder.     
4. Go to 'File' and open the folder **kiosk_user_patterns** from where you have cloned it. The folder should have some sub-folders and files. 


### Initial Check: getting_started.ipynb
1. Open getting_started.ipynb
2. Execute the different cells 
3. The first cell will install packages on your system, while the second cell will print your username. The username is the same as your account on your OS (e.g., mine is JVARGH7).

### Updating config.py
1. Copy the block of code from Line 6 onwards and update with the path to your folders. If you are using MacOS, you can find the folder path in one of several ways. See this [discussion](https://apple.stackexchange.com/questions/317992/is-there-any-way-to-get-the-path-of-a-folder-in-macos) here. If you are using Windows, see this [post](https://www.wikihow.com/Find-a-File%27s-Path-on-Windows) or click on the Address bar, and copy the path. See my example.    


### Testing the paths
1. Go to data/kupdat01_exploring datasets.ipynb. This is a Jupyter notebook (similar to getting_started.ipynb) and is one of the most intuitive ways of running code. Setting it up may be a little intimidating at first, but it gets easier over time.    
2. What it does is to display the first 10 rows of the datasets stored as .parquet files. Parquet files are data structures that allow for efficient querying of large datasets. You can read more about them here:
    - https://www.youtube.com/watch?v=Yxeic7WXzFw --> Watch before starting
    - https://www.youtube.com/watch?v=O42LUmJZPx0 ---> Watch before starting
    - https://hbs-rcs.github.io/large_data_in_R/ --> Difference between traditional R data storage and processing, and large data storage and processing
    - https://duckdb.org/2024/04/02/duckplyr.html --> Read if you need to go beyond parquet