from pathlib import Path
import getpass

USER = getpass.getuser()

if USER == "MY_USERNAME":  # Replace with your actual username
    PATH_KIOSK_USER_PATTERNS_FOLDER = Path("")  # Set your path here
    PATH_KIOSK_USER_PATTERNS_REPO = Path("")    # Set your path here

if USER == "JVARGH7":
    PATH_KIOSK_USER_PATTERNS_FOLDER = Path("C:/Cloud/OneDrive - Emory University/Papers/Pursuant User Profiles")
    PATH_KIOSK_USER_PATTERNS_REPO = Path("C:/code/external/kiosk_user_patterns")
    # test
if USER == "carolinechizak":  # Replace with your actual username
    PATH_KIOSK_USER_PATTERNS_FOLDER = Path("/Users/carolinechizak/Library/CloudStorage/OneDrive-SharedLibraries-Emory/Varghese, Jithin Sam - Pursuant User Profiles")  # Set your path here
    PATH_KIOSK_USER_PATTERNS_REPO = Path("/Users/carolinechizak/kiosk_user_patterns")    # Set your path here
