# Python script to copy modfiles from the latest result of fpm build
# Go through all folders in the build folder and find the most recent one containing ".mod" files
# Copy all the .mod files to the mods folder for Intellisense

import os
import shutil
from datetime import datetime

# The modfiles will be in a directory something like build/nvfortran_834DFB6002712825
def find_latest_mod_folder(build_folder):
    latest_time = datetime.min
    latest_folder = None

    # Go through all folders in the build folder
    for folder_name in os.listdir(build_folder):
        folder_path = os.path.join(build_folder, folder_name)
        
        if os.path.isdir(folder_path):
            # Check if this folder contains .mod files
            has_mod_files = any(file.endswith('.mod') for file in os.listdir(folder_path))
            
            if has_mod_files:
                folder_time = datetime.fromtimestamp(os.path.getmtime(folder_path))
                if folder_time > latest_time:
                    latest_time = folder_time
                    latest_folder = folder_path

    return latest_folder

def copy_mod_files(latest_folder, mods_folder):
    if latest_folder:
        for file in os.listdir(latest_folder):
            if file.endswith('.mod'):
                shutil.copy(os.path.join(latest_folder, file), mods_folder)
    else:
        print("No .mod files found in the build folder.")

latest_folder = find_latest_mod_folder('../build')
mods_folder = '.'
os.system(f"rm -f {mods_folder}/*.mod *.o")  # Clear existing mod files
copy_mod_files(latest_folder, mods_folder)