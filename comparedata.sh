#!/bin/bash

# Define the directories
dir1="data_old"
dir2="data"
output_file="log/diff_data.txt"

## Clear the output file if it exists
> "$output_file"

# Find and compare .csv files
find "$dir1" -type f -name "*.csv" -exec sh -c '
 for file; do
     relative_path=${file#'"$dir1"'/}
         if [ -e "'"$dir2"'/$relative_path"  ]; then
               git diff --no-index "$file" "'"$dir2"'/$relative_path" >> "'"$output_file"'"
                   fi
                     done
                     ' sh {} +

dir1="out_old"
dir2="out"
output_file="log/diff_out.txt"

> "$output_file"

# Find and compare .csv files
find "$dir1" -type f -name "*.csv" -exec sh -c '
 for file; do
     relative_path=${file#'"$dir1"'/}
	 if [ -e "'"$dir2"'/$relative_path"  ]; then
	       git diff --no-index "$file" "'"$dir2"'/$relative_path" >> "'"$output_file"'"
		   fi
		     done
		     ' sh {} +
