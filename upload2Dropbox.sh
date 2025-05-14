# !/bin/bash

# Dropbox remote name in rclone
REMOTE="dropbox"
DROPBOX_FOLDER="Nuolja"  # Change to your Dropbox folder

DIRECTORIES="$1"

# Check if at least one file is provided
if [ -z "$DIRECTORIES"  ]; then
	echo "Usage: $0 <obj1> <obj2> ..."
	exit 1
fi
# create temporary directory
mkdir -p "temporary"
# copy files and directories to temporary directory
cp -r "$DIRECTORIES" "temporary/"
# compress the temporary directory into a zip file
zip -r "temporary.zip" "temporary/"
FILE="temporary.zip"
# Loop through each file and upload it
if [ -e "$FILE"  ]; then
	echo "Uploading: $FILE..."
	# Print directoris at DROPBOX_FOLDER
	# echo "Directories in $DROPBOX_FOLDER:"
	rclone copy "$FILE" "$REMOTE:$DROPBOX_FOLDER" --progress
else
	echo "Error: File '$FILE' not found!"
fi

# Clean up temporary directory
rm -rf "temporary"
rm -f "temporary.zip"
