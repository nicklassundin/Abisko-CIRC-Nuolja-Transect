# !/bin/bash

# Dropbox remote name in rclone
REMOTE="dropbox"
DROPBOX_FOLDER="Nuolja"  # Change to your Dropbox folder

DIRECTORIES="$@"

# Check if at least one file is provided
if [ -z "$DIRECTORIES"  ]; then
	echo "Usage: $0 <obj1> <obj2> ..."
	bash upload2Dropbox.sh R/ script.R descriptions/ README.md
	exit 1
fi
# create temporary directory
# generate a zip file name based on git commit hash
# like 3.1.2-3-gb2f4c5d-[date]
FILENAME=$(git rev-parse --short HEAD)
DATE=$(date +%Y-%m-%d)
FILENAME="${DATE}-${FILENAME}"
mkdir -p "$FILENAME"

for file in "$@"; do
		# copy files and directories to temporary directory
		cp -r "$file" "$FILENAME/$file"
done

# compress the temporary directory into a zip file
zip -r "$FILENAME.zip" "$FILENAME/"
# Loop through each file and upload it
if [ -e "$FILENAME.zip"  ]; then
	echo "Uploading: $FILENAME..."
	# Print directoris at DROPBOX_FOLDER
	# echo "Directories in $DROPBOX_FOLDER:"
	rclone copy "$FILENAME.zip" "$REMOTE:$DROPBOX_FOLDER" --progress
else
	echo "Error: File '$FILENAME' not found!"
fi

# Clean up temporary directory
rm -rf "$FILENAME"
rm -f "$FILENAME.zip"
