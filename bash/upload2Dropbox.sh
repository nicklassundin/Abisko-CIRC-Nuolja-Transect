# !/bin/bash

# Dropbox remote name in rclone
REMOTE="dropbox"
DROPBOX_FOLDER="Nuolja"  # Change to your Dropbox folder

# create temporary directory
# generate a zip file name based on git commit hash
# like 3.1.2-3-gb2f4c5d-[date]
FILENAME=$(git rev-parse --short HEAD)
DATE=$(date +%Y-%m-%d)
FILENAME="${DATE}-${FILENAME}"
END=".tar.gz"
git archive --format=tar.gz -o "$FILENAME$END" HEAD

# compress the temporary directory into a zip file
zip -r "$FILENAME$END" "$FILENAME/"
# Loop through each file and upload it
if [ -e "$FILENAME$END"  ]; then
	echo "Uploading: $FILENAME..."
	# Print directoris at DROPBOX_FOLDER
	# echo "Directories in $DROPBOX_FOLDER:"
	rclone copy "$FILENAME$END" "$REMOTE:$DROPBOX_FOLDER" --progress
else
	echo "Error: File '$FILENAME$END' not found!"
fi

rm -f "$FILENAME$END"
