#!/bin/bash
generate_tree() {
	local dir="$1"
	local prefix="$2"

	shopt -s nullglob dotglob
	local items=("$dir"/*)
	for i in "${!items[@]}"; do
		local item="${items[i]}"
		local name=$(basename "$item")
		# Skip hidden directories and files
		if [[ -d "$item" && "$name" == .*  ]]; then
			continue
		fi
		# Skipe tests directory
		if [[ -d "$item" && "$name" == tests  ]]; then
			continue
		fi
		# Skip directory_tree.md
		if [[ -f "$item" && "$name" == directory_tree.md  ]]; then
			continue
		fi
		# Skip files/directories that are in .gitignore
		if  git check-ignore -q "$item"; then
			echo "Skipping $item"
			continue
		fi


		if [[ -d "$item" || "$name" == *.R || "$name" == *.md  ]]; then
			local connector="├──"
			[[ $i -eq $((${#items[@]} - 1))  ]] && connector="└──"

			# echo "$prefix$connector $name" >> "directory_tree.md"
			# Add explicit newline
			echo -e "$prefix$connector $name" >> "directory_tree.md"

			[[ -d "$item"  ]] && generate_tree "$item" "$prefix│   "
		fi
	done
}

Ensure the file is created before writing
: > "directory_tree.md"

echo "#### Directory Tree" >> "directory_tree.md"
echo '```tree' >> "directory_tree.md"
echo "project_directory/" >> "directory_tree.md"

generate_tree "$PWD" ""
echo '```' >> "directory_tree.md"

README_FILE="README.md"
OUTPUT_FILE="README_OUT.md"

if ! grep -q '<!-- TREE START -->' "$README_FILE"; then
	cp "$README_FILE" "$OUTPUT_FILE"
	echo -e "\n<!-- TREE START -->### Directory Tree \n 
	$(cat directory_tree.md)\n\n
	<!-- TREE END -->" >> "$OUTPUT_FILE"
else
	# Replace content between TREE markers with the new tree and save to README_OUT.md
	awk -v tree="$(cat directory_tree.md)" '
	/<!-- TREE START -->/ {print; print tree; found=1; next}
	/<!-- TREE END -->/ {found=0}
	!found' "$README_FILE" > "$OUTPUT_FILE"
fi

cp README.md README.md.bak
mv README_OUT.md README.md

git add README.md
rm directory_tree.md
