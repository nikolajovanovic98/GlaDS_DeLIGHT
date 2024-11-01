#!/bin/bash 

# Glacier name to access directory
read -p "What glacier are we concerned with again? (spell the dir name) " glacier_name

# CSV file name containing the coordinates
read -p "Bitte CSV file name: " csv_file

csv_file=$glacier_name/$csv_file

# Check if the CSV file exists
if [[ ! -f "$csv_file" ]]; then
    echo "Error: CSV file '$csv_file' does not exist."
    exit 1
fi

# Txt file name to conver to
txt_file="${glacier_name}.txt"

# Remove the first row and commas
sed '1d; s/,/ /g' "$csv_file" > "$txt_file"

# Ask for resolution 
read -p "Resolution: " res

# Mesh commands: 

python Contour2geo.py -r "$res" -i "$txt_file" -o "${glacier_name}.geo"

gmsh -1 -2 "${glacier_name}.geo" -o "${glacier_name}.msh"

ElmerGrid 14 2 "${glacier_name}.msh"-autoclean -metis 4 0 
ElmerGrid 14 5 "${glacier_name}.msh" -autoclean -metis 4 0

# Move all files to the directory you created in step 1 
mv "${glacier_name}."* $glacier_name/
