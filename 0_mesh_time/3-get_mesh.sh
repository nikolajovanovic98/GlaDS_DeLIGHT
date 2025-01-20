#!/bin/bash 

# Glacier name to access directory
read -p "What glacier are we concerned with again? (spell the dir name) " glacier_name

# CSV file name containing the coordinates
read -p "Bitte CSV file name: " csv_file

# Name of the files _MESH
read -p "Name of the file (e.g. HIN_mesh): " MESH

csv_file=$glacier_name/$csv_file

# Check if the CSV file exists
#if [[ ! -f "$csv_file" ]]; then
#    echo "Error: CSV file '$csv_file' does not exist."
#fi

# Txt file name to conver to
txt_file="${MESH}.txt"

# Remove the first row and commas
sed '1d; s/,/ /g' "$csv_file" > "$txt_file"

# Ask for resolution 
read -p "Resolution: " res

# Mesh commands: 

python Contour2geo.py -r "$res" -i "$txt_file" -o "${MESH}.geo"

gmsh -1 -2 "${MESH}.geo" -o "${MESH}.msh"

ElmerGrid 14 2 "${MESH}.msh" -autoclean  #-metis 4 0 
ElmerGrid 14 5 "${MESH}.msh" -autoclean  #-metis 4 0

# Move all files to the directory you created in step 1 
mv "${MESH}"* $glacier_name/ 
