#!/bin/bash 

# activate conda env in case
conda activate igm 

# read the array 
read -p "Enter RGI region (text file): " textfile
read -p "Enter RGI region (dir): " rgi
readarray glacier_ids < $textfile 
echo ${glacier_ids[@]}

for str in ${glacier_ids[@]}; do
	echo "Now working on glacier $str." 
        
	#convert to lowercase for elmer
	MESH=${str,,}
	echo $MESH

	# read csv
	csv_file=$rgi/$str/coords.csv 
	if [[ ! -f "$csv_file" ]]; then 
		echo "Error: CSV file $csv_file does not exist." 
	fi

	# txt file to convert CSV to
	txt_file="${MESH}.txt"
	#remove the fisrt row and commas
	sed '1d; s/,/ /g' "$csv_file" > "$txt_file"
	
	# what resoluton do i set? should it be the same for all? \
	res=50.

	# create the mesh
	python Contour2geo.py -r "$res" -i "$txt_file" -o "${MESH}.geo"
	gmsh -1 -2 "${MESH}.geo" -o "${MESH}.msh"

	# generate serial mesh and a VTU file 
	# Create the Elmer serial mesh directory
	ElmerGrid 14 2 "${MESH}.msh" -autoclean

	# Generate VTU file (but ElmerGrid will name it poorly)
	ElmerGrid 14 5 "${MESH}.msh" -autoclean

	# Fix VTU file name
	mv rgi2000-v7*.vtu "${MESH}.vtu"

	# Generate partitioned mesh with correct output directory
	ElmerGrid 2 2 "${MESH}" -metis 4 0 -out "${MESH}"

	# Fix the partitioning directory name if still wrong
	if [ ! -d "${MESH}/partitioning.4" ]; then
		# Find the wrongly named directory and move its contents
		wrong_part_dir=$(find . -maxdepth 1 -type d -name "rgi2000-v7*" -not -name "${MESH}")
		if [ -n "$wrong_part_dir" ] && [ -d "$wrong_part_dir/partitioning.4" ]; then
			mv "$wrong_part_dir/partitioning.4" "${MESH}/"
			rm -r "$wrong_part_dir"
		fi
	fi

	mv ${MESH}.* $MESH/ 
	mv $MESH $rgi/$str
done
