#!/bin/bash
#
# read the array
read -p "Enter RGI region (text file): " textfile
readarray glacier_ids < $textfile
echo ${glacier_ids[@]}

for str in ${glacier_ids[@]}; do

	MESH=${str,,}
	echo $MESH

	if [ ! -d "$MESH" ]; then
		echo "Directory $MESH does not exist!"
  		continue
	fi

	python calc_bed.py --dir $MESH/

	export MESH_DIR="${MESH}"
	export NCFILE="${MESH}/input_saved_cook.nc"
	export OUTFILE="${MESH}.result"
	export POSTFILE="${MESH}.vtu"

	envsubst < template.sif > initialise_DEM.sif

	mpirun -np 4 ElmerSolver initialise_DEM.sif
done
