#!/bin/bash

conda activate cdo

read -p "Enter region (dir): " rgi
read -p "Enter region (text file): " textfile
readarray glacier_ids < $rgi/$textfile

for str in ${glacier_ids[@]}; do
	echo "Now working on glacier $str."

	cdo selvar,usurf "$rgi/$str/input_saved.nc" "$rgi/$str/${str}_usurf.nc"
done
