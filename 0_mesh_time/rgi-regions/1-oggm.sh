# !/bin/bash

# activate conda in case
conda activate igm

# give the folder name
read -p "Enter region (dir): " rgi
# read the array 
read -p "Enter region (text file): " textfile
readarray glacier_ids < $rgi/$textfile 
echo ${glacier_ids[@]}

for str in ${glacier_ids[@]}; do 
	echo "Now working on glacier $str." 

	# update json file
	json_file="params.json"
	jq --arg oggm_RGI_ID "$str" '.oggm_RGI_ID = $oggm_RGI_ID' "$json_file" > tmp.json && mv tmp.json "$json_file" 
	echo "JSON file updated successfully!" 

	# run igm 
	echo "Running IGM" 
	igm_run 

	# check if dir already exists
	if [[ -d "$rgi/$str" ]]; then 
		echo "Directory $str already exists." 
	else
		mkdir "$rgi/$str" 
		echo "Directory $str created."
	fi

	mv input_saved.nc $rgi/$str/ 

	#. clean.sh
done
