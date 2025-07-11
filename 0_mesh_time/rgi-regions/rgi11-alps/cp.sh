#!/bin/bash

read -p "Enter region (text file): " textfile
readarray glacier_ids < $textfile
echo ${glacier_ids[@]}

for str in ${glacier_ids[@]}; do 

    newdir=${str,,}
    cp -r $str/$newdir rgi11-alps/
done
