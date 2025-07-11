#!/bin/bash

read -p "Enter region (text file): " textfile
readarray glacier_ids < $textfile
echo ${glacier_ids[@]}

for str in ${glacier_ids[@]}; do 

    newdir=${str,,}
    cp $str/*_optimised.nc $str/$newdir/input_saved_cook.nc
    cp $str/input_saved.nc $str/$newdir/
done
