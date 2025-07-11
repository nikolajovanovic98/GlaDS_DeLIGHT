# Automated workflow for glacier mesh generation (multiple glaciers)

**THE FOLLOWING MUST BE EXECUTED IN THE "rgi-regions" DIR**

------------------------------------------------------
### STEP 1 Get glacier outline via OGGM and IGM 
------------------------------------------------------

*source 1-oggm.sh*

- you will be prompted to give a selection of glacier RGI IDs from a given region (this shoud be a text file, see example) as well as the region name
- the script runs IGM to download the NetCDF file, creates a directory labeled with the glacier RGI ID and moves the NetCDF file there
	- e.g., this_dir/file.nc --> this_dir/glacier_RGI_ID/file.nc 

------------------------------------------------------
### STEP 2 Open QGIS and run 2-get_coords.py           
------------------------------------------------------

- open the python pluggin, then open 2-get_coords.py
- after the prompt, navigate to your glacier directory to find the input_saved.nc file 
- select the variable (or rather type) "icemask" (without quotation marks)  
- this generates the second longest line (hopefully) corresponding to the glacier outline
- then, vertices are generated, populated with x and y coords
- then, the user is prompted to save the csv file (it should be saved in the glacier dir created in step 1)

------------------------------------------------------
### STEP 3 Run 3-get_mesh.sh                      
------------------------------------------------------

*source 3-get_mesh.sh*

- this converts the csv file to a readable .txt file 
- the user is prompted to give the name of the text file containing all glacier IDs as well as the dir of the region in question
- the rest of the script generates both serial and partitioned mesh and creates a VTU file
