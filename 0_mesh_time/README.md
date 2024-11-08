# Instructions on how to obtain coordinates of the desired glacier flowline and generate a triangular mesh 

**THE FOLLOWING SHOULD BE EXECUTED IN A DIR CONTAINING ALL GLACIER DIRs**
- e.g., /this_dir/aletsch --> all scripts contained in $this_dir (you need .json file for oggm, Contour2geo.py, glacier dirs, and the scripts detailed below) 

------------------------------------------------------
### STEP 1 Extract the netCDF file using IGM and OGGM 
------------------------------------------------------

source get_outline.sh to run it 

- you will be prompted to give the RGI and the glacier name, both of which you can copy from GLIMS 
- the script creates a directory with the glacier name and moves the nc file there
	- e.g., this_dir/file.nc --> this_dir/glacier_name/file.nc 

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

- this converts the csv file to a readable .txt file 
- the user is prompted to give the name of the xy coords csv file (located in glacier dir; give full name!!! e.g., coords.csv)
- the user is prompted to give the name of the glacier (to navigate to the dir created earlier),
so it has to be spelled the same way as the dir created 
- the rest of the script generates the mesh and creates a vtu file hooray  
