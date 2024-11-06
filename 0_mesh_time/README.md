# Instructions on how to obtain coordinates of the desired glacier flowline and generate a triangular mesh 

**THE FOLLOWING SHOULD BE EXECUTED IN A DIR CONTAINING GLACIER DIRs**
- e.g., /this_dir/aletsch --> all scripts contained in $this_dir (you need .json file for oggm, Contour2geo.py, glacier dirs, and the scripts detailed below) 

------------------------------------------------------
### STEP 1 Extract the netCDF file using IGM and OGGM #
------------------------------------------------------

source get_outline.sh to run it 

- you will be prompted to give the RGI and the glacier name, both of which you can copy from GLIMS 
- the script creates a directory with the glacier name and moves the nc file there
	- e.g., this_dir/file.nc --> this_dir/glacier_name/file.nc 

------------------------------------------------------
### STEP 2 Open QGIS and run 2-polygonise.py           #
------------------------------------------------------

- open the python pluggin, then open 2-polygonise.py
- after the prompt, navigate to your glacier directory to find the input_saved.nc file 
- select the variable (or rather type) "icemask" (without quotation marks)  
- this should generate vertices along the glacier outline (and exlude the four outliers) 
- after this, you have to run the command MMQGIS --> Modify --> Delete Duplicate Geometries
- you'll have to save the new file and load it again with script 3 (this should ideally be fixed and included in one giant script)

------------------------------------------------------
### STEP 3 Run 3-save_xy_coords.py                     #
------------------------------------------------------

- this script populates the layer with x and y coordinates (in whatever CRS they are) 
- saving doesn't work, save it manually to your glacier dir (NEEDS FIXING because it's annoying!!!) 

------------------------------------------------------
### STEP 4 Run 4-get_mesh.sh                           #
------------------------------------------------------

- this converts the csv file to a readable .txt file 
- the user is prompted to give the name of the xy coords file 
- the user is prompted to give the name of the glacier (to navigate to the dir created earlier),
so it has to be spelled the same way as the dir created 
- the rest of the script generates the mesh and creates a vtu file hooray 
IMPORTANT: fix this because it does not create the elmer dir 
