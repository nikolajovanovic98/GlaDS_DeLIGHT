# Instructions on how to read DEM into the mesh 

**THE FOLLOWING SHOULD BE EXECUTED IN A DIR CONTAINING ALL GLACIER MESH DIRs**
- e.g., /this_dir/GLACIER_MESH_DIR (here, ale_mesh for Aletsch) --> all scripts contained in $this_dir (you need Physical_Parameters.IN, glacier dirs, and the scripts detailed below) 

------------------------------------------------------
### STEP 1 Calculate bed elevation.
------------------------------------------------------

python calc_bed.py --dir GLACIER_MESH_DIR

- Before doing anything, make sure to copy the output directory from ElmerGrid from Step 1 (the directory containing mesh.boundary, mesh.nodes ...) 
- To this output directory, further copy the input_saved.nc file necessary to read surface and bed topography from. 
- To run the Python script, provide the name of the mesh directory containing the input_saved.nc. 

------------------------------------------------------
### STEP 2 Initialise DEM.            
------------------------------------------------------

- If not running a parallel simulation, simply (but don't forget to specify the mesh dir in the SIF file!): 

ElmerSolver initialise_DEM.sif 

- If running parallel:

mpirun -np number_of_partitions ElmerSolver initialise_DEM.sif

- Extrusion.sif and Physical_Parameters.IN are read in initialise_DEM.sif.
