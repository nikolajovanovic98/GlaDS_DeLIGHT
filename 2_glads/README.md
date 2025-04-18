### GlaDS by Nikola Jovanovic

# Instructions on how to successfully run GlaDS for a given glacier 

- For a successful simulation, the directory containing the GlaDS SIF file should also contain the mesh directory and Physical_Parameters.IN
	- If you want to include moulins, you also need the following: moulin_nodes.py, makemoulin.py, and nearest_coordinates.xy 
------------------------------------------------------
### STEP 1 Copy the mesh from 1_import_DEM.
------------------------------------------------------

cp -r ../1_import_DEM/ale_mesh . 
mkdir ale_mesh/results

- ale_mesh needs to contain the .result file from the previous step as well
- create a "results" directory in ale_mesh to store channel results

------------------------------------------------------
### STEP 2 Include moulins (optional).            
------------------------------------------------------

- moulin_nodes.py script generates random nodes for moulins and interpolates them to the nearest nodes on the mesh.
- In moulin_nodes.py, for random moulin generation: 
	1. Specify the location of the mesh. 
	2. Specify the power for probability distribution (I set it to 4.), for random generation.
	3. Specify the number of moulins. 
	4. Specify the name of the output file and convert it from .txt to .xy
 - For moulin generation based on basins:
	1. Specify the location of the mesh.
	2. Compute basins, moulins at lowest elevations in each basin.
	3. Interpolate to mesh nodes. 
	4. Specify the name of the output file and convert it from .txt to .xy
- In makemoulin.py:
	- In the command line, simply: makemoulin.py --meshdir ale_mesh --moulin moulin_file --partition number_of_partition
	- This should add moulins as BCs (The output should be something like "Found 3 moulins on partition 1"
		- NOTE: If it doesn't find 3 moulin nodes, maybe re-run the moulin_nodes.py script again 

------------------------------------------------------
### STEP 3 Run GlaDS.            
------------------------------------------------------
- For BCs (without moulins):
	- BC1 corresponds to the glacier outline
	- BC2 is the bed
	- BC3 is the surface
- With moulins (the numbering continues with the extruded boundaries of the first two!):
	- BC1 corresponds to the glacier outline
	- BC2 is the boundary condition for moulins (if there are 3 moulins then, Target Boundaries(3) =  2 3 4)
	- BC5 is the bed BC
	- BC6 is the surface BC. 

- If not running a parallel simulation, first:
	- Linear System Direct Method = UMFPACK
	- and simply (but don't forget to specify the mesh dir in the SIF file!):

ElmerSolver glads_niki.sif

- If running parallel:
	- Linear System Direct Method = MUMPS

mpirun -np number_of_partitions ElmerSolver glads_niki.sif
