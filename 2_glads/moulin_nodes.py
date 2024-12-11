# Description: This script is used to generate the nodes of the moulins in the glacier.

# ------------------ Libraries ------------------
import numpy as np 
import matplotlib.pyplot as plt 
from netCDF4 import Dataset
import xarray as xr
from scipy.interpolate import RegularGridInterpolator
from scipy.spatial import cKDTree

# ------------------ Functions ------------------

class MoulinNodesGenerator:
    """
    This class is used to generate the nodes of the moulins in the glacier.
    """
    def __init__(self, nc_path, mesh_path):
        self.nc_path = nc_path
        self.mesh_path = mesh_path
        self.usurf, self.x, self.y, self.mask = self.read_nc_files()
        self.xmesh, self.ymesh = self.read_mesh()


    def read_nc_files(self):
        """
        Read the netCDF files and return the data
        """
        # Load the netCDF file
        ds = xr.open_dataset(self.nc_path)
        usurf = ds['usurf']
        x = ds['x']
        y = ds['y']
        mask = ds['icemask']

        return usurf, x, y, mask
    
    def plot_usurf(self):
        """
        Plot the surface of the glacier
        """
        # Plot the surface of the glacier
        fig, ax = plt.subplots(figsize=(10, 10))

        # Plot the surface
        c = ax.pcolormesh(self.x, self.y, self.usurf*self.mask, cmap='viridis')
        ax.set_title('Surface of the glacier')
        ax.set_xlabel('x')
        ax.set_ylabel('y')
        plt.colorbar(c, ax=ax, label='Elevation (m)')
        plt.show()

    def generate_moulins(self, number_of_moulins = 30, power = 4, elevation_treshold = 3000):
        """
        Generate the nodes of the moulins
        """
        x, y = np.meshgrid(self.x, self.y)
        x = x.flatten()
        y = y.flatten()
        usurf = self.usurf.values.flatten()
        mask = self.mask.values.flatten()

        valid_indices = np.where(usurf*mask != 0)

        x = x[valid_indices]
        y = y[valid_indices]
        usurf = usurf[valid_indices]

        if elevation_treshold is not False:
            valid_indices = np.where(usurf <= elevation_treshold)
            x = x[valid_indices]
            y = y[valid_indices]
            usurf = usurf[valid_indices]

        inverted_surf = np.max(usurf) - usurf
        inverted_surf = inverted_surf**power
        probabilities = inverted_surf / np.sum(inverted_surf)

        selected_indices = np.random.choice(len(usurf), size=number_of_moulins, p=probabilities)
        selected_x = x[selected_indices]
        selected_y = y[selected_indices]

        return selected_x, selected_y

    def read_mesh(self):
        """
        Read the mesh text file
        """
        # Open the file
        xmesh = np.loadtxt(self.mesh_path, usecols=(2))
        ymesh = np.loadtxt(self.mesh_path, usecols=(3))
        
        return xmesh, ymesh
    
    def interp_to_mesh_coords(self, selected_x, selected_y):
        """
        Interpolate the moulins to the mesh coordinates
        """

        coordinates = np.column_stack((selected_x, selected_y))
        mesh_coordinates = np.column_stack((self.xmesh, self.ymesh))

        # Create a KDTree
        tree = cKDTree(mesh_coordinates)
        distances, indices = tree.query(coordinates)
        nearest_nodes = np.column_stack((self.xmesh[indices], self.ymesh[indices]))  

        return nearest_nodes
    
    def plot_moulins(self, selected_x, selected_y, nearest_nodes):
        """
        Plot the moulins
        """
        fig, ax = plt.subplots(figsize=(10, 10))

        # Plot the surface of the glacier
        c = ax.pcolormesh(self.x, self.y, self.usurf*self.mask, cmap='viridis')
        ax.set_title('Surface of the glacier')
        ax.set_xlabel('x')
        ax.set_ylabel('y')
        plt.colorbar(c, ax=ax, label='Elevation (m)')

        # Plot the moulins
        ax.scatter(selected_x, selected_y, color='red', label='Moulins')
        ax.scatter(nearest_nodes[:, 0], nearest_nodes[:, 1], color='blue', label='Nearest nodes')
        ax.legend()
        plt.show()

    def save_to_file(self, nearest_nodes, output_path):
        """
        Save the nearest nodes to a file
        """
        np.savetxt(output_path, nearest_nodes, fmt='%f', delimiter=' ')

# ------------------ Main ------------------

# Give the name of the glacier directory to access the netCDF file
#glacier = input("Enter the glacier directory name: ")
glacier = 'aletsch'

# Define the path to the netCDF file
nc_path = f'/opt/work/elmer_stuff/0_mesh_time/{glacier}/input_saved.nc'
mesh_path = 'mainmesh/mesh.nodes'
output_path = 'moulin_coords.txt'

# Create an instance of the class
moulin_generator = MoulinNodesGenerator(nc_path, mesh_path)

# Plot the surface of the glacier
moulin_generator.plot_usurf()

# Generate the moulins
selected_x, selected_y = moulin_generator.generate_moulins(number_of_moulins=30, 
                                                           power=4, 
                                                           elevation_treshold=2500)

# Interpolate the moulins to the mesh coordinates
nearest_nodes = moulin_generator.interp_to_mesh_coords(selected_x, selected_y)

# Plot the moulins
moulin_generator.plot_moulins(selected_x, selected_y, nearest_nodes)

# Save the nearest nodes to a file
moulin_generator.save_to_file(nearest_nodes, output_path)
