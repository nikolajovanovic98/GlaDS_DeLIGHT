# Description: This script is used to generate the nodes of the moulins in the glacier.
# Set moulin kind: random generation scaled with elevation, watershed, or Voronoi 

# ------------------ Libraries ------------------
import numpy as np 
import os
import glob
import matplotlib.pyplot as plt 
from netCDF4 import Dataset
import xarray as xr
from scipy.spatial import cKDTree
from skimage.segmentation import watershed
from skimage.morphology import local_minima
from scipy.ndimage import label

# ------------------ Config ------------------

import os
print("Working directory:", os.getcwd())

CONFIG = {
    'nc_path': 'moulintest1/input_saved.nc',
    'mesh_path': 'moulintest1/mesh.nodes',
    'output_path': 'test.txt',
    'elevation_threshold': 2800,
    'number_of_moulins': 50,
    'power': 2
}

# ------------------ Functions ------------------

class MoulinNodesGenerator:
    """
    This class is used to generate the nodes of the moulins in the glacier
    based on a) the lowest elevation in a basin created by the watershed algorithm, 
    b) randomly selected points or c) lowest elevation nodes in Voronoi
    diagrams generated around randomly selected points in b)  
    """
    def __init__(self, nc_path, mesh_path):
        self.nc_path = nc_path
        self.mesh_path = mesh_path
        self.usurf, self.x, self.y, self.mask = self.read_nc_files()
        self.xmesh, self.ymesh = self.read_mesh()

    def read_nc_files(self):
        """
        Read the OGGM netCDF files and return the data
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
        Plot the surface of the glacier to check
        """
        fig, ax = plt.subplots(figsize=(10, 10))
        c = ax.pcolormesh(self.x, self.y, self.usurf*self.mask, cmap='viridis')
        ax.set_title('Surface of the glacier')
        ax.set_xlabel('x')
        ax.set_ylabel('y')
        plt.colorbar(c, ax=ax, label='Elevation (m)')
        plt.show()

    def generate_basins(self):
        """
        Generate the basins of the glacier
        using the watershed algorithm from skimage library.
        """
        usurf = np.where(self.mask==1, self.usurf, np.nan)
        markers = label(local_minima(usurf, connectivity=0.001))[0]
        segments = watershed(usurf, markers=markers, mask=~np.isnan(usurf))
        return segments
    
    def plot_basins(self, segments):
        """
        Plot the basins
        """
        fig, axs = plt.subplots(figsize=(16, 7), ncols=2)
        c = axs[0].pcolormesh(self.x, self.y, self.usurf*self.mask, cmap='viridis')
        axs[0].set_title('Surface of the glacier')
        axs[0].set_xlabel('x')
        axs[0].set_ylabel('y')
        plt.colorbar(c, ax=axs[0], label='Elevation (m)')

        b = axs[1].imshow(segments, extent=[self.x.min(), self.x.max(), self.y.min(), self.y.max()], origin='lower', alpha=0.9, cmap='tab20')
        axs[1].set_title('Basins')
        axs[1].set_xlabel('x') 
        axs[1].set_ylabel('y')
        plt.colorbar(b, ax=axs[1], label='Basin ID')
        plt.show()

    def generate_basin_moulins(self, segments, elevation_threshold=2000):
        """
        Generate the nodes of the moulins as the lowest elevation nodes in each basin.    
        """
        unique_segments = np.unique(segments[segments > 0])
        moulins = []
        usurf = np.where(self.mask==1, self.usurf, np.nan)
        for segment in unique_segments:
            y, x = np.where(segments == segment)
            elevations = usurf[y, x]
            min_elevation = np.min(elevations)
            if min_elevation < elevation_threshold:
                index = np.argmin(elevations)
                min_x = x[index]
                min_y = y[index]
                moulins.append((min_x, min_y))
        return moulins
    
    def generate_random_points(self, number_of_moulins=30, power=4, elevation_threshold=3000):
        """
        Generate the nodes of the moulins randomly
        with probability depending on the elevation.
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

        if elevation_threshold is not False:
            valid_indices = np.where(usurf <= elevation_threshold)
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
    
    def generate_voronoi(self, selected_x, selected_y):

        x = self.x 
        y = self.y 
        mask = self.mask.values
        usurf = self.usurf.values

        # get grid and ice mask
        X, Y = np.meshgrid(x,y)
        valid_mask = mask == 1 

        # get points within ice mask
        valid_X, valid_Y = X[valid_mask], Y[valid_mask]
        valid_points = np.column_stack([valid_X, valid_Y])

        # get voronoi
        tree = cKDTree(np.column_stack([selected_x, selected_y]))
        _, moulin_ids  = tree.query(valid_points)

        # assogm ids
        voronoi_map = -1 * np.ones(valid_mask.shape, dtype=int)
        voronoi_map[valid_mask] = moulin_ids

        # get the lowest node
        num_regions = np.max(voronoi_map) + 1
        lowest_nodes = []

        # loop through regions
        for id in range(num_regions):
            region_indices = moulin_ids == id

            if not np.any(region_indices):
                continue

            region_usurf = usurf[valid_mask][region_indices]
            min_idx = np.argmin(region_usurf)
            region_points = valid_points[region_indices]

            lowest_nodes.append(region_points[min_idx])

        lowest_nodes = np.array(lowest_nodes) 

        return lowest_nodes, voronoi_map, moulin_ids, valid_points


    def read_mesh(self):
        """
        Read the mesh text file
        """
        x_list = []
        y_list = []

        if os.path.isfile(self.mesh_path):
            xmesh = np.loadtxt(self.mesh_path, usecols=(2))
            ymesh = np.loadtxt(self.mesh_path, usecols=(3))
        else:
            part_files = sorted(glob.glob(os.path.join(self.mesh_path, "part.*.nodes")))
            if not part_files:
                raise FileNotFoundError("No mesh file or partitioned files found.")
            for file in part_files:
                x = np.loadtxt(file, usecols=(2))
                y = np.loadtxt(file, usecols=(3))
                x_list.append(x)
                y_list.append(y)
            xmesh = np.concatenate(x_list)
            ymesh = np.concatenate(y_list)
        
        return xmesh, ymesh
    
    def interp_basin_to_mesh_coords(self, moulins):
        """
        Interpolate the moulins to the mesh coordinates
        """
        x = [self.x[moulin[0]] for moulin in moulins]
        y = [self.y[moulin[1]] for moulin in moulins]
        coordinates = np.column_stack((x, y))
        mesh_coordinates = np.column_stack((self.xmesh, self.ymesh))

        tree = cKDTree(mesh_coordinates)
        distances, indices = tree.query(coordinates)
        nearest_nodes = np.column_stack((self.xmesh[indices], self.ymesh[indices]))  
        return nearest_nodes

    def interp_random_to_mesh_coords(self, lowest_nodes):
        """
        Interpolate the moulins to the mesh coordinates
        """
        coordinates = lowest_nodes
        mesh_coordinates = np.column_stack((self.xmesh, self.ymesh))
        tree = cKDTree(mesh_coordinates)
        distances, indices = tree.query(coordinates)
        nearest_nodes = np.column_stack((self.xmesh[indices], self.ymesh[indices]))  
        return nearest_nodes
    
    def interp_all_to_mesh_coords(self, selected_x, selected_y, moulins):
        """
        Interp both basin- and randomly generated moulins to 
        mesh coordinates
        """
        x_basin = [self.x[moulin[0]] for moulin in moulins]
        y_basin = [self.y[moulin[1]] for moulin in moulins]

        mesh_coordinates = np.column_stack((self.xmesh, self.ymesh))
        random_coordinates = np.column_stack((selected_x, selected_y))
        basin_coordinates = np.column_stack((x_basin, y_basin))
        coordinates = np.concatenate((basin_coordinates, random_coordinates), axis=0)

        tree = cKDTree(mesh_coordinates)
        distances, indices = tree.query(coordinates)
        nearest_nodes = np.column_stack((self.xmesh[indices], self.ymesh[indices]))
        
        nearest_nodes = np.unique(nearest_nodes, axis=0)
        return nearest_nodes

    def plot_basin_moulins(self, moulins, nearest_nodes, segments):
        """
        Plot the moulins
        """
        fig, ax = plt.subplots(figsize=(10, 10))
        b = ax.imshow(segments, extent=[self.x.min(), self.x.max(), self.y.min(), self.y.max()], origin='lower', alpha=0.9, cmap='tab20')

        first_moulin = moulins[0]
        ax.scatter(self.x[first_moulin[0]], self.y[first_moulin[1]], color='red', label='Moulins')

        for moulin in moulins[1:]:
            ax.scatter(self.x[moulin[0]], self.y[moulin[1]], color='red', s=100)

        ax.scatter(nearest_nodes[:, 0], nearest_nodes[:, 1], color='blue', label='Nearest nodes')
        ax.legend()
        plt.show()
    
    def plot_random_moulins(self, lowest_nodes, nearest_nodes, voronoi_map):
        """
        Plot the moulins
        """
        import matplotlib.colors as mcolors
        import numpy.ma as ma

        # mask the off-glacier Voronoi cells
        voronoi_masked = ma.masked_where(voronoi_map == -1, voronoi_map)

        num_regions = np.max(voronoi_masked) + 1  

        # use a colormap with enough discrete colors
        cmap = plt.get_cmap('rainbow', num_regions)

        # create boundaries and normalization for discrete colors
        bounds = np.arange(-0.5, num_regions + 0.5, 1)
        norm = mcolors.BoundaryNorm(bounds, cmap.N)

        # mesh grid
        X, Y = np.meshgrid(self.x, self.y)

        fig, ax = plt.subplots(figsize=(12, 10))

        # plot Voronoi map confined to glacier
        vor = ax.imshow(voronoi_masked, extent=[self.x.min(), self.x.max(), self.y.min(), self.y.max()],
                        origin='lower', cmap=cmap, norm=norm, alpha=0.8)

        # glacier outline 
        glacier_outline = ax.contour(X, Y, self.mask, levels=[0.5], colors='k', linewidths=1.5)

        # surface elevation contours
        elev_contours = ax.contour(X, Y, self.usurf, levels=10, colors='white', linewidths=0.7, alpha=0.5)

        # Plot moulin positions
        #ax.scatter(moulins[:, 0], moulins[:, 1], c='red', edgecolor='k', s=60, label='Moulins')
        ax.scatter(lowest_nodes[:,0], lowest_nodes[:, 1], c='red', edgecolor='k', s=60, label='Moulins')
        ax.scatter(nearest_nodes[:,0], nearest_nodes[:,1], c='green', edgecolors='k', label='Nearest nodes')

        ax.set_title("Moulin Voronoi Regions (Confined to Glacier)", fontsize=14)
        ax.set_xlabel("X [m]")
        ax.set_ylabel("Y [m]")
        ax.legend(loc='upper right')

        # Colorbar with correct ticks
        cbar = fig.colorbar(vor, ax=ax, label='Moulin ID', ticks=np.arange(num_regions))
        plt.tight_layout()
        plt.savefig('voronoi.png', dpi=200)
        plt.show()

    def plot_all_moulins(self, moulins, segments, selected_x, selected_y, nearest_nodes):
        """
        Plot both randomly generated and basin moulins
        and the interpolated nodes.
        """
        fig, ax = plt.subplots(figsize=(8, 8))
        b = ax.imshow(segments, extent=[self.x.min(), self.x.max(), self.y.min(), self.y.max()], origin='lower', alpha=0.9, cmap='tab20')
        plt.colorbar(b, ax=ax, label="Basin ID")

        ax.set_title('Both methods for moulin generation')
        ax.set_xlabel('x[m]')
        ax.set_ylabel('y[m]')      

        first_moulin = moulins[0]
        ax.scatter(self.x[first_moulin[0]], self.y[first_moulin[1]], color='red', label='Basin moulins')
        for moulin in moulins[1:]:
            ax.scatter(self.x[moulin[0]], self.y[moulin[1]], color='red')
        
        ax.scatter(selected_x, selected_y, color='green', label='Random')
        ax.scatter(nearest_nodes[:,0], nearest_nodes[:, 1], color='blue', label='Nearest nodes')
        ax.legend()
        plt.show()

    def save_to_file(self, lowest_nodes, output_path):
        """
        Save the nearest nodes to a file
        """
        np.savetxt(output_path, lowest_nodes, fmt='%f', delimiter=' ')
        print(f'A total of {len(lowest_nodes)} found.')

    
    def runoff_load(self):

        pass

    def runoff_calc(self, moulin_ids, voronoi_map, runoff):
        """
        Calculate runoff in each Voronoi region.
        """
        num_regions = np.max(moulin_ids) + 1
        valid_mask = self.mask == 1
        runoff_list = []
        FirstPass = True

        for id in range(num_regions):
            region_indices = moulin_ids == id 
            region_runoff  = runoff.values[valid_mask][region_indices]

            if FirstPass:
                runoff_sum = np.zeros(region_indices.shape)
            
            runoff_sum[region_indices] = np.sum(region_runoff)

            FirstPass = False
        
        runoff_map = -1 * np.ones(self.mask.shape, dtype=int)
        runoff_map[valid_mask] = runoff_sum
        
        return runoff_sum, runoff_map


# ------------------ Main ------------------

def run(config=CONFIG):
    moulin_generator = MoulinNodesGenerator(config['nc_path'], config['mesh_path'])

    # plot glacier surface
    moulin_generator.plot_usurf()

    # generate moulins by basin and random method
    selected_x, selected_y = moulin_generator.generate_random_points(
        number_of_moulins=config['number_of_moulins'],
        power=config['power'],
        elevation_threshold=config['elevation_threshold']
    )

    # generate Voronoi diagrams
    lowest_nodes, voronoi_map, moulin_ids, valid_points = moulin_generator.generate_voronoi(selected_x, selected_y)
    
    # interpolate moulins to mesh coordinates
    nearest_nodes = moulin_generator.interp_random_to_mesh_coords(lowest_nodes)
    
    # plot the moulins        
    moulin_generator.plot_random_moulins(lowest_nodes, nearest_nodes, voronoi_map)

    # save results
    moulin_generator.save_to_file(nearest_nodes, config['output_path'])

if __name__ == "__main__":
    run()
