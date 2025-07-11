import argparse
import xarray as xr
from netCDF4 import Dataset
import numpy as np

def main(glacier_dir):
    # Load the netCDF file
    filename = f'{glacier_dir}/input_saved_cook.nc'

    with Dataset(filename, mode='a') as ds:
        if 'ubed' in ds.variables:
            print("Variable 'ubed' already exists.")
        else:
            surf_top = ds.variables['usurf']
            thk = ds.variables['thk']

            # Check for NaN or invalid values
            print("Checking for NaN or infinite values:")
            print(f"NaN in surf_top: {np.any(np.isnan(surf_top[:]))}")
            print(f"NaN in thk: {np.any(np.isnan(thk[:]))}")
            print(f"Inf in surf_top: {np.any(np.isinf(surf_top[:]))}")
            print(f"Inf in thk: {np.any(np.isinf(thk[:]))}")
            
            # Ensure that dimensions match
            if surf_top.shape != thk.shape:
                print(f"Dimension mismatch: surf_top shape {surf_top.shape}, thk shape {thk.shape}")
                return

            # Create bed topography variable
            bed_top = ds.createVariable('ubed', thk.datatype, thk.dimensions)
            bed_top[:] = surf_top[:] - thk[:]

            bed_top.units = 'm'
            bed_top.long_name = 'Bed topography'

            print("Successfully created a variable for bed topography!")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Calculate bed topography for a glacier.')
    parser.add_argument('--dir', type=str, required=True, help='Directory name of the glacier')

    args = parser.parse_args()
    main(args.dir)
