import argparse
import xarray as xr
from netCDF4 import Dataset
import numpy as np

def main(glacier_dir):
    # Load the netCDF file
    filename = f'{glacier_dir}/input_saved.nc'

    with Dataset(filename, mode='a') as ds:
        if 'ubed' in ds.variables:
            print("Variable 'ubed' already exists.")
        else:
            surf_top = ds.variables['usurf']
            thk = ds.variables['thk']

            # Create bed topography variable
            bed_top = ds.createVariable('ubed', np.float32, thk.dimensions)
            bed_top[:] = surf_top[:] - thk[:]

            bed_top.units = 'm'
            bed_top.long_name = 'Bed topography'

            print("Successfully created a variable for bed topography!")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Calculate bed topography for a glacier.')
    parser.add_argument('--dir', type=str, required=True, help='Directory name of the glacier')

    args = parser.parse_args()
    main(args.dir)