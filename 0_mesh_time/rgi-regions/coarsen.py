import xarray as xr 
import numpy as np
import sys

# input file
txtfile = sys.argv[1]
output_dir = sys.argv[2]
glacier_ids = np.genfromtxt(txtfile, dtype=str, encoding='utf-8')

# coarsening loop
for gid in glacier_ids:

    # open dataset
    nc_path = f'{output_dir}/{gid}/{gid}_usurf.nc'
    ds = xr.open_dataset(nc_path)
    usurf = ds['usurf']

    # res
    dx = ds['x'][1] - ds['x'][0]
    dy = ds['y'][1] - ds['y'][0]

    if (dx == 25.) and ( dy == 25.):
        new_usurf = usurf.coarsen(x=4, y=4, boundary='trim').mean()
    elif (dx == 50.) and (dy == 50.):
        new_usurf = usurf.coarsen(x=2, y=2, boundary='trim').mean()
    elif (dx == 100.) and (dy == 100.):
        new_usurf = usurf 
    elif (dx == 200.) and (dy == 200.):
        new_x = np.arange(ds['x'].min(), ds['x'].max() + 100, 100)
        new_y = np.arange(ds['y'].min(), ds['y'].max() + 100, 100)
        new_usurf = usurf.interp(x=new_x, y=new_y)  
    else:
        raise Exception(f"{gid}: The resolution is not 25, 50, or 100 m")
    
    output_file = f"{output_dir}/{gid}/{gid}_usurf_100m.nc"
    new_usurf.to_netcdf(output_file)
    print(f"Saved {output_file}")