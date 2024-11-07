# Import necessary libraries
from qgis.core import (QgsRasterLayer, QgsProject, QgsVectorLayer, 
                       QgsFeature, QgsGeometry, QgsPointXY)
from PyQt5.QtWidgets import QFileDialog, QInputDialog
import processing

# Prompt user to select a .nc file
file_dialog = QFileDialog()
netcdf_file_path, _ = file_dialog.getOpenFileName(None, 
                        "Select NetCDF File", "", 
                        "NetCDF Files (*.nc)")

# Initialize the raster layer variable
raster_layer = None

# Load the NetCDF file as a raster layer if a file was selected and a variable name is provided
if netcdf_file_path:
    variable_name, ok = QInputDialog.getText(None, 
                                             "Enter Variable Name", 
                                             "Enter the name of the variable to load:")
    if ok and variable_name:
        # Attempt to load the raster layer
        raster_layer = QgsRasterLayer(f'NETCDF:"{netcdf_file_path}":{variable_name}', variable_name)
        
        # Check if the layer is valid and add it to the project
        if raster_layer.isValid():
            QgsProject.instance().addMapLayer(raster_layer)
            print(f"Successfully added the raster layer: {variable_name}")
        else:
            print("Failed to load the specified variable as a raster layer.")
    else:
        print("No variable name entered.")
else:
    print("No file selected!")

# Polygonize the raster layer if it was loaded successfully
if raster_layer and raster_layer.isValid():
    
    # Run the 'rasterpolygonize' processing tool
    params = {
        'INPUT': raster_layer,
        'BAND': 1,  # Assuming the first band, adjust if necessary
        'FIELD': 'DN',  # Field name for raster values in the vector output
        'OUTPUT': 'TEMPORARY_OUTPUT' 
    }
    result = processing.run("gdal:polygonize", params)

    # Load and add the resulting polygonized layer
    vector_layer = QgsVectorLayer(result['OUTPUT'], f"{variable_name}_polygonized", "ogr")
    if vector_layer.isValid():
        QgsProject.instance().addMapLayer(vector_layer)
        print(f"Successfully added the polygonized vector layer: {variable_name}_polygonized")
    else:
        print("Failed to load the polygonized vector layer.")
        
if vector_layer and vector_layer.isValid():
    
    # Define the output path (temporary layer)  
    output_path = None 
    
    # Create a new memory layer for the extracted vertices
    new_layer = QgsVectorLayer("Point?crs=EPSG:4326", "Extracted Vertices", "memory")
    provider = new_layer.dataProvider()

    # Get the bounding box of the raster layer
    extent = raster_layer.extent()
    min_x = extent.xMinimum()
    max_x = extent.xMaximum()
    min_y = extent.yMinimum()
    max_y = extent.yMaximum()

    # Iterate through each feature in the layer
    for feature in vector_layer.getFeatures():
        geom = feature.geometry()  # Get the geometry of the feature
        
        # Extract vertices based on geometry type
        if geom.isMultipart():  # Handle multi-part geometries
            for part in geom.asMultiPoint():
                point = QgsPointXY(part.x(), part.y())
                # Exclude points that are on the edge
                if not (point.x() == min_x or point.x() == max_x or point.y() == min_y or point.y() == max_y):
                    new_feature = QgsFeature()
                    new_feature.setGeometry(QgsGeometry.fromPointXY(point))
                    provider.addFeatures([new_feature])  # Add new feature to provider
        else:
            # Assuming the geometry is a polygon
            for point in geom.asPolygon()[0]:  # Get the exterior ring of the polygon
                new_feature = QgsFeature()
                new_feature.setGeometry(QgsGeometry.fromPointXY(point))
                # Exclude points that are on the edge
                if not (point.x() == min_x or point.x() == max_x or point.y() == min_y or point.y() == max_y):
                    provider.addFeatures([new_feature])  # Add new feature to provider

    # Update the layer's extent and add it to the project
    new_layer.updateExtents()
    QgsProject.instance().addMapLayer(new_layer)

    print("New vertex layer created successfully!")

 
else:
    print("Vector layer not found!")

print("Use MMQGIS --> Modify --> Delete Duplicate Geometries to obtain a new layer with vertices")
