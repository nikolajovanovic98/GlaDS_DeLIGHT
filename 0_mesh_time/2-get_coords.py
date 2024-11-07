# Import necessary libraries
from qgis.core import (QgsRasterLayer, QgsProject, QgsVectorLayer, 
                       QgsFeature, QgsGeometry, QgsPoint, QgsPointXY, QgsProcessingFeatureSourceDefinition, QgsField)
from PyQt5.QtWidgets import QFileDialog, QInputDialog
import processing
from PyQt5.QtCore import QVariant

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

    # Select only glacier polygons based on a specific value or range
    glacier_value = 1  # Adjust this value to match the glacier mask
    expression = f'"DN" = {glacier_value}'
    vector_layer.selectByExpression(expression)

    # Run the 'polygonstolines' processing tool to convert polygons to lines
    polygon_to_lines_params = {
        'INPUT': vector_layer,
        'OUTPUT': 'memory:'
    }
    polygon_to_lines_result = processing.run("qgis:polygonstolines", polygon_to_lines_params)
    lines_layer = polygon_to_lines_result['OUTPUT']
    QgsProject.instance().addMapLayer(lines_layer)
    print("Converted polygonized glacier mask to line features.")

    # Separate closed loop features into individual features
    new_features = []
    feature_id = 0

    for feature in lines_layer.getFeatures():
        geom = feature.geometry()

        # Check if it's a closed loop (the first point equals the last point)
        if geom.isMultipart():
            for part in geom.asMultiPolyline():
                first_point = part[0]  # First point
                last_point = part[-1]  # Last point
                if first_point == last_point:
                    # If closed, create a new feature with this geometry
                    new_feature = QgsFeature()
                    # Convert QgsPointXY to QgsPoint (2D -> 3D)
                    new_part = [QgsPoint(p.x(), p.y()) for p in part]  # Convert to QgsPoint
                    new_feature.setGeometry(QgsGeometry.fromPolyline(new_part))  # Set the geometry as a single polyline
                    new_feature.setAttributes([feature['DN']])  # Preserve original attributes
                    new_feature.setId(feature_id)  # Assign a unique ID
                    new_features.append(new_feature)
                    feature_id += 1
        else:
            part = geom.asPolyline()
            first_point = part[0]  # First point
            last_point = part[-1]  # Last point
            if first_point == last_point:
                # If closed, create a new feature with this geometry
                new_feature = QgsFeature()
                # Convert QgsPointXY to QgsPoint (2D -> 3D)
                new_part = [QgsPoint(p.x(), p.y()) for p in part]  # Convert to QgsPoint
                new_feature.setGeometry(QgsGeometry.fromPolyline(new_part))  # Set the geometry as a single polyline
                new_feature.setAttributes([feature['DN']])  # Preserve original attributes
                new_feature.setId(feature_id)  # Assign a unique ID
                new_features.append(new_feature)
                feature_id += 1

    # Sort features by length (in descending order)
    new_features.sort(key=lambda f: f.geometry().length(), reverse=True)

    # Get the second longest feature
    second_longest_feature = new_features[1] if len(new_features) > 1 else None

    if second_longest_feature:
        # Create a new memory layer to store the second longest loop
        memory_layer = QgsVectorLayer('LineString?crs=' + lines_layer.crs().authid(), "Second Longest Loop", "memory")
        memory_provider = memory_layer.dataProvider()
        memory_provider.addFeature(second_longest_feature)  # Add the second longest feature
        QgsProject.instance().addMapLayer(memory_layer)

        print(f"Successfully saved the second longest loop to a new layer.")

        # Extract vertices from the second longest loop
        geometry = second_longest_feature.geometry()
        vertices = []

        if geometry.isMultipart():
            for part in geometry.asMultiPolyline():
                for point in part:
                    vertices.append((point.x(), point.y()))
        else:
            vertices = [(point.x(), point.y()) for point in geometry.asPolyline()]

        # --- CREATE A NEW LAYER WITH VERTICES ---
        vertex_layer = QgsVectorLayer("Point?crs=" + lines_layer.crs().authid(), "Vertices of Second Longest Loop", "memory")
        vertex_provider = vertex_layer.dataProvider()

        # Add the X, Y coordinates to the vertex layer
        vertex_provider.addAttributes([QgsField("X", QVariant.Double), QgsField("Y", QVariant.Double)])
        vertex_layer.updateFields()

        # Add points as features to the vertex layer
        for vertex in vertices:
            point = QgsPointXY(vertex[0], vertex[1])
            feature = QgsFeature()
            feature.setGeometry(QgsGeometry.fromPointXY(point))
            feature.setAttributes([vertex[0], vertex[1]])
            vertex_provider.addFeature(feature)

        QgsProject.instance().addMapLayer(vertex_layer)

        print("Successfully created a vertex layer.")

        # --- EXPORT TO CSV ---
        # Path for CSV export (you can set this dynamically)
        csv_file_path, _ = QFileDialog.getSaveFileName(None, "Save Vertices as CSV", "", "CSV Files (*.csv)")

        if csv_file_path:
            # Open the CSV file for writing
            with open(csv_file_path, 'w') as file:
                # Write header
                file.write("X,Y\n")
                # Write the coordinates of each vertex
                for vertex in vertices:
                    file.write(f"{vertex[0]},{vertex[1]}\n")

            print(f"Vertices have been successfully exported to {csv_file_path}")
        else:
            print("No file selected for CSV export.")
    else:
        print("No second longest loop found. Not enough features.")

else:
    print("Vector layer not found!")
