# Import necessary libraries
from qgis.core import QgsVectorLayer, QgsProject, QgsField, QgsFeature
from qgis.PyQt.QtCore import QVariant
from PyQt5.QtWidgets import QFileDialog

# Prompt user to select a vector layer
file_dialog = QFileDialog()
vector_file_path, _ = file_dialog.getOpenFileName(None, 
                        "Select Vector Layer", "", 
                        "Vector Files (*.shp *.geojson *.gpkg)")

# Load the selected vector layer
if vector_file_path:
    vector_layer = QgsVectorLayer(vector_file_path, "Loaded Vertices", "ogr")
    if vector_layer.isValid():
        QgsProject.instance().addMapLayer(vector_layer)
        print(f"Successfully added the vector layer: {vector_layer.name()}")

        # Start editing the layer to add new fields
        if not vector_layer.isEditable():
            vector_layer.startEditing()
        
        # Add X and Y fields to the layer
        vector_layer.dataProvider().addAttributes([
            QgsField("X_Coord", QVariant.Double),
            QgsField("Y_Coord", QVariant.Double)
        ])
        vector_layer.updateFields()

        # Update the new fields with the X and Y coordinates
        for feature in vector_layer.getFeatures():
            geom = feature.geometry()
            if geom.isMultipart():  # Handle multi-part geometries
                point = geom.asMultiPoint()[0]  # Take the first point as an example
            else:
                point = geom.asPoint()

            # Get X and Y coordinates, formatted to 5 decimal places
            x_coord = round(point.x(), 5)
            y_coord = round(point.y(), 5)

            # Update the feature with new coordinates
            feature['X_Coord'] = x_coord
            feature['Y_Coord'] = y_coord

            # Update the feature in the layer
            vector_layer.updateFeature(feature)

        # Commit the changes to the layer
        vector_layer.commitChanges()
        print("X and Y coordinate fields added and populated successfully!")
    else:
        print("Failed to load the vector layer.")
else:
    print("No file selected!")
