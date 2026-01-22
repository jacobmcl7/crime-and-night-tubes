# this script creates a map of the stations in London and the treated and control zones

# it is in a project called graphic_visualisation, not geographic_visualisation - fix this

# also need to clean this up and make some more graphs

# insert the shapefile for stations
tube_stations = r"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers (WIP)\Crime and night tubes\Data\Underground_Stations\Underground_Stations.shp"
tube_stations = arcpy.management.MakeFeatureLayer(tube_stations, "tube_stations")

# create a 2km buffer around each station
buffer_2km = arcpy.analysis.Buffer(
    in_features="tube_stations",
    out_feature_class=r"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\graphic_visualisation\graphic_visualisation.gdb\buffer_2km",
    buffer_distance_or_field="2 Kilometers",
    line_side="FULL",
    line_end_type="ROUND",
    dissolve_option="NONE",
    dissolve_field=None,
    method="PLANAR")

# dissolve the buffers into a single feature
arcpy.management.Dissolve(
    in_features="buffer_2km",
    out_feature_class=r"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\graphic_visualisation\graphic_visualisation.gdb\buffer_2km_Dissolve",
    dissolve_field=None,
    statistics_fields=None,
    multi_part="MULTI_PART",
    unsplit_lines="DISSOLVE_LINES",
    concatenation_separator=""
)

# subset the tube stations to those where NIGHT_TUBE = 'Yes'
night_tube_stations = arcpy.management.MakeFeatureLayer(
    in_features="tube_stations",
    out_layer="night_tube_stations",
    where_clause="NIGHT_TUBE = 'Yes'"
)

# now make a 1km buffer around these night tube stations
buffer_1km_night = arcpy.analysis.Buffer(
    in_features="night_tube_stations",
    out_feature_class=r"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\graphic_visualisation\graphic_visualisation.gdb\buffer_1km_night",
    buffer_distance_or_field="1 Kilometers",
    line_side="FULL",
    line_end_type="ROUND",
    dissolve_option="NONE",
    dissolve_field=None,
    method="PLANAR")

# dissolve the night tube buffers into a single feature
arcpy.management.Dissolve(
    in_features="buffer_1km_night",
    out_feature_class=r"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\graphic_visualisation\graphic_visualisation.gdb\buffer_1km_night_Dissolve",
    dissolve_field=None,
    statistics_fields=None,
    multi_part="MULTI_PART",
    unsplit_lines="DISSOLVE_LINES",
    concatenation_separator=""
)


# colour the graphs 

import arcpy

def set_polygon_symbology(layer, fill_rgba, outline_rgba, outline_width=1.5):
    """Apply simple polygon symbology to a layer."""
    sym = layer.symbology
    if hasattr(sym, 'renderer') and sym.renderer.type == "SimpleRenderer":
        sym.renderer.symbol.color = {'RGB': fill_rgba}
        sym.renderer.symbol.outlineColor = {'RGB': outline_rgba}
        sym.renderer.symbol.outlineWidth = outline_width
        layer.symbology = sym

aprx = arcpy.mp.ArcGISProject("CURRENT")
map_obj = aprx.listMaps()[0]

# Blue buffer
lyr_2km = map_obj.listLayers("buffer_2km_Dissolve")[0]
set_polygon_symbology(lyr_2km, [0, 120, 200, 50], [0, 80, 150, 100])

# blue tube stations
lyr_stations = map_obj.listLayers("tube_stations")[0]
set_polygon_symbology(lyr_stations, [0, 120, 200, 255], [0, 0, 0, 255], outline_width=1)

# Red buffer
lyr_1km = map_obj.listLayers("buffer_1km_night_Dissolve")[0]
set_polygon_symbology(lyr_1km, [200, 50, 50, 50], [150, 30, 30, 100])

# red night tube stations
lyr_night_stations = map_obj.listLayers("night_tube_stations")[0]
set_polygon_symbology(lyr_night_stations, [200, 50, 50, 255], [0, 0, 0, 255], outline_width=1)

# now create a layout, and organise it nicely
# this has to be done within the UI, so just export the layout once done
# I also just export the completed layout frame manually