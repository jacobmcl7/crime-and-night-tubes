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



########################################################################

# now make a heatmap for change in thefts and robberies around London after vs before treatment

# load in the excel data created in data_visualisation.r
arcpy.conversion.ExcelToTable(
    Input_Excel_File=r"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers (WIP)\Crime and night tubes EXTRA DATA\theft_robbery_summary.xlsx",
    Output_Table=r"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\graphic_visualisation\graphic_visualisation.gdb\theft_robbery_summary_ExcelToTable",
    Sheet="",
    field_names_row=1,
    cell_range=""
)

# load in wards shapefile
wards = r"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers (WIP)\Crime and night tubes EXTRA DATA\Wards shapefile\WD_MAY_2024_UK_BSC.shp"
wards = arcpy.management.MakeFeatureLayer(wards, "wards")

# geocode the theft_robbery_summary table
crime_locations = r"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\graphic_visualisation\graphic_visualisation.gdb\crime_locations"
arcpy.management.XYTableToPoint(
    in_table="theft_robbery_summary_ExcelToTable",
    out_feature_class=crime_locations,
    x_field="longitude",
    y_field="latitude",
    z_field=None,
    coordinate_system='GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]];-400 -400 1000000000;-100000 10000;-100000 10000;8.98315284119521E-09;0.001;0.001;IsHighPrecision'
)

# now join ward ID to each point
temp_join = r"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\graphic_visualisation\graphic_visualisation.gdb\temp_join"
arcpy.analysis.SpatialJoin(
    target_features=crime_locations,
    join_features=wards,
    out_feature_class=temp_join,
    join_operation="JOIN_ONE_TO_ONE",
    match_option="WITHIN"
)

# calculate mean thefts_diff by ward
output_table = r"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\graphic_visualisation\graphic_visualisation.gdb\thefts_diff_by_ward"
arcpy.analysis.Statistics(
    in_table=temp_join,
    out_table=output_table,
    statistics_fields=[["thefts_robberies_prop_diff", "MEAN"]],
    case_field="WD24CD"
)

# now join the averages back to the ward polygons
arcpy.management.JoinField(
    in_data=wards,
    in_field="WD24CD",
    join_table=output_table,
    join_field="WD24CD",
    fields=["MEAN_thefts_robberies_prop_diff"]
)

# Now manually make the chloropleth map in ArcGIS Pro:
# 1. Right-click layer > Symbology
# 2. Choose Graduated Colors
# 3. Set Field to MEAN_thefts_robberies_prop_diff
# 4. Choose a color ramp and number of classes