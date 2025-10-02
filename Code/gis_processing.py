# this file contains the code for the spatial processing in ArcGIS

# insert the shapefile for stations
tube_stations = r"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers (WIP)\Crime and night tubes\Data\Underground_Stations\Underground_Stations.shp"
tube_stations = arcpy.management.MakeFeatureLayer(tube_stations, "tube_stations")

# import the data to table
arcpy.conversion.ExcelToTable(
    Input_Excel_File=r"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers (WIP)\Crime and night tubes EXTRA DATA\london_crime_data-2015.xlsx",
    Output_Table=r"C:\Users\jpmcl\AppData\Local\Temp\ArcGISProTemp14320\Untitled\Default.gdb\london_crime_data2015_ExcelToTable",
    Sheet="",
    field_names_row=1,
    cell_range=""
)

# geocode them
arcpy.management.ConvertCoordinateNotation(
    in_table="london_crime_data2015_ExcelToTable",
    out_featureclass=r"C:\Users\jpmcl\AppData\Local\Temp\ArcGISProTemp14320\Untitled\Default.gdb\london_crime_d_ConvertCoordi",
    x_field="Longitude",
    y_field="Latitude",
    input_coordinate_format="DDM_2",
    output_coordinate_format="DDM_2",
    id_field=None,
    spatial_reference='PROJCS["British_National_Grid",GEOGCS["GCS_OSGB_1936",DATUM["D_OSGB_1936",SPHEROID["Airy_1830",6377563.396,299.3249646]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Transverse_Mercator"],PARAMETER["False_Easting",400000.0],PARAMETER["False_Northing",-100000.0],PARAMETER["Central_Meridian",-2.0],PARAMETER["Scale_Factor",0.9996012717],PARAMETER["Latitude_Of_Origin",49.0],UNIT["Meter",1.0]];-5220400 -15524400 10000;-100000 10000;-100000 10000;0.001;0.001;0.001;IsHighPrecision',
    in_coor_system='GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]',
    exclude_invalid_records="INCLUDE_INVALID"
)

# for each crime: get the nearest station, and get how far away from it it is
