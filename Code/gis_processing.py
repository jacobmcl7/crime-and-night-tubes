# this file contains the code for the spatial processing in ArcGIS

# this must be run through the Python window in ArcGIS, in the project titled crime_data_geocoding saved in the appropriate directory

# ideally, we would get the distance from each crime location to each station, but this is far too computationally expensive
# instead, we create a near table, which gets all stations within x distance from each point
# the goal is to get a datafile that contains only crime ID, station ID, and distance between them, for all crimes within x distance of a station
# note that this leaves out all the crimes further than that distance away, but I can just merge these in with the original crime data, using the crime identifier

# note: can we use the crime location as the unit of observation?


#####################################################################


# insert the shapefile for stations
tube_stations = r"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers (WIP)\Crime and night tubes\Data\Underground_Stations\Underground_Stations.shp"
tube_stations = arcpy.management.MakeFeatureLayer(tube_stations, "tube_stations")

years = ['2015', '2016', '2017']

for year in years:

    # import the data to table
    arcpy.conversion.ExcelToTable(
        Input_Excel_File=fr"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers (WIP)\Crime and night tubes EXTRA DATA\london_crime_data-{year}.xlsx",
        Output_Table=fr"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\crime_data_geocoding\crime_data_geocoding.gdb\crime_data",
        Sheet="",
        field_names_row=1,
        cell_range=""
    )

    # geocode them (British National Grid)
    arcpy.management.ConvertCoordinateNotation(
        in_table=f"crime_data",
        out_featureclass=fr"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\crime_data_geocoding\crime_data_geocoding.gdb\geolocated_data",
        x_field="Longitude",
        y_field="Latitude",
        input_coordinate_format="DDM_2",
        output_coordinate_format="DDM_2",
        id_field=None,
        spatial_reference='PROJCS["British_National_Grid",GEOGCS["GCS_OSGB_1936",DATUM["D_OSGB_1936",SPHEROID["Airy_1830",6377563.396,299.3249646]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Transverse_Mercator"],PARAMETER["False_Easting",400000.0],PARAMETER["False_Northing",-100000.0],PARAMETER["Central_Meridian",-2.0],PARAMETER["Scale_Factor",0.9996012717],PARAMETER["Latitude_Of_Origin",49.0],UNIT["Meter",1.0]];-5220400 -15524400 10000;-100000 10000;-100000 10000;0.001;0.001;0.001;IsHighPrecision',
        in_coor_system='GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]',
        exclude_invalid_records="INCLUDE_INVALID"
    )

    # make a near table, of all stations within 0.5km
    arcpy.analysis.GenerateNearTable(
        in_features="geolocated_data",
        near_features="tube_stations",
        out_table=r"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\crime_data_geocoding\crime_data_geocoding.gdb\near_table",
        search_radius="0.5 Kilometers",
        location="NO_LOCATION",
        angle="NO_ANGLE",
        closest="ALL",
        closest_count=0,
        method="PLANAR",
        distance_unit="Kilometers"
    )

    # merge in the crime data, for each crime-station combo in the near table
    arcpy.management.JoinField(
        in_data="near_table",
        in_field="IN_FID",
        join_table="crime_data",
        join_field="OBJECTID",
        fields=None,
        fm_option="NOT_USE_FM",
        field_mapping=None,
        index_join_fields="NO_INDEXES"
    )

    # merge in the station data, for each crime-station combo in the near table
    arcpy.management.JoinField(
        in_data="near_table",
        in_field="NEAR_FID",
        join_table="tube_stations",
        join_field="FID",
        fields=None,
        fm_option="NOT_USE_FM",
        field_mapping=None,
        index_join_fields="NO_INDEXES"
    )

    # remove all fields other than the crime ID, the station name, and the distance to that station
    arcpy.management.DeleteField(
        in_table="near_table",
        drop_field="IN_FID;NEAR_FID;NEAR_RANK;Month;Longitude;Latitude;Location;LSOA_code;LSOA_name;Crime_type;Last_outcome_category;OBJECTID_1;ATCOCODE;MODES;ACCESSIBIL;NIGHT_TUBE;NETWORK;DATASET_LA;FULL_NAME",
        method="DELETE_FIELDS"
    )

    # export the near table as an excel file, to be merged in with the crime data
    arcpy.conversion.TableToExcel(
        Input_Table='near_table',
        Output_Excel_File=fr"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers (WIP)\Crime and night tubes EXTRA DATA\crime_station_pairs_{year}.xlsx",
        Use_field_alias_as_column_header="NAME",
        Use_domain_and_subtype_description="CODE"
    )


# note: I think there are too many pairs when we do it like this - the excel files get corrupted/broken, probably because the file is too big
# two things to do:
# - split the data up into more subfiles in the R section
# - look over a shorter distance from the station
# DONE NOW - DISTANCE CHANGED TO 0.5KM AND IT WORKS




##################################################################

# previously, I was doing the following:

# # insert the shapefile for stations
# tube_stations = r"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers (WIP)\Crime and night tubes\Data\Underground_Stations\Underground_Stations.shp"
# tube_stations = arcpy.management.MakeFeatureLayer(tube_stations, "tube_stations")

# # create a 250m buffer around each station (arbitrary, and will be varied)
# buffer_output = fr"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\crime_data_geocoding\crime_data_geocoding.gdb\tube_stations_PairwiseBuffer"
# arcpy.analysis.PairwiseBuffer(
#     in_features="tube_stations",
#     out_feature_class=buffer_output,
#     buffer_distance_or_field="0.25 Kilometers",
#     dissolve_option="NONE",
#     dissolve_field=None,
#     method="PLANAR",
#     max_deviation="0 Meters"
# )

# years = ['2015', '2016', '2017']

# for year in years:

#     # import the data to table
#     input_excel = fr"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers (WIP)\Crime and night tubes EXTRA DATA\london_crime_data-{year}.xlsx"
#     output_table = fr"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\crime_data_geocoding\crime_data_geocoding.gdb\london_crime_data{year}_ExcelToTable"
#     arcpy.conversion.ExcelToTable(
#         Input_Excel_File=input_excel,
#         Output_Table=output_table,
#         Sheet="",
#         field_names_row=1,
#         cell_range=""
#     )

#     # geocode them (British National Grid)
#     out_featureclass_bng = fr"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\crime_data_geocoding\crime_data_geocoding.gdb\london_crime_d_{year}_ConvertCoordi"
#     arcpy.management.ConvertCoordinateNotation(
#         in_table=f"london_crime_data{year}_ExcelToTable",
#         out_featureclass=out_featureclass_bng,
#         x_field="Longitude",
#         y_field="Latitude",
#         input_coordinate_format="DDM_2",
#         output_coordinate_format="DDM_2",
#         id_field=None,
#         spatial_reference='PROJCS["British_National_Grid",GEOGCS["GCS_OSGB_1936",DATUM["D_OSGB_1936",SPHEROID["Airy_1830",6377563.396,299.3249646]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Transverse_Mercator"],PARAMETER["False_Easting",400000.0],PARAMETER["False_Northing",-100000.0],PARAMETER["Central_Meridian",-2.0],PARAMETER["Scale_Factor",0.9996012717],PARAMETER["Latitude_Of_Origin",49.0],UNIT["Meter",1.0]];-5220400 -15524400 10000;-100000 10000;-100000 10000;0.001;0.001;0.001;IsHighPrecision',
#         in_coor_system='GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]',
#         exclude_invalid_records="INCLUDE_INVALID"
#     )

#     # spatial join: for each crime, whether it is in the 250m buffer
#     spatial_join_output = fr"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\crime_data_geocoding\crime_data_geocoding.gdb\crime_data_spatialjoin_250m_{year}"
#     arcpy.analysis.SpatialJoin(
#         target_features=f"london_crime_d_{year}_ConvertCoordi",
#         join_features=buffer_output,
#         out_feature_class=spatial_join_output,
#         join_operation="JOIN_ONE_TO_MANY",  # so every single buffer is recorded!
#         join_type="KEEP_ALL",
#         match_option="INTERSECT",
#         search_radius=None,
#         distance_field_name=None
#     )

#     # ISSUE: in the spatial join it only seems to pick one buffer zone if it is in multiple. Need to sort this out

#     # export the data as an excel file
#     arcpy.conversion.TableToExcel(
#         Input_Table=spatial_join_output,
#         Output_Excel_File=fr"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers (WIP)\Crime and night tubes EXTRA DATA\crime_data_processed_{year}.xlsx",
#         Use_field_alias_as_column_header="NAME",
#         Use_domain_and_subtype_description="CODE"
#     )