# this file contains the code for the spatial processing in ArcGIS

# this must be run through the Python window in ArcGIS, in the project titled crime_data_geocoding saved in the appropriate directory

# ideally, we would get the distance from each crime location to each station, but this is far too computationally expensive
# instead, we create a near table, which gets all stations within x distance from each point
# the goal is to get a datafile that contains only location, station ID, and distance between them, for all locations of crime within x distance of a station
# note that this leaves out all the crimes further than that distance away, but I can just merge these in with the original crime data, using the crime identifier


#####################################################################


# insert the shapefile for stations
tube_stations = r"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers (WIP)\Crime and night tubes\Data\Underground_Stations\Underground_Stations.shp"
tube_stations = arcpy.management.MakeFeatureLayer(tube_stations, "tube_stations")


# insert the ward shapefile
wards = r"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers (WIP)\Crime and night tubes EXTRA DATA\Wards shapefile\WD_MAY_2024_UK_BSC.shp"
wards = arcpy.management.MakeFeatureLayer(wards, "wards")


# loop over the location excel files

indices = ['1', '2', '3']

for index in indices:

    # import the data to table
    arcpy.conversion.ExcelToTable(
        Input_Excel_File=fr"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers (WIP)\Crime and night tubes EXTRA DATA\london_crime_locations_{index}.xlsx",
        Output_Table=fr"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\crime_data_geocoding\crime_data_geocoding.gdb\crime_locations",
        Sheet="",
        field_names_row=1,
        cell_range=""
    )

    # geocode them (British National Grid)
    arcpy.management.ConvertCoordinateNotation(
        in_table=f"crime_locations",
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

    # record the ward containing each location
    arcpy.analysis.SpatialJoin(
        target_features="geolocated_data",
        join_features="wards",
        out_feature_class=r"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\crime_data_geocoding\crime_data_geocoding.gdb\crime_locations_ward",
        join_operation="JOIN_ONE_TO_ONE",
        join_type="KEEP_ALL",
        field_mapping='Longitude "Longitude" true true false 8 Double 0 0,First,#,geolocated_data,Longitude,-1,-1;Latitude "Latitude" true true false 8 Double 0 0,First,#,geolocated_data,Latitude,-1,-1;DDMLat "DDMLat" true true false 255 Text 0 0,First,#,geolocated_data,DDMLat,0,254;DDMLon "DDMLon" true true false 255 Text 0 0,First,#,geolocated_data,DDMLon,0,254;ORIG_OID "ORIG_OID" true true false 4 Long 0 0,First,#,geolocated_data,ORIG_OID,-1,-1;WD24CD "WD24CD" true true false 9 Text 0 0,First,#,wards,WD24CD,0,8;WD24NM "WD24NM" true true false 53 Text 0 0,First,#,wards,WD24NM,0,52',
        match_option="INTERSECT"
    )

    # make a near table, of all stations within 2km
    arcpy.analysis.GenerateNearTable(
        in_features="geolocated_data",
        near_features="tube_stations",
        out_table=r"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\crime_data_geocoding\crime_data_geocoding.gdb\near_table",
        search_radius="2 Kilometers",
        location="NO_LOCATION",
        angle="NO_ANGLE",
        closest="ALL",
        closest_count=0,
        method="PLANAR",
        distance_unit="Kilometers"
    )

    # merge in the location data, for each location-station combo in the near table
    arcpy.management.JoinField(
        in_data="near_table",
        in_field="IN_FID",
        join_table="crime_locations_ward",
        join_field="OBJECTID",
        fields=None,
        fm_option="NOT_USE_FM",
        field_mapping=None,
        index_join_fields="NO_INDEXES"
    )

    # merge in the station data, for each location-station combo in the near table
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

    # remove all fields other than the location, its ward details, the station name, and the distance to that station
    arcpy.management.DeleteField(
        in_table="near_table",
        drop_field="IN_FID;NEAR_FID;NEAR_RANK;DDMLat;DDMLon;Join_Count;TARGET_FID;ORIG_OID;OBJECTID_1;ATCOCODE;MODES;ACCESSIBIL;NIGHT_TUBE;NETWORK;DATASET_LA;FULL_NAME",
        method="DELETE_FIELDS"
    )

    # export the near table as an excel file, to be merged in with the crime data
    arcpy.conversion.TableToExcel(
        Input_Table='near_table',
        Output_Excel_File=fr"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers (WIP)\Crime and night tubes EXTRA DATA\location_station_pairs_{index}.xlsx",
        Use_field_alias_as_column_header="NAME",
        Use_domain_and_subtype_description="CODE"
    )