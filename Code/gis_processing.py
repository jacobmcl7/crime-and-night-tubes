# this file contains the code for the spatial processing in ArcGIS

# insert the shapefile for stations
tube_stations = r"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers (WIP)\Crime and night tubes\Data\Underground_Stations\Underground_Stations.shp"
tube_stations = arcpy.management.MakeFeatureLayer(tube_stations, "tube_stations")

# create a 250m buffer around each station (arbitrary, and will be varied)
buffer_output = fr"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\crime_data_geocoding\crime_data_geocoding.gdb\tube_stations_PairwiseBuffer"
arcpy.analysis.PairwiseBuffer(
    in_features="tube_stations",
    out_feature_class=buffer_output,
    buffer_distance_or_field="0.25 Kilometers",
    dissolve_option="NONE",
    dissolve_field=None,
    method="PLANAR",
    max_deviation="0 Meters"
)

years = ['2015', '2016', '2017']

for year in years:

    # import the data to table
    input_excel = fr"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers (WIP)\Crime and night tubes EXTRA DATA\london_crime_data-{year}.xlsx"
    output_table = fr"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\crime_data_geocoding\crime_data_geocoding.gdb\london_crime_data{year}_ExcelToTable"
    arcpy.conversion.ExcelToTable(
        Input_Excel_File=input_excel,
        Output_Table=output_table,
        Sheet="",
        field_names_row=1,
        cell_range=""
    )

    # geocode them (British National Grid)
    out_featureclass_bng = fr"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\crime_data_geocoding\crime_data_geocoding.gdb\london_crime_d_{year}_ConvertCoordi"
    arcpy.management.ConvertCoordinateNotation(
        in_table=f"london_crime_data{year}_ExcelToTable",
        out_featureclass=out_featureclass_bng,
        x_field="Longitude",
        y_field="Latitude",
        input_coordinate_format="DDM_2",
        output_coordinate_format="DDM_2",
        id_field=None,
        spatial_reference='PROJCS["British_National_Grid",GEOGCS["GCS_OSGB_1936",DATUM["D_OSGB_1936",SPHEROID["Airy_1830",6377563.396,299.3249646]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Transverse_Mercator"],PARAMETER["False_Easting",400000.0],PARAMETER["False_Northing",-100000.0],PARAMETER["Central_Meridian",-2.0],PARAMETER["Scale_Factor",0.9996012717],PARAMETER["Latitude_Of_Origin",49.0],UNIT["Meter",1.0]];-5220400 -15524400 10000;-100000 10000;-100000 10000;0.001;0.001;0.001;IsHighPrecision',
        in_coor_system='GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]',
        exclude_invalid_records="INCLUDE_INVALID"
    )

    # spatial join: for each crime, whether it is in the 250m buffer
    spatial_join_output = fr"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\crime_data_geocoding\crime_data_geocoding.gdb\crime_data_spatialjoin_250m_{year}"
    arcpy.analysis.SpatialJoin(
        target_features=f"london_crime_d_{year}_ConvertCoordi",
        join_features=buffer_output,
        out_feature_class=spatial_join_output,
        join_type="KEEP_ALL",
        match_option="INTERSECT",
        search_radius=None,
        distance_field_name=None
    )

    # export the data as an excel file
    arcpy.conversion.TableToExcel(
        Input_Table=spatial_join_output,
        Output_Excel_File=fr"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers (WIP)\Crime and night tubes EXTRA DATA\crime_data_processed_{year}.xlsx",
        Use_field_alias_as_column_header="NAME",
        Use_domain_and_subtype_description="CODE"
    )

# can we use the crime location as the unit of observation?