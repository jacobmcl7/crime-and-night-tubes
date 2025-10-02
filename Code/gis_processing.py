# this file contains the code for the spatial processing in ArcGIS

# insert the shapefile for stations
tube_stations = r"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers (WIP)\Crime and night tubes\Data\Underground_Stations\Underground_Stations.shp"
tube_stations = arcpy.management.MakeFeatureLayer(tube_stations, "tube_stations")

years = ['2015', '2016', '2017']

for year in years:
    # import the data to table
    input_excel = fr"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers (WIP)\Crime and night tubes EXTRA DATA\london_crime_data-{year}.xlsx"
    output_table = fr"C:\Users\jpmcl\AppData\Local\Temp\ArcGISProTemp12936\Untitled\Default.gdb\london_crime_data{year}_ExcelToTable"
    arcpy.conversion.ExcelToTable(
        Input_Excel_File=input_excel,
        Output_Table=output_table,
        Sheet="",
        field_names_row=1,
        cell_range=""
    )

    # # geocode them (British National Grid)
    # out_featureclass_bng = fr"C:\Users\jpmcl\AppData\Local\Temp\ArcGISProTemp14320\Untitled\Default.gdb\london_crime_d_{year}_ConvertCoordi"
    # arcpy.management.ConvertCoordinateNotation(
    #     in_table=f"london_crime_data{year}_ExcelToTable",
    #     out_featureclass=out_featureclass_bng,
    #     x_field="Longitude",
    #     y_field="Latitude",
    #     input_coordinate_format="DDM_2",
    #     output_coordinate_format="DDM_2",
    #     id_field=None,
    #     spatial_reference='PROJCS["British_National_Grid",GEOGCS["GCS_OSGB_1936",DATUM["D_OSGB_1936",SPHEROID["Airy_1830",6377563.396,299.3249646]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Transverse_Mercator"],PARAMETER["False_Easting",400000.0],PARAMETER["False_Northing",-100000.0],PARAMETER["Central_Meridian",-2.0],PARAMETER["Scale_Factor",0.9996012717],PARAMETER["Latitude_Of_Origin",49.0],UNIT["Meter",1.0]];-5220400 -15524400 10000;-100000 10000;-100000 10000;0.001;0.001;0.001;IsHighPrecision',
    #     in_coor_system='GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]',
    #     exclude_invalid_records="INCLUDE_INVALID"
    # )

    # geocode them (WGS 1984)
    out_featureclass_wgs = fr"C:\Users\jpmcl\AppData\Local\Temp\ArcGISProTemp12936\Untitled\Default.gdb\london_crime_d_{year}_ConvertCoordi"
    arcpy.management.ConvertCoordinateNotation(
        in_table=f"london_crime_data{year}_ExcelToTable",
        out_featureclass=out_featureclass_wgs,
        x_field="Longitude",
        y_field="Latitude",
        input_coordinate_format="DDM_2",
        output_coordinate_format="DDM_2",
        id_field=None,
        spatial_reference='GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]];-400 -400 1000000000;-100000 10000;-100000 10000;8.98315284119521E-09;0.001;0.001;IsHighPrecision',
        in_coor_system='GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]',
        exclude_invalid_records="INCLUDE_INVALID"
    )

# merge the yearly shapefiles together into one
arcpy.management.Merge(
    inputs="london_crime_d_2017_ConvertCoordi;london_crime_d_2016_ConvertCoordi;london_crime_d_2015_ConvertCoordi",
    output=r"C:\Users\jpmcl\AppData\Local\Temp\ArcGISProTemp12936\Untitled\Default.gdb\london_crime_d_Merge",
    field_mappings='Month "Month" true true false 255 Text 0 0,First,#,london_crime_d_2017_ConvertCoordi,Month,0,254,london_crime_d_2016_ConvertCoordi,Month,0,254,london_crime_d_2015_ConvertCoordi,Month,0,254;Longitude "Longitude" true true false 8 Double 0 0,First,#,london_crime_d_2017_ConvertCoordi,Longitude,-1,-1,london_crime_d_2016_ConvertCoordi,Longitude,-1,-1,london_crime_d_2015_ConvertCoordi,Longitude,-1,-1;Latitude "Latitude" true true false 8 Double 0 0,First,#,london_crime_d_2017_ConvertCoordi,Latitude,-1,-1,london_crime_d_2016_ConvertCoordi,Latitude,-1,-1,london_crime_d_2015_ConvertCoordi,Latitude,-1,-1;Location "Location" true true false 255 Text 0 0,First,#,london_crime_d_2017_ConvertCoordi,Location,0,254,london_crime_d_2016_ConvertCoordi,Location,0,254,london_crime_d_2015_ConvertCoordi,Location,0,254;LSOA_code "LSOA.code" true true false 255 Text 0 0,First,#,london_crime_d_2017_ConvertCoordi,LSOA_code,0,254,london_crime_d_2016_ConvertCoordi,LSOA_code,0,254,london_crime_d_2015_ConvertCoordi,LSOA_code,0,254;LSOA_name "LSOA.name" true true false 255 Text 0 0,First,#,london_crime_d_2017_ConvertCoordi,LSOA_name,0,254,london_crime_d_2016_ConvertCoordi,LSOA_name,0,254,london_crime_d_2015_ConvertCoordi,LSOA_name,0,254;Crime_type "Crime.type" true true false 255 Text 0 0,First,#,london_crime_d_2017_ConvertCoordi,Crime_type,0,254,london_crime_d_2016_ConvertCoordi,Crime_type,0,254,london_crime_d_2015_ConvertCoordi,Crime_type,0,254;Last_outcome_category "Last.outcome.category" true true false 255 Text 0 0,First,#,london_crime_d_2017_ConvertCoordi,Last_outcome_category,0,254,london_crime_d_2016_ConvertCoordi,Last_outcome_category,0,254,london_crime_d_2015_ConvertCoordi,Last_outcome_category,0,254;DDMLat "DDMLat" true true false 255 Text 0 0,First,#,london_crime_d_2017_ConvertCoordi,DDMLat,0,254,london_crime_d_2016_ConvertCoordi,DDMLat,0,254,london_crime_d_2015_ConvertCoordi,DDMLat,0,254;DDMLon "DDMLon" true true false 255 Text 0 0,First,#,london_crime_d_2017_ConvertCoordi,DDMLon,0,254,london_crime_d_2016_ConvertCoordi,DDMLon,0,254,london_crime_d_2015_ConvertCoordi,DDMLon,0,254;ORIG_OID "ORIG_OID" true true false 4 Long 0 0,First,#,london_crime_d_2017_ConvertCoordi,ORIG_OID,-1,-1,london_crime_d_2016_ConvertCoordi,ORIG_OID,-1,-1,london_crime_d_2015_ConvertCoordi,ORIG_OID,-1,-1',
    add_source="NO_SOURCE_INFO"
)

# export the merged data as a shapefile
arcpy.conversion.ExportFeatures(
    in_features="london_crime_d_Merge",
    out_features=r"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers (WIP)\Crime and night tubes EXTRA DATA\processed_crime_data.shp",
    where_clause="",
    use_field_alias_as_name="NOT_USE_ALIAS",
    field_mapping='Month "Month" true true false 255 Text 0 0,First,#,london_crime_d_Merge,Month,0,254;Longitude "Longitude" true true false 8 Double 0 0,First,#,london_crime_d_Merge,Longitude,-1,-1;Latitude "Latitude" true true false 8 Double 0 0,First,#,london_crime_d_Merge,Latitude,-1,-1;Location "Location" true true false 255 Text 0 0,First,#,london_crime_d_Merge,Location,0,254;LSOA_code "LSOA.code" true true false 255 Text 0 0,First,#,london_crime_d_Merge,LSOA_code,0,254;LSOA_name "LSOA.name" true true false 255 Text 0 0,First,#,london_crime_d_Merge,LSOA_name,0,254;Crime_type "Crime.type" true true false 255 Text 0 0,First,#,london_crime_d_Merge,Crime_type,0,254;Last_outcome_category "Last.outcome.category" true true false 255 Text 0 0,First,#,london_crime_d_Merge,Last_outcome_category,0,254;DDMLat "DDMLat" true true false 255 Text 0 0,First,#,london_crime_d_Merge,DDMLat,0,254;DDMLon "DDMLon" true true false 255 Text 0 0,First,#,london_crime_d_Merge,DDMLon,0,254;ORIG_OID "ORIG_OID" true true false 4 Long 0 0,First,#,london_crime_d_Merge,ORIG_OID,-1,-1',
    sort_field=None
)

# for each crime: get the nearest station, and get how far away from it it is
