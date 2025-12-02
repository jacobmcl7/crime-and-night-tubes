# this script does the geocoding of house addresses

# this needs to be run within the house_processing_tubes ArcGIS Pro project, in the Python window

# first link up to the geocoding API
# we need to access a locator to geocode the addresses. To do this, we use ESRI's free UK locator, which we access as follows
# go Insert > Connections > Server > New ArcGIS Server, and then paste in the following URL: https://datahub.esriuk.com/arcgis/rest/services/gb_locators/os_open_names_locator/GeocodeServer
# this must be done first, before starting the geocoding loop

# insert the shapefile for stations
tube_stations = r"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers (WIP)\Crime and night tubes\Data\Underground_Stations\Underground_Stations.shp"
tube_stations = arcpy.management.MakeFeatureLayer(tube_stations, "tube_stations")

# import the house price data
arcpy.conversion.ExcelToTable(
    Input_Excel_File=r"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers (WIP)\Crime and night tubes EXTRA DATA\house_data_pre_geocoding_test.xlsx",
    Output_Table=r"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\house_processing_tubes\house_processing_tubes.gdb\Price_Paid",
    Sheet="",
    field_names_row=1,
    cell_range=""
)

# now geocode the addresses (using the locator)
arcpy.geocoding.GeocodeAddresses(
    in_table="Price_Paid",
    address_locator=r"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\house_processing_tubes\GeocodeServer on datahub.esriuk.com.ags\gb_locators\os_open_names_locator.GeocodeServer",
    in_address_fields="Full_Location <None> VISIBLE NONE;PlaceName V8 VISIBLE NONE;Street V10 VISIBLE NONE;PopulatedPlace V12 VISIBLE NONE;DistrictBorough V13 VISIBLE NONE;CountyUnitary V14 VISIBLE NONE;Postcode V4 VISIBLE NONE",
    out_feature_class=r"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\house_processing_tubes\house_processing_tubes.gdb\Price_Paid_GeocodeAddresses",
    out_relationship_type="STATIC",
    country=None,
    location_type="",
    category=None,
    output_fields=""
)

# make a near table, of all stations within 2km
arcpy.analysis.GenerateNearTable(
    in_features="Price_Paid_GeocodeAddresses",
    near_features="tube_stations",
    out_table=r"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\house_processing_tubes\house_processing_tubes.gdb\near_table",
    search_radius="2 Kilometers",
    location="NO_LOCATION",
    angle="NO_ANGLE",
    closest="ALL",
    closest_count=0,
    method="PLANAR",
    distance_unit="Kilometers"
)

# merge in the house data, for each house in the near table
arcpy.management.JoinField(
    in_data="near_table",
    in_field="IN_FID",
    join_table="Price_Paid_GeocodeAddresses",
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

# # remove all fields other than the location, some relevant info about it (e.g. price), the station name, and the distance to that station
# arcpy.management.DeleteField(
#     in_table="near_table",
#     drop_field="IN_FID;NEAR_FID;NEAR_RANK;DDMLat;DDMLon;Join_Count;TARGET_FID;ORIG_OID;OBJECTID_1;ATCOCODE;MODES;ACCESSIBIL;NIGHT_TUBE;NETWORK;DATASET_LA;FULL_NAME",
#     method="DELETE_FIELDS"
# )

# export the near table as a csv file, to be merged in with the crime data
arcpy.conversion.ExportTable(
    in_table="near_table",
    out_table=r"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers (WIP)\Crime and night tubes EXTRA DATA\test.csv",
    where_clause="",
    use_field_alias_as_name="NOT_USE_ALIAS",
    field_mapping='IN_FID "IN_FID" true true false 4 Long 0 0,First,#,near_table,IN_FID,-1,-1;NEAR_FID "NEAR_FID" true true false 4 Long 0 0,First,#,near_table,NEAR_FID,-1,-1;NEAR_DIST "NEAR_DIST" true true false 8 Double 0 0,First,#,near_table,NEAR_DIST,-1,-1;NEAR_RANK "NEAR_RANK" true true false 4 Long 0 0,First,#,near_table,NEAR_RANK,-1,-1;Score "Score" true true false 8 Double 0 0,First,#,near_table,Score,-1,-1;Match_type "Match_type" true true false 2 Text 0 0,First,#,near_table,Match_type,0,1;Match_addr "Match_addr" true true false 250 Text 0 0,First,#,near_table,Match_addr,0,249;NamesUri "NamesUri" true true false 100 Text 0 0,First,#,near_table,NamesUri,0,99;Name1 "Name1" true true false 250 Text 0 0,First,#,near_table,Name1,0,249;Name1Lang "Name1Lang" true true false 3 Text 0 0,First,#,near_table,Name1Lang,0,2;Name2 "Name2" true true false 250 Text 0 0,First,#,near_table,Name2,0,249;Name2Lang "Name2Lang" true true false 3 Text 0 0,First,#,near_table,Name2Lang,0,2;Type "Type" true true false 30 Text 0 0,First,#,near_table,Type,0,29;LocalType "LocalType" true true false 250 Text 0 0,First,#,near_table,LocalType,0,249;MostDetailViewRes "MostDetailViewRes" true true false 4 Long 0 0,First,#,near_table,MostDetailViewRes,-1,-1;LeastDetailViewRes "LeastDetailViewRes" true true false 4 Long 0 0,First,#,near_table,LeastDetailViewRes,-1,-1;MbrXMin "MbrXMin" true true false 4 Long 0 0,First,#,near_table,MbrXMin,-1,-1;MbrYMin "MbrYMin" true true false 4 Long 0 0,First,#,near_table,MbrYMin,-1,-1;MbrXMax "MbrXMax" true true false 4 Long 0 0,First,#,near_table,MbrXMax,-1,-1;MbrYMax "MbrYMax" true true false 4 Long 0 0,First,#,near_table,MbrYMax,-1,-1;X "X" true true false 8 Double 0 0,First,#,near_table,X,-1,-1;Y "Y" true true false 8 Double 0 0,First,#,near_table,Y,-1,-1;PostcodeDistrict "PostcodeDistrict" true true false 4 Text 0 0,First,#,near_table,PostcodeDistrict,0,3;PostcodeDistrictUri "PostcodeDistrictUri" true true false 60 Text 0 0,First,#,near_table,PostcodeDistrictUri,0,59;PopulatedPlace "PopulatedPlace" true true false 103 Text 0 0,First,#,near_table,PopulatedPlace,0,102;PopulatedPlaceUri "PopulatedPlaceUri" true true false 60 Text 0 0,First,#,near_table,PopulatedPlaceUri,0,59;PopulatedPlaceType "PopulatedPlaceType" true true false 80 Text 0 0,First,#,near_table,PopulatedPlaceType,0,79;DistrictBorough "DistrictBorough" true true false 80 Text 0 0,First,#,near_table,DistrictBorough,0,79;DistrictBoroughUri "DistrictBoroughUri" true true false 80 Text 0 0,First,#,near_table,DistrictBoroughUri,0,79;DistrictBoroughType "DistrictBoroughType" true true false 80 Text 0 0,First,#,near_table,DistrictBoroughType,0,79;CountyUnitary "CountyUnitary" true true false 80 Text 0 0,First,#,near_table,CountyUnitary,0,79;CountyUnitaryUri "CountyUnitaryUri" true true false 80 Text 0 0,First,#,near_table,CountyUnitaryUri,0,79;CountyUnitaryType "CountyUnitaryType" true true false 80 Text 0 0,First,#,near_table,CountyUnitaryType,0,79;Region "Region" true true false 30 Text 0 0,First,#,near_table,Region,0,29;RegionUri "RegionUri" true true false 60 Text 0 0,First,#,near_table,RegionUri,0,59;Country "Country" true true false 30 Text 0 0,First,#,near_table,Country,0,29;CountryUri "CountryUri" true true false 60 Text 0 0,First,#,near_table,CountryUri,0,59;RelatedSpatialObject "RelatedSpatialObject" true true false 30 Text 0 0,First,#,near_table,RelatedSpatialObject,0,29;SameAsDbpedia "SameAsDbpedia" true true false 100 Text 0 0,First,#,near_table,SameAsDbpedia,0,99;SameAsGeonames "SameAsGeonames" true true false 100 Text 0 0,First,#,near_table,SameAsGeonames,0,99;Addr_type "Addr_type" true true false 20 Text 0 0,First,#,near_table,Addr_type,0,19;Rank "Rank" true true false 10 Text 0 0,First,#,near_table,Rank,0,9;Status "Status" true true false 1 Text 0 0,First,#,near_table,Status,0,254;IN_Full_Location "Full_Location" true true false 300 Text 0 0,First,#,near_table,IN_Full_Location,0,299;IN_PlaceName "PlaceName" true true false 100 Text 0 0,First,#,near_table,IN_PlaceName,0,99;IN_Street "Street" true true false 100 Text 0 0,First,#,near_table,IN_Street,0,99;IN_PopulatedPlace "PopulatedPlace" true true false 100 Text 0 0,First,#,near_table,IN_PopulatedPlace,0,99;IN_DistrictBorough "DistrictBorough" true true false 80 Text 0 0,First,#,near_table,IN_DistrictBorough,0,79;IN_CountyUnitary "CountyUnitary" true true false 80 Text 0 0,First,#,near_table,IN_CountyUnitary,0,79;IN_Postcode "Postcode" true true false 10 Text 0 0,First,#,near_table,IN_Postcode,0,9;USER_V2 "V2" true true false 4 Long 0 0,First,#,near_table,USER_V2,-1,-1;USER_V3 "V3" true true false 8 DateOnly 0 0,First,#,near_table,USER_V3,-1,-1;USER_V4 "V4" true true false 255 Text 0 0,First,#,near_table,USER_V4,0,254;USER_V5 "V5" true true false 255 Text 0 0,First,#,near_table,USER_V5,0,254;USER_V6 "V6" true true false 255 Text 0 0,First,#,near_table,USER_V6,0,254;USER_V7 "V7" true true false 255 Text 0 0,First,#,near_table,USER_V7,0,254;USER_V8 "V8" true true false 255 Text 0 0,First,#,near_table,USER_V8,0,254;USER_V9 "V9" true true false 255 Text 0 0,First,#,near_table,USER_V9,0,254;USER_V10 "V10" true true false 255 Text 0 0,First,#,near_table,USER_V10,0,254;USER_V11 "V11" true true false 255 Text 0 0,First,#,near_table,USER_V11,0,254;USER_V12 "V12" true true false 255 Text 0 0,First,#,near_table,USER_V12,0,254;USER_V13 "V13" true true false 255 Text 0 0,First,#,near_table,USER_V13,0,254;USER_V14 "V14" true true false 255 Text 0 0,First,#,near_table,USER_V14,0,254;USER_V15 "V15" true true false 255 Text 0 0,First,#,near_table,USER_V15,0,254;USER_V16 "V16" true true false 255 Text 0 0,First,#,near_table,USER_V16,0,254;OBJECTID_1 "OBJECTID" true true false 2 Short 0 0,First,#,near_table,OBJECTID_1,-1,-1;NAME "NAME" true true false 28 Text 0 0,First,#,near_table,NAME,0,27;LINES "LINES" true true false 72 Text 0 0,First,#,near_table,LINES,0,71;ATCOCODE "ATCOCODE" true true false 11 Text 0 0,First,#,near_table,ATCOCODE,0,10;MODES "MODES" true true false 24 Text 0 0,First,#,near_table,MODES,0,23;ACCESSIBIL "ACCESSIBIL" true true false 39 Text 0 0,First,#,near_table,ACCESSIBIL,0,38;NIGHT_TUBE "NIGHT_TUBE" true true false 3 Text 0 0,First,#,near_table,NIGHT_TUBE,0,2;NETWORK "NETWORK" true true false 18 Text 0 0,First,#,near_table,NETWORK,0,17;DATASET_LA "DATASET_LA" true true false 8 Date 0 0,First,#,near_table,DATASET_LA,-1,-1;FULL_NAME "FULL_NAME" true true false 36 Text 0 0,First,#,near_table,FULL_NAME,0,35',
    sort_field=None
)