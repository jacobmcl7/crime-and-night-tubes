# this script creates a map of the averaged treatment effects in London, from the BJS regression results

# NOTE: to run this file multiple times, you must restart the kernel each time

##========================================================
# preliminaries
##========================================================

# import relevant libraries
import sys, os 
import arcpy
import numpy as np 
import pandas as pd 

# set arcgis environment
arcpy.env.overwriteOutput = True

# set filepath
root = r"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers (WIP)"


##========================================================
# cleaning
##========================================================

# import the TE estimates as a dataframe, from csv
df = pd.read_csv(os.path.join(root, "Crime and night tubes EXTRA DATA", "BJS results", "BJS_TE_estimates.csv"))

# average the TE estimates at the MSOA-season level

# first split month into first 4 characters (year) and last 2 (month)
df['year'] = df['month'].str[:4]
df['month'] = df['month'].str[-2:]

# keep only those in 2017 (i.e. once all lines open)
df = df[df['year'] == '2017']

# create a season variable
df['season'] = df['month'].map({'01': 'Winter', '02': 'Winter', '03': 'Winter', '04': 'Spring', '05': 'Spring', '06': 'Spring', '07': 'Summer', '08': 'Summer', '09': 'Summer', '10': 'Autumn', '11': 'Autumn', '12': 'Autumn'})

# now average TE estimates by MSOA and season
df_avg = df.groupby(['msoa21nm', 'season'])['tauhat'].mean().reset_index()

# reshape to one row per MSOA, one column per season
seasons = ['Winter', 'Spring', 'Summer', 'Autumn']
wide = df_avg.pivot(index='msoa21nm', columns='season', values='tauhat').reindex(columns=seasons)
wide.index = wide.index.astype(str).str.strip()
lookup = wide.to_dict('index')      # {msoa: {'Winter': x, 'Spring': y, ...}}

# number of quantile bins (for plotting)
n_bins = 10

# common quantile edges across all seasons (n_bins + 1 edges)
edges = np.nanquantile(wide.to_numpy(), np.linspace(0, 1, n_bins + 1))
lower_bounds  = edges[:-1].tolist()
common_bounds = edges[1:].tolist()

##========================================================
# make a gdb, get the shapefile in and merge with TE ests
##========================================================

# make the gdb
out_gdb = os.path.join(root, "Crime and night tubes EXTRA DATA", "Geodatabases", "TE_maps.gdb")
if not arcpy.Exists(out_gdb):
    arcpy.management.CreateFileGDB(os.path.dirname(out_gdb), os.path.basename(out_gdb))

# load in the shapefile and get a field ready for the TE estimates for each season
msoa_shp = os.path.join(root, "Crime and night tubes EXTRA DATA", "MSOA shapefile", "MSOA_2021_EW_BGC_V3.shp")
fc = os.path.join(out_gdb, "MSOA21_TE")
arcpy.management.CopyFeatures(msoa_shp, fc)

# add one field per season
season_fields = [f"tau_{s}" for s in seasons]
for f in season_fields:
    arcpy.management.AddField(fc, f, "DOUBLE")

# match the TE estimates to the MSOA shapefile
n_matched = 0
with arcpy.da.UpdateCursor(fc, ["msoa21nm"] + season_fields) as cur:
    for row in cur:
        vals = lookup.get(str(row[0]).strip())
        if vals is None:
            new = [None] * len(seasons)          # unmatched -> NULL
        else:
            n_matched += 1
            new = [None if pd.isna(vals[s]) else float(vals[s]) for s in seasons]
        cur.updateRow([row[0]] + new)

print(f"Matched {n_matched} of {len(lookup)} MSOAs in the csv")



##========================================================
# plotting
##========================================================
aprx_path = os.path.join(root, "Crime and night tubes EXTRA DATA", "Maps", "TE_maps", "TE_maps.aprx")  # existing blank Pro project
aprx = arcpy.mp.ArcGISProject(aprx_path)

# remove any previous versions so the script can be rerun
for old in aprx.listMaps("TE map"):
    aprx.deleteItem(old)
for old in aprx.listLayouts("TE layout"):
    aprx.deleteItem(old)

# map and layer
m = aprx.createMap("TE map")
for lyr0 in m.listLayers():          # drop default basemap if one was added
    m.removeLayer(lyr0)

# --- TE layer (only MSOAs with estimates) ---
lyr = m.addDataFromPath(fc)
lyr.name = "Mean treatment effect"
lyr.definitionQuery = " OR ".join(f"{f} IS NOT NULL" for f in season_fields)

# --- outline layer: all London MSOAs, no fill, faint grey borders ---
london_boroughs = [
    "Barking and Dagenham", "Barnet", "Bexley", "Brent", "Bromley", "Camden",
    "City of London", "Croydon", "Ealing", "Enfield", "Greenwich", "Hackney",
    "Hammersmith and Fulham", "Haringey", "Harrow", "Havering", "Hillingdon",
    "Hounslow", "Islington", "Kensington and Chelsea", "Kingston upon Thames",
    "Lambeth", "Lewisham", "Merton", "Newham", "Redbridge", "Richmond upon Thames",
    "Southwark", "Sutton", "Tower Hamlets", "Waltham Forest", "Wandsworth", "Westminster",
]
london_query = " OR ".join(f"MSOA21NM LIKE '{b} %'" for b in london_boroughs)

lyr_out = m.addDataFromPath(fc)      # added last, so it draws on top
lyr_out.name = "MSOA boundaries"
lyr_out.definitionQuery = london_query

sym_out = lyr_out.symbology
sym_out.updateRenderer("SimpleRenderer")
sym_out.renderer.symbol.color = {"RGB": [0, 0, 0, 0]}             # transparent fill
sym_out.renderer.symbol.outlineColor = {"RGB": [120, 120, 120, 60]}  # grey, 60% opacity
sym_out.renderer.symbol.outlineWidth = 0.2
lyr_out.symbology = sym_out

# layout (A4 portrait)
lyt = aprx.createLayout(210, 297, "MILLIMETER", "TE layout")

# square map frame: 180 x 180 mm, leaving room for the title above and legend below
mf = lyt.createMapFrame(arcpy.Extent(15, 95, 195, 275), m, "Main map")
mf.camera.setExtent(mf.getLayerExtent(lyr_out, False, True))     # fit to all of London

# legend in a fixed box below the frame: x 15-195 mm, y 15-85 mm
legend = lyt.createMapSurroundElement(arcpy.Point(15, 15), "LEGEND", mf, name="Legend")
legend.fittingStrategy = "AdjustColumnsAndFont"   # wrap into columns / shrink text to fit the box
legend.setAnchor("BOTTOM_LEFT_CORNER")
legend.elementWidth = 180
legend.elementHeight = 70
legend.elementPositionX = 15
legend.elementPositionY = 15

for itm in legend.items:             # keep the outline layer out of the legend
    if itm.name == "MSOA boundaries":
        legend.removeItem(itm)

# title above the frame
title = aprx.createTextElement(lyt, arcpy.Point(105, 285), "POINT", "placeholder", 16)
title.setAnchor("CENTER_POINT")
title.elementPositionX = 105
title.elementPositionY = 285
# colour ramp (names vary by Pro version; check with [r.name for r in aprx.listColorRamps()])
ramps = aprx.listColorRamps("Yellow-Green-Blue (Continuous)")

# one PDF per season
out_dir = os.path.join(root, "Crime and night tubes", "Output", "Figures")
os.makedirs(out_dir, exist_ok=True)

for s in seasons:
    sym = lyr.symbology
    sym.updateRenderer("SimpleRenderer")            # force a full reset
    sym.updateRenderer("GraduatedColorsRenderer")
    sym.renderer.classificationField = f"tau_{s}"
    sym.renderer.colorRamp = ramps[0]
    sym.renderer.breakCount = n_bins

    for brk, lb, ub in zip(sym.renderer.classBreaks, lower_bounds, common_bounds):
        brk.upperBound = ub
        brk.label = f"{lb:.3f} to {ub:.3f}"
        brk.symbol.outlineWidth = 0                 # borders come from the outline layer
    lyr.symbology = sym

    title.text = f"Mean treatment effect by MSOA, {s} 2017"
    lyt.exportToPNG(os.path.join(out_dir, f"TE_{s}_2017.png"), resolution=300)
    print("Exported", s)




##############################################################################
##############################################################################
##############################################################################
##############################################################################


# now plot the difference in means for locations relative to the first six months of 2016
# no estimation required, and in levels so more interpretable

# import the full data
df = pd.read_csv(os.path.join(root, "Crime and night tubes EXTRA DATA", "final_data_for_stata.csv"))
 
# get a variable summing theft from the person and robbery
df['theft_robbery'] = df['theft_from_the_person'] + df['robbery']
 
# clean it to get the average count for each month in each MSOA
msoa_col = "MSOA21NM"
crime_col = "theft_robbery"
 
# get total crimes per MSOA per month (summing across locations)
monthly = df.groupby([msoa_col, "period"])[crime_col].sum()
 
# balance the panel: MSOA-months with no rows are treated as zero crimes
all_msoas = monthly.index.get_level_values(msoa_col).unique()
full_idx = pd.MultiIndex.from_product([all_msoas, range(1, 37)], names=[msoa_col, "period"])
monthly = monthly.reindex(full_idx, fill_value=0).reset_index()
 
# baseline: monthly average over periods 1-12
baseline = (monthly[monthly["period"].between(1, 12)]
            .groupby(msoa_col)[crime_col].mean())
 
# three-month windows and the season each corresponds to
windows = {"Winter": (25, 27), "Spring": (28, 30), "Summer": (31, 33), "Autumn": (34, 36)}
seasons = list(windows.keys())
 
wide = pd.DataFrame(index=baseline.index)
for s, (lo, hi) in windows.items():
    win_avg = (monthly[monthly["period"].between(lo, hi)]
               .groupby(msoa_col)[crime_col].mean())
    wide[s] = win_avg - baseline                 # aligned on MSOA
 
wide.index = wide.index.astype(str).str.strip()
lookup = wide.to_dict('index')      # {msoa: {'Winter': x, 'Spring': y, ...}}
 
# number of quantile bins (for plotting)
n_bins = 10
 
# common quantile edges across all seasons (n_bins + 1 edges)
edges = np.nanquantile(wide.to_numpy(), np.linspace(0, 1, n_bins + 1))
lower_bounds  = edges[:-1].tolist()
common_bounds = edges[1:].tolist()
 
##========================================================
# make a gdb, get the shapefile in and merge with crime differences
##========================================================
 
# make the gdb
out_gdb = os.path.join(root, "Crime and night tubes EXTRA DATA", "Geodatabases", "TE_maps.gdb")
if not arcpy.Exists(out_gdb):
    arcpy.management.CreateFileGDB(os.path.dirname(out_gdb), os.path.basename(out_gdb))
 
# load in the shapefile and get a field ready for the crime differences for each season
msoa_shp = os.path.join(root, "Crime and night tubes EXTRA DATA", "MSOA shapefile", "MSOA_2021_EW_BGC_V3.shp")
fc = os.path.join(out_gdb, "MSOA21_crime_diff")
arcpy.management.CopyFeatures(msoa_shp, fc)
 
# add one field per season
season_fields = [f"d_{s}" for s in seasons]
for f in season_fields:
    arcpy.management.AddField(fc, f, "DOUBLE")
 
# match the crime differences to the MSOA shapefile
n_matched = 0
with arcpy.da.UpdateCursor(fc, [msoa_col] + season_fields) as cur:
    for row in cur:
        vals = lookup.get(str(row[0]).strip())
        if vals is None:
            new = [None] * len(seasons)          # unmatched -> NULL
        else:
            n_matched += 1
            new = [None if pd.isna(vals[s]) else float(vals[s]) for s in seasons]
        cur.updateRow([row[0]] + new)
 
print(f"Matched {n_matched} of {len(lookup)} MSOAs in the csv")
 
 
 
##========================================================
# plotting
##========================================================
aprx_path = os.path.join(root, "Crime and night tubes EXTRA DATA", "Maps", "TE_maps", "TE_maps.aprx")  # existing blank Pro project
aprx = arcpy.mp.ArcGISProject(aprx_path)
 
# remove any previous versions so the script can be rerun
for old in aprx.listMaps("Crime diff map"):
    aprx.deleteItem(old)
for old in aprx.listLayouts("Crime diff layout"):
    aprx.deleteItem(old)
 
# map and layer
m = aprx.createMap("Crime diff map")
for lyr0 in m.listLayers():          # drop default basemap if one was added
    m.removeLayer(lyr0)
 
# --- crime difference layer (only MSOAs with values) ---
lyr = m.addDataFromPath(fc)
lyr.name = "Change in monthly theft/robbery"
lyr.definitionQuery = " OR ".join(f"{f} IS NOT NULL" for f in season_fields)
 
# --- outline layer: all London MSOAs, no fill, faint grey borders ---
london_boroughs = [
    "Barking and Dagenham", "Barnet", "Bexley", "Brent", "Bromley", "Camden",
    "City of London", "Croydon", "Ealing", "Enfield", "Greenwich", "Hackney",
    "Hammersmith and Fulham", "Haringey", "Harrow", "Havering", "Hillingdon",
    "Hounslow", "Islington", "Kensington and Chelsea", "Kingston upon Thames",
    "Lambeth", "Lewisham", "Merton", "Newham", "Redbridge", "Richmond upon Thames",
    "Southwark", "Sutton", "Tower Hamlets", "Waltham Forest", "Wandsworth", "Westminster",
]
london_query = " OR ".join(f"MSOA21NM LIKE '{b} %'" for b in london_boroughs)
 
lyr_out = m.addDataFromPath(fc)      # added last, so it draws on top
lyr_out.name = "MSOA boundaries"
lyr_out.definitionQuery = london_query
 
sym_out = lyr_out.symbology
sym_out.updateRenderer("SimpleRenderer")
sym_out.renderer.symbol.color = {"RGB": [0, 0, 0, 0]}             # transparent fill
sym_out.renderer.symbol.outlineColor = {"RGB": [120, 120, 120, 60]}  # grey, 60% opacity
sym_out.renderer.symbol.outlineWidth = 0.2
lyr_out.symbology = sym_out
 
# layout (A4 portrait)
lyt = aprx.createLayout(210, 297, "MILLIMETER", "Crime diff layout")
 
# square map frame: 180 x 180 mm, leaving room for the title above and legend below
mf = lyt.createMapFrame(arcpy.Extent(15, 95, 195, 275), m, "Main map")
mf.camera.setExtent(mf.getLayerExtent(lyr_out, False, True))     # fit to all of London
 
# legend in a fixed box below the frame: x 15-195 mm, y 15-85 mm
legend = lyt.createMapSurroundElement(arcpy.Point(15, 15), "LEGEND", mf, name="Legend")
legend.fittingStrategy = "AdjustColumnsAndFont"   # wrap into columns / shrink text to fit the box
legend.setAnchor("BOTTOM_LEFT_CORNER")
legend.elementWidth = 180
legend.elementHeight = 70
legend.elementPositionX = 15
legend.elementPositionY = 15
 
for itm in legend.items:             # keep the outline layer out of the legend
    if itm.name == "MSOA boundaries":
        legend.removeItem(itm)
 
# title above the frame
title = aprx.createTextElement(lyt, arcpy.Point(105, 285), "POINT", "placeholder", 16)
title.setAnchor("CENTER_POINT")
title.elementPositionX = 105
title.elementPositionY = 285
# colour ramp (names vary by Pro version; check with [r.name for r in aprx.listColorRamps()])
ramps = aprx.listColorRamps("Yellow-Green-Blue (Continuous)")
 
# one PNG per season
out_dir = os.path.join(root, "Crime and night tubes", "Output", "Figures")
os.makedirs(out_dir, exist_ok=True)
 
for s in seasons:
    sym = lyr.symbology
    sym.updateRenderer("SimpleRenderer")            # force a full reset
    sym.updateRenderer("GraduatedColorsRenderer")
    sym.renderer.classificationField = f"d_{s}"
    sym.renderer.colorRamp = ramps[0]
    sym.renderer.breakCount = n_bins
 
    for brk, lb, ub in zip(sym.renderer.classBreaks, lower_bounds, common_bounds):
        brk.upperBound = ub
        brk.label = f"{lb:.3f} to {ub:.3f}"
        brk.symbol.outlineWidth = 0                 # borders come from the outline layer
    lyr.symbology = sym
 
    title.text = f"Change in monthly theft/robbery by MSOA, {s} 2017"
    lyt.exportToPNG(os.path.join(out_dir, f"crime_diff_{s}_2017.png"), resolution=300)
    print("Exported", s)