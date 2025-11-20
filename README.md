This is the repo containing the code and some of the data for a current project investigating the effect of London's night tubes on the crime patterns around the stations on lines where it opened.

---

The Code folder contains the following:
- file_deletion.R - this just takes the downloaded R files and deletes the irrelevant ones
- data_extraction_pre_geocoding.R - this cleans the crime data to a point where it is ready to be geocoded
- geocoding.py - this does the geocoding, via ArcGIS
- data_extraction_post_geocoding.R - this does further cleaning of the geocoded data, and merges it back into the original data to form the dataset for analysis
- data_analysis.R - this begins the analysis

---
I have not included the intermediate data here, because at points it was quite large. Please contact me at jacobmcloughlin5@gmail.com if you want it.
