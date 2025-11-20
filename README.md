This is the repo containing the code and some of the data for a current project investigating the effect of London's night tubes on the crime patterns around the stations on lines where it opened.

---

The Code folder contains the following:
- file_deletion.R - this just takes the downloaded R files and deletes the irrelevant ones
- data_extraction_pre_geocoding.R - this cleans the crime data to a point where it is ready to be geocoded
- geocoding.py - this does the geocoding, via ArcGIS
- data_extraction_post_geocoding.R - this does further cleaning of the geocoded data, and merges it back into the original data to form the dataset for analysis
- data_analysis.R - this begins the analysis

---

The Data folder contains the input data, as downloaded, with the redundant files removed. I have not included the intermediate data here, because at points it was quite large. Please contact me at jacobmcloughlin5@gmail.com if you want it.

--- 

The repo also includes:
- a document on the sources of my input data, data_sources.docx
- an outline of what I've done so far, brief_outline.pdf
- my to-do list
