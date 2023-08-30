# SWMP Water Quality Synthesis

## About the project

To be filled in later

The final dataset used in this project was downloaded from the CDMO through the [Advanced Query System, Zip download option](https://cdmo.baruch.sc.edu/aqs/zips.cfm "link to AQS zip download page") on 8/29/2023. 'All Reserves and Stations' was selected. The date range selected was 2002 - 2022. The downloaded zip file was 1.6 GB.

## Repository Organization

Sub-folders of this repository were created to provide some organization of the different coding needs for the project. Current sub-folders are:

-   `Data` - meant to contain data, although the full dataset is not committed due to its size.\
-   `Data_processing` - scripts that start with downloaded data, combine all years for each station, perform QA/QC, and provide various levels of aggregation.\
-   `helper_files` - scripts of functions and definitions (e.g. which parameters and flags/codes to use) that are needed across sub-folders. Also contains some csv files with helpful information on sampling stations (from the CDMO dowload) and QAQC flags and codes.\
-   `initial_explorations` - lots of scripts to visualize and explore the data before final processing and reporting. There are additional sub-folders here for different topics. Initially these folders were *not* sub-folders of anything else, so data paths will most likely need to be updated if they are to be re-run. Additionally, these explorations were performed on data files *before* the final CDMO download. Some data files have been updated since this occurred.
    -   `MDLs` - explorations of nutrient data that may be below detection.\
    -   `model_packages` - explorations of different R packages that can be used to calculate trends on SWMP data - how each of them works, and how results compare between them.\
    -   `other` - assorted scripts for individuals' exploration that don't fall under other needed categories.\
    -   `QAQC_flags` - explorations of data by QA/QC flag status, based on definitions of which flags and codes will lead to data being kept or discarded for this project.
