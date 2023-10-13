# SWMP Water Quality Synthesis

## About the project

To be filled in later

The final dataset used in this project was downloaded from the CDMO through the [Advanced Query System, Zip download option](https://cdmo.baruch.sc.edu/aqs/zips.cfm "link to AQS zip download page") on 8/29/2023. 'All Reserves and Stations' was selected. The date range selected was 2002 - 2022. The downloaded zip file was 1.6 GB.

## Repository Organization

Sub-folders of this repository were created to provide some organization of the different coding needs for the project. Current sub-folders are:

-   `Data` - meant to contain data, although the full dataset is not committed due to its size.\
-   `helper_files` - scripts of functions and definitions (e.g. which parameters and flags/codes to use) that are needed across sub-folders. Also contains some csv files with helpful information on sampling stations (from the CDMO dowload) and QAQC flags and codes.\
-   `Outputs` - where files generated from analyses and visualization code will belong.\
-   `R` - code! Several subfolders:
    -   `Data_processing` - scripts that were used to process downloaded data by combining files from a station, removing data points that did not pass various QA/QC criteria, and aggregating to daily and monthly levels.\
    -   `Long-Term_trend_analysis` - will be the subdirectory for trend scripts and tests.\
    -   `initial_explorations` - scripts that were written to visualize and explore the data before final download and processing occurred. There are additional sub-folders here for different topics, described in a separate README within this directory.
