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

## Compilation of details and additional READMEs

While working on code, I have generally tried to keep detailed READMEs in the folder of whatever I've been working on at the time. That's great for ease of updating information, but less great for helping other people find information. So here are some places to find additional information:

-   [Entire process of data wrangling](https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/tree/main/R/Data_processing#readme) - from download, through various steps of aggregation, to the final data files. This is the README for the R/Data_processing directory.  
-   QA/QC codes and which data points were kept vs. discarded  
    -   Overall: [google doc](https://docs.google.com/document/d/1v6HwTjdK_qAIYaV42jX1LKnWlwehssJhuoEq8JFGbRE/edit#bookmark=id.n47s9mcv87w2) with rationale / [Code](https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/blob/04779207950c5eef58d7fa2991627dafdf0f0805/helper_files/definitions.R#L57), definitions file  
    -   [Spreadsheet](https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/blob/main/helper_files/QAQC_FlagsCodes_NUT.csv) of nutrient flags/codes and decisions  
-   Final data files in Box:  
    -   [Downloaded files](https://uwmadison.box.com/s/4obwowqf52zah2hkmvdj4zv0wu64zfiu) - data and metadata files. Data files include one .csv per station per year. Metadata files are one per data type (WQ/MET/NUT) per year.   
    -   [Aggregated by month](https://uwmadison.box.com/s/7krz7h6zi4qcpdke0mmvyxppqfjvtz4e) and data type (WQ/MET/NUT)  
    -   Aggregated by day: [.RData format](https://uwmadison.box.com/s/6lzq2nxaf2uiyqwov96dn0fi9pcplk2l) / [.csv format](https://uwmadison.box.com/s/0xhk0fopoqsgrenj0nx4102e3f9ybzdx)  
-   [Explanation of long-term trend calculations](https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/tree/main/Outputs/calculated_trends#explanation-of-trend-calculations) (in /Outputs/calculated_trends README)  
    -   [Calculated trends file](https://drive.google.com/file/d/1tGZPO1oxCL1ngwuVikBeK7yolvMLlt4Q/view?usp=drive_link) - currently only in google drive; move to github!    
    -   [Data dictionary](https://drive.google.com/file/d/1HJ9gumgCBpwaTja39O8knyJ_-WMXBN3K/view?usp=drive_link) for csv that explains parameters, measurements, transformations, and model specifications - currently only in google drive; need to move this onto github!  
    -   [Data dictionary](https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/blob/main/Outputs/calculated_trends/data_dictionary_trend_analyses.csv) for csv that contains output of trend analyses  
-   [Compiled predictors](https://drive.google.com/file/d/1xJjptKdpDNnzZxfhz11vnIGLFvMaV8Xk/view?usp=drive_link) csv - includes long-term trend outputs from above, as well as seasonal amplitudes and overall medians. Currently only in google drive; move this to github at some point.  
