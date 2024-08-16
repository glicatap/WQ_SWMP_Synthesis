---
output:
  pdf_document: default
  html_document: default
---
## Purpose  

This sub-directory is for scripts to move data through the process from CDMO download -> producing a single file for each station -> subsetting to the years, parameters, and stations we want to focus on -> creating a single file for each of WQ, NUT, MET.  

## Structure  

Data processing scripts are in this `R/Data_processing` folder, and work on files in the `Data` folder. Subfolders of the `Data` folder are:  

-  `downloaded` - files from the AQS zip download    
-  `compiled_by_stn` - .Rdata files for each individual station, containing all years, all parameters, and qa/qc flag columns. Empty data columns and their associated flag columns will be removed. NUT files contain only grab samples (not diel). Data frames are named as the station only; e.g. `gndbhwq`.      
-  `QAQCd_by_stn` - .Rdata files for each individual station. Only required SWMP parameters have been kept. If data points do not fall within this Synthesis project's 'keep' parameters (defined in the `helper_files/definitions.R` script), they have been replaced with `NA`. Some older files contained `-99` rather than `NA`; these values have been replaced with `NA`. For nutrient columns, a column indicating whether each value has been censored has been added (0 if not censored; 1 if censored, e.g. below the MDL). WQ and MET files remain at their 15- or 30-minute timesteps. F_ columns (those with QA/QC flags and codes) have been removed; all data points present in these files are considered "acceptable" for this project's purposes. Data frames are named as the station, followed by "_qc"; e.g. `gndbhwq_qc`.  
-  `QAQCd_monthly` - aggregation of WQ, MET, and NUT data to monthly values, originating from the 15- or 30-minute data (for WQ/MET) and monthly replicate grab samples (NUT). This folder contains .RData files. Data frames are named as the station, followed by "_monthly"; e.g. `gndbhwq_monthly`. Each row contains a single year and month, and each column is some sort of aggregation of a parameter.    
    -  for WQ and MET: nValid for how many data points that month passed qa/qc; and min, median, max, mean, sd, and iqr for all of the parameters that should be processed that way. For precipitation, columns are nValid and a monthly total. For PAR, values were first summed at the daily level; then nValid, min, median, max, mean, sd, and iqr were calculated on the daily totals.  
    -  Monthly aggregated stats were only calculated if a parameter had at least one week's worth of valid (non-NA) data points during that month. For typical 15-minute data, the cutoff was 672 points. MET data has always been collected at 15-minute intervals. WQ data was collected at 30-minute intervals at many stations prior to 2007. When this was the case, the cutoff for calculation of monthly stats was 336 points. The script `proc03_QAQCdStns_to_QCQCdMonthly.R` contains code determining whether time values of xx:15 and xx:45 were present within a given month; if so, data were at 15-minute intervals and the cutoff was set to 672. Otherwise the data were collected at 30-minute intervals and the cutoff was 336. nValid is present for every parameter, but values of min/median/max/etc. when nValid is below these cutoffs were turned into `NA`.    
    -  In the WQ files, I've tabulated how many DO_mgl values were <2 and <5. Those both come with 'nValid' and 'total' (so the proportion for how many readings in a month were < 2 mg/L could be calculated as `doLessThan2_total/doLessThan2_nValid`).  
    -  for NUT: Valid monthly replicate values were averaged. For the censored column, if at least one valid replicate was identified as censored, the average was also identified as censored.  
-  `QAQCd_monthly_byType` - compilation of all stations for each data type. Contains 6 files: one `.RData` and one `.csv` for each data type: `SWMP_WQ`, `SWMP_MET`, `SWMP_NUT`. Once quality-checked and any questions/issues are addressed, these will be the final monthly files used in SWMP Synthesis data analysis. **Note**: these files contain all stations that have data - inactive stations or those with short duration will need to be removed before analysis, if appropriate. Additionally, the number of rows may change as we ensure all year-month combinations are represented for each station (e.g. we may need to insert blank rows for months with no data).    
    -  WQ file contains 27,605 rows from 154 stations  
    -  MET file contains 6,909 rows from 37 stations  
    -  NUT file contains 28,693 rows from 156 stations  
-  `QAQCd_daily` - aggregation of WQ and MET data to daily values. This folder contains .RData files. Data frames are named as the station, followed by "_daily"; e.g. `gndbhwq_daily`. 
    -  Each row is a date, and each column is some sort of aggregation of a parameter, in the same way aggregation was performed for monthly values: nValid for how many data points that day passed qa/qc; and min, median, max, mean, sd, and iqr for all of the parameters that should be processed that way. For precipitation and PAR, there's just nValid and total, which sums up the values for the day. There was no minimum number of valid points within a day for aggregations to be performed.    
    -  In the WQ files, I've tabulated how many DO_mgl values were <2 and <5. Those both come with 'nValid' and 'total' (so the proportion for how many readings in a day were < 2 mg/L could be calculated as `doLessThan2_total/doLessThan2_nValid`).  
-  `QAQCd_daily_csvs` - the .RData files from the step above, with a column added for station, and saved out as .csv files. These are easier to share than .RData files but are also larger.  

## Running the scripts  

Scripts are in the `R/Data_processing` folder of the `WQ_SWMP_Synthesis` directory. Scripts should be run within the `WQ_SWMP_Synthesis.Rproj` project file. The `here` package is used to make file paths relative to the root directory.  

Each script should be run in a clean R session (e.g., if you were already using R via RStudio, go to the 'Session' menu and select 'Restart R'; or use your favorite keyboard shortcut).

**Note:** these data files were processed on a computer with 12 cores. The `foreach` and `doParallel` packages were used to employ parallel processing. These scripts are set to detect the number of cores on a user's computer, and use that number minus 2 (leaving 2 for other processes) for parallel computing.  

### Necessary packages  

These scripts employ several packages that may need to be installed. Versions used are included in parentheses. Generally none of them use the latest updates except for `SWMPr`.   

-  for data compilation: `SWMPr` (2.4.3.9000), `purrr` (1.0.1)    
-  for data wrangling and selection: `dplyr` (1.1.0), `tidyr` (1.3.0), `stringr` (1.5.0), `lubridate` (1.9.0), `janitor` (2.1.0)    
-  for overall running: `here` (1.0.1), `foreach` (1.5.2), `doParallel` (1.0.17), `beepr` (1.3)  

All packages are available on CRAN and their latest versions can be installed with the following command:  

```{r}
install.packages(c("SWMPr", "purrr", "dplyr", "tidyr", "stringr", "lubridate", "janitor", "here", "foreach", "doParallel", "beepr"))
```

### Order to run scripts  

The first script only combines data for a station, and does not subset based on parameters or QA/QC flags and codes. This should only need to be run once for any given CDMO download.  

The second subsets to only SWMP-required parameters and this project's acceptable QA/QC codes, and should be re-run any time the parameters of the data synthesis project change (which will hopefully be infrequent).   

1.  `proc01_downloaded_to_compiledStns.R` - for each station, reads in and collates all files. For nutrients, only keeps grab samples (`collMethd = 1`). Removes empty columns. Generates one `.RData` file for each station.    
2.  `proc02_compiledStns_to_QAQCdStns.R` - for each station, reads in the file and replaces data flagged/coded in ways the workgroup has agreed to discard with NAs. For nutrients, inserts a column for each parameter defining whether the data point is left-censored. Generates one `.RData` file for each station.  
3.  `proc02b_corrections_stnNUTs.R` - for certain NUT stations, corrects issues related to NO23 calculation and/or 0s in data. Pulls the `.RData` files from the above step, re-saves them as `aaabbnut_qcUncorrected.RData`, corrects the problems, and saves the corrected data frame with the original `aaabbnut_qc.RData` name to be used in further compilation scripts below.  
4.  `proc03_QAQCdStns_to_QAQCdMonthly.R` - for WQ, MET, and NUT stations, aggregates to monthly values as described above.   
5.  `proc04_QAQCdMonthly_byType.R` - Using monthly aggregated files from step 5, combines all monthly information for each type (WQ, MET, NUT) into a single file, containing all stations. Writes both `.RData` and `.csv` files for each.  
6.  `proc05_QAQCdStns_to_QAQCdDaily.R` - optional; for WQ and MET stations, aggregates to daily values as described above.  
7.  `proc06_QAQCdDaily_to_csv.R` - generate `.csv` files from the `.RData` files above.  

To write out session info from the day of processing, open an R session and run the following code. It will open all libraries used and capture the session info in a text file, in the `Data_processing` folder. The following uses the `devtools` package, which can be installed with the command `install.packages("devtools")`.     

```{r}
library(SWMPr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(foreach)
library(doParallel)

flnm <- paste0("session_info_", Sys.Date(), ".txt")
file_out <- here::here("R", "Data_processing", flnm)
capture.output(devtools::session_info(), file = file_out)
```

## Most recent runs of data processing  

Final data files were downloaded from the CDMO on 8/30/2023.  

1.  `proc01_downloaded_to_compiledStns.R` - 9/18/2023. Originally run on 8/30/2023 but re-run to ensure I hadn't only selected active stations, or wq + nut stations. At this point, we mean to include all stations.      
2.  `proc02_compiledStns_to_QAQCdStns.R` - 10/16/2023; updated to remove -99 values  
2b.  `proc02b_corrections_stnNUTs.R` - 5/8/2024 & 5/10/2024; correcting NO23 calculated/censored values at OWC; replacing 0s or negative values with MDLs at several reserves. NUT compilation scripts below were re-run after this was completed.  
3.  `proc03_QAQCdStns_to_QAQCdMonthly.R` -  5/10/2024 NUTS only, after making corrections in 2b. 10/16/2023 WQ/MET; after removing -99s AND requiring at least 1 week's worth of valid data in a month before calculating stats   
4.  `proc04_QAQCdMonthly_to_MonthlyByType.R` - 5/10/2024 NUTs; 10/16/2023 WQ/MET.  
5.  `proc05_QAQCdStns_to_QAQCdDaily.R` - 10/17/2023; after removal of -99s  
6.  `proc06_QAQCdDaily_to_csv.R` - 10/17/2023  
