## Purpose  

This sub-directory is for scripts to move data through the process from CDMO download -> producing a single file for each station -> subsetting to the years, parameters, and stations we want to focus on -> creating a single file for each of WQ, NUT, MET.  

## Structure  

Because there's so much data, I've added the `Data/` folder to `.gitignore`. In earlier versions of code, the subdirectories below were in this same `Data_processing` folder; but with the final download from the CDMO on 8/30/23 (request submitted to AQS on 8/29/23; zip downloaded the next morning), I have moved subdirectories to the main `Data` folder. Data processing scripts will remain in this `Data_processing` folder, and work on files in the `Data` folder. Those subfolders are:  

-  `downloaded` - files from the AQS zip download    
-  `compiled_by_stn` - .Rdata files for each individual station, containing all years, all parameters, and qa/qc flag columns. Empty data columns and their associated flag columns will be removed. Data frames are named as the station only; e.g. `gndbhwq`.      
-  `QAQCd_by_stn` - .Rdata files for each individual station. Only required SWMP parameters have been kept. If data points do not fall within this Synthesis project's 'keep' parameters (defined in the `helper_files/definitions.R` script), they have been replaced with `NA`. For nutrient columns, a column indicating whether each value has been censored has been added (0 if not censored; 1 if censored, e.g. below the MDL). WQ and MET files remain at their 15-minute timesteps. F_ columns (those with QA/QC flags and codes) have been removed; all data points present in these files are considered "acceptable" for this project's purposes. Data frames are named as the station, followed by "_qc"; e.g. `gndbhwq_qc`.  
-  `QAQCd_daily` - aggregation of WQ and MET data to daily values. This folder contains .RData files. Data frames are named as the station, followed by "_daily"; e.g. `gndbhwq_daily`. 
    -  Each row is a date, and each column is some sort of aggregation of a parameter: nValid for how many data points that day passed qa/qc; and min, median, max, mean, sd, and iqr for all of the parameters that should be processed that way. For things like precipitation and PAR, there's just nValid and total, which sums up the values for the day.  
    -  In the WQ files, I've tabulated how many DO_mgl values were <2 and <5. Those both come with 'nValid' and 'total' (so the proportion for how many readings in a day were < 2 mg/L could be calculated as `doLessThan2_total/doLessThan2_nValid`).  
-  `QAQCd_daily_csvs` - the .RData files from the step above, with a column added for station, and saved out as .csv files. These are easier to share than .RData files but are also larger.  
-  `QAQCd_monthly` - aggregation of WQ, MET, and NUT data to monthly values, originating from the 15-minute data (for WQ/MET) and monthly replicate grab samples (NUT). This folder contains .RData files. Data frames are named as the station, followed by "_monthly"; e.g. `gndbhwq_monthly`. Each row contains a single year and month, and each column is some sort of aggregation of a parameter.  
    -  for WQ and MET: nValid for how many data points that month passed qa/qc; and min, median, max, mean, sd, and iqr for all of the parameters that should be processed that way. For precipitation, columns are nValid and a monthly total. For PAR, values were first summed at the daily level; then nValid, min, median, max, mean, sd, and iqr were calculated on the daily totals.  
    -  In the WQ files, I've tabulated how many DO_mgl values were <2 and <5. Those both come with 'nValid' and 'total' (so the proportion for how many readings in a month were < 2 mg/L could be calculated as `doLessThan2_total/doLessThan2_nValid`).  
    -  for NUT: Valid monthly replicate values were averaged. For the censored column, if at least one valid replicate was identified as censored, the average was also identified as censored.  

## Running the scripts  

**Note:** these data files were processed on a computer with 12 cores. The `foreach` and `doParallel` packages were used to employ 10 cores for parallel processing. If someone else runs these scripts, you may need to adjust the core number in the scripts (line 17 in `downloaded_to_compiledStns.R` and line 22 in `compiledStns_to_QAQCdStns.R`).  

### Order to run scripts  

The first script only combines data for a station, and does not subset based on parameters or QA/QC flags and codes. This should only need to be run once for any given CDMO download.  

The second subsets to only SWMP-required parameters and this project's acceptable QA/QC codes, and should be re-run any time the parameters of the data synthesis project change (which will hopefully be infrequent).   

1.  `downloaded_to_compiledStns.R` - for each station, reads in and collates all files. For nutrients, only keeps grab samples (`collMethd = 1`). Removes empty columns. Generates one `.RData` file for each station.    
2.  `compiledStns_to_QAQCdStns.R` - for each station, reads in the file and replaces data flagged/coded in ways the workgroup has agreed to discard with NAs. For nutrients, inserts a column for each parameter defining whether the data point is left-censored. Generates one `.RData` file for each station.  
3.  `QAQCdStns_to_QAQCdDaily.R` - for WQ and MET stations, aggregates to daily values as described above.  
4.  `QAQCdDaily_to_csv.R` - generate `.csv` files from the `.RData` files above.  
5.  `QAQCdStns_to_QAQCdMonthly.R` - for WQ, MET, and NUT stations, aggregates to monthly values as described above. This script could be run anytime after step 2 above (`compiledStns_to_QAQCdStns.R`).  
6.  `QAQCdMonthly_to_MonthlyByType.R` - not written or run yet! Will use monthly files from step 5, and combine all monthly information for each type into a single file. Will result in 3 final files: one each for WQ, MET, and NUT.  

To write out session info from the day of processing, open an R session and run the following code. It will open all libraries used and capture the session info in a text file, in the `Data_processing` folder.    

```{r}
library(SWMPr)
library(dplyr)
library(stringr)
library(lubridate)
library(foreach)
library(doParallel)

flnm <- paste0("session_info_", Sys.Date(), ".txt")
file_out <- here::here("Data_processing", flnm)
capture.output(devtools::session_info(), file = file_out)
```

## Most recent runs of data processing  

Final data files were downloaded from the CDMO on 8/30/2023.  

1.  `downloaded_to_compiledStns.R` - 9/18/2023. Originally run on 8/30/2023 but re-run to ensure I hadn't only selected active stations, or wq + nut stations. At this point, we mean to include all stations.      
2.  `compiledStns_to_QAQCdStns.R` - 9/18/2023  
3.  `QAQCdStns_to_QAQCdDaily.R` - 9/18/2023  
4.  `QAQCdDaily_to_csv.R` - 9/18/2023  
5.  `QAQCdStns_to_QAQCdMonthly.R` -  9/18/2023   
6.  `QAQCdMonthly_to_MonthlyByType.R` - not written or run yet! 


## Future scripts  

-  can probably put all monthly aggregations of a single type into a single file: 20 yrs x 12 months/yr x 1 row/month x 150 stations = 36,000 rows - generally comparable to a single year's worth of 15-minute data.   
