## Purpose  

This sub-directory is for scripts to move data through the process from CDMO download -> producing a single file for each station -> subsetting to the years, parameters, and stations we want to focus on -> creating a single file for each of WQ, NUT, MET.  

## Structure  

Because there's so much data, I've added the `Data/` folder to `.gitignore`. In earlier versions of code, the subdirectories below were in this same `Data_processing` folder; but with the final download from the CDMO on 8/29/23, I have moved subdirectories to the main `Data` folder. Data processing scripts will remain in this `Data_processing` folder, and work on files in the `Data` folder. Those subfolders are:  

-  `downloaded` - files from the AQS zip download    
-  `compiled_by_stn` - .Rdata files for each individual station, containing all years, all parameters, and qa/qc flag columns. Empty data columns and their associated flag columns will be removed.      
-  `QAQCd_by_stn` - .Rdata files for each individual station. Only required SWMP parameters have been kept. If data points do not fall within this Synthesis project's 'keep' parameters (defined in the `helper_files/definitions.R` script), they have been replaced with `NA`. For nutrient columns, a column indicating whether each value has been censored has been added. WQ and MET files remain at their 15-minute timesteps. F_ columns (those with QA/QC flags and codes) have been removed; all data points present in these files are considered "acceptible" for this project's purposes.   

## Running the scripts  

**Note:** these data files were processed on a computer with 12 cores. The `foreach` and `doParallel` packages were used to employ 10 cores for parallel processing. If someone else runs these scripts, you may need to adjust the core number in the scripts (line 17 in `downloaded_to_compiledStns.R` and line 22 in `compiledStns_to_QAQCdStns.R`).  

### Order to run scripts  

The first script only combines data for a station, and does not subset based on parameters or QA/QC flags and codes. This should only need to be run once for any given CDMO download.  

The second subsets to only SWMP-required parameters and this project's acceptable QA/QC codes, and should be re-run any time the parameters of the data synthesis project change (which will hopefully be infrequent).   

1.  `downloaded_to_compiledStns.R` - for each station, reads in and collates all files. For nutrients, only keeps grab samples (`collMethd = 1`). Removes empty columns. Generates one `.RData` file for each station.    
2.  `compiledStns_to_QAQCdStns.R` -     

## Future scripts  

Scripts are still needed to aggregate data:  

-  daily for WQ and MET    
-  then to monthly for WQ and MET  
-  monthly for NUT (need to average replicate values)  
-  can probably put all monthly aggregations of a single type into a single file  