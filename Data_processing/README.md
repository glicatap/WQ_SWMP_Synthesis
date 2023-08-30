## Purpose  

This sub-directory is for scripts to move data through the process from CDMO download -> producing a single file for each station -> subsetting to the years, parameters, and stations we want to focus on -> creating a single file for each of WQ, NUT, MET.  


## Structure  

Because there's so much data, I've added the `Data/` folder to `.gitignore`. In earlier versions of code, the subdirectories below were in this same `Data_processing` folder; but with the final download from the CDMO on 8/29/23, I have moved subdirectories to the main `Data` folder. Data processing scripts will remain in this `Data_processing` folder, and work on files in the `Data` folder. Those subfolders are:  

-  `downloaded` - files from the AQS zip download    
-  `compiled_by_stn` - .Rdata files for each individual station, containing all years, all parameters, and qa/qc flag columns. Empty data columns and their associated flag columns will be removed.      
-  `QAQCd_by_stn` - .Rdata files for each individual station *that exists for both WQ and NUT data types* (note this is a subset of all stations). Only required SWMP parameters have been kept. If data points do not fall within this Synthesis project's 'keep' parameters (defined in the `helper_files/definitions.R` script), they have been replaced with `NA`. For nutrient columns, a column indicating whether each value has been censored has been added. WQ and MET files remain at their 15-minute timesteps. F_ columns (those with QA/QC flags and codes) have been removed.   

## Order to run scripts  

The first script only combines data for a station, and does not subset based on parameters or QA/QC flags and codes. This should only need to be run once for any given CDMO download.  

The second script *does* subset based on parameters and QA/QC codes, and should be re-run any time the parameters of the data synthesis project change (which will hopefully be infrequent).   

1.  `downloaded_to_compiledStns.R` - for each station, reads in and collates all files. For nutrients, only keeps grab samples (`collMethd = 1`). Removes empty columns. Generates one `.RData` file for each station.    
2.  `compiledStns_to_QAQCdStns.R` - of all WQ and NUT stations, selects only those with both types of data.  

## Future scripts  

Scripts are still needed to aggregate data:  

-  daily for WQ and MET    
-  then to monthly for WQ and MET  
-  monthly for NUT (need to average replicate values)  
-  can probably put all monthly aggregations of a single type into a single file  