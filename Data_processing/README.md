## Purpose  

This sub-directory is for scripts to move data through the process from CDMO download -> producing a single file for each station -> subsetting to the years, parameters, and stations we want to focus on -> creating a single file for each of WQ, NUT, MET.  


## Structure  

Because there's so much data, I'm going to add all sub-directories here to `.gitignore`. But the subfolders are:  

-  `downloaded` - files from the AQS zip download    
-  `compiled_by_stn` - .Rdata files for each individual station, containing all years, all parameters, and qa/qc flag columns. Empty data columns and their associated flag columns will be removed.      
-  `compiled_by_type` - .Rdata files for each of WQ, NUT, and MET. These files will only contain a subset of data: only stations that have both NUT and WQ data; only data after 2002.    

## Order to run scripts  

The first two scripts below only combine data together, and do not subset based on parameters or QA/QC flags and codes. Stations that do not have data for both WQ and NUT will be excluded from the 2nd step. These should only need to be run once for any given CDMO download.  

The third script *does* subset based on parameters and QA/QC codes, before aggregating by month, and should be re-run any time the parameters of the data synthesis project change (which will hopefully be infrequent).  

1.  `downloaded_to_compiledStns.R` - for each station, reads in and collates all files. For nutrients, only keeps grab samples (`collMethd = 1`). Removes empty columns. Generates one `.RData` file for each station.    
2.  `compiledStns_to_compiledTypes.R` - of all WQ and NUT stations, selects only those with both types of data. Then loads and combines all NUT stations into a single `NUT.RData` file, and loads and combines all WQ stations into a single `WQ.RData` file.  
3.  `compiledTypes_to_compiledByMonth.R`  