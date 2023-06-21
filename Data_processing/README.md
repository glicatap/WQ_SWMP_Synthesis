## Purpose  

This sub-directory is for scripts to move data through the process from CDMO download -> producing a single file for each station -> subsetting to the years, parameters, and stations we want to focus on -> creating a single file for each of WQ, NUT, MET.  


## Structure  

Because there's so much data, I'm going to add all sub-directories here to `.gitignore`. But the subfolders are:  

-  `downloaded` - files from the AQS zip download    
-  `compiled_by_stn` - .Rdata files for each individual station, containing all years, all parameters, and qa/qc flag columns. Empty data columns and their associated flag columns will be removed.      
-  `compiled_by_type` - .Rdata files for each of WQ, NUT, and MET. These files will only contain a subset of data: only stations that have both NUT and WQ data; only data after 2002.    