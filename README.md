# SWMP Water Quality Synthesis

## About the project

To be filled in later

The final dataset used in this project was downloaded from the CDMO through the [Advanced Query System, Zip download option](https://cdmo.baruch.sc.edu/aqs/zips.cfm "link to AQS zip download page") on 8/29/2023. 'All Reserves and Stations' was selected. The date range selected was 2002 - 2022. The downloaded zip file was 1.6 GB.

# Repository Organization  

Sub-folders of this repository were created to provide some organization of the different coding needs for the project. Most sub-folders have a README with more detail. Current sub-folders are:

-   `Data` - meant to contain data, as processed by scripts in `R/Data_processing`. The full dataset is not committed to github due to its size but is available on Box - see below for links. Described well by the [Data Processing readme](https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/tree/main/R/Data_processing#readme).   
-   `helper_files` - scripts of functions and definitions (e.g. which parameters and flags/codes to use) that are needed across sub-folders. Also contains some csv files with helpful information on sampling stations (from the CDMO download) and QAQC flags and codes.  
-   `Outputs` - files generated from analyses and visualization code. This includes calculated trends, predictive model outputs, and visualizations. [Outputs readme](https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/tree/main/Outputs#readme)    
-   `R` - code! Several subfolders. The first two below are the most important from a reproducibility standpoint - other folders were used in early explorations, but anything going into the publication should be reproducible from the `Data_processing` and/or `Analyses_for_paper` folders.    
    -   **`Analyses_for_paper`** - main folder for calculations and modeling. Linked closely to the `Outputs` folder. [Analyses_for_paper README](https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/tree/main/R/Analyses_for_paper#readme)  
    -   **`Data_processing`** - scripts that were used to process downloaded data by combining files from a station, removing data points that did not pass various QA/QC criteria, and aggregating to daily and monthly levels. [Data Processing readme](https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/tree/main/R/Data_processing#readme)    
    -   `Long-Term_trend_analysis` - originally the subdirectory for trend scripts and tests.  
    -   `initial_explorations` - scripts that were written to visualize and explore the data before final download and processing occurred. There are additional sub-folders here for different topics, described in a separate README within this directory.


# Compilation of details and additional READMEs

While working on code, I have generally tried to keep detailed READMEs in the folder of whatever I've been working on at the time. That's great for ease of updating information, but less great for helping other people find information. So here are some places to find additional information:

-   [Entire process of data wrangling](https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/tree/main/R/Data_processing#readme) - from download, through various steps of aggregation, to the final data files. This is the README for the R/Data_processing directory.  
-   QA/QC codes and which data points were kept vs. discarded  
    -   Overall: [google doc](https://docs.google.com/document/d/1v6HwTjdK_qAIYaV42jX1LKnWlwehssJhuoEq8JFGbRE/edit#bookmark=id.n47s9mcv87w2) with rationale / [Code](https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/blob/04779207950c5eef58d7fa2991627dafdf0f0805/helper_files/definitions.R#L57), definitions file  
    -   [Spreadsheet](https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/blob/main/helper_files/QAQC_FlagsCodes_NUT.csv) of nutrient flags/codes and decisions  
-   Final data files in Box:  
    -   [Downloaded files](https://uwmadison.box.com/s/4obwowqf52zah2hkmvdj4zv0wu64zfiu) - data and metadata files. Data files include one .csv per station per year. Metadata files are one per data type (WQ/MET/NUT) per year.   
    -   [QAQCd files by station](https://uwmadison.box.com/s/aebc6s72u5q5y9bcgbtn74qknj78j2s5) - data files; one .RData file per station, with all years combined and only approved data points retained.  
    -   [Aggregated by month](https://uwmadison.box.com/s/7krz7h6zi4qcpdke0mmvyxppqfjvtz4e) and data type (WQ/MET/NUT); only approved data points retained.    
    -   Aggregated by day (WQ/MET only): [.RData format](https://uwmadison.box.com/s/6lzq2nxaf2uiyqwov96dn0fi9pcplk2l) / [.csv format](https://uwmadison.box.com/s/0xhk0fopoqsgrenj0nx4102e3f9ybzdx)  
-   [Explanation of long-term trend calculations](https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/tree/main/R/Analyses_for_paper#long-term-trend-calculations) (in the `R/Analyses_for_paper` readme)  
    -   [Calculated trends file](https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/blob/main/Outputs/02_calculated_long-term-trends/long-term-trends.csv) - in Outputs/02_calculated_long-term-trends folder      
    -   [Data dictionary](https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/blob/main/Outputs/02_calculated_long-term-trends/data_dictionary_trend_parameters.csv) for csv that explains parameters, measurements, transformations, and model specifications    
    -   [Data dictionary](https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/blob/main/Outputs/02_calculated_long-term-trends/data_dictionary_trend_parameters.csv) for csv that contains output of trend analyses  
-   [Explanation of modeling, model selection, and model averaging](https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/tree/main/R/Analyses_for_paper#predictive-modeling-model-selection-model-averaging) (in the `R/Analyses_for_paper` readme)    
-   Overall medians: [WQ/NUT stations](https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/blob/main/Outputs/01_calculated_medians/WQ-NUT_overallMedians.csv); [MET stations](https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/blob/main/Outputs/01_calculated_medians/MET_overallMedians.csv). Nutrient medians were calculated using robust regression on order statistics (ROS) in order to account for censored data, via the `NADA` R package.    
-   Seasonality:  [Median amplitudes](https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/blob/main/Outputs/03_calculated_seasonality/seasonal_amplitude_medians.csv); [Trends in median amplitudes](https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/blob/main/Outputs/03_calculated_seasonality/seasonal_amplitude_trends.csv)  
-   [Compiled predictors](https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/blob/main/Outputs/04_compiled_predictors/compiled_predictors.csv) csv - includes long-term trend outputs from above, as well as medians. When other predictor values are obtained, they will be added.  


# Statistical Methods Summary  

If I were writing this up for a manuscript, this would be my starting point. For a much more detailed write-up, see the [`Statistical Methods - Detailed`](https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/tree/main/R/Analyses_for_paper#statistical-methods-detailed) portion of the `R/Analyses_for_paper` readme.      


General data formatting was performed and graphics were made using the `tidyverse` meta-package for R (Wickham et al. 2019).

**Long-term trend calculations**  

Long-term trends in the monthly median values of parameters were calculated as the linear effect of time after accounting for seasonality as a cyclical spline using Generalized Additive Models (GAMs), via the `bam()` function of the `mgcv` package (v. 1.8.41; Wood 2017). The `bam()` function was also used to adjust for autocorrelation in residuals, when present. For WQ and MET parameters, we used the default `family = gaussian()`. The exception for WQ parameters was proportion of time DO was < 2 mg/L; because this was a proportion, `family = betar()` was used. For NUT parameters, values were first log-transformed, and a matrix was created with an addition column indicating whether a point was censored. This response matrix was used with `family = cnorm()` to perform regression using censored data techniques (see, e.g., Helsel 2011). See **Table ?? [I recommend turning 'data_dictionary_trend_parameters into a table, even if only supplementary]** for details on each parameter, its units, any transformations, and the distribution used.   


**Assessing predictors of eutrophication trends**

To assess how well environmental conditions and changes in environmental conditions explain or predict changes in our three key response variables, chlorophyll *a* and dissolved oxygen as DO mg/L and as proportion of time DO < 2 mg/L ('doLT2'), we used an information theoretic model selection framework (Burnham and Anderson 2002). We carefully chose predictors for each of the three responses; see **Table ??** for predictors used in each model. No interactions were included. All variables were centered and standardized to 1 standard deviation before model-fitting, to help with model convergence in mixed models, to enable comparison between standardized coefficients in final models, and to ensure appropriate model averaging (Harrison et al. 2018, Grueber et al. 2011, Symonds and Moussalli 2011). When appropriate for interpreting results, coefficients were back-calculated to either their original units or to percent-per-year (when log transformations had been performed).  

Global models were constructed with the predictors and a random effect for Reserve and fit using the `lme()` function of the R `nlme` package (v. 3.1.160; Pinherio et al. 2022) using Restricted Maximum Likelihood (REML). To determine whether the random effect was necessary, a simple linear model without the random effect was constructed via `nlme::gls()`, also using REML, to enable comparison via AIC (Zuur et al. 2009, section 5.7). When the random effect was required (chla models), the global model was then re-fit with Maximum Likelihood (ML) for subsequent model selection; when random effects are present but fixed effects vary, REML does not generate comparable AIC values (Zuur et al. 2009). When the random effect was not required (DO models), global models were re-fit as simple linear models, using `stats::lm()` (R Core Team 2022).

All model selection, averaging, and variable importance calculations were performed using the R `MuMIn` package (v. 1.47.5; Barton 2023). All-subsets selection was performed using the `dredge()` function. Due to varying recommendations on AIC thresholds for top model sets (e.g. Bolker et al. 2009, Burnham and Anderson 2002, Richards 2008) and based on the number of models that would be included using different thresholds (Grueber et al. 2011), we generated top model sets for each response based on delta AICc for thresholds of 2, 4, and 6. Results using delta < 4 are presented here; results for deltas 2 and 6 are provided in supplementary materials. Final standardized coefficients were generated by averaging the top model sets with `model.avg()`, using the full-model method, which is appropriate for comparing relative effect sizes when there is high model uncertainty (Symonds and Moussalli 2011). Variable importance was assessed partly by comparing standardized coefficients but also by using metrics calculated by the `sw()` function. The sum of Akaike weights of models in which a predictor appears can be interpreted as the probability that the predictor is in the “best” model (Grueber et al. 2011, Symonds and Moussalli 2011). The function also shows how many models from the top set a predictor appeared in. Predictors that are in many models and/or highly weighted models will have higher weights than those in few and/or low-weighted models.  