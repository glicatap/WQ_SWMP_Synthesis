# R/Analyses_for_paper Folder  

The structure of this folder mirrors that of the `Outputs` folder.  

## 01_medians  

Based on the monthly processed data files, overall medians were calculated for WQ, NUT, and MET parameters. For NUT parameters, regression on order statistics (ROS) was used (a function within `01_median_calculation.qmd` incorporates `NADA::cenros()` to accomplish this) to account for censored data. At HUD stations, NO3 was used as a proxy for NO23.  

Outputs were written to the `Outputs/01_calculated_medians` folder.  



## 02_long-term-trends  

`02_long-term_trend_analyses.Rmd` is the file that loops through every key parameter and applies Generalized Additive Modeling to account for seasonality, autocorrelation, and censoring and then calculate a linear long-term trend. See [this explanation of trend calculation in the Outputs README.](https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/tree/main/Outputs#explanation-of-trend-calculations)  

`02b_long-term-trend_summaries.qmd` makes histograms, a table, and a graph to summarize the distribution of trends for each key parameter.  

`02c_long-term-trend_NUTs_toPctPerYear.qmd` back-calculates nutrient trends, which were calculated on log(nut), to % change per year, and saves the results to a csv in the Outputs folder.  

Outputs were written to the `Outputs/02_calculated_long-term-trends` folder; see the [Outputs README](https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/blob/main/Outputs/README.md) for file details.  


## 03_seasonality  

`03_seasonalAmplitude_and_itsTrends.Rmd` calls on a custom function to find, for each parameter, the yearly min, max, and mean (of the monthly median values). The function calculates annual amplitude by subtracting min from max for each year. It then calculates normalized annualized amplitude by dividing annual amplitude by the mean. This follows Cloern and Jassby 2008: Complex seasonal patterns of primary producers at the land-sea interface. *Ecology Letters* 11: 1294-1303.  

Trends in these annual normalized amplitudes were calculated using simple linear regression.  

Outputs were written to the `Outputs/03_calculated_seasonality` folder.  


## 04_compiling_predictors  

These scripts combine station/parameter medians (from folder `01` outputs), long-term trends (from folder `02` outputs), seasonality metrics (from folder `03` outputs), and external parameters (from SWMP CLUE and CF/DP's clustering) into a single data frame to be used in modeling.  


Outputs were written to the `Outputs/04_compiled_predictors` folder.  


## 05_predictive_modeling  

This folder contains one `.qmd` file for each response variable. In these files, the global model for each response (using predictors agreed upon by the data analysis team) is generated and evaluated by checking fit, residuals, and influential points. For details on the process, see the Modeling README. **LINK**  

`050_setup.R` is a helper file that reads in the data and compiled predictors, performs PCA for the latitudinally-related variables (latitude, median DO mg/L, median PAR, median temperature), and attaches the scores from PC1 of that PCA to the predictor data frame. **If I do this, that file also constructs the data frames for each predictor model used in folders `05` and `06`.**


Outputs from the `.qmd` files were written by default to the folder containing the script; these have been copied to the `Outputs/05_predictive_modeling` folder.  


## 06_model_selection  

For details on model selection process decisions, see the Modeling README. **LINK**  

Files in this folder implement all-subsets selection from the global models evaluated in folder `05_predictive_modeling`, and then implement model averaging for top-model sets. Each response variable gets its own scripts, of which there are two: `06a` for dredging (using `MuMIn::dredge()`), and `06b` for model averaging.  The scripts have an abbreviation for the response variable in the names - e.g. the chlorophyll a trend model has all-subsets selection performed in `06a_chl_dredging.R`, then model averaging in `06b_chl_model-avgd_outputs.R`. DO mg/L trend is represented by 'domgl', and proportion of time DO < 2 mg/L trend is represented by 'doLT2' (LT = "less than").  

`060_predicting_functions.R` contains functions to generate the same tables and graphs for different sets of models and parameters. It is used more in folder `06b_model_interpretation` than in this folder, but was created here before splitting the scripts further.  


Outputs were written to the `Outputs/06_model_selection` folder, in the subfolder `R_objects`. These objects have not been pushed to github due to size, but are available on Box. **LINK**  


## 06b_model_interpretation  

This folder contains two `.qmd` files and their output: `Overall_Outputs.qmd` and `Overall_Outputs_supplementary.qmd`.  

`Overall_Outputs.qmd` contains outputs from model-averaging when delta AICc < 4, as agreed by the work group. It starts with graphs and tables summarizing standardized coefficients from averaged models for each response. Then, the top several predictors for each response (and the response itself) are back-transformed to either original units or %/year change (when a log-transformation has happened), and graphs of the key predictors and their expected response values are provided. This is to help interpret the effect of each predictor, in more understandable units than standardized coefficients provide.  

`Overall_Outputs_supplementary.qmd` contains graphs and tables summarizing standardized coefficients using deltas of 2 and 6 as thresholds, for comparison to what will be reported in the main paper ("how does choice of delta AICc threshold affect the results?"). Individual predictor plots were not made in this file.  


Outputs of the `.qmd` files are by default saved in the R directory where the file is. The latest versions have been copied to the `Outputs/06_model_selection` folder.  