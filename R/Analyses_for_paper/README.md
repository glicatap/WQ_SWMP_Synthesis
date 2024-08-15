# R/Analyses_for_paper Folder

The structure of this folder mirrors that of the `Outputs` folder. Because this folder contains the code generating all calculations and analyses, further detail on statistical methods is below the explanation of this folder's contents.

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

Files in this folder implement all-subsets selection from the global models evaluated in folder `05_predictive_modeling`, and then implement model averaging for top-model sets. Each response variable gets its own scripts, of which there are two: `06a` for dredging (using `MuMIn::dredge()`), and `06b` for model averaging. The scripts have an abbreviation for the response variable in the names - e.g. the chlorophyll a trend model has all-subsets selection performed in `06a_chl_dredging.R`, then model averaging in `06b_chl_model-avgd_outputs.R`. DO mg/L trend is represented by 'domgl', and proportion of time DO \< 2 mg/L trend is represented by 'doLT2' (LT = "less than").

`060_predicting_functions.R` contains functions to generate the same tables and graphs for different sets of models and parameters. It is used more in folder `06b_model_interpretation` than in this folder, but was created here before splitting the scripts further.

Outputs were written to the `Outputs/06_model_selection` folder, in the subfolder `R_objects`. These objects have not been pushed to github due to size, but are available on Box. **LINK**

## 06b_model_interpretation

This folder contains two `.qmd` files and their output: `Overall_Outputs.qmd` and `Overall_Outputs_supplementary.qmd`.

`Overall_Outputs.qmd` contains outputs from model-averaging when delta AICc \< 4, as agreed by the work group. It starts with graphs and tables summarizing standardized coefficients from averaged models for each response. Then, the top several predictors for each response (and the response itself) are back-transformed to either original units or %/year change (when a log-transformation has happened), and graphs of the key predictors and their expected response values are provided. This is to help interpret the effect of each predictor, in more understandable units than standardized coefficients provide.

`Overall_Outputs_supplementary.qmd` contains graphs and tables summarizing standardized coefficients using deltas of 2 and 6 as thresholds, for comparison to what will be reported in the main paper ("how does choice of delta AICc threshold affect the results?"). Individual predictor plots were not made in this file.

Outputs of the `.qmd` files are by default saved in the R directory where the file is. The latest versions have been copied to the `Outputs/06_model_selection` folder.

# Statistical Methods

## Station selection

Only SWMP stations that started collecting data before 2013, and that were active as of the end of 2022, are represented here. Only stations that exist for BOTH WQ and NUT data types are represented. Water quality stations that were abnormally deep (long-term median depth \> 6m: pdbgd, kachd, and kacsd) were removed after trend calculation.

## Data point inclusion

See the data processing README files for explanations on data point inclusion/exclusion. Generally, data points flagged as either suspect or rejected were excluded from these analyses. For nutrients, only grab samples were included (not diel/ISCO samples).

## Monthly aggregation

Again, see the data processing README files. Additionally, see the `Outputs/02_calculated_long-term-trends/data_dictionary_trend_parameters.csv` file for parameters analyzed and transformations made. For WQ and MET, we are generally working with monthly median values. For NUTs, we are working with averaged replicates for each month.

## Long-term Median calculations

For WQ and MET, long-term median values were simply the median of all monthly medians. For NUT parameters, which involve censoring (values below the detection limit) and possibly changing detection limit values, the probability-based robust regression on order statistics (ROS) method was used. See Helsel, D.R. 2011: Statistics for Censored Environmental Data using Minitab and R. Wiley & Sons, Inc. See also [Dennis Helsel's videos and slides for Statitics for Data with Nondetects (Censored Data)](https://practicalstats.com/videos/nadavids.html).

Functions from the `NADA` R package allow calculation of summary statistics on censored data in several ways. We used `cenros()`, ROS, because as Helsel notes in the above sources, this is the most flexible method and works with 0, 1, or multiple detection limits. Kaplan-Meier estimation is fairly common in other fields, but it does not work well when there's a single detection limit. The other option, Maximum Likelihood Estimation (MLE), doesn't work well when none of the values are censored - while developing code, all methods were tested, and MLE did not return the same value as the regular sample median method did (and which is what we'd want) when there were no censored points.

For nutrient medians to be calculated, we required that more than one calendar year be present in the data, and at least 12 individual values.

## Long-term Trend calculations

We have generally used GAMs (generalized additive models) to calculate trends. A seasonal term is included, with 12 knots if possible and the number of months represented in the data frame otherwise (e.g. stations where sondes are removed part of the year due to ice). Autocorrelation of residuals is automatically checked for and if present, the model is re-run to account for the autocorrelation. The reported trend in the outputs is the LINEAR trend through time (per year) of the parameter. To account for censoring, autocorrelation, and seasonality, we used `mgcv::bam`.

For NUT parameters: values were log transformed [as of 5/29/2024, this is natural-log; replacing log10, as natural-log yields more interpretable coefficinets; see Gelman et al. 2021], and marked as censored (e.g. below the minimum detection limit, or MDL) or uncensored. This created a response matrix that was used in `mgcv::bam` with `family = cnorm()` to account for censoring.

For WQ monthly medians, we also used `mgcv::bam` for consistency in outputs. There is no censoring in these parameters, so we used `family = gaussian()`.

For WQ proportion of DO below 2 and 5: These calculations were made before monthly aggregation - each valid 15-minute data point was marked TRUE/FALSE for below 2 and 5, respectively (in separate columns). During monthly aggregation, the total TRUE for each month was divided by the total number of valid DO points for the month, leading to a proportion per month. Trends were again calculated in `mgcv::bam()` with a seasonal term and an autocorrelation term if necessary. Because this response is a proportion, we used `family = betar()`. The `eps` option, which adjusts exact 0s and 1s, was set to 1/10th of the minimum number of readings per month (1/27900).

For MET parameters, we again used the `bam` code written for water quality, as the properties are similar. Performed tests on 3 parameters: monthly median air temperature (C), monthly total precipitation (mm), and the monthly median of daily total PAR. Monthly precipitation was square-root transformed before analysis (this produced the best residual diagnostics on 5 stations explored).

### Seasonal trends

This part is simpler and rougher: we have not accounted for autocorrelation or any "wiggliness" in the data. We simply split data into four seasons: Winter (Jan, Feb, Mar); Spring (Apr, May, Jun); Summer (Jul, Aug, Sep); Fall (Oct, Nov, Dec) and calculated a linear trend. For WQ medians, we used the simple `lm()` function. Nutrients still used `mgcv::bam()` to account for censoring (`family = cnorm()`), and DO proportions also used `mgcv::bam()` with `family = betar()`.

### Additional

p-values have NOT been adjusted from any of these analyses, so be wary about declaring any individual trend significant based on its reported p-value. Be especially wary about seasonal p-values, as autocorrelation is not accounted for.

## Predictive Modeling, Model Selection, Model Averaging

-   Construction and checking of big predictive models

    -   Selected predictors (incl. Par trend exclusion)

    -   Collinearity

    -   Latitudinal PCA

    -   Influential observations

    -   REML for checking random effects, then ML for model selection (Zuur et al. 2009, section 5.7)

    -   Goodness of fit

-   Centering and scaling variables (including response, to get standardized coefficients), to streamline model convergence and help in model selection

-   Model selection and averaging

    -   MuMIn::dredge; justify all-subsets selection

    -   Top model set, AICc thresholds, etc.

    -   importance/sum of weights/n models

    -   Nesting vs. not

    -   Full-model averaging
