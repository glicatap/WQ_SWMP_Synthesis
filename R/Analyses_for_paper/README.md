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

`050_setup.R` is a helper file that reads in the data and compiled predictors from earlier steps, performs PCA for the latitudinally-related variables (latitude, median DO mg/L, median PAR, median temperature), and attaches the scores from PC1 of that PCA to the predictor data frame. 


## 06_model_selection

For details on model selection process decisions, see the detailed **Statistical Methods** section below.  

Files in this folder implement all-subsets selection from the global models evaluated in folder `05_predictive_modeling`, and then implement model averaging for top-model sets. Each response variable gets its own scripts, of which there are two: `06a` for dredging (using `MuMIn::dredge()`), and `06b` for model averaging. The scripts have an abbreviation for the response variable in the names - e.g. the chlorophyll a trend model has all-subsets selection performed in `06a_chl_dredging.R`, then model averaging in `06b_chl_model-avgd_outputs.R`. DO mg/L trend is represented by 'domgl', and proportion of time DO \< 2 mg/L trend is represented by 'doLT2' (LT = "less than").

`060_predicting_functions.R` contains functions to generate the same tables and graphs for different sets of models and parameters. It is used more in folder `06b_model_interpretation` than in this folder, but was created here before splitting the scripts further.

Outputs were written to the `Outputs/06_model_selection` folder, in the subfolder `R_objects`. These objects are available in both the github repository and on Box. See the [06_model_selection section](https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/tree/main/Outputs#06_model_selection) of the `Outputs` readme for details on data frames contained within each .RData object.  

## 06b_model_interpretation

This folder contains two `.qmd` files and their output: `Overall_Outputs.qmd` and `Overall_Outputs_supplementary.qmd`.

`Overall_Outputs.qmd` contains outputs from model-averaging when delta AICc \< 4, as agreed by the work group. It starts with graphs and tables summarizing standardized coefficients from averaged models for each response. Then, the top several predictors for each response (and the response itself) are back-transformed to either original units or %/year change (when a log-transformation has happened), and graphs of the key predictors and their expected response values are provided. This is to help interpret the effect of each predictor, in more understandable units than standardized coefficients provide.

`Overall_Outputs_supplementary.qmd` contains graphs and tables summarizing standardized coefficients using deltas of 2 and 6 as thresholds, for comparison to what will be reported in the main paper ("how does choice of delta AICc threshold affect the results?"). Individual predictor plots were not made in this file.

Outputs of the `.qmd` files are by default saved in the R directory where the file is. The latest versions have been copied to the `Outputs/06_model_selection` folder.


# Statistical Methods - Detailed  

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

#### Seasonal trends

This part is simpler and rougher: we have not accounted for autocorrelation or any "wiggliness" in the data. We simply split data into four seasons: Winter (Jan, Feb, Mar); Spring (Apr, May, Jun); Summer (Jul, Aug, Sep); Fall (Oct, Nov, Dec) and calculated a linear trend. For WQ medians, we used the simple `lm()` function. Nutrients still used `mgcv::bam()` to account for censoring (`family = cnorm()`), and DO proportions also used `mgcv::bam()` with `family = betar()`.

#### Additional

p-values have NOT been adjusted from any of these analyses, so be wary about declaring any individual trend significant based on its reported p-value. Be especially wary about seasonal p-values, as autocorrelation is not accounted for.

## Predictive Modeling, Model Selection, Model Averaging

### Constructing and evaluating global models

All variables were centered and scaled to 1 standard deviation before model-fitting, to help with model convergence in mixed models, to enable comparison between standardized coefficients in final models, and to ensure appropriate model selection and averaging (Harrison et al. 2018, Grueber et al. 2011, Symonds and Moussalli 2011). When appropriate for interpreting results, coefficients were back-calculated to either their original units or to percent-per-year (when log transformations had been performed).  

Global models were constructed with the predictors and a random effect for Reserve and fit using the `lme()` function of the R `nlme` package (v. 3.1.160; Pinherio et al. 2022) using Restricted Maximum Likelihood (REML). To determine whether the random effect was necessary, a simple linear model without the random effect was constructed via `nlme::gls()`, also using REML, to enable comparison via AIC (Zuur et al. 2009, section 5.7). When the random effect was required (chla models), the global model was then re-fit with Maximum Likelihood (ML) for subsequent model selection; when random effects are present but fixed effects vary, REML does not generate comparable AIC values (Zuur et al. 2009). When the random effect was not required (DO models), global models were re-fit as simple linear models, using `stats::lm()` (R Core Team 2022).

### Model selection and averaging

-   All-subsets selection was performed on the global models, using `MuMIn::dredge()`. This practice is frequently looked down on (Burnham and Anderson 2002, Grueber et al. 2011); however, we were careful to include only potentially important predictors in our model, and they could legitimately be influencing the responses through any combination of their subsets.

-   Top model set, AICc thresholds, etc.

    -   different recommendations for AIC: Burnham and Anderson 2002 are often cited for using delta \< 2, though they back away from this in later papers (e.g. Burnham et al. 2011). Richards 2008 recommended 6 as the value required to be 95% sure the model with the lowest Expected Kullback-Leibler Distance is in the top model set. Bolker 2009 recommended 10 for ecological studies. Grueber et al. 2011 recommend considering the number of models in each group, because "too many" are likely to include spurious predictors (but guidance for what is "too many" is lacking). Burnham and Anderson 2002 also describe a 95% confidence set, which is supported by Symonds and Moussalli 2011.

    -   for our models, we chose delta \< 4 as the threshold to focus on in the paper. We focused on the chla models when making this decision, but numbers for the DO models looked similar. The 95% confidence set contained almost half the candidate models, which was a very high number. Even delta \< 6 contained a high number of models (\~2000). Delta \< 2 contained only 30 models. However, when plotting delta values in order, there was really no demarcation between delta of 2 and delta just higher than 2 - there was no reason to think this was a point of differentation where information drops off. Delta around 4 resulted in a more reasonable number of models in the top set and was near an "elbow" in the deltas plot, so we decided on it. We also conducted analyses using deltas of 2 and 6, and present these in supplementary information.

    -   AICc used (e.g. Burnham and Anderson 2004) (not 'regular' AIC) - it involves a correction for small sample size and approximates AIC when sample size is large enough, so there is no downside.

    -   AIC and mixed models: Grueber et al. 2011 mention in their Table 2 that AICc can be problematic when random effects are present. Zuur et al. 2009 recommend, when performing model selection on mixed models, fitting and evaluating the global model with REML, and determining the best random effects structure using likelihood ratio tests or AIC/BIC on models fitted via REML. REML must be used when comparing models with the same fixed effects but nested random effects. Degrees of freedom are calculated differently between REML and ML, and AIC cannot be compared between the two methods (Zuur et al. 2009). Then, to find the optimal fixed effect structure, use ML estimation to compare models with nested fixed effects but the same random effects.

    -   In our global model fitting and evaluation, we did use REML when fitting the model (using `nlme::lme()`) and determining whether a random effect for Reserve was needed. When it was (chla models), the global model was then re-fit with `REML = FALSE`, so the ML fit could be used in all-subsets selection and AIC could be compared between the different subsets. When the random effect was not required (DO models), global models were re-fit as simple linear models, using `stats::lm()`.

    -   Zuur et al. 2009 also say the final 'best' model should be re-fit and presented using REML. However, they were talking about a single best model; when I performed model averaging on models re-fit with REML, it changed the model weights an unacceptable amount (any models below the top 2 had essentially 0 weight and were not included in the averaging). So for our model averaging, I used the models as fitted with ML.

-   **Variable Importance**: `sw`, the sum of Akaike weights for models in which a predictor appears, generated from the `MuMIn::sw()` function, can be interpreted as the probability that the predictor is in the “best” model (Grueber et al. 2011, Symonds and Moussalli 2011). The function also shows how many models from the top set a predictor appeared in. Predictors that are in a lot of models and/or the most highly weighted models will have higher weights than those in few and/or low-weighted models.

-   **Nesting** vs. not (Richards 2008; referenced in Grueber et al. 2011 and Harrison et al. 2018) - Richards (2008) suggested that to avoid the problem of selecting overly complex models, models should be removed from the selected set if a simpler nested version of the model has also been selected and has a lower AIC. Clear guidance for this is lacking. We generated values and plots of importance values for all top models vs. with more complex models removed, but because differences in AIC were so small across the top model set, were uncomfortable completely discarding more complex models that might have nearly identical AICc values. Additionally, Lukacs et al. 2009 suggest that full-model averaging "can help to reduce the problems caused by the model selection bias towards over-complex (and indeed under-complex) models" (quote is from Symonds and Moussalli 2011, citing Lukacs et al. 2009). *We can present the non-nested model importance values and/or graphs in supplementary information* but otherwise did not pursue this issue.

-   **Full-model averaging** - also known as the "zero method" (Grueber et al. 2011), this method treats each predictor as if it has a coefficient in all averaged models. If a predictor does not appear in a model, the value incorporated into the averaging for that predictor is 0 and thus is a method of shrinkage. This method is recommended when using all-subsets selection, when there is large uncertainty as to the "best" model, and "when the aim of the study is to determine which factors have the strongest effect on the response variable" (quote from Grueber et al. 2011, who cite Nakagawa & Freckleton 2010). This contrasts with the *subset method*, which only averages coefficients of the predictor for the models in which that predictor appears. Grueber et al. 2011 and Symonds and Moussalli 2011 are great citations for these decisions.

# References

Bartoń K (2023). _MuMIn: Multi-Model Inference_. R package version 1.47.5, <https://CRAN.R-project.org/package=MuMIn>.

Beck MW (2016). “SWMPr: An R Package for Retrieving, Organizing, and Analyzing Environmental Data for Estuaries.” _The R Journal_, *8*(1), 219-232. doi:10.32614/RJ-2016-015 <https://doi.org/10.32614/RJ-2016-015>.

Bolker, B. M., Brooks, M. E., Clark, C. J., Geange, S. W., Poulsen, J. R., Stevens, M. H. H., & White, J.-S. S. (2009). Generalized linear mixed models: A practical guide for ecology and evolution. *Trends in Ecology & Evolution*, *24*(3), Article 3. <https://doi.org/10.1016/j.tree.2008.10.008>

Burnham, K. P., & Anderson, D. R. (Eds.). (2002). *Model Selection and Multimodel Inference*. Springer New York. <https://doi.org/10.1007/b97636>

Burnham, K. P., & Anderson, D. R. (2004). Multimodel Inference: Understanding AIC and BIC in Model Selection. *Sociological Methods & Research*, *33*(2), 261–304. <https://doi.org/10.1177/0049124104268644>

Burnham, K. P., Anderson, D. R., & Huyvaert, K. P. (2011). AIC model selection and multimodel inference in behavioral ecology: Some background, observations, and comparisons. *Behavioral Ecology and Sociobiology*, *65*(1), 23–35. <https://doi.org/10.1007/s00265-010-1029-6>

Cloern, J. E., & Jassby, A. D. (2008). Complex seasonal patterns of primary producers at the land–sea interface. Ecology Letters, 11(12), 1294–1303. https://doi.org/10.1111/j.1461-0248.2008.01244.x

Grueber, C. E., Nakagawa, S., Laws, R. J., & Jamieson, I. G. (2011). Multimodel inference in ecology and evolution: Challenges and solutions. *Journal of Evolutionary Biology*, *24*(4), 699–711. <https://doi.org/10.1111/j.1420-9101.2010.02210.x>

Harrison, X. A., Donaldson, L., Correa-Cano, M. E., Evans, J., Fisher, D. N., Goodwin, C. E. D., Robinson, B. S., Hodgson, D. J., & Inger, R. (2018). A brief introduction to mixed effects modelling and multi-model inference in ecology. *PeerJ*, *6*, e4794. <https://doi.org/10.7717/peerj.4794>

Helsel, D. R. (2011). Statistics for censored environmental data using Minitab and R (Second edition). Wiley.

Nakagawa, S., & Schielzeth, H. (2013). A general and simple method for obtaining R2 from generalized linear mixed-effects models. *Methods in Ecology and Evolution*, *4*(2), 133–142. <https://doi.org/10.1111/j.2041-210x.2012.00261.x>

Pinheiro J, Bates D, R Core Team (2022). _nlme: Linear and Nonlinear Mixed Effects Models_. R package version 3.1-160, <https://CRAN.R-project.org/package=nlme>.

R Core Team (2022). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

Richards, S. A. (2008). Dealing with overdispersed count data in applied ecology. *Journal of Applied Ecology*, *45*(1), 218–227. <https://doi.org/10.1111/j.1365-2664.2007.01377.x>

Symonds, M. R. E., & Moussalli, A. (2011). A brief guide to model selection, multimodel inference and model averaging in behavioural ecology using Akaike’s information criterion. *Behavioral Ecology and Sociobiology*, *65*(1), 13–21. <https://doi.org/10.1007/s00265-010-1037-6>

Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, Grolemund G, Hayes A, Henry L, Hester J, Kuhn M, Pedersen TL, Miller E, Bache SM, Müller K, Ooms J, Robinson D, Seidel DP, Spinu V, Takahashi K, Vaughan D, Wilke C, Woo K, Yutani H (2019). “Welcome to the tidyverse.” _Journal of Open Source Software_, *4*(43), 1686. doi:10.21105/joss.01686 <https://doi.org/10.21105/joss.01686>.

Wood, S.N. (2017). Generalized Additive Models: An Introduction with R (2nd edition). Chapman and Hall/CRC. <doi:10.1201/9781315370279>

Zuur, A. F., Ieno, E. N., Walker, N. J., Saveliev, A. A., & Smith, G. M. (2009). *Mixed effects models and extensions in ecology with R*. Springer.
