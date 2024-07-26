# have to fit the models inside model.avg in order to predict
# modavg_all <- model.avg(test, fit = TRUE)

make_predictions <- function(data, predictor, avgd_mod,
                             means, sds,
                             sd.range = c(-3, 3),
                             response.is.log.trend = FALSE,
                             predictor.is.log.trend = FALSE){
  # data is unquoted data frame of data that went into the model
  # predictor is quoted column name for what you want predictions of
  # avgd_mod is unquoted model.average object that has fitted models in it
  # e.g. avgd_mod <- model.avg(top_models, fit = TRUE)
  
  # means is a data frame of means for each predictor, for back-calculating
  # sds is a data frame of standard deviations for each predictor, for back-calculating
  # sd.range is a numeric vector of the range of standard deviations in the predictor for which Y should be predicted
  
  # pull out mean and sd
  predictor_mean <- means[[predictor]]
  predictor_sd <- sds[[predictor]]
    
  # set up data frames
  dat <- data
  newdata.names <- names(dat)[3:ncol(dat)]
  newdata.sds <- seq(min(sd.range), max(sd.range), by = 0.1)
  # start newdata as a matrix with 0s
  # a column for every variable; we'll replace what we want as a predictor
  # with newdata.sds later
  newdata.matrix <- matrix(data = 0,
                           nrow = length(newdata.sds),
                           ncol = length(newdata.names))
  newdata <- data.frame(newdata.matrix)
  names(newdata) <- newdata.names
  
  # insert the -3 to +3 SD vector into the column
  # for the specified predictor
  newdata[[predictor]] <- newdata.sds
  
  
  # make predictions
  predictions <- predict(avgd_mod,
                         newdata = newdata,
                         se.fit = TRUE,
                         re.form = NA,
                         level = 0)
  
  predictions_df <- data.frame(predictor.sd = newdata[[predictor]],
                               predictor.natural = (newdata[[predictor]] * predictor_sd) + predictor_mean,
                               predicted = predictions$fit,
                               se = predictions$se) |> 
    mutate(ci_low = predicted - 1.96*se,
           ci_high = predicted + 1.96*se)
  
  if(response.is.log.trend == TRUE){
    predictions_df <- predictions_df |> 
      mutate(pct_per_year = exp(predicted) * 100 - 100,
             ci_low = exp(ci_low) * 100 - 100,
             ci_high = exp(ci_high) * 100 - 100)
  }
  
  if(predictor.is.log.trend == TRUE){
    predictions_df <- predictions_df |> 
      mutate(predictor.pct_per_year = exp(predictor.natural) * 100 - 100)
  }
  
  return(predictions_df)
}

# this doesn't take care of logarithmic or other transformations - may need to do that in another step

graph_predictions <- function(predictions,
                              response.is.log.trend = FALSE,
                              predictor.is.log.trend = FALSE){
  # predictions is a data frame with predictions and ses
  # must have columns: predictor.natural, ci_low, ci_high, and predicted
  
  # set up aesthetics based on inputs
  if(predictor.is.log.trend == TRUE & response.is.log.trend == FALSE){
    p <- ggplot(predictions,
                aes(x = predictor.pct_per_year,
                    y = predicted,
                    ymin = ci_low,
                    ymax = ci_high))
  } else if(predictor.is.log.trend == TRUE & response.is.log.trend == TRUE){
    p <- ggplot(predictions,
                aes(x = predictor.pct_per_year,
                    y = pct_per_year,
                    ymin = ci_low,
                    ymax = ci_high))
  } else if(predictor.is.log.trend == FALSE & response.is.log.trend == TRUE){
    p <- ggplot(predictions,
                aes(x = predictor.natural,
                    y = pct_per_year,
                    ymin = ci_low,
                    ymax = ci_high))
  } else {
    p <- ggplot(predictions,
                aes(x = predictor.natural,
                    y = predicted,
                    ymin = ci_low,
                    ymax = ci_high))
  }
  
  # add layers to the plot
  p <- p +
    geom_hline(yintercept = 0,
               linetype = "solid",
               col = "gray10",
               linewidth = 0.3) +
    geom_vline(xintercept = 0,
               linetype = "solid",
               col = "gray10",
               linewidth = 0.3) +
    geom_ribbon(fill = "gray80",
                alpha = 0.6) +
    geom_line(col = "blue",
              linewidth = 0.7) +
    theme_bw() 
  
  # return the plot
  p
}


