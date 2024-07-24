# have to fit the models inside model.avg in order to predict
modavg_all <- model.avg(test, fit = TRUE)

make_predictions <- function(data, predictor, avgd_mod,
                             means, sds){
  # data is unquoted data frame of data that went into the model
  # predictor is quoted column name for what you want predictions of
  # avgd_mod is unquoted model.average object that has fitted models in it
  # e.g. avgd_mod <- model.avg(top_models, fit = TRUE)
  
  # means is a data frame of means for each predictor, for back-calculating
  # sds is a data frame of standard deviations for each predictor, for back-calculating

  
  # pull out mean and sd
  predictor_mean <- means[[predictor]]
  predictor_sd <- sds[[predictor]]
    
  # set up data frames
  dat <- data
  newdata.names <- names(dat)[3:ncol(dat)]
  newdata.sds <- seq(-3, 3, by = 0.1)
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
                         re.form = NA)
  
  predictions_df <- data.frame(predictor.sd = newdata[[predictor]],
                               predictor.natural = (newdata[[predictor]] * predictor_sd) + predictor_mean,
                               predicted = predictions$fit,
                               se = predictions$se) |> 
    mutate(ci_low = predicted - 1.96*se,
           ci_high = predicted + 1.96*se)
  
  return(predictions_df)
}

# this doesn't take care of logarithmic or other transformations - may need to do that in another step

graph_predictions <- function(predictions){
  # predictions is a data frame with predictions and ses
  # must have columns: predictor.natural, ci_low, ci_high, and predicted
  ggplot(predictions) +
    geom_ribbon(aes(x = predictor.natural,
                    ymin = ci_low,
                    ymax = ci_high),
                fill = "gray",
                alpha = 0.6) +
    geom_hline(yintercept = 0,
               linetype = "dashed",
               col = "gray20") +
    geom_line(aes(x = predictor.natural,
                  y = predicted),
              linewidth = 1,
              col = "blue") +
    theme_bw() 
}


