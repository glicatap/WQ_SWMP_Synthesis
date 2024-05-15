# no do proportion trends

no_doPropTrend <- trends_wqnutmet |>
  filter(is.na(Slope),
         parameter == "do_proportion_below2") |> 
  select(station) |> 
  mutate(station = substr(station, 1, 5)) |> 
  unlist()


no_doPropTrend[which(no_doPropTrend %in% all_predictors$station)]
# do proportion below 2 model will have to be run with 5 fewer stations
# or we switch to proportion below 5


# what about proportion below 5
no_doPropTrend <- trends_wqnutmet |>
  filter(is.na(Slope),
         parameter == "do_proportion_below5") |> 
  select(station) |> 
  mutate(station = substr(station, 1, 5)) |> 
  unlist()


no_doPropTrend[which(no_doPropTrend %in% all_predictors$station)]

# just one: lksol