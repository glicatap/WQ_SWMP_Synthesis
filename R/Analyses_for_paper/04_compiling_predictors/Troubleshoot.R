data<- read.csv(here::here("Outputs", 
                                     "04_compiled_predictors",
                                     "compiled_predictors_MDL.csv"))

data2 <- read.csv(here::here("Outputs", 
                                     "04_compiled_predictors",
                                     "compiled_predictors_v3.csv"))


plot(data$no23f_median,data2$no23f_median)
abline(0,1,col="red")



plot(data$po4f_median,data2$po4f_median)
abline(0,1,col="red")


plot(data$nh4f_median,data2$nh4f_median)
abline(0,1,col="red")

plot(data$no23f_mdl_trend,data2$no23f_trend)
abline(0,1,col="red")


plot(data$po4f_mdl_trend,data2$po4f_trend)
abline(0,1,col="red")
