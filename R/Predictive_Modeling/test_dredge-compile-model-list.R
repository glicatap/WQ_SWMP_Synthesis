# use this after getting to chunk 19 of building-up-to-main4
# making a model (mod2) with chl_df3
# see how long it takes to determine all possible subsets
# at this point, no categorical predictors from SWMP CLUE
# number of models = 2^(k-1) where k is the number of predictors
# this data frame has 24 predictors and leads to ~8.4 million models
# it took 16 minutes to generate the list, using 10 cores
# that did NOT fit any of the models
# it took up all my RAM and the resulting list object was 37GB (more than my RAM)
# I gave up before being able to lapply and pull out information -
# am worried about whether that will work - may need to test on smaller batches


library(MuMIn)
library(doParallel)


# establish cluster
cl<-makeCluster(10)  
registerDoParallel(cl)


# name models, using cluster, and time it
strt <- Sys.time()
options(na.action = "na.fail")
mod_subsets <- MuMIn::dredge(mod2, eval = FALSE,
                             cluster = cl)
end <- Sys.time()

end - strt
beepr::beep(8)

# turn off cluster
stopCluster(cl)

# examine
mod_subsets2 <- lapply(mod_subsets, as.character) |> 
  bind_rows()
mod_subsets2 <- mod_subsets2[2, ] |> 
  pivot_longer(1:ncol(mod_subsets2),
               names_to = "model",
               values_to = "call")