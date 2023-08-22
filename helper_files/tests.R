#' tests file to make sure code is working correctly
#' 
#' 

library(testthat)

#' check 'qaqc' functions  
par1 <- c(5, 6, 7, 8, 9, 10, 11)
f_par1 <- c("<1>", 
            "<1> [GIT]", 
            "<1> [GIT] (CND)", 
            "<1> [GIT] (CSM)", 
            "<1> [GIT] [CSM]",
            "<-3>",
            "<0>")
f_par2 <- c("<1> (CAB)",
            "<-3>",
            "<4>  B - K",
            "<4> B - S",
            "<-4> [SBL]",
            "<-4> [SBL] (CHB)",
            "<0> [CAB]")
expect_identical(qaqc_wq(par1, f_par1), c(NA, 6, NA, 8, 9, NA, 11))
expect_identical(qaqc_nut(par1, f_par2), c(5, NA, 7, NA, 9, NA, 11))
