#' tests file to make sure code is working correctly

library(testthat)
source(here::here("helper_files", "definitions.R"))
source(here::here("helper_files", "functions.R"))

# qaqc funs ----
#' check 'qaqc' functions for individual vectors 

# what I start with
par1 <- c(5, 6, 7, 8, 9, 10, 11)
f_par1 <- c("<1>", 
            "<1> [GIT]", 
            "<1> [GIT] (CND)", 
            "<1> [GIT] (CSM)", 
            "<1> [GIT] [CSM]",
            "<-3>",
            "<0>")
f_par2 <- c("<1> (CAB)",            # keep - algae bloom. not censored.
            "<-3>",                 # discarded (censored = NA)
            "<4>  B - K",           # keep; censored
            "<4> B - S",            # discarded due to S (censored = NA)
            "<-4> [SBL]",           # keep; censored
            "<-4> [SBL] (CHB)",     # discarded due to CHB (censored = NA)
            "<0> [CAB]")            # keep; not censored
# what I expect for nutrients
dfnut <- data.frame(data = c(5, NA, 7, NA, 9, NA, 11),
                    censored = c(0L, NA, 1L, NA, 1L, NA, 0L))
# test wq
expect_identical(qaqc_wq(par1, f_par1), c(NA, 6, NA, 8, 9, NA, 11))
# test nut
expect_identical(qaqc_nut(par1, f_par2), dfnut)

# qaqc_df ----
#' check 'qaqc_df' function

#' wq
#' what I start with
parms <- c("sal", "do_mgl")
df1 <- data.frame(index = 1:10,
                  stn = rep("abcyzwq", 10),
                  date = Sys.Date() + 1:10,
                  sal = rep(5, 10),
                  f_sal = c("<0>", "<1>", "<1> (CAB)", "<1> [GIT]", "<1> [GIT] (CSM)", 
                            "<-3>", "<4>", "<-4>", "<1> [GIT] (CDA)", "<0> (CSM)"),
                  do_mgl = rep(8, 10),
                  f_do_mgl = c(c("<0>", "<1> [SWM]", "<-3> (CAB)", "<-3> [GIT]", "<-2> [GIT] (CSM)", 
                                 "<-3>", "<4>", "<-4>", "<-2>", "<0> (CSM)")))
#' what I expect from using functions
df2 <- data.frame(index = 1:10,
                  stn = rep("abcyzwq", 10),
                  date = Sys.Date() + 1:10,
                  sal = c(5, NA, NA, 5, 5, 
                          NA, 5, NA, NA, 5), 
                  do_mgl = c(8, NA, NA, NA, NA, 
                             NA, 8, NA, NA, 8))
#' test it
expect_identical(qaqc_df(df1, parms, "wq"), df2)

#' nut
#' what I start with
parms <- c("po4f", "chla_n")
df1 <- data.frame(index = 1:10,
                  stn = rep("abcyzwq", 10),
                  date = Sys.Date() + 1:10,
                  po4f = rep(5, 10),
                  f_po4f = c("<0>", "<1>", "<1> (CAB)", "<-4> [SBL]", "<4>  B - K", 
                            "<-3>", "<4>", "<5> (CSM)", "<-4> [SBL] (CHB)", "<0> (CSM)"),
                  chla_n = rep(8, 10),
                  f_chla_n = c(c("<0>", "<1> (CAB)", "<-3> (CAB)", "<4>  S - K", "<4>  K - H", 
                                 "<-3>", "<4>", "<4>  K", "<-2>", "<0> (CSM)")))
#' what I expect
df2 <- data.frame(index = 1:10,
                  stn = rep("abcyzwq", 10),
                  date = Sys.Date() + 1:10,
                  po4f = c(5, NA, 5, 5, 5, 
                           NA, 5, 5, NA, 5), 
                  po4f_cens = c(0L, NA, 0L, 1L, 1L,
                                NA, 0L, 0L, NA, 0L),
                  chla_n = c(8, 8, NA, NA, NA, 
                             NA, 8, 8, NA, 8),
                  chla_n_cens = c(0L, 0L, NA, NA, NA,
                                  NA, 0L, 0L, NA, 0L))
#' test it
expect_identical(qaqc_df(df1, parms, "nut"), df2)

# censoring ----
#' what I start with
df1 <- data.frame(index = 1:12,
                  stn = rep("abcyzwq", 12),
                  year = rep(2015, 12),
                  month = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6),
                  po4f = c(5, NA, 5, 5, 5, 
                           NA, 5, 5, NA, NA,
                           5, 5), 
                  po4f_cens = c(0L, NA, 0L, 1L, 1L,
                                NA, 0L, 0L, NA, NA,
                                1L, 1L),   # 0 & NA; 0 & 1; 1 & NA; 0 & 0; NA & NA; 1 & 1
                  chla_n = c(8, 8, NA, NA, NA, 
                             NA, 8, 8, NA, 8,
                             8, 8),
                  chla_n_cens = c(0L, 0L, NA, NA, NA,
                                  NA, 0L, 0L, NA, 0L,
                                  0L, 1L))  # 0 & 0; NA & NA; NA & NA; 0 & 0; NA & 0; 0 & 1
#' what I expect
df2 <- data.frame(year = rep(2015, 6),
                  month = as.numeric(1:6),
                  po4f = c(5, 5, 5, 5, NA, 5),
                  chla_n = c(8, NA, NA, 8, 8, 8),
                  po4f_cens = c(0L, 1L, 1L, 0L, NA, 1L),
                  chla_n_cens = c(0L, NA, NA, 0L, 0L, 1L))
#' what I get using the functions
df_summ <- df1 %>% 
  summarize(.by = c(year, month),
            across(any_of(c("po4f", "chla_n")),
                   ~modFunn(.x, mean)),
            across(any_of(c("po4f_cens", "chla_n_cens")),
                   cens_fun))
#' test it
expect_equal(df_summ, df2, check.attributes = FALSE)


# Analysis Helper fns ----

#' data frame subsetting  
#' what I start with
df1 <- data.frame(
  station = c(rep("abcdenut", 8),
              rep("abcdewq", 8)),
  year = c(rep(2011, 4), rep(2012, 4),
           rep(2011, 4), rep(2012, 4)),
  month = c(9:12, 1:4,
            9:12, 1:4),
  nh4f = c(rep(1, 5), NA, rep(1, 2), rep(NA, 8)),
  nh4f_cens = c(0, 0, 1, 1, 1, NA, 0, 0, rep(NA, 8)),
  spcond = c(rep(NA, 8),
             rep(20, 5), NA, rep(20, 2))
)

dec.date <- lubridate::decimal_date(
  lubridate::ymd(
    paste(df1$year, df1$month, "15", sep = "-")))[1:8]

#' what I expect: nut
df2 <- data.frame(
  station = "abcdenut",
  year = c(rep(2011, 4), rep(2012, 4)),
  month = c(9:12, 1:4),
  dec_date = dec.date,
  focal_param = "nh4f",
  value = c(rep(1, 5), NA, rep(1, 2)),
  cens = c(0, 0, 1, 1, 1, NA, 0, 0),
  lognut = c(rep(0, 5), NA, rep(0, 2)),
  cens_lognut = c(0, 0, -Inf, -Inf, -Inf, NA, 0, 0))
df2$lognut_mat <- cbind(df2$lognut, df2$cens_lognut)
df2$ARrestart = c(TRUE, rep(FALSE, 5), TRUE, FALSE)

#' test it
df_test <- subset_df("nut", df1, "abcdenut", "nh4f")
expect_equal(df_test, df2, check.attributes = FALSE)


#' what I expect: wq
dec.date <- lubridate::decimal_date(
  lubridate::ymd(
    paste(df1$year, df1$month, "15", sep = "-")))[9:16]
df3 <- data.frame(
  station = "abcdewq",
  year = c(rep(2011, 4), rep(2012, 4)),
  month = c(9:12, 1:4),
  dec_date = dec.date,
  focal_param = "spcond",
  value = c(rep(20, 5), NA, rep(20, 2)),
  ARrestart = c(TRUE, rep(FALSE, 5), TRUE, FALSE)
)


#' test it
df_test <- subset_df("wq", df1, "abcdewq", "spcond")
expect_equal(df_test, df3, check.attributes = FALSE)
