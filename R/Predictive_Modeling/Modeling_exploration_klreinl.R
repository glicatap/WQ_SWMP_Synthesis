library(tidyverse)
library(corrplot)
library(vegan)
library(car)  # for vif()

setwd("C:/Users/kreinl1/OneDrive/OneDrive - UW-Madison/GitHub/WQ_SWMP_Synthesis/Data")

master_df <- read.csv("master_df.csv")
master_df_scaled <- read.csv("master_df_scaled.csv")

setwd("C:/Users/kreinl1/OneDrive/OneDrive - UW-Madison/GitHub/WQ_SWMP_Synthesis/R/Predictive_Modeling")

colnames(master_df)[1] <- "station"
colnames(master_df_scaled)[1] <- "station"


correlation_matrix <- cor(master_df_scaled[,2:31],
                          method = "spearman",
                          use = "pairwise.complete.obs")

# Visualize the correlation matrix
corrplot(correlation_matrix, method = "color")


tmp <- master_df_scaled
Q <- cor(tmp[2:31], 
         method = "spearman",
         use = "pairwise.complete.obs")
testRes = cor.mtest(tmp[2:31], conf.level = 0.95)
corrplot(Q, 
         type = "upper",
         order = 'FPC',
         method = "ellipse",
         p.mat = testRes$p, 
         sig.level = 0.05,
         insig = "label_sig",
         pch.cex = 2.5,
         pch.col = "gray20",
         tl.pos = "lt",
         tl.cex = 0.8,
         tl.srt = 45)
corrplot(Q, add = TRUE,
         type = "lower",
         order = 'FPC',
         method = "number",
         insig = "n",
         diag = FALSE,
         tl.pos = "n",
         cl.pos = "n",
         number.cex = 0.6)

correlation_estimates <- Q
tmp<-data.frame(correlation_estimates)

##spearman critical value for 30 pairs (DF=n-2, so 28) is 0.318 for 
#alpha = 0.05

lower_bound<- -0.318
upper_bound <- 0.318

# Replace values within the range with NA
test<-replace(tmp, tmp <= lower_bound & tmp >= upper_bound, 0)

# Plot correlation matrix with custom color ramp
corrplot(as.matrix(test), 
         type = "upper",
         order = 'FPC',
         method = "ellipse",
         sig.level = 0.05,
         insig = "label_sig",
         pch.cex = 2.5,
         pch.col = "gray20",
         tl.pos = "lt",
         tl.cex = 0.8,
         tl.srt = 45) 


#####################################################################
# Get the row and column names of the correlation matrix
col_names <- colnames(correlation_estimates)

# Initialize empty vectors to store pairs and correlation values
pairs <- character()
correlation_values <- numeric()

# Loop through the upper triangular part of the correlation matrix
for (i in 1:(length(col_names) - 1)) {
  for (j in (i + 1):length(col_names)) {
    pairs <- c(pairs, paste(col_names[i], col_names[j], sep = "-"))
    correlation_values <- c(correlation_values, correlation_estimates[i, j])
  }
}

# Create a data frame with pairs and corresponding correlation estimates
correlation_df <- data.frame(Pairs = pairs, Correlation = correlation_values)


filtered_correlation_df <- correlation_df[correlation_df$Correlation > 0.7 | correlation_df$Correlation < -0.7, ]

# Print the filtered data frame
print(filtered_correlation_df)


################################


# Load the knitr package
library(knitr)

# Print the data frame as a formatted table
kable(filtered_correlation_df)



##Let's do this again for the 0.7 threshold

lower_bound<- -0.7
upper_bound <- 0.7

# Replace values within the range with NA
test<-replace(tmp, tmp >= lower_bound & tmp <= upper_bound, 0)

# Plot correlation matrix with custom color ramp
corrplot(as.matrix(test), 
         type = "upper",
         order = 'FPC',
         method = "ellipse",
         sig.level = 0.05,
         insig = "label_sig",
         pch.cex = 2.5,
         pch.col = "gray20",
         tl.pos = "lt",
         tl.cex = 0.8,
         tl.srt = 45) 


#####################################################################
# Get the row and column names of the correlation matrix
col_names <- colnames(correlation_estimates)

# Initialize empty vectors to store pairs and correlation values
pairs <- character()
correlation_values <- numeric()

# Loop through the upper triangular part of the correlation matrix
for (i in 1:(length(col_names) - 1)) {
  for (j in (i + 1):length(col_names)) {
    pairs <- c(pairs, paste(col_names[i], col_names[j], sep = "-"))
    correlation_values <- c(correlation_values, correlation_estimates[i, j])
  }
}

# Create a data frame with pairs and corresponding correlation estimates
correlation_df <- data.frame(Pairs = pairs, Correlation = correlation_values)


filtered_correlation_df <- correlation_df[correlation_df$Correlation > 0.7 | correlation_df$Correlation < -0.7, ]

# Print the filtered data frame
print(filtered_correlation_df)


unique(filtered_correlation_df$Pairs)

################################


# Load the knitr package
library(knitr)

# Print the data frame as a formatted table
kable(filtered_correlation_df)


