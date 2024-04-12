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


correlation_matrix <- cor(master_df[,2:31],
                          method = "spearman",
                          use = "pairwise.complete.obs")

# Visualize the correlation matrix
corrplot(correlation_matrix, method = "color")


tmp <- master_df
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
test<-replace(tmp, tmp >= lower_bound & tmp <= upper_bound, 0)



# Define custom breaks and colors for the color ramp
my_breaks <- c(-1, -0.318,0, 0.318, 1)  # Define breaks
my_colors <- c("red", "white", "white", "red")  # Define corresponding colors

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



upper_triangular <- test[upper.tri(test)]

# Print the extracted upper triangular part
print(upper_triangular)
