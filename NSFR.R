library(reticulate) # import Python packages
library(ggplot2)
library(dplyr)
library(expm)
library(tidyr)
library(YieldCurve)
library(zoo) 
library(pracma)
library(plotly)
library(ftsa)
library(glmnet)
library(lmridge)
library(lmtest)
library(statespacer)
library(scales)

setwd("/Users/HPL/Desktop/bond_yields/")
source("Functions/NS_loading.R")
source("Functions/match_maturity.R")
source("Functions/KF_NS.R")
source("Functions/kpca.R")

########################
##### Prepare data #####
########################
# US monthly data 
US <- read.csv("original_data/TVC_US01Y, 1M.csv") 

US$time <- as.Date(strtrim(US$time, 10))
US <- US %>%
  filter(time >= as.Date("2010-01-01") & time <= as.Date("2020-12-31"))

rownames(US) <- US$time
US <- US[, c(2, 4, 6, 7, 8, 9, 10, 11, 12, 14)]
mat_US <- c(1, 3, 6, 12, 24, 36, 60, 84, 120, 360) # maturity of US data

# UK monthly data
UK <- read.csv("original_data/TVC_GB01Y, 1M.csv")

UK$time <- as.Date(strtrim(UK$time, 10))
UK <- UK %>%
  filter(time >= as.Date("2010-01-01") & time <= as.Date("2020-12-31"))

rownames(UK) <- UK$time
UK <- UK[, c(2, 3, 4, 5, 6, 7, 9, 14, 17, 19)]
mat_UK <- c(1, 3, 6, 12, 24, 36, 60, 120, 240, 360) # maturity of UK data

# Replace NA with interpolated values
missing_date <- rownames(US)[!(rownames(US) %in% rownames(UK))]
UK[missing_date, ] <- NA
UK <- UK[order(as.Date(rownames(UK))), ]
row_name <- rownames(UK)
UK <- as.data.frame(na.approx(UK))  
rownames(UK) <- row_name

# JP monthly data
JP <- read.csv("original_data/TVC_JP01Y, 1M.csv")
JP$time <- as.Date(strtrim(JP$time, 10))
JP <- JP %>%
  filter(time >= as.Date("2010-01-01") & time <= as.Date("2020-12-31"))
rownames(JP) <- JP$time
JP <- JP[, c(3, 4, 5, 6, 8, 13, 15, 16)]
mat_JP <- c(6, 12, 24, 36, 60, 120, 240, 360)

# FR monthly data
FR <- read.csv("original_data/TVC_FR02Y, 1M.csv")
FR$time <- as.Date(strtrim(FR$time, 10))
FR$time <- FR$time - 1
FR <- FR %>%
  filter(time >= as.Date("2010-01-01") & time <= as.Date("2020-12-31"))
rownames(FR) <- FR$time
FR <- FR[, c(2, 3, 4, 5, 7, 8, 10, 15, 17, 19)]
mat_FR <- c(1, 3, 6, 9, 24, 36, 60, 120, 240, 360)

# Replace NA with interpolated values
missing_date <- rownames(US)[!(rownames(US) %in% rownames(FR))]
FR[missing_date, ] <- NA
FR <- FR[order(as.Date(rownames(FR))), ]
row_name <- rownames(FR)
FR <- as.data.frame(na.approx(FR))  
rownames(FR) <- row_name

# DE monthly data
DE <- read.csv("original_data/TVC_DE01Y, 1M.csv")
DE$time <- as.Date(strtrim(DE$time, 10))
DE <- DE %>%
  filter(time >= as.Date("2010-01-01") & time <= as.Date("2020-12-31"))
rownames(DE) <- DE$time
DE <- DE[, c(3, 4, 5, 6, 7, 8, 10, 15, 17, 19)]
mat_DE <- c(3, 6, 9, 12, 24, 36, 60, 120, 240, 360)

# Replace NA with interpolated values
missing_date <- rownames(US)[!(rownames(US) %in% rownames(DE))]
DE[missing_date, ] <- NA
DE <- DE[order(as.Date(rownames(DE))), ]
row_name <- rownames(DE)
DE <- as.data.frame(na.approx(DE))  
rownames(DE) <- row_name

# IT monthly data
IT <- read.csv("original_data/TVC_IT02Y, 1M.csv")
IT$time <- as.Date(strtrim(IT$time, 10))
IT <- IT %>%
  filter(time >= as.Date("2010-01-01") & time <= as.Date("2020-12-31"))
rownames(IT) <- IT$time
IT <- IT[, c(4, 5, 7, 8, 10, 15, 19)]
mat_IT <- c(6, 9, 24, 36, 60, 120, 360)

# Replace NA with interpolated values
missing_date <- rownames(US)[!(rownames(US) %in% rownames(IT))]
IT[missing_date, ] <- NA
IT <- IT[order(as.Date(rownames(IT))), ]
row_name <- rownames(IT)
IT <- as.data.frame(na.approx(IT))  
rownames(IT) <- row_name
IT["2010-01-31", 4] <- 0.6845

# AU monthly data
AU <- read.csv("original_data/TVC_AU01Y, 1M.csv")
AU$time <- as.Date(strtrim(AU$time, 10))
AU <- AU %>%
  filter(time >= as.Date("2010-01-01") & time <= as.Date("2020-12-31"))
rownames(AU) <- AU$time
AU <- AU[, c(2, 3, 4, 6, 11)]
mat_AU <- c(12, 24, 36, 60, 120)

# EU monthly data
EU <- read.csv("original_data/TVC_EU01Y, 1M.csv")
EU$time <- as.Date(strtrim(EU$time, 10))
EU <- EU %>%
  filter(time >= as.Date("2010-01-01") & time <= as.Date("2020-12-31"))
rownames(EU) <- EU$time
EU <- EU[, c(2, 3, 4, 5, 6, 7, 8, 10, 12, 15, 17, 19)]
mat_EU <- c(1, 3, 6, 9, 12, 24, 36, 60, 84, 120, 240, 360)

# Replace NA with interpolated values
row_name <- rownames(EU)
EU <- as.data.frame(na.approx(EU))  
rownames(EU) <- row_name

# Match maturity through Static Nelson-Siegel model
maturity <- c(1, 3, 6, 9, 12, 24, 36, 60, 84, 120, 240, 360) # maturity to be matched
US <- match_maturity(data = US, 
                     original_maturity = mat_US, 
                     target_maturity = maturity)$target_data
UK <- match_maturity(data = UK, 
                     original_maturity = mat_UK, 
                     target_maturity = maturity)$target_data
JP <- match_maturity(data = JP, 
                     original_maturity = mat_JP, 
                     target_maturity = maturity)$target_data
FR <- match_maturity(data = FR, 
                     original_maturity = mat_FR, 
                     target_maturity = maturity)$target_data
DE <- match_maturity(data = DE, 
                     original_maturity = mat_DE, 
                     target_maturity = maturity)$target_data
IT <- match_maturity(data = IT, 
                     original_maturity = mat_IT, 
                     target_maturity = maturity)$target_data
AU <- match_maturity(data = AU, 
                     original_maturity = mat_AU, 
                     target_maturity = maturity)$target_data

# In-sample and out-of-sample data
US_in <- US %>%
  filter(as.Date(rownames(US)) >= as.Date("2010-01-01") & as.Date(rownames(US)) <= as.Date("2019-12-31"))
US_out <- US %>%
  filter(as.Date(rownames(US)) >= as.Date("2020-01-01") & as.Date(rownames(US)) <= as.Date("2020-12-31"))

UK_in <- UK %>%
  filter(as.Date(rownames(UK)) >= as.Date("2010-01-01") & as.Date(rownames(UK)) <= as.Date("2019-12-31"))
UK_out <- UK %>%
  filter(as.Date(rownames(UK)) >= as.Date("2020-01-01") & as.Date(rownames(UK)) <= as.Date("2020-12-31"))

JP_in <- JP %>%
  filter(as.Date(rownames(JP)) >= as.Date("2010-01-01") & as.Date(rownames(JP)) <= as.Date("2019-12-31"))
JP_out <- JP %>%
  filter(as.Date(rownames(JP)) >= as.Date("2020-01-01") & as.Date(rownames(JP)) <= as.Date("2020-12-31"))

FR_in <- FR %>%
  filter(as.Date(rownames(FR)) >= as.Date("2010-01-01") & as.Date(rownames(FR)) <= as.Date("2019-12-31"))
FR_out <- FR %>%
  filter(as.Date(rownames(FR)) >= as.Date("2020-01-01") & as.Date(rownames(FR)) <= as.Date("2020-12-31"))

DE_in <- DE %>%
  filter(as.Date(rownames(DE)) >= as.Date("2010-01-01") & as.Date(rownames(DE)) <= as.Date("2019-12-31"))
DE_out <- DE %>%
  filter(as.Date(rownames(DE)) >= as.Date("2020-01-01") & as.Date(rownames(DE)) <= as.Date("2020-12-31"))

IT_in <- IT %>%
  filter(as.Date(rownames(IT)) >= as.Date("2010-01-01") & as.Date(rownames(IT)) <= as.Date("2019-12-31"))
IT_out <- IT %>%
  filter(as.Date(rownames(IT)) >= as.Date("2020-01-01") & as.Date(rownames(IT)) <= as.Date("2020-12-31"))

AU_in <- AU %>%
  filter(as.Date(rownames(AU)) >= as.Date("2010-01-01") & as.Date(rownames(AU)) <= as.Date("2019-12-31"))
AU_out <- AU %>%
  filter(as.Date(rownames(AU)) >= as.Date("2020-01-01") & as.Date(rownames(AU)) <= as.Date("2020-12-31"))

EU_in <- EU %>%
  filter(as.Date(rownames(EU)) >= as.Date("2010-01-01") & as.Date(rownames(EU)) <= as.Date("2019-12-31"))
EU_out <- EU %>%
  filter(as.Date(rownames(EU)) >= as.Date("2020-01-01") & as.Date(rownames(EU)) <= as.Date("2020-12-31"))

# Save data
#write.csv(UK_in, "data_matlab/UK12Con.csv")
#write.csv(UK_out, "data_matlab/UK12Con_Out.csv")

# Data for stress testing
# Stress testing 1: temporary shock
index <- which(as.Date(rownames(US_in)) >= as.Date("2015-01-01") & 
                 as.Date(rownames(US_in)) < as.Date("2016-01-01"))

# Case 1.1: short-end maturity 
contracts <- 1: 8
US_st1_1 <- US_in
US_st1_1[index, contracts] <- US_st1_1[index, contracts] * 2

# Case 1.2: middle 
contracts <- 9: 10
US_st1_2 <- US_in
US_st1_2[index, contracts] <- US_st1_2[index, contracts] * 2

# Case 1.3: long-end maturity
contracts <- 11: 12
US_st1_3 <- US_in
US_st1_3[index, contracts] <- US_st1_3[index, contracts] * 2

# Case 1.4: entire curve
contracts <- 1: 12
US_st1_4 <- US_in
US_st1_4[index, contracts] <- US_st1_4[index, contracts] * 2

# Plot 
US_st1_1 %>%
  pivot_longer(cols = 1: dim(US_in)[2], names_to = "Contracts", values_to = "Price") %>%
  mutate( Date = rep(as.Date(rownames(US_in)), each = length(maturity)) ) %>%
  plot_ly(x = ~Date, y = ~Price, type = "scatter", mode = "lines", color = ~Contracts) %>%
  layout(title = "Time series of US bond yields", xaxis = list(title = "Date"))

# Stress testing 2: permanent shock 
index <- which(as.Date(rownames(US_in)) >= as.Date("2015-01-01"))

# Case 2.1: short-end maturity
contracts <- 1: 8
US_st2_1 <- US_in
US_st2_1[index, contracts] <- US_st2_1[index, contracts] * 2

# Case 2.2: middle
contracts <- 9: 10
US_st2_2 <- US_in
US_st2_2[index, contracts] <- US_st2_2[index, contracts] * 2

# Case 2.3: long-end maturity
contracts <- 11: 12
US_st2_3 <- US_in
US_st2_3[index, contracts] <- US_st2_3[index, contracts] * 2

# Case 2.4: entire curve
contracts <- 1: 12
US_st2_4 <- US_in
US_st2_4[index, contracts] <- US_st2_4[index, contracts] * 2

# Plot
US_st2_1 %>%
  pivot_longer(cols = 1: dim(US_in)[2], names_to = "Contracts", values_to = "Price") %>%
  mutate( Date = rep(as.Date(rownames(US_in)), each = length(maturity)) ) %>%
  plot_ly(x = ~Date, y = ~Price, type = "scatter", mode = "lines", color = ~Contracts) %>%
  layout(title = "Time series of US bond yields", xaxis = list(title = "Date"))

# Time series plot
dat <- US_st2_4
mat_names <- c("1 month", "3 months", "6 months", "9 months", 
                   "1 year", "2 years", "3 years", "5 years", 
                   "7 years", "10 years", "20 years", "30 years")
colnames(dat) <- mat_names

colors = c(rgb(254,224,210, maxColorValue = 255), 
           rgb(252,187,161, maxColorValue = 255),
           rgb(252,146,114, maxColorValue = 255),
           rgb(251,106,74, maxColorValue = 255),
           rgb(239,59,44, maxColorValue = 255),
           rgb(203,24,29, maxColorValue = 255),
           rgb(165,15,21, maxColorValue = 255),
           rgb(103,0,13, maxColorValue = 255),
           rgb(158,202,225, maxColorValue = 255),
           rgb(49,130,189, maxColorValue = 255),
           rgb(161,217,155, maxColorValue = 255),
           rgb(49,163,84, maxColorValue = 255))

dat %>%
  pivot_longer(cols = 1: dim(dat)[2], names_to = "Maturity", values_to = "Price") %>%
  mutate( Date = rep(as.Date(rownames(dat)), each = length(maturity)) ) %>%
  mutate(Maturity = factor(Maturity, levels = mat_names)) %>%
  ggplot(aes(x = Date, y = Price, color = Maturity)) +
  geom_line() + 
  theme_classic() + 
  scale_color_manual(values = colors) + 
  xlab("Date") + 
  ylab("Yield") +
  ylim(-0.5, 7)

###########################
##### KPCA on US data #####
###########################
dat_kpca <- US_in # data for KPCA
use_python("/Users/HPL/anaconda3/bin/python3") # select python version
pd <- import("pandas")
np <- import("numpy")
sk_dec <- import("sklearn.decomposition")
sk_met <- import("sklearn.metrics")
sk_ms <- import("sklearn.model_selection")

# Estimate hyper-parameters
score <- function(estimator, X, Y = NULL) {
  X_reduced <- estimator$fit_transform(X)
  X_preimage <- estimator$inverse_transform(X_reduced)
  return(-sk_met$mean_squared_error(X, X_preimage))
}

param_grid <- list(gamma = seq(from = 0.001, to = 1, by = 0.001))

Q <- 3 # number of factors. (max = 12)
kpca_machine <- sk_dec$KernelPCA(kernel = "rbf",  
                                 n_components = as.integer(Q), 
                                 fit_inverse_transform = TRUE)

set.seed(1234)
grid_search <- sk_ms$GridSearchCV(kpca_machine, 
                                  param_grid, 
                                  cv = as.integer(3), scoring = score)
hp_fit <- grid_search$fit(t(as.matrix(dat_kpca))) # hyper-parameter 

hp_fit$best_params_

# Extract factors
Q <- 3
U <- kpca(data = dat_kpca, kernel = "rbf", gamma = hp_fit$best_params_$gamma, Q = Q)$U

# heatmap of U
colnames(U) <- paste("PC", 1: Q, sep = "")
U %>%
  as.data.frame() %>%
  gather(key = "PC", value = "values") %>%
  ggplot(mapping = aes(y = rep(as.Date(rownames(dat_kpca)), Q), x = PC, fill = values)) + 
  geom_tile() + 
  ylab("Time") + 
  ggtitle("Heatmap of U")

# Save factors
#write.csv(U, "data_matlab/US_5Factors.csv", row.names = FALSE)

##################################
##### Reconstruct US factors #####
##################################
US_fore <- as.matrix(read.csv("data_r/US_DNS_Prediction_st1_4.csv", header = FALSE)) # US yields predicted by DNS model
rownames(US_fore) <- rownames(US_out)
colnames(US_fore) <- colnames(US_out)
US_fore <- rbind(US_in, US_fore) # combine in-sample data and predicted data

U_recon <- kpca(data = US_fore, kernel = "rbf", gamma = hp_fit$best_params_$gamma, Q = 3)$U # reconstructed factors

# Save data
#write.csv(U_recon, "data_matlab/US_3Factors_Reconstructed.csv", row.names = FALSE)

###################################
##### Functional coefficients #####
###################################
# Original: 0.083
# st1_4: 0.081
# st2_4: 0.037 
gamma <- as.matrix(read.csv("data_r/Coe/coe_EU_st2_4.csv", header = FALSE))
phi_tilde_t <- kpca(data = US_in, kernel = "rbf", gamma = 0.037, Q = 3)$phi_tilde_t
gamma_functional <- as.data.frame(t(gamma %*% phi_tilde_t))
colnames(gamma_functional) <- paste("Contract", 1: dim(UK)[2])

colors = c(rgb(254,224,210, maxColorValue = 255), 
           rgb(252,187,161, maxColorValue = 255),
           rgb(252,146,114, maxColorValue = 255),
           rgb(251,106,74, maxColorValue = 255),
           rgb(239,59,44, maxColorValue = 255),
           rgb(203,24,29, maxColorValue = 255),
           rgb(165,15,21, maxColorValue = 255),
           rgb(103,0,13, maxColorValue = 255),
           rgb(158,202,225, maxColorValue = 255),
           rgb(49,130,189, maxColorValue = 255),
           rgb(161,217,155, maxColorValue = 255),
           rgb(49,163,84, maxColorValue = 255))

gamma_functional %>% 
  pivot_longer(cols = 1: dim(UK)[2], names_to = "Contract", values_to = "Values") %>%
  mutate(US_maturity = rep(maturity, each = dim(UK)[2])) %>% 
  mutate(Contract = factor(Contract, levels = paste("Contract", 1: dim(UK)[2]))) %>%
  ggplot(aes(x = US_maturity, y = Values, color = Contract)) + 
  geom_line() + 
  theme_classic(base_size = 20) + 
  #theme(legend.text = element_text(size = 14),
  #      legend.title = element_text(size = 10)) +
  scale_color_manual(labels = c(expression(gamma[1](tau)), 
                                expression(gamma[2](tau)), 
                                expression(gamma[3](tau)),
                                expression(gamma[4](tau)),
                                expression(gamma[5](tau)),
                                expression(gamma[6](tau)),
                                expression(gamma[7](tau)),
                                expression(gamma[8](tau)),
                                expression(gamma[9](tau)),
                                expression(gamma[10](tau)),
                                expression(gamma[11](tau)),
                                #expression(gamma[12](tau))), values = hue_pal()(12)) + 
                                expression(gamma[12](tau))), values = colors) + 
  xlab("Time to maturity") + 
  labs(color = expression(paste(gamma, " function")))
  #ylim(-0.4, 0.6)
  #ylim(-0.5, 0.5)
  #ylim(-0.8, 0.8)
  #ylim(-0.4, 0.4)
  #ylim(-0.8, 1.2)
  #ylim(-0.7, 1)
  #ylim(-0.3, 0.4)
  #ylim(-3, 1.5)

gamma_functional %>%
  gather(key = "Contract", value = "Values") %>%
  mutate(US_maturity = rep(maturity, times = dim(UK)[2])) %>%
  plot_ly(x = ~US_maturity, y = ~Values, type = "scatter", mode = "lines", color = ~Contract) %>%
  layout(title = "Functional coefficients", xaxis = list(title = "US Contract"))

#########################
##### Moving window #####
#########################
date <- rownames(US)

width <- 60 # width of moving windos
n_period <- 12 # number of forecasting periods
n_window <- dim(US)[1] - n_period - width + 1

Q <- 3

for (i in 1: n_window) {
  US_fore <- as.matrix(read.csv(paste("data_r/mw/", i, ".csv", sep = ""), header = FALSE))
  rownames(US_fore) <- date[(i+width): (i+width+n_period-1)]
  colnames(US_fore) <- colnames(US)
  US_fore <- rbind(US[i: (i+width-1), ], US_fore)
  
  U <- kpca(data = US_fore, kernel = "rbf", gamma = hp_fit$best_params_$gamma, Q = 3)$U
  
  # Save data
  #write.csv(U, paste("data_matlab/mw_U/Recon", i, ".csv", sep = ""), row.names = FALSE)
}

