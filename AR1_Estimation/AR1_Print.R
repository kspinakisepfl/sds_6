#If you already have the necessary data, use this file to print it

#Setup
setwd("C:/Users/kspin/Documents/R Outputs/Projects/SDS_6/AR1_Estimation")
require("pacman")
pacman::p_load(boot,latex2exp,tseries,ggplot2,cowplot)


#Sources
source("AR1_Printer_fct.R")
source("AR1_Printer_fct_MOD.R")





load("C:/Users/kspin/Documents/R Outputs/Projects/SDS_6/AR1_Estimation/usable_data/Results_200_1k_20_181_10iters.RData")

AR1_Printer_fct(Result, 25, 30, "blue", "red", "purple", "black")

load("C:/Users/kspin/Documents/R Outputs/Projects/SDS_6/AR1_Estimation/usable_data/Results_2k_1k_20_181_10iters.RData")

AR1_Printer_fct(Result, 25, 30, "blue", "red", "purple", "black")



#If the data is modified, use this

load("C:/Users/kspin/Documents/R Outputs/Projects/SDS_6/AR1_Estimation/usable_data/Results_MODIFIED_200_1k_10_181_10iters.RData")
Result1 <- Result
load("C:/Users/kspin/Documents/R Outputs/Projects/SDS_6/AR1_Estimation/usable_data/Results_200_1k_50_181_10iters.RData")
Result2 <- Result

AR1_Printer_fct_MOD(Result1, Result2, 25, 30, "purple", "black")