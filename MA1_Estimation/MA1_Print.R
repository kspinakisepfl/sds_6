#If you already have the necessary data, use this file to print it

#Setup
setwd("C:/Users/kspin/Documents/R Outputs/Projects/SDS_6/MA1_Estimation")
require("pacman")
pacman::p_load(boot,latex2exp,tseries,ggplot2)


#Sources
source("MA1_Printer_fct.R")
source("MA1_Printer_fct_MOD.R")





load("C:/Users/kspin/Documents/R Outputs/Projects/SDS_6/MA1_Estimation/usable_data/Results_200_1k_20_181_10iters.RData")

MA1_Printer_fct(Result, 25, 30, "blue", "red", "purple", "black")




#If the data is modified, use this

load("C:/Users/kspin/Documents/R Outputs/Projects/SDS_6/MA1_Estimation/usable_data/Results_MODIFIED_200_1k_10_181_10iters.RData")
Result1 <- Result
load("C:/Users/kspin/Documents/R Outputs/Projects/SDS_6/MA1_Estimation/usable_data/Results_MODIFIED_200_1k_50_181_10iters.RData")
Result2 <- Result

MA1_Printer_fct_MOD(Result1, Result2, 25, 30, "purple", "black")