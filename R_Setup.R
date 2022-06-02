########## R SETUP ###########

setwd("C:/Users/kspin/Documents/R Outputs/Projects/SDS_6/MA1_Estimation")

require("pacman")
pacman::p_load(boot,latex2exp,tseries,ggplot2)

#automatic install of packages if they are not installed already
list.of.packages <- c(
  "foreach",
  "doParallel",
  "ranger",
  "palmerpenguins",
  "tidyverse",
  "kableExtra"
)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages) > 0){
  install.packages(new.packages, dep=TRUE)
}

#loading packages
for(package.i in list.of.packages){
  suppressPackageStartupMessages(
    library(
      package.i, 
      character.only = TRUE
    )
  )
}


parallel::detectCores()

n.cores <- parallel::detectCores() - 1

#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)

#check cluster definition (optional)
print(my.cluster)

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

#check if it is registered (optional)
foreach::getDoParRegistered()

foreach::getDoParWorkers()


#TO CLOSE CLUSTER, parallel::stopCluster(cl = my.cluster)
