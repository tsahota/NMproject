## Author: klgk669
## First created: 2018-03-20
## Description: Display $COV results
## Keywords: 

########################################
## load packages and source functions here

library(NMproject)
library(ggplot2)
source("Scripts/read.table.nm.R")


########################################
## main script here

covariance_result <- function(r,trans=TRUE){
  dc <- read.table.nm(r$output$psn.cor,header = TRUE,skip=1)
  
  names(dc)[1] <- "Var1"
  dc$Var1 <- names(dc)[-1]
  
  n_ests <- nrow(dc)/length(unique(dc$Var1))
  
  dc$EST.NO <- rep(1:n_ests,each=length(unique(dc$Var1)))
  dc <- dc %>% dplyr::filter(EST.NO == max(EST.NO))
  dc$EST.NO <- NULL
  
  dc <- dc %>% tidyr::gather(key = "Var2", value="value",-Var1)
  
  dc$Var1 <- factor(dc$Var1)
  dc$Var2 <- factor(dc$Var2)
  
  if(trans){
    dp <- r$param_info
    current_levels <- levels(dc$Var1)
 
  dl <- data.frame(cl = current_levels)
    dl$ORD <- 1:nrow(dl)
    dp <- dp[,c("Name","Parameter")]
    names(dp)[2] <- "cl"
    dl <- merge(dp,dl,all = TRUE)
    dl$new_names <- dl$cl
    dl$new_names[!is.na(dl$Name)] <- paste0(dl$Name[!is.na(dl$Name)]," (",
                                            dl$cl[!is.na(dl$Name)],")")
    levels(dc$Var1) <- dl$new_names
    levels(dc$Var2) <- dl$new_names
    
  }
  
  dc <- dc %>% dplyr::filter(!value %in% 0)
  dc <- dc %>% dplyr::filter(as.numeric(Var1) > as.numeric(Var2)) ## lower corner
  
  p <- ggplot(dc, aes(x= Var1, y= Var2, fill = value)) + theme_bw() +
    geom_tile() + 
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Correlation") +
    geom_text(aes(label = round(value,2))) +
    theme(axis.text.x  = element_text(angle=90,vjust=0))
  
  file_name <- paste0("Results/covariance_plot_",basename(r$ctl),".pdf")
  pdf(file_name,width = 7,height=5)
  print(p)
  dev.off()
  
  return(file_name)
  
}


cov_matrix_nm <- function(r){
  source("Scripts/read.table.nm.R")
  dc <- read.table.nm(r$output$psn.cov,header = TRUE,skip=1)
  par_names <- names(dc)[-1]
  dc <- dc[,-1]
  
  nvars <- ncol(dc)
  dc <- dc[(nrow(dc)-nvars+1):nrow(dc),]
  
  dc <- as.matrix(dc)
  row.names(dc) <- par_names
  dc
}
