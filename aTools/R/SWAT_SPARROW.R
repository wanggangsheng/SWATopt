library(agricolae)
setwd("/Users/wg4/Dropbox (ORNL)/Rcode")
rm(list=ls())
source("wgs.R")
datafile <- "SWAT_SPARROW_TN.txt"
# datafile <- "SWAT_SPARROW_TP.txt"
data1 <- read.table(datafile,header = TRUE)

colName <- colnames(data1)
iXY <- c(1,2)
treatment = colName[iXY[1]]
response = colName[iXY[2]]
model1 <- eval(bquote(aov(.(as.name(response)) ~ .(as.name(treatment)), data = data1)))
summary(model1)

main1 = paste(response,"\ndealt with different ", treatment)
LSD1 <- LSD1.test(model1,treatment, p.adj="bonferroni",main=main1,group=TRUE)


attach(data1)
KW <- eval(bquote(KW.test(.(as.name(response)), .(as.name(treatment)), probs = 0.05, cont = NULL)))
