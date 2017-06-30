# setwd("/Users/wg4/Dropbox (ORNL)/Rcode")

# source("wgs.R")
library(ggplot2)
library(ggthemes)
library(stringr)
rm(list=ls())
path <- getwd()
print(path)

dir_io <- "SWAT_TRB"

# color1 <- c("cornflowerblue","darkgreen")
color1 <- c("cornflowerblue","greenyellow")
color2 <- c("cornflowerblue","blue","greenyellow","darkgreen")
color3 <- c("cyan","cornflowerblue","blue","greenyellow","green","darkgreen")
color4 <- c("cyan","skyblue","cornflowerblue","blue","greenyellow","lightgreen","green","darkgreen")
color5 <- c("cyan","skyblue","darkcyan","cornflowerblue","blue","greenyellow","lightgreen","yellow","green","darkgreen")



# cex.axis2 <- 0.7
# color_use <- color10
##------------------------------------------------

##------------------------------------------------
# xlab1 <- paste0("Percent Change\n","Post-bioenergy BC1 vs Pre-bioenergy Baseline")
# file_par0 <- "SWAT_output_sub_BC1vsBase.csv"

xlab1 <- paste0("Percent Change\n","Post-bioenergy HH3 vs Pre-bioenergy Baseline")
file_par0 <- "SWAT_output_sub_HH3vsBase.csv"

file_name <- str_sub(file_par0,1,nchar(file_par0)-4)
##------------------------------------------------
path_io <- paste0(path, "/", dir_io)
file_par <- paste0(path_io,"/", file_par0)

path_out <- path_io

print(file_par)
par <- read.table(file_par, sep=",", header=TRUE)

names_col <- colnames(par)
print(names_col)

# Measure <- unique(par$Measure) #c("Loading", "Concentration")
# VAR  <- unique(par$Variable) #c("Water", "TSS", "TP","TN","NO3")

# nVAR <- length(VAR)
# nMeasure <- length(Measure)
# nlabels <- nVAR*nMeasure
# ylabels <- vector("list", length=nlabels)
# for(i in 1:nVAR){
# 	for(j in 1:nMeasure){
# 		k = (i-1)*nMeasure + j
# 		ylabels[k] = paste0(VAR[i],"_",Measure[j])
# 	}
# }


fn <- paste0(path_out,"/",file_name,".pdf")
pdf(fn)

# coord_flip: horizontal
 sp <- eval(bquote(ggplot(par, aes(x=.(as.name(names_col[2])), y=.(as.name(names_col[4])), fill=.(as.name(names_col[1])))) + 
    geom_boxplot() + coord_flip(ylim=c(-200,200))))

 # sp <- ggplot(par, aes(x=names_col[2], y=Percent_Change, fill=names_col[1])) + 
 #    geom_boxplot() + coord_flip(ylim=c(-200,200)) 

# Set tick marks on y axis
# a tick mark is shown on every 5
sp <- sp + scale_fill_manual(values=color1) + scale_y_continuous(breaks=seq(-200,200,50)) + labs(y=xlab1)
print(sp)
print(fn)
dev.off()
# =========================================

# ggplot(par, aes(x=Variable, y=Percent_Change, fill=Measure)) + 
#     geom_boxplot()	



# par(mar=c(5,5,4,1))

# boxplot(Percent_Change~Measure+Variable,data=par,
# 		xlab=xlab1, col=color1,cex.axis=cex.axis2,horizontal=TRUE, ylim=c(-200,200),axes=FALSE)

# boxplot(Percent_Change~Measure+Variable,data=par,
# 		xlab=xlab1, col=color_use, cex.axis=cex.axis2,horizontal=TRUE, ylim=c(-200,200),axes=FALSE)

# box(lty="solid",col="black")
# abline(v=xticks,lty="dashed", lwd=1,col="gray")

# # yat <- pretty(y)
# xticks <- seq(-200, 200, by=50)
# axis(side=1, at=xticks)

# by2 <- 3
# min2 <- 1.5
# max2 <- min2 + by2*(nVAR-1)
# axis(side=2, at=seq(min2,max2,by=by2), labels=VAR[nVAR:1],las=2)

# #Add a legend
# legend("topleft", legend = c("Loading","Concentration") , 
# col = color1 , bty = "n", pch=20 , pt.cex = 3, cex = 1, horiz = FALSE, inset = c(0.03, 0.1))
 





## RUN all SITE x VAR
##------------------------------------------------
# for(iSITE in 1:length(SITE)){
# 	for(jVAR in 1:length(VAR)){

# for(iSITE in 2:2){
# 	for(jVAR in 2:2){
##------------------------------------------------

# prefix_io <- paste0(SITE[iSITE],"_",VAR[jVAR])
# print("=============================================")
# print(prefix_io)
# print("=============================================")
# if(iSITE==1){
# 	color_use <- color3
# } else {
# 		color_use <- color5
# 	}


# if(jVAR==1){ ## "VAR"
# 	npar<- 15  ## TM_C ~ ENZM
# 	n0 <- 3
# 	par_log <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)  ## log-transformation, 1=yes, 0=no

# 	par_label <- vector("list", length=npar)
# 	par_label[1] <- expression(paste(italic(TC)," [mg C ", cm^-3,"]"))
# 	par_label[2] <- expression(paste(italic(TOC)," [mg C ", cm^-3,"]"))
# 	par_label[3] <- expression(paste(italic(SOC)," [mg C ", cm^-3,"]"))
# 	par_label[4] <- expression(paste(italic(P[1])," [mg C ", cm^-3,"]"))
# 	par_label[5] <- expression(paste(italic(P[2])," [mg C ", cm^-3,"]"))
# 	par_label[6] <- expression(paste(italic(M)," [mg C ", cm^-3,"]"))
# 	par_label[7] <- expression(paste(italic(Q)," [mg C ", cm^-3,"]"))
# 	par_label[8] <- expression(paste(italic(D)," [mg C ", cm^-3,"]"))
# 	par_label[9] <- expression(paste(italic(B)," [mg C ", cm^-3,"]"))
# 	par_label[10] <- expression(paste(italic(BA)," [mg C ", cm^-3,"]"))
# 	par_label[11] <- expression(paste(italic(BD)," [mg C ", cm^-3,"]"))
# 	par_label[12] <- expression(paste(italic(E)," [mg C ", cm^-3,"]"))
# 	par_label[13] <- expression(paste(italic(EP[1])," [mg C ", cm^-3,"]"))
# 	par_label[14] <- expression(paste(italic(EP[2])," [mg C ", cm^-3,"]"))
# 	par_label[15] <- expression(paste(italic(EM)," [mg C ", cm^-3,"]")) 

# 	if(iSITE==1){
# 		file_par0 <- "MENDout_BF_10cm_100yr_Var.csv"
		
# 		} else {
# 			file_par0 <- "MENDout_PF_10cm_100yr_Var_D+.csv"
# 		}


# 	} else if(jVAR==2) { ## "RATE"
# 	npar<- 13  ## kPOM1 ~ CUE
# 	n0 <- 3
# 	par_log <- c(1,1,1,1,1,1,1,1,1,1,1,0,0)  ## log-transformation, 1=yes, 0=no
# 	par_label <- vector("list", length=npar)
# 	par_label[1] <- expression(italic(k[P1]))
# 	par_label[2] <- expression(italic(k[P2]))
# 	par_label[3] <- expression(italic(k[M]))
# 	par_label[4] <- expression(italic(k[D]))
# 	par_label[5] <- expression(italic(k[BA]))
# 	par_label[6] <- expression(italic(k[BA_in]))
# 	par_label[7] <- expression(italic(k[BD]))
# 	par_label[8] <- expression(italic(k[BD_in]))
# 	par_label[9] <- expression(italic(k[B]))  
# 	par_label[10] <- expression(italic(k[B_in]))
# 	par_label[11] <- expression(italic(phi))
# 	par_label[12] <- expression(paste("Active fraction of microbes (", italic(r),")"))  ## r
# 	par_label[13] <- expression(paste("Carbon use efficiency (", italic(CUE),")")) 

# 		if(iSITE==1){
# 			file_par0 <- "MENDout_BF_10cm_100yr_Rate.csv"
# 		} else {
# 			file_par0 <- "MENDout_PF_10cm_100yr_Rate_D+.csv"
# 		}

# 	} else { ## "FLUX"
# 	npar<- 38  ## TM_C ~ ENZM
# 	n0 <- 3
# 	par_log <- c(rep(0,npar))  ## log-transformation, 1=yes, 0=no
# 	par_label <- vector("list", length=npar)

# 		if(iSITE==1){
# 			file_par0 <- "MENDout_BF_10cm_100yr_Flux.csv"
# 		} else {
# 			file_par0 <- "MENDout_PF_10cm_100yr_Flux_D+.csv"
# 		}

# 	} ##if(jVAR==1)



	## Mann-Whitney U Test/Wilcoxon rank sum test (wilcox.test)
	## Kruskal-Wallis Test:
	# if(iSITE==1 & par_name[j]=="CUE") {
	# 	diff2 <- eval(bquote(kruskal.test(.(as.name(par_name[j]))~Season,data=par[par$CUE>=-0.2,]))) 
	# } else {
	# 	diff2 <- eval(bquote(kruskal.test(.(as.name(par_name[j]))~Season,data=par))) 
	# }
	# pvalue_season <- diff2$p.value
	# print(diff2)

	# cat("Dry:\n")
	# attach(par_dry)
	# diff1_dry <- eval(bquote(KW.test(.(as.name(par_name[j])),Scenario,probs = 0.05, cont = NULL))) 
	# detach(par_dry)
	# print(diff1_dry)

	# model1_dry <- eval(bquote(aov(.(as.name(par_name[j])) ~ Scenario, data = par_dry)))
	# # summary(model1_dry)
	# LSD1_dry <- LSD1.test(model1_dry,"Scenario", p.adj="bonferroni",main=par_name[j],group=TRUE)
	# diff1_letter_dry<-KW.group(LSD1_dry,diff1_dry)
	# LSD1_dry$M <- diff1_letter_dry
	# KW_diff_dry <- LSD1_dry[order(LSD1_dry[,1]),]
	# print(KW_diff_dry)
	

	# diff1_dry <- eval(bquote(kruskal.test(.(as.name(par_name[j]))~Scenario,data=par_dry))) 
	# pvalue_dry <- diff1_dry$p.value
	# # print(diff1_dry$p.value)
	
	# cat("Wet:\n")
	# attach(par_wet)
	# diff1_wet <- eval(bquote(KW.test(.(as.name(par_name[j])),Scenario,probs = 0.05, cont = NULL))) 
	# detach(par_wet)
	# print(diff1_wet)

	# model1_wet <- eval(bquote(aov(.(as.name(par_name[j])) ~ Scenario, data = par_wet)))
	# # summary(model1_dry)
	# LSD1_wet <- LSD1.test(model1_wet,"Scenario", p.adj="bonferroni",main=par_name[j],group=TRUE)
	# diff1_letter_wet<-KW.group(LSD1_wet,diff1_wet)
	# LSD1_wet$M <- diff1_letter_wet
	# KW_diff_wet <- LSD1_wet[order(LSD1_wet[,1]),]
	# print(KW_diff_wet)
	# KW_diff_letter <- c(KW_diff_dry$M,toupper(KW_diff_wet$M))
	# print(KW_diff_letter)
	# nLetter <- length(KW_diff_letter)

	# diff1_wet <- eval(bquote(kruskal.test(.(as.name(par_name[j]))~Scenario,data=par_wet))) 
	# pvalue_wet <- diff1_wet$p.value
	# print(diff1_wet$p.value)

	# bp1 <- eval(bquote(boxplot(Percent_Change~Measure+Variable,data=par, plot=0)))

	

	# if(iSITE==1 & par_name[j]=="CUE") {
	# 	eval(bquote(boxplot(.(as.name(par_name[j]))~Scenario+Season,data=par[par$CUE>=-0.2,],
	# 		xlab=xlab1,ylab=par_label[i], col=color_use, cex.axis=cex.axis2)))
	# 	} else { ## !=CUE
	# if(par_log[i]==0){
	# 	eval(bquote(boxplot(.(as.name(par_name[j]))~Scenario+Season,data=par,
	# 		xlab=xlab1,ylab=par_label[i], col=color_use, cex.axis=cex.axis2)))   #names=paste(bp1$names,"\n",KW_diff_letter)
	# 	} else {
	# 	eval(bquote(boxplot(.(as.name(par_name[j]))~Scenario+Season,data=par,
	# 			xlab=xlab1,ylab=par_label[i], col=color_use, cex.axis=cex.axis2, log="y")))
	# 	}
	# 	} ## if(par_name[j]=="CUE")
	# title1 <- paste0(SITE[iSITE]," Site; Kruskal-Wallis Test:\n",
	# 	"Factor = Season       (i.e., Dry/Wet): p-value = ",round(diff2$p.value,2),"\n",
	# 	"Factor = Scenario (in Dry Season): p-value = ",round(diff1_dry$p.value,2),"\n",
	# 	"Factor = Scenario (in Wet Season): p-value = ",round(diff1_wet$p.value,2))

	# title(main = title1, adj=0, cex.main=0.85)

	# text(x=seq_along(KW_diff_letter)[1:nLetter/2],y=max(bp1$stats),KW_diff_letter[1:nLetter/2], cex=1, col="blue")
	# text(x=seq_along(KW_diff_letter)[nLetter/2+1:nLetter],y=max(bp1$stats),KW_diff_letter[nLetter/2+1:nLetter], cex=1, col="darkgreen")
	
	
# } ## for(i in 1:npar)
##------------------------------------------------
# } ##for(jVAR in 1:length(VAR))
# } ##for(iSITE in 1:2)
##------------------------------------------------
