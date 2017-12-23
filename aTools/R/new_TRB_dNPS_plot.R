# setwd("/Users/wg4/Dropbox (ORNL)/Model/SWATopt/aTools/R")
# source("new_TRB_dNPS_plot.R")

# source("wgs.R")
library(ggplot2)
library(ggthemes)
library(stringr)
rm(list=ls())
path <- getwd()
print(path)


dir_io <- "SWAT_TRB"
dir_inout <- c("in_data", "out_data", "out_plot")
path_io <- paste0(path, "/", dir_io)
path_in  <- paste0(path_io, "/", dir_inout[1])
path_out <- paste0(path_io, "/", dir_inout[2])
path_fig <- paste0(path_io, "/", dir_inout[3])

# color1 <- c("cornflowerblue","darkgreen")
color1 <- c("cornflowerblue","greenyellow")
color2 <- c("cornflowerblue","blue","greenyellow","darkgreen")
color3 <- c("cyan","cornflowerblue","blue","greenyellow","green","darkgreen")
color4 <- c("cyan","skyblue","cornflowerblue","blue","greenyellow","lightgreen","green","darkgreen")
color5 <- c("cyan","skyblue","darkcyan","cornflowerblue","blue","greenyellow","lightgreen","yellow","green","darkgreen")

Qvar <- c("ET","WYLD", "SURQ", "LATQ", "TGWQ", "SGWQ", "DGWQ")
Pvar <- c("TSS", "TP", "ORGP", "MINP", "SOLP", "SEDP")
Nvar <- c("TN", "ORGN", "NO3", "SURNO3", "LATNO3", "GWNO3")
Cvar <- c("FLOW", "TSS", "TP", "ORGP", "MINP", "TN", "ORGN", "NO3")  #concentration
vars <- list(Qvar,Pvar,Nvar,Cvar)

# cex.axis2 <- 0.7
# color_use <- color10
##------------------------------------------------
## Input data files:
xlab <- c(rep(paste0("Percent Change in Loading"),3),
			   paste0("Percent Change in Concentration"))
file_in <-  c("Load_diff_perc_Rplot_Q.txt",
			  "Load_diff_perc_Rplot_P.txt",
			  "Load_diff_perc_Rplot_N.txt",
			  "Conc_diff_perc_Rplot.txt")
nplot <- length(file_in)

ymax0 	<- c(15, 50, 100, 75)
ybreak0 <- c( 5, 10,  25, 25)

fn_ttest <- c("Load_diff_ttest_pvalue.txt", "Conc_diff_ttest_pvalue.txt")
fi_ttest <- paste0(path_out,"/", fn_ttest)
Load_diff_ttest_pvalue <- read.table(fi_ttest[1], sep="", header=TRUE)
Conc_diff_ttest_pvalue <- read.table(fi_ttest[2], sep="", header=TRUE)
##------------------------------------------------

for (i in 1:nplot){
	xlab1 <- xlab[i]
	file_par0 <- file_in[i]
	ymax <- ymax0[i]
	ybreak <- ybreak0[i]

	file_name <- str_sub(file_par0,1,nchar(file_par0)-4)
	##------------------------------------------------
	file_par <- paste0(path_out,"/", file_par0)
	print(file_par)
	# par <- read.table(file_par, sep=",", header=TRUE)
	par <- read.table(file_par, sep="", header=TRUE)
	# par$Scenario <- factor(par$Scenario, levels=rev(par$Scenario))
	par$Variable <- factor(par$Variable, levels=rev(vars[[i]]))   # plot variables from top to bottom
    scen <- levels(par$Scenario)
	names_col <- colnames(par)
	print(names_col)
	# "Scenario"       "Variable"       "Subbasin"       "Percent_Change"

	fn <- paste0(path_fig,"/",file_name,".pdf")
	pdf(fn)

	# coord_flip: horizontal
	sp <- eval(bquote(ggplot(par, 
							aes(x=.(as.name(names_col[2])), 
								y=.(as.name(names_col[4])), 
								fill=.(as.name(names_col[1])))) + 
	    					geom_boxplot() + 
	    					coord_flip(ylim=c(-ymax,ymax)) ))  ## flip x & y

	# Set tick marks on y axis
	# a tick mark is shown on every 5
	
	sp <- sp + 
			scale_fill_manual(values=color1) + 
			scale_y_continuous(breaks=seq(-ymax,ymax,ybreak)) + 
			labs(y=xlab1) + 
			geom_hline(yintercept = 0, color = "red", linetype="dashed", size=0.5) +
			guides(fill = guide_legend(reverse=TRUE))  ## reverse legend to match the orders of boxplots
			
	#--------------------------------------------------------
	# plot significance level for diff	
	if(substr(file_par0,1,4)=="Load"){
		pvalue0 <- Load_diff_ttest_pvalue
	} else {
		pvalue0 <- Conc_diff_ttest_pvalue
	}
	pvalue1 <- pvalue0[,eval(c("Scenario",vars[[i]]))]

	nletter <- length(vars[[i]])*2
	sig_letter <- character(nletter)
    pvalue2 <- numeric(nletter)
    for(j in 1:(nletter/2)){  
    	## assign values reversely (right column to left) becasue we reserve the variables (from top to bottom)
    	pvalue2[j*2 - 1] <- pvalue1[which(pvalue1$Scenario==scen[1]),ncol(pvalue1)+1-j]
    	pvalue2[j*2] <- pvalue1[which(pvalue1$Scenario==scen[2]),ncol(pvalue1)+1-j]
    }

    for(j in 1:nletter){
    	if(pvalue2[j]<0.001) {
    		sig_letter[j] <- "**"
    	} else if (pvalue2[j]<0.05) {
    		sig_letter[j] <- "*"
    	} else {
    		sig_letter[j] <- "ns"   ## not significant
    	}
    }

	par.sum <- aggregate(Percent_Change ~ Scenario + Variable, par, mean)
	sp1 <- sp + geom_text(data=par.sum, 
						aes(y=ymax,label=sig_letter),
						size=4,col="red", 
						position=position_dodge(width=0.8))  
	#--------------------------------------------------------

	print(sp1)

	print(fn)
	dev.off()
}
# =========================================


