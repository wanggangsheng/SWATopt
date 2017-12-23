# setwd("~/Dropbox (ORNL)/Model/SWATopt/aTools/R")
# source("new_TRB_dNPS_dLULC_plot.R")

library(corrplot)
library(sensitivity)
rm(list=ls())


#--------------------------------------------------------------------
# col_usr <- c("darkgreen","green","greenyellow","yellow","gray","antiquewhite","skyblue","cornflowerblue","blue","darkblue")
col_usr8 <- c("darkgreen","green","yellow","gray","antiquewhite","skyblue","blue","darkblue")
# col_usr10 <- c("darkgreen","green","greenyellow","yellow","gray","antiquewhite","skyblue","cornflowerblue","blue","darkblue")

#--------------------------------------------------------------------
cor.mtest <- function(mat, conf.level = 0.95) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    diag(lowCI.mat) <- diag(uppCI.mat) <- 1
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
            lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
            uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
        }
    }
    return(list(p.mat, lowCI.mat, uppCI.mat))
}
#--------------------------------------------------------------------
Qvar <- c("ET","WYLD", "SURQ", "LATQ", "TGWQ", "SGWQ", "DGWQ")
Pvar <- c("TSS", "TP", "ORGP", "MINP", "SOLP", "SEDP")
Nvar <- c("TN", "ORGN", "NO3", "SURNO3", "LATNO3", "GWNO3")
QPNvar <- c(Qvar, Pvar, Nvar)
Lvar <- c("Urban", "Crop", "Hay", "Pasture", "BioCrop", "Switchgrass", "Miscanthus", "Willow")
#--------------------------------------------------------------------

path <- getwd()
print(path)

dir_io <- "SWAT_TRB"
dir_inout <- c("in_data", "out_data", "out_plot")
path_io <- paste0(path, "/", dir_io)
path_in  <- paste0(path_io, "/", dir_inout[1])
path_out <- paste0(path_io, "/", dir_inout[2])
path_fig <- paste0(path_io, "/", dir_inout[3])

#--------------------------------------------------------------------
cases <- c("Base", "BC1", "HH3")
ncase <- length(cases)
Scenario <- character(ncase -1)
for(i in 1:(ncase-1)){
	Scenario[i] = paste0(cases[i+1],".vs.",cases[1])
}
#--------------------------------------------------------------------
print(paste0(">>>Read input data in ",path_in))
# fn_dNPS <- paste0("Load_diff_perc_",Scenario,".txt")  #  %difference
fn_dNPS <- paste0("Load_diff_",Scenario,".txt")         #  difference
fi_dNPS <- paste0(path_out,"/", fn_dNPS)
dNPS <- vector("list",ncase-1)
for (i in 1:(ncase-1)){
	dNPS[[i]] <- read.table(fi_dNPS[i], sep="", header=TRUE)
	dNPS[[i]] <- dNPS[[i]][,eval(QPNvar)]
}

fn_dLULC <- paste0("sub_area_diff_perc_lulc1_",Scenario,".txt")
fi_dLULC <- paste0(path_out,"/", fn_dLULC)
dLULC <- vector("list",ncase-1)
for (i in 1:(ncase-1)){
	dLULC[[i]] <- read.table(fi_dLULC[i], sep="", header=TRUE)
	Lvar1 <- Lvar
	Lvar1 <- Lvar1[which(Lvar1 %in% colnames(dLULC[[i]]))]

	# specifically for "Urban"
	Lvar1a <- "Urban"
	if((Lvar1a %in% Lvar1)&(abs(mean(dLULC[[i]][,eval(Lvar1a)]))<=1)) {
		Lvar1 <- Lvar1[which(Lvar1 != Lvar1a)]
	}
	dLULC[[i]] <- dLULC[[i]][,eval(Lvar1)]
}
#--------------------------------------------------------------------

for(k in 1:(ncase-1)){
	fn1 <- paste0("cor_dNPS_dLULC_",Scenario[k])
	fn2 <- paste0(path_fig,"/",fn1,".pdf")
	pdf(fn2)

	#fac: LULC; par: WaterQuality
	npar = ncol(dNPS[[k]])  # x-axis
	nfac = ncol(dLULC[[k]])  # y-axis  BC1 = 10, HH3 = 13
	par = dNPS[[k]]
	fac = dLULC[[k]]

	cor1<-cor(fac,par)
	pvalue=matrix(nrow=nfac,ncol=npar)
	for(i in 1:(nfac)) {
		for(j in 1:(npar)){
			test<-cor.test(fac[,i],par[,j])
			pvalue[i,j]=test$p.value
		}
	}

	colnames(pvalue)<-colnames(cor1)
	rownames(pvalue)<-rownames(cor1)


	sig=matrix(nrow=nfac,ncol=npar)
	colnames(sig)<-colnames(cor1)
	rownames(sig)<-rownames(cor1)

	for(i in 1:(nfac)) {
		for(j in 1:(npar)){
			if(pvalue[i,j]<=0.05){
				sig[i,j]='**'
			} 	else if(pvalue[i,j]<=0.1){
					sig[i,j]='*'
				}	else {
					sig[i,j]='-'
				}
		}
	}

	print("Significance of Correlation:")
	print(sig,quote=FALSE)
	print("Correlation:")
	print(round(cor1,2))
	print("Significance of Correlation (p-value):")
	print(round(pvalue,2))

	srrc<-matrix(,nrow=nfac,ncol=npar)
	for(j in 1:npar){
		par0 <- par[,j]
		srrc0<-src(X=fac,y=par0, nboot=0,rank=TRUE)  ##SRCC: Standardized Rank Correlation Coefficient
		srrc[,j]<-srrc0[7][[1]][,1]
	}
	rownames(srrc)<-colnames(fac)
	colnames(srrc)<-colnames(par)

	# print("Standardized Rank Correlation Coefficients:")
	# print(round(srrc,2))

	prcc<-matrix(,nrow=nfac,ncol=npar)  
	for(j in 1:npar){
		par0 <- par[,j]
		prcc0<-pcc(X=fac,y=par0, rank=FALSE,nboot=0, conf=0.95)  ##PRCC: Partial Rank Correlation Coefficients
		prcc[,j]<-prcc0[7][[1]][,1]
	}
	rownames(prcc)<-colnames(fac)
	colnames(prcc)<-colnames(par)

	print(paste0("Printing",fn2))
	# corrplot(cor1,p.mat=pvalue,insig="blank",is.corr=TRUE,method="circle", outline=TRUE, type = "full",tl.pos="lt",tl.col="black",order="original",cl.align.text='l',cl.lim=c(-1,1),col=col_usr8,cl.length=11)
	corrplot(cor1,p.mat=pvalue,insig="blank",is.corr=TRUE,method="circle", 
		outline=TRUE, type = "full", order="original",
		tl.pos="lt",tl.col="black",
		cl.align.text='l',cl.lim=c(-1,1),col=col_usr8)
	dev.off()


	# ------------------------------------------------------------
	fn1 <- paste0("cor_dLULC_",Scenario[k])
	fn2 <- paste0(path_fig,"/",fn1,".pdf")
	print(paste0("Printing",fn2))
	cor2 <- cor(fac)
	res2 <- cor.mtest(fac, 0.95)
	pdf(fn2)
	corrplot.mixed(cor2,p.mat = res2[[1]],insig="blank",col=col_usr8)
	dev.off()

	# ------------------------------------------------------------
	fn1 <- paste0("cor_dNPS_",Scenario[k])
	fn2 <- paste0(path_fig,"/",fn1,".pdf")
	print(paste0("Printing",fn2))
	cor3 <- cor(par)
	res3 <- cor.mtest(par, 0.95)
	pdf(fn2)
	corrplot.mixed(cor3,p.mat = res3[[1]],insig="blank",col=col_usr8,number.cex=9/npar,tl.cex=9/npar)
	dev.off()
} ## for(i in 1:ncase-1)
