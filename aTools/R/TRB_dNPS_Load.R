
# setwd("~/Dropbox (ORNL)/Model/SWATopt/aTools/R")
# source("TRB_dNPS_Load.R")

# library(corrplot)
# library(sensitivity)
rm(list=ls())
path <- getwd()
print(path)

dir_io <- "SWAT_TRB"
dir_inout <- c("in_data", "out_data", "out_plot")
path_io <- paste0(path, "/", dir_io)
path_in  <- paste0(path_io, "/", dir_inout[1])
path_out <- paste0(path_io, "/", dir_inout[2])
path_fig <- paste0(path_io, "/", dir_inout[3])

# nvar <- 6
Qvar <- c("ET","WYLD", "SURQ", "LATQ", "TGWQ", "SGWQ", "DGWQ")
Pvar <- c("TSS", "TP", "ORGP", "MINP", "SOLP", "SEDP")
Nvar <- c("TN", "ORGN", "NO3", "SURNO3", "LATNO3", "GWNO3")
QPNvar <- list(Qvar,Pvar,Nvar)

file_out_name <- c("Q", "P", "N")
file_out <-  paste0(path_out,"/Load_diff_perc_Rplot_",file_out_name,".txt") 

dmin <- 0.001
ncol_skip <- 7  # number of columns to skip, data not for analysis: "SUB"     "GIS"     "MON"     "AREA"    "PRECIP"  "SNOMELT" "PET"     
cases <- c("Base", "BC1", "HH3")
ncase <- length(cases)
nscen <- ncase*(ncase - 1)
Scenario <- character(nscen)
for(i in 1:(nscen-1)){
	for(j in (i+1):nscen){
		Scenario[i] = paste0(cases[j],".vs.",cases[i])
	}
	
}

print(paste0(">>>Read input data in ",path_in))
file_in_name <- paste0("sub_mean_annual_",cases,".txt")
file_in <- paste0(path_in,"/", file_in_name)
data_in <- vector("list",ncase)
for (i in 1:ncase){
	data_in[[i]] <- read.table(file_in[i], sep="", header=TRUE)
}

print(">>>Process input data")
data_new <- data_in
for (i in 1:ncase){
	data_new[[i]]$TGWQ 	<- data_new[[i]]$WYLD - data_new[[i]]$SURQ - data_new[[i]]$LATQ	# total aquifer discharge
	data_new[[i]]$SGWQ 	<- data_new[[i]]$GWQ  											# shallow aquifer discharge
	data_new[[i]]$DGWQ 	<- data_new[[i]]$TGWQ - data_new[[i]]$SGWQ						# deep aquifer discharge
	data_new[[i]]$TSS 	<- data_new[[i]]$SYLD
	data_new[[i]]$NO3 	<- data_new[[i]]$SURNO3 + data_new[[i]]$LATNO3 + data_new[[i]]$GWNO3
	data_new[[i]]$TN 	<- data_new[[i]]$NO3 + data_new[[i]]$ORGN
	data_new[[i]]$MINP 	<- data_new[[i]]$SOLP + data_new[[i]]$SEDP
	data_new[[i]]$TP 	<- data_new[[i]]$MINP + data_new[[i]]$ORGP
}
nsub <- nrow(data_new[[1]])
ncol0 <- ncol(data_new[[1]])
ncol <- ncol0 - ncol_skip

print(paste0(">>>Write output data [diff & %diff] to ",path_out))
data_diff <- vector("list",nscen)
data_diff_perc <- vector("list",nscen)
fn1 <-  paste0(path_out,"/Load_diff_",Scenario,".txt") 
fn2 <-  paste0(path_out,"/Load_diff_perc_",Scenario,".txt") 
k <- 0
for (i in 1:(ncase-1)){
	for(j in (i+1):ncase){

		data_diff[[i]] <- data_new[[i]][,(ncol_skip+1):ncol0]
		data_diff[[i]] <- data_new[[i + 1]][,(ncol_skip+1):ncol0] - data_new[[1]][,(ncol_skip+1):ncol0]

		data_diff_perc[[i]] <- data_diff[[i]]/data_new[[1]][,(ncol_skip+1):ncol0]*100  # %change

		write.table(round(data_diff[[i]],4),fn1[i],sep="\t",row.names=FALSE)
		write.table(round(data_diff_perc[[i]],2),fn2[i],sep="\t",row.names=FALSE)
	}
}

# Rplot_diff_Load_Q <- matrix(nrow=(ncase-1)*nvar*nsub,ncol=4)
print(paste0(">>>Write output data [%diff for R plot] to ",path_out))
colClasses <- c("character", "character","integer","numeric")
colnames <- c("Scenario", "Variable", "Subbasin", "Percent_Change")

Rplot_diff_Load <- vector("list",3)  # ile_out_name <- c("Q", "P", "N")
for (k in 1:3){
	Rplot_diff_Load[[k]] <- read.table(text = "", colClasses = colClasses, col.names = colnames)
	nvar <- length(QPNvar[[k]])
	for (i in 1:nvar){
		irow_beg <- (i-1)*(ncase-1)*nsub + 1
		irow_end <- irow_beg + (ncase-1)*nsub - 1

		Rplot_diff_Load[[k]][irow_beg:irow_end,2] <- QPNvar[[k]][i]
		for (j in 1:(ncase-1)){
			# jcol_select <- which(colnames(data_diff_perc[[j]])%in%Qvar)
			jcol_select <- which(colnames(data_diff_perc[[j]])==QPNvar[[k]][i])

			jrow_beg <- irow_beg + (j-1)*nsub 
			jrow_end <- jrow_beg + nsub - 1

			Rplot_diff_Load[[k]][jrow_beg:jrow_end,1] <- Scenario[j]
			Rplot_diff_Load[[k]][jrow_beg:jrow_end,3] <- as.integer(seq(1:nsub))
			Rplot_diff_Load[[k]][jrow_beg:jrow_end,4] <- as.numeric(data_diff_perc[[j]][,jcol_select])
		}
	}

	write.table(Rplot_diff_Load[[k]],file_out[k],sep="\t",row.names=FALSE)
}





#--------------------------------------------------------------------
# Input data
# file_in <- "SUB_dNPS_dLULC_BC1.txt"
# file_in <- "SUB_dNPS_dLULC_HH3.txt"
# file_in <- "SUB_dNPS_dLULC_ALL.txt"
#--------------------------------------------------------------------
# col_usr <- c("darkgreen","green","greenyellow","yellow","gray","antiquewhite","skyblue","cornflowerblue","blue","darkblue")
# #--------------------------------------------------------------------
# cor.mtest <- function(mat, conf.level = 0.95) {
#     mat <- as.matrix(mat)
#     n <- ncol(mat)
#     p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
#     diag(p.mat) <- 0
#     diag(lowCI.mat) <- diag(uppCI.mat) <- 1
#     for (i in 1:(n - 1)) {
#         for (j in (i + 1):n) {
#             tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
#             p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
#             lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
#             uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
#         }
#     }
#     return(list(p.mat, lowCI.mat, uppCI.mat))
# }
# #--------------------------------------------------------------------



# # data_in <- read.table(file_in, sep=",", header=TRUE)
# data_in <- read.table(file_in, sep="", header=TRUE)
# file_name <- str_sub(file_in,1,nchar(file_in)-4)
# fn <- paste0(file_name,".pdf")
# pdf(fn)

# #fac: LULC; par: WaterQuality
# npar = 21  # x-axis
# nfac = 13  # y-axis  BC1 = 10, HH3 = 13
# par = data_in[,3:(npar+2)]
# fac = data_in[,(npar+3):(npar+2+nfac)]

# cor1<-cor(fac,par)
# pvalue=matrix(nrow=nfac,ncol=npar)
# for(i in 1:(nfac)) {
# 	for(j in 1:(npar)){
# 		test<-cor.test(fac[,i],par[,j])
# 		pvalue[i,j]=test$p.value
# 	}
# }

# colnames(pvalue)<-colnames(cor1)
# rownames(pvalue)<-rownames(cor1)


# sig=matrix(nrow=nfac,ncol=npar)
# colnames(sig)<-colnames(cor1)
# rownames(sig)<-rownames(cor1)

# for(i in 1:(nfac)) {
# 	for(j in 1:(npar)){
# 		if(pvalue[i,j]<=0.05){
# 			sig[i,j]='**'
# 		} 	else if(pvalue[i,j]<=0.1){
# 				sig[i,j]='*'
# 			}	else {
# 				sig[i,j]='-'
# 			}
# 	}
# }

# print("Significance of Correlation:")
# print(sig,quote=FALSE)
# print("Correlation:")
# print(round(cor1,2))
# print("Significance of Correlation (p-value):")
# print(round(pvalue,2))



# srrc<-matrix(,nrow=nfac,ncol=npar)
# for(j in 1:npar){
# 	par0 <- par[,j]
# 	srrc0<-src(X=fac,y=par0, nboot=0,rank=TRUE)  ##SRCC: Standardized Rank Correlation Coefficient
# 	srrc[,j]<-srrc0[7][[1]][,1]
# }
# rownames(srrc)<-colnames(fac)
# colnames(srrc)<-colnames(par)

# # print("Standardized Rank Correlation Coefficients:")
# # print(round(srrc,2))

# prcc<-matrix(,nrow=nfac,ncol=npar)  
# for(j in 1:npar){
# 	par0 <- par[,j]
# 	prcc0<-pcc(X=fac,y=par0, rank=FALSE,nboot=0, conf=0.95)  ##PRCC: Partial Rank Correlation Coefficients
# 	prcc[,j]<-prcc0[7][[1]][,1]
# }
# rownames(prcc)<-colnames(fac)
# colnames(prcc)<-colnames(par)

# print(paste0("Printing",fn))
# corrplot(cor1,p.mat=pvalue,insig="blank",is.corr=TRUE,method="circle", outline=TRUE, type = "full",tl.pos="lt",tl.col="black",order="original",cl.align.text='l',cl.lim=c(-1,1),cl.length=11,col=col_usr)
# dev.off()


# # corrplot(cor1,p.mat=pvalue,insig="blank",is.corr=TRUE,method="circle", outline=TRUE, type = "full",tl.pos="lt",tl.col="black",order="original",cl.align.text='l',cl.lim=c(-1,1),cl.length=11,col=col_usr)
# # ------------------------------------------------------------
# fn2 <- paste0(file_name,"_fac.pdf")
# print(paste0("Printing",fn2))
# cor2 <- cor(fac)
# res2 <- cor.mtest(fac, 0.95)
# pdf(fn2)
# corrplot.mixed(cor2,p.mat = res2[[1]],insig="blank",col=col_usr)
# dev.off()

# # ------------------------------------------------------------
# fn3 <- paste0(file_name,"_par.pdf")
# print(paste0("Printing",fn3))
# cor3 <- cor(par)
# res3 <- cor.mtest(par, 0.95)
# pdf(fn3)
# corrplot.mixed(cor3,p.mat = res3[[1]],insig="blank",col=col_usr)
# dev.off()

