library(gplots)
library(dplyr)
library(data.table)

## Compute mean values for marginalized parameters
marginalize <- function(path="../agentData_allAlphas.csv"){

### Read CSV into R
data <- read.csv(file=path, colClasses="numeric", header = FALSE)
###

### Prepare Data
names(data)[1] <- "t1_mean"
names(data)[2] <- "t1_sigma"
names(data)[3] <- "t1_alpha"

names(data)[4] <- "t2_mean"
names(data)[5] <- "t2_sigma" 
names(data)[6] <- "t2_alpha"

names(data)[7] <- "EU"

marginalize_alphas <- function(){
  alphas1 = unique(data$t1_alpha)
  alphas2 = unique(data$t2_alpha)
  
  data2d_a = matrix(0, nrow=length(alphas1), ncol=length(alphas2))
  
  for(a1 in 1:length(alphas1)){
    for(a2 in 1:length(alphas2)){
      data2d_a[a1, a2] = sum(data$EU[data$t1_alpha == alphas1[a1] & data$t2_alpha == alphas2[a2]])/160000
    }
  }
  rownames(data2d_a) <- alphas1
  colnames(data2d_a) <- alphas2
  
  write.csv(data2d_a, file="alpha_marg.csv")
  
  cols <- colorRampPalette(c("royalblue4","royalblue","lightblue","white","lightsalmon","firebrick2","darkred"))(50) 
  
  pdf('alpha_marg.pdf', width=9, height=9)
  heatmap.2(data2d_a,dendrogram='none', Rowv=FALSE,  density.info="histogram", 
            denscol="black",  
            Colv=FALSE,trace='none',col=cols,  cexRow = 2, cexCol = 2)
  dev.off()
}

marginalize_sigmas <- function(){
  sigmas1 = unique(data$t1_sigma)
  sigmas2 = unique(data$t2_sigma)
  
  data2d_s = matrix(0, nrow=length(sigmas1), ncol=length(sigmas2))
  
  for(s1 in 1:length(sigmas1)){
    for(s2 in 1:length(sigmas2)){
      data2d_s[s1, s2] = sum(data$EU[data$t1_sigma == sigmas1[s1] & data$t2_sigma == sigmas2[s2]])/10000
    }
  }
  
  rownames(data2d_s) <- round(sigmas1,1)
  colnames(data2d_s) <- round(sigmas2,1)
  
  write.csv(data2d_s, file="sigma_marg.csv")
  
  cols <- colorRampPalette(c("royalblue4","royalblue","lightblue","white","lightsalmon","firebrick2","darkred"))(50) 
  
  pdf('sigma_marg.pdf', width=9, height=9)
  heatmap.2(data2d_s,dendrogram='none', Rowv=FALSE, Colv=FALSE,trace='none',col=cols, cexRow = 1.8, cexCol = 1.8)
  dev.off()
}

marginalize_means <- function(){
  means1 = unique(data$t1_mean)
  means2 = unique(data$t2_mean)
  
  data2d_m = matrix(0, nrow=length(means1), ncol=length(means2))
  
  for(m1 in 1:length(means1)){
    for(m2 in 1:length(means2)){
      data2d_m[m1, m2] = sum(data$EU[data$t1_mean == means1[m1] & data$t2_mean == means2[m2]])/10000
    }
  }
  
  rownames(data2d_m) <- means1
  colnames(data2d_m) <- means2
  
  write.csv(data2d_m, file="mean_marg.csv")
  
  cols <- colorRampPalette(c("royalblue4","royalblue","lightblue","white","lightsalmon","firebrick2","darkred"))(50) 
  
  pdf('mean_marg.pdf', width=9, height=9)
  heatmap.2(data2d_m,dendrogram='none', Rowv=FALSE, Colv=FALSE,trace='none',col=cols, cexRow = 1.8, cexCol = 1.8)
  dev.off()
}

marginalize_sigmas_fixedAlpha <- function(a=1){
    sigmas1 = unique(data$t1_sigma)
    sigmas2 = unique(data$t2_sigma)
    
    data2d_sf = matrix(0, nrow=length(sigmas1), ncol=length(sigmas2))
    
    data_a1 = data[data$t1_alpha==a & data$t2_alpha==a,]
    
    for(s1 in 1:length(sigmas1)){
      for(s2 in 1:length(sigmas2)){
        data2d_sf[s1, s2] = sum(data_a1$EU[data_a1$t1_sigma == sigmas1[s1] & data_a1$t2_sigma == sigmas2[s2]])/400
      }
    }
  
    rownames(data2d_sf) <- round(sigmas1,1)
    colnames(data2d_sf) <- round(sigmas2,1)
    fileString=paste("sigma_marg_alpha=",a,".csv")
    fileStringpdf=paste("sigma_marg_alpha=",a,".pdf")
    
    write.csv(data2d_sf, file=fileString)
    
    cols <- colorRampPalette(c("royalblue4","royalblue","lightblue","white","lightsalmon","firebrick2","darkred"))(50) 
    
    pdf(fileStringpdf, width=9, height=9)
    heatmap.2(data2d_sf, par(mar=c(20, 0.1, 4, 4)), dendrogram='none',key=FALSE,Rowv=FALSE, Colv=FALSE,trace='none',col=cols, cexRow = 1.8, cexCol = 1.8)
    dev.off()
}

marginalize_sigmas_fixedMean <- function(m=1.5){
  sigmas1 = unique(data$t1_sigma)
  sigmas2 = unique(data$t2_sigma)
  
  data2d_sf = matrix(0, nrow=length(sigmas1), ncol=length(sigmas2))
  
  data_m1 = data[data$t1_mean==m & data$t2_mean==m,]
  
  for(s1 in 1:length(sigmas1)){
    for(s2 in 1:length(sigmas2)){
      data2d_sf[s1, s2] = sum(data_m1$EU[data_m1$t1_sigma == sigmas1[s1] & data_m1$t2_sigma == sigmas2[s2]])/25
    }
  }
  
  rownames(data2d_sf) <- sigmas1
  colnames(data2d_sf) <- sigmas2
  fileString=paste("sigma_marg_mean=",m,".csv")
  fileStringpdf=paste("sigma_marg_mean=",m,".pdf")
  
  write.csv(data2d_sf, file=fileString)
  
  cols <- colorRampPalette(c("royalblue4","royalblue","lightblue","white","lightsalmon","firebrick2","darkred"))(50) 
  
  pdf(fileStringpdf, width=20, height=20)
  heatmap.2(data2d_sf,dendrogram='none', Rowv=FALSE, Colv=FALSE,trace='none',col=cols)
  dev.off()
}
  


marginalize_alphas()
#marginalize_sigmas()
#marginalize_means()
#marginalize_sigmas_fixedAlpha(1)
#marginalize_sigmas_fixedAlpha(50)
}
marginalize()
