rm(list=ls())
library(sommer)
library(tidyverse)
library(ggplot2)
library(paletteer)
library(hrbrthemes)

setwd("~/LSU Maria Montiel/Projects/project_mp/project_mp_01paper/project_mp_epistasis/Cross validation/data/")
my_palette <- c("#EE3A8C", "#4682B4", "#2F4F4F","#FFA500")


######------------------Cross Validation K Fold-----------------------######


pheno <- read.csv("blues_corrected_spatial.csv")
geno <- readRDS("geno.last.rds")
geno.v <- geno[rownames(geno) %in% pheno$germplasmName, ]
G_mat <- A.mat(X=as.matrix(geno.v), min.MAF = .05)
G_e_mat <- E.mat(X=as.matrix(geno.v), min.MAF = .05)
pheno$germplasmName2 <- pheno$germplasmName



ID <- colnames(pheno)[1]
res1 <- data.frame()
cyc=25
k=5

for (r in 1:25){
  df. <- pheno
  df. <- df.[sample(1:nrow(df.),nrow(df.)),]
  G_mat_CV <- G_mat
  G_mat.v <- G_mat
  pred.loo <- data.frame()
  for (tr in c("yield", "milling", "dth", "chalk"#, "grain_length"
  )){
    acc.k=NULL
    flds <- caret::createFolds(seq(1:nrow(df.)),
                               k = k, list = TRUE, returnTrain = FALSE)
    pred.i <- data.frame()
    for(fd in 1:k){
      df <- df.
      pred.k <- data.frame()
      geno.k <- df[flds[[fd]],"germplasmName"]
      df[df[,ID]%in%geno.k,tr] <- NA    #NA masking of the lines in the k-fold to be predicted
      df[,ID] <- as.factor(df[,ID])
      frm.cv <- paste(tr,"~ 1  ",collapse = " ")
      fix_frm.cv <- formula(paste(frm.cv, collapse = " "))
      mm.gb <-mmer(fixed = fix_frm.cv,
                   random =  ~  vsr(germplasmName ,Gu=G_mat.v) ,
                   rcov = ~vsr(units),
                   #tolParInv = 100000,
                   data = droplevels(df[df$germplasmName %in% rownames(G_mat.v),])
      )
      pred <- data.frame(germplasmName=names(mm.gb$U[[1]][[tr ]]),
                         gebv=as.numeric(mm.gb$U[[1]][[tr ]]))
      
      rownames(pred) <- pred[,1]
      pred.k <- rbind(pred.k,pred[geno.k ,])
      pred <- merge(pred.k,df.[,c(ID,tr)],by=ID)
      acc.k <- c(acc.k,cor(pred[,2],pred[,3],use = "complete.obs"))
      pred.i <- rbind(pred.i,pred.k)
    }
    pred.i <- merge(pred.i,df.[,c(ID,tr)],by=ID)
    
    res1 <- rbind(res1,data.frame(#pop_id=pop,
      trait=tr,n=nrow(df.),
      acc.k=mean(acc.k,na.rm = T),
      acc.i=cor(pred.i[,2],pred.i[,3],use = "complete.obs"))
    )
    
  }
}

write.csv(res1, "CV.csv")

## fin

