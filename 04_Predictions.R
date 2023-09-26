rm(list=ls())
library(sommer)
library(ggplot2)

##### Working Directory and Data Loading ####

setwd("")

#### Training set
pheno.tr <- read.csv("Training.csv",header = T)

#### Validation Set
pheno.v0 <- read.csv("prediction.csv",header = T)
pheno.tr2 <- pheno.tr[!pheno.tr$germplasmName %in% pheno.v0$germplasmName,]

### All Phenotype
pheno.v0$chalk=NA
pheno.all <- rbind(pheno.tr2,pheno.v0)
str(pheno.v0)

#### Genotype and GRM
geno <- readRDS("geno.last.rds")
geno.v <- geno[rownames(geno) %in% pheno.v0$germplasmName, ]
geno.tr <- geno[rownames(geno) %in% pheno.tr2$germplasmName, ]
geno.all <- rbind(geno.v,geno.tr)
G_mat <- A.mat(X=as.matrix(geno.all), min.MAF = .05)

# Model using only additive effects
mm.gp <- mmer(fixed = chalk ~ 1, 
              random = ~vsr(germplasmName,Gu=G_mat),
              rcov = ~ vsr(units),
              data = droplevels(pheno.all[pheno.all$germplasmName %in% rownames(geno.all),])
)

pheno.v0 <- read.csv("prediction.csv",header = T)
gebv.blup <- merge(pheno.v0, 
                   data.frame(germplasmName=names(mm.gp$U$`u:germplasmName`$chalk),
                              gebv=as.numeric(mm.gp$U$`u:germplasmName`$chalk)),
                   by = "germplasmName")

cor(gebv.blup$gebv, gebv.blup$chalk)

# ---- Fin----