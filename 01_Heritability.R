rm(list=ls())
library(sommer)
library(SpATS)
library(lme4)
library(car)


### Heritability for each model
setwd("")
# chech dimensions pheno = geno
pheno.v0 <- read.csv("pheno.csv")
pheno.v0$Whole.milling.percentage.LSU_01.0000100
# Genotype -------------------------------------------------------------------
# ~ GBLUP: GRM
geno1 <- readRDS("geno.rds")
geno2 <- readRDS("geno2.rds") # in case you have two files
geno <- rbind(geno1, geno2)
geno.v <- geno[rownames(geno) %in% pheno.v0$germplasmName, ]

# efectos aditivos, de dominancia y espistasis
G_mat <- A.mat(X=as.matrix(geno.v), min.MAF = .05)
#add the other matrices with sommer functions

#### Model 1: additive effects - Calculating h2 ####------------------------------------------

pheno.v1 <- pheno.v0[pheno.v0$germplasmName %in% rownames(G_mat), ]
#pheno.v1$germplasmName2 <- pheno.v1$germplasmName # epistasis or dominance accounted with a cloned id factor (if you need)

pheno.v1$rowf <- as.factor(pheno.v1$rowNumber)
pheno.v1$colf <- as.factor(pheno.v1$colNumber)
pheno.v1$repf <- as.factor(pheno.v1$replicate)

mm.a <- mmer(fixed = yield ~ 1, #lets do for yield but here you put your trait
             random = ~vsr(germplasmName,Gu=G_mat),
             rcov = ~ vsr(units),
             data = pheno.v1
)

#summary(mm.a)$varcomp
# heritability
#vpredict(mm.a, h2 ~ (V1) / ( V1+V2)) # broad sense
vpredict(mm.a, h2 ~ (V1) / ( V1+V2)) # narrow sense

#Spatial correction
mm.sp.a <- mmer(fixed = yield ~ 1, 
              random = ~vsr(germplasmName,Gu=G_mat) +
              vsr(rowf) + vsr(colf) + spl2Da(rowNumber,colNumber),
              rcov = ~ vsr(units),
              data = pheno.v1
)

#summary(mm.sp.a)$varcomp

# heritability
#vpredict(mm.sp.a, h2 ~ (V1) / ( V1+V5)) # broad sense
vpredict(mm.sp.a, h2 ~ (V1) / ( V1+V5)) # narrow sense



