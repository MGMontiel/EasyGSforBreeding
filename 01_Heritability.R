rm(list=ls())
library(sommer)
library(SpATS)
library(lme4)
library(car)


### Heritability for each model
setwd("C:/Users/mmontiel/OneDrive - LSU AgCenter/Documents/LSU Maria Montiel/Projects/project_mp/project_mp_01paper/project_mp_epistasis/Metrics-blups/")
# chech dimensions pheno = geno
pheno.v0 <- read.csv("project_21mp2_2.csv")
pheno.v0$Whole.milling.percentage.LSU_01.0000100
# Genotype -------------------------------------------------------------------
# ~ GBLUP: GRM
geno1 <- readRDS("geno.last.rds")
geno2 <- readRDS("geno22.rds")
geno <- rbind(geno1, geno2)
geno.v <- geno[rownames(geno) %in% pheno.v0$germplasmName, ]

# efectos aditivos, de dominancia y espistasis
G_mat <- A.mat(X=as.matrix(geno.v), min.MAF = .05)
E_mat <- E.mat(X=as.matrix(geno.v), min.MAF = .05) # default (additive x additive) type="A#A"

#### Model 1: additive effects - Calculating h2 and Gblups####------------------------------------------

pheno.v1 <- pheno.v0[pheno.v0$germplasmName %in% rownames(G_mat), ]
pheno.v1$germplasmName2 <- pheno.v1$germplasmName # epistasis accounted with a cloned id factor

pheno.v1$rowf <- as.factor(pheno.v1$rowNumber)
pheno.v1$colf <- as.factor(pheno.v1$colNumber)
pheno.v1$repf <- as.factor(pheno.v1$replicate)

mm.a <- mmer(fixed = Whole.milling.percentage.LSU_01.0000100 ~ 1, 
             random = ~vsr(germplasmName,Gu=G_mat),
             rcov = ~ vsr(units),
             data = pheno.v1
)

#summary(mm.a)$varcomp
# heritability
#vpredict(mm.a, h2 ~ (V1) / ( V1+V2)) # broad sense
vpredict(mm.a, h2 ~ (V1) / ( V1+V2)) # narrow sense

#Spatial correction
mm.sp.a <- mmer(fixed = Whole.milling.percentage.LSU_01.0000100 ~ 1, 
              random = ~vsr(germplasmName,Gu=G_mat) +
              vsr(rowf) + vsr(colf) + spl2Da(rowNumber,colNumber),
              rcov = ~ vsr(units),
              data = pheno.v1
)

#summary(mm.sp.a)$varcomp

# heritability
#vpredict(mm.sp.a, h2 ~ (V1) / ( V1+V5)) # broad sense
vpredict(mm.sp.a, h2 ~ (V1) / ( V1+V5)) # narrow sense


#### Model 2: epistatic and additive  effects ####------------------------------------------
mm.ae <- mmer(fixed = Whole.milling.percentage.LSU_01.0000100 ~ 1, 
              random = ~vsr(germplasmName,Gu=G_mat) + vsr(germplasmName2,Gu=E_mat),
              rcov = ~ vsr(units),
              data = pheno.v1
)

# heritability
#summary(mm.ae)$varcomp
#vpredict(mm.ae, h2 ~ (V1+V2) / ( V1+V2+V3)) # broad sense (we don't have without dominance?)
vpredict(mm.ae, h2 ~ (V1) / ( V1+V3)) # narrow sense

#Spatial correction

mm.sp.i <- mmer(fixed = Whole.milling.percentage.LSU_01.0000100 ~ 1, 
              random = ~vsr(germplasmName,Gu=G_mat)+ vsr(germplasmName2, Gu=E_mat)+
                vsr(rowf) + vsr(colf) + spl2Da(rowNumber,colNumber),
              rcov = ~ vsr(units),
              data = pheno.v1
)

#summary(mm.sp.i)$varcomp

# heritability
vpredict(mm.sp.i, h2 ~ (V1+V2) / ( V1+V2+V6)) # broad sense
vpredict(mm.sp.i, h2 ~ (V1) / ( V1+V6)) # narrow sense


