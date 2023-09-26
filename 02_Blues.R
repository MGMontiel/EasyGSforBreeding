rm(list=ls())
library(sommer)
library(SpATS)
library(lme4)
library(car)

# Reading data --------------------------------------------------------------
setwd("~/LSU Maria Montiel/Projects/project_mp/project_mp_01paper/project_mp_epistasis/Metrics-blups/")

# Phenotype -----------------------------------------------------------------
pheno.v0 <- read.csv("project_21mp2_2.csv", header = T)
#pheno.v0$Grain.length.average.LSU_01.0000102

# checking outliers from the pheno ------------------------------------------

# outlier detection and elimination
fit <- lm(Yield.LSU_01.0000138 ~ germplasmName, data = pheno.v0)
(outlier <- names(outlierTest(fit)$p))
pheno.v0[outlier, "Yield.LSU_01.0000138"] <- NA

fit <- lm(Days.to.heading.LSU_01.0000130 ~ germplasmName, data = pheno.v0)
(outlier <- names(outlierTest(fit)$p))
pheno.v0[outlier, "Days.to.heading.LSU_01.0000130"] <- NA

fit <- lm(Grain.length.average.LSU_01.0000102 ~ germplasmName, data = pheno.v0)
(outlier <- names(outlierTest(fit)$p))
pheno.v0[outlier, "Grain.length.average.LSU_01.0000102"] <- NA

fit <- lm(Chalk.impact.LSU_01.0000108 ~ germplasmName, data = pheno.v0)
(outlier <- names(outlierTest(fit)$p))
pheno.v0[outlier, "Chalk.impact.LSU_01.0000108"] <- NA

fit <- lm(Whole.milling.percentage.LSU_01.0000100 ~ germplasmName, data = pheno.v0)
(outlier <- names(outlierTest(fit)$p))
pheno.v0[outlier, "Whole.milling.percentage.LSU_01.0000100"] <- NA

# Adjust phenotype by the spatial trends
pheno.v1 <- pheno.v0

pheno.v1$rowf <- as.factor(pheno.v1$rowNumber)
pheno.v1$colf <- as.factor(pheno.v1$colNumber)
pheno.v1$repf <- as.factor(pheno.v1$replicate)

nrow <- max(pheno.v1$rowNumber)
ncol <- max(pheno.v1$colNumber)
nseg.row <- nrow
nseg.col <- ncol

trait_columns <- c("Yield.LSU_01.0000138", "Days.to.heading.LSU_01.0000130", "Whole.milling.percentage.LSU_01.0000100", 
                   "Chalk.impact.LSU_01.0000108")#,
                   #"Grain.length.average.LSU_01.0000102",
                  

# Initialize a list to store adjusted results for each trait
adjusted_results <- list()

# Loop through each trait
for (trait in trait_columns) {
  # weights for ID's - adjust residual for further analysis 
  
  fitR <- SpATS(response = trait, 
                fixed = ~ 1, 
                random = ~ colf + rowf,# + repf, 
                spatial = ~ PSANOVA(colNumber, rowNumber, nseg = c(nseg.col, nseg.row)), 
                genotype = "germplasmName", 
                genotype.as.random = TRUE, 
                data = pheno.v1)
  
  
  vcov.mme <- fitR$vcov$C11_inv 
  w <- diag(vcov.mme)
  # extracting the blues
  blups <- SpATS::predict.SpATS(fitR,which = "germplasmName" )
  adjusted <- cbind(blups, w)
  
  adjusted_results[[trait]] <- adjusted
}

# Combine results from the list into a single data frame
combined_results <- do.call(rbind, lapply(names(adjusted_results), function(trait) {
  data.frame(germplasmName = rownames(adjusted_results[[trait]]), trait = trait, adjusted_value = adjusted_results[[trait]])
}))

write.csv(combined_results, "blues.csv")




