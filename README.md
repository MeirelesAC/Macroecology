# Macroecology
Macroecology Projects

library(ape)
library(geiger)



setwd("C:/Users/Projeto Caatinga/Desktop/Macroecologia Trabalho Final")

mam.tree <- read.tree("arvore_estudo2.tre")
mam.data <- read.table("dados_estudo2.csv", header = TRUE, sep = ",")

plot(mam.tree)  #arvore geral


carnivora <- drop.tip(mam.tree, c(1:243, 324:419))

index <- match(carnivora$tip.label, mam.data$MSW93_Binomial)
mam.info <- mam.data[index, ]

plot(carnivora)  #Arvore do estudo 


library(letsR)
library(phytools)
library(caper)
library(phylolm)




comp.data <- comparative.data(phy = mam.tree,
                              data = mam.info[, c("MSW93_Binomial", "X5.1_AdultBodyMass_g", "X22.1_HomeRange_km2","X17.1_MaxLongevity_m")], names.col = "MSW93_Binomial", vcv = T)


### Modelos

mod.geral <- pgls(log(X22.1_HomeRange_km2) ~ log(X5.1_AdultBodyMass_g) + log(X17.1_MaxLongevity_m), data = comp.data)
sum.geral <- summary(mod.geral)

mod.bodylonge <- pgls(log(X22.1_HomeRange_km2) ~ log(X5.1_AdultBodyMass_g) : log(X17.1_MaxLongevity_m), data = comp.data)
sum.bodylonge <- summary(mod.bodylonge)

mod.bodymass <- pgls(log(X22.1_HomeRange_km2) ~ log(X5.1_AdultBodyMass_g), data = comp.data)
sum.bodymass <- summary(mod.bodymass)

mod.longevity <- pgls(log(X22.1_HomeRange_km2) ~ log(X17.1_MaxLongevity_m), data = comp.data)
summary(mod.longevity)

mod.null <- pgls(log(X22.1_HomeRange_km2) ~ 1, data = comp.data)
summary(mod.null)

mod.aic <- AIC(mod.geral,mod.bodymass,mod.longevity,mod.bodylonge)
aicw(mod.aic[,2])


AIC(mod.geral, mod.bodylonge, mod.bodymass, mod.longevity)


abline(mod.null)

