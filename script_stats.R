# METADATA ----------------------------------------------------------------

## SIMON VON SACHSEN-COBURG UND GOTHA'S MSc THESIS
## Created: Faro, 15th July 2021
## Last modification: 01/08/2021

## Carmen de los Santos & Simon Coburg
## email: cbsantos@ualg.pt / simon.vonsachsencoburgundgotha@imbrsea.eu

## CODE FOR
#1 DATA STRUCTURE
#2 STATISTICS
#3 LM PLOTS


# SETTINGS ----------------------------------------------------------------

# load libraries
packages <- c("tidyverse",      # for data science (general) - includes ggplot2
              "readxl",         # for reading xlsx files
              "devtools",
              "ggpubr",
              "EnvStats",
              "AICcmodavg",
              "rstatix",
              "car",
              "lindia",
              "lme4",
              "svglite")

for (i in seq_along(packages)) {
        if(!do.call(require, list(package = packages[i]))) {
                do.call(install.packages, list(pkgs = packages[i]))
                do.call(require, list(package = packages[i]))
        }
}


# clean working environment
rm(list=ls())
cat("\f")

# set working directory
setwd("~/Documents/IMBRSea/Thesis S4/RStudio Uptake rates")
getwd()


# DATA SOURCES --------------------------------------------------------------------

# load SOURCES
data.sou  <- read_excel("~/Documents/IMBRSea/Thesis S4/Database uptake rate_final.xlsx",
                        sheet="sources",na="NA",skip=3)

# load ENVIRONMENTAL
data.env  <- read_excel("~/Documents/IMBRSea/Thesis S4/Database uptake rate_final.xlsx",
                        sheet="environmental",na="NA",skip=3)

# load EXPERIMENTAL
data.exp  <- read_excel("~/Documents/IMBRSea/Thesis S4/Database uptake rate_final.xlsx",
                        sheet="experimental",na="NA",skip=3)
str(data.exp)

data.exp$sampling_inter <- as.numeric(data.exp$sampling_inter)

nrow(data.exp)
#[1] 878

# DATA STRUCTURE ---------------------------------------------------------------

# create genera column
data.exp$genera <- sapply(strsplit(data.exp$species," "),"[",1)
table(data.exp$genera)

# create a column for nutrient group (organic or inorganic)
data.exp$nutrient_group <- ifelse(data.exp$nutrient=="Amino acid" | data.exp$nutrient=="Urea","organic","inorganic")
table(data.exp$nutrient_group,data.exp$nutrient) # check grouping is ok

# create a dataset for leaves/fronds/whole algae only
table(data.exp$species_compartm,data.exp$species_type)
data.lea <- data.exp[data.exp$species_compartm=="Leaves" | data.exp$species_compartm=="Fronds"
                     | data.exp$species_compartm=="Whole algae",]
table(data.lea$species_compartm,data.lea$species_type)

# create a new category
table(data.exp$species_compartm,data.exp$species_type)
data.cat <- data.exp[data.exp$species_compartm=="Leaves" | data.exp$species_compartm=="Fronds"
                     | data.exp$species_compartm=="Whole algae" | data.exp$species_compartm=="Roots",]
table(data.cat$species_compartm,data.cat$species_type)


data.cat$category1 <- paste0(data.cat$species_type,"-",data.cat$species_compartm)
table(data.cat$category1)
data.cat$category2 <- ifelse(data.cat$category1=="Algae-Whole algae" | data.cat$category1=="Algae-Fronds","Algae",data.lea$category1)
table(data.cat$category2)


# DATA SUMMARY (data.lea) ------------------------------------------------------------
# data structure
table(data.lea$species_type, data.lea$nutrient, data.lea$type_uptake)

table(data.lea$species_type,data.lea$type_uptake)

table(data.lea$species_type,data.lea$species_compartm)

# number of species
species <- data.frame(table(data.lea$species,data.lea$species_phyla))
species <- species[species$Freq!=0,]
table(species$Var2)

# number of values Vmax
table(is.na(data.lea$Vmax))
data <- data.lea[is.na(data.lea$Vmax)==F,]
table(data$type_uptake,data$nutrient,data$species_type)
table(data$type_uptake,data$species_phyla)
table(data$type_uptake,data$nutrient,data$species_phyla)

# number of values alpha
table(is.na(data.lea$alpha))
data <- data.lea[is.na(data.lea$alpha)==F,]
table(data$type_uptake,data$nutrient,data$species_type)
table(data$type_uptake,data$species_phyla)
table(data$type_uptake,data$nutrient,data$species_phyla)

# clean
rm(data,species)

# median, IQR

# FILTER by species type
data.seagrass <- data.lea[data.lea$species_type == "Seagrass",]
data.algae <- data.lea[data.lea$species_type == "Algae",]

summary(data.seagrass$Vmax)
summary(data.algae$Vmax)

summary(data.seagrass$alpha)
summary(data.algae$alpha)

        
# DATA SUMMARY INORGANIC (data.lea) ------------------------------------------------------------

# FILTER OUT inorganic nutrients
data.organic <- data.lea[data.lea$nutrient_group=="organic",]
nrow(data.organic)

# FILTER OUT organic nutrients
data.lea <- data.lea[data.lea$nutrient_group=="inorganic",]
nrow(data.lea)

# data structure
table(data.lea$species_type,data.lea$nutrient)
table(data.lea$species_type,data.lea$type_uptake)
table(data.lea$species_type,data.lea$species_compartm)

# number of species
species <- data.frame(table(data.lea$species,data.lea$species_phyla))
species <- species[species$Freq!=0,]
table(species$Var2)

# number of values Vmax
table(is.na(data.lea$Vmax))
data <- data.lea[is.na(data.lea$Vmax)==F,]
table(data$type_uptake,data$nutrient,data$species_type)
table(data$type_uptake,data$nutrient,data$species_phyla)

# number of values alpha
table(is.na(data.lea$alpha))
data <- data.lea[is.na(data.lea$alpha)==F,]
table(data$type_uptake,data$nutrient,data$species_type)
table(data$type_uptake,data$nutrient,data$species_phyla)

# clean
rm(data,species)


# STAT - CORRELATIONS (LEAVES/FRONDS/WHOLE ALGAE - INORGANIC NUTRIENTS) --------------------------------------------------------------

# data set - important columns
# FIXED FACTORS      >> "species_type","nutrient","type_uptake","species_phyla"
# RANDOM FACTORS     >> "species","genera"
# CO-VARIABLE        >> "temperature_experiment"
# RESPONSE VARIABLES >> "Vmax","alpha"

# correlation Vmax and alpha (species type)

g <- ggplot(data.lea,aes(x=log10(alpha),y=log10(Vmax),colour=species_type)) +
        geom_point(shape=21,alpha=0.5) +
        geom_smooth(method="lm") +
        #stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
        #scale_x_log10() +
        #scale_y_log10() +
        facet_grid(.~type_uptake) +
        labs(x="\n(log10) Alpha [l g^-1 dw h^-1]", y="(log10) Vmax [µmol g^-1 dw h^-1]\n") + #adding [\n] to axis label to increase gap
        theme_bw() +
        theme(axis.text=element_text(size=11), axis.title.y = element_text(size=12)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "bottom")

g
ggsave(filename = "Linear correlation.svg", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)

ggplot(data.lea,aes(y=log10(alpha),x=log10(Vmax), colour=species_type)) +
        geom_point(shape=21,alpha=0.5) +
        geom_smooth(method="lm") +
        #stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
        #scale_x_log10() +
        #scale_y_log10() +
        facet_grid(.~type_uptake) +
        labs(y="\n(log10) Alpha [l g^-1 dw h^-1]", x="(log10) Vmax [µmol g^-1 dw h^-1]\n") + #adding [\n] to axis label to increase gap
        theme_bw() +
        theme(axis.text=element_text(size=11), axis.title.y = element_text(size=12)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "bottom")


ggplot(data.lea,aes(x=log10(alpha),y=log10(Vmax), colour=species_type)) +
        geom_point(shape=21,alpha=0.5) +
        geom_smooth(method="lm") +
        stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
        

ggplot(data.lea,aes(x=log10(alpha),y=log10(Vmax),colour=species_type)) +
        geom_point(shape=21,alpha=0.5) +
        geom_smooth(method="lm")

ggplot(data.lea,aes(x=alpha,y=Vmax,colour=type_uptake)) +
        geom_point(shape=21,alpha=0.5) +
        geom_smooth(method="lm") +
        scale_x_log10() +
        scale_y_log10()

ggplot(data.lea,aes(y=Vmax,x=species_type)) +
        geom_boxplot() +
        geom_smooth(method="lm") +
        scale_y_log10()

ggplot(data.lea,aes(y=Vmax,x=species_type,colour=type_uptake)) +
        geom_boxplot() +
        geom_smooth(method="lm") +
        scale_y_log10()


# Ancova
mod <- lm(log10(Vmax)~log10(alpha)*species_type*type_uptake,data.lea)
summary(mod)
Anova(mod)

# mod <- lm(log10(alpha)~log10(Vmax)*species_type*type_uptake,data.lea)
# summary(mod)
# Anova(mod)

# Correlation Vmax and alpha (species phyla)
ggplot(data.lea,aes(y=log10(Vmax),x=log10(alpha),colour=species_phyla)) +
        geom_point(shape=21,alpha=0.5) +
        geom_smooth(method="lm") +
        stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
        # scale_x_log10() +
        # scale_y_log10()

# ggplot(data.lea,aes(y=alpha,x=Vmax,colour=species_phyla)) +
#         geom_point(shape=21,alpha=0.5) +
#         geom_smooth(method="lm") +
#         scale_x_log10() +
#         scale_y_log10()

# EFFECT TEMPERATURE ------------------------------------------------------

g <- ggplot(data.lea,aes(x=temperature_experiment, y=log10(Vmax), colour=species_type)) +
        geom_point(shape=21) +
        geom_smooth(method="lm") +
        #facet_grid(.~type_uptake) +
        stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
        labs(x="\nTemperature [ºC]", y="(log10) Vmax [µmol g^-1 dw h^-1]\n") + #adding [\n] to axis label to increase gap
        theme_bw() +
        theme(axis.text=element_text(size=11), axis.title.y = element_text(size=12)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "bottom")

g
ggsave(filename = "Linear correlation temperature Vmax.svg", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)
        
        

g <- ggplot(data.lea,aes(x=temperature_experiment,y=log10(alpha),colour=species_type)) +
        geom_point(shape=21) +
        geom_smooth(method="lm") +
        #facet_grid(.~type_uptake) +
        stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
        labs(x="\nTemperature [ºC]", y="(log10) Alpha [l g^-1 dw h^-1]\n") + #adding [\n] to axis label to increase gap
        theme_bw() +
        theme(axis.text=element_text(size=11), axis.title.y = element_text(size=12)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "bottom")

g
ggsave(filename = "Linear correlation temperature alpha.svg", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)


mod <- lm(log10(Vmax)~temperature_experiment*species_type,data.lea)
summary(mod)
mod <- lm(log10(Vmax)~temperature_experiment*species_type*type_uptake,data.lea)
Anova(mod)

mod <- lm(log10(alpha)~temperature_experiment*species_type*type_uptake,data.lea)
summary(mod)


# KRUSKALL / WILCOXON -----------------------------------------------------

# # Testing for normality
# shapiro.test(data.lea$Vmax)
# shapiro.test(data.lea$alpha)

# Adjust accordingly to Vmax or alpha

# species types
kruskal.test(species_type ~ alpha, data = data.lea)

pairwise.wilcox.test(data.lea$alpha, data.lea$species_type,
                     p.adjust.method = "BH")
# uptake types
kruskal.test(type_uptake ~ alpha, data = data.lea)

pairwise.wilcox.test(data.lea$alpha, data.lea$type_uptake,
                     p.adjust.method = "BH")

# nutrients
kruskal.test(nutrient ~ alpha, data = data.lea)

pairwise.wilcox.test(data.lea$alpha, data.lea$nutrient,
                     p.adjust.method = "BH")

# phyla
kruskal.test(species_phyla ~ alpha, data = data.lea)

pairwise.wilcox.test(data.lea$alpha, data.lea$species_phyla,
                     p.adjust.method = "BH")

