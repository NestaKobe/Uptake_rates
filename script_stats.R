# METADATA ----------------------------------------------------------------

## SIMON VON SACHSEN-COBURG UND GOTHA'S MSc THESIS
## Created: Faro, 08th July 2021
## Last modification: 08/07/2021

## Simon Coburg and Carmen dos Santos
## email: simon.vonsachsencoburgundgotha@imbrsea.eu / cbsantos@ualg.pt

## CODE FOR
#2. STATS

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


# DATA --------------------------------------------------------------------

# load SOURCES
data.sou  <- read_excel("~/Documents/IMBRSea/Thesis S4/Database uptake rate_final.xlsx",
                        sheet="sources",na="NA",skip=3)
str(data.sou)
names(data.sou)

# load ENVIRONMENTAL
data.env  <- read_excel("~/Documents/IMBRSea/Thesis S4/Database uptake rate_final.xlsx",
                        sheet="environmental",na="NA",skip=3)
str(data.env)
names(data.env)

# load EXPERIMENTAL
data.exp  <- read_excel("~/Documents/IMBRSea/Thesis S4/Database uptake rate_final.xlsx",
                        sheet="experimental",na="NA",skip=3)
str(data.exp)
names(data.exp)

data.exp$Vmax   <- as.numeric(data.exp$Vmax)
data.exp$Km     <- as.numeric(data.exp$Km)
data.exp$alpha  <- as.numeric(data.exp$alpha)
data.exp$sampling_inter <- as.numeric(data.exp$sampling_inter)

# MERGE spreadsheets
data.ee <- merge(data.env,data.exp,by="id_short")
data.all <- merge(data.sou, data.ee, by="study_id")

        nrow(data.all)
        #[1] 1170

# FILTER OUT roots/rhizomes
data.new <- filter(data.all, !species_compartm %in% c("Roots/rhizoids"))
#View(data.all)
        nrow(data.all)
        #[1] 1036

# FILTER OUT nutrients AMINO ACID + UREA
data.new <- filter(data.new, !nutrient %in% c("Amino acid"))
        nrow(data.new)
        #[1] 978
data.new <- filter(data.new, !nutrient %in% c("Urea"))
        nrow(data.all)
        #[1] 958

# FILTER by species type
data.seagrass <- data.all[data.all$species_type == "Seagrass",]
data.algae <- data.all[data.all$species_type == "Algae",]

# FILTER by uptake type
data.surge <- data.all[data.all$type_uptake =="Surge",]
data.intern <- data.all[data.all$type_uptake == "Int. contr. phase",]


# EXPLORATORY -------------------------------------------------------------

# Species
species_list <- unique(data.all[,c("species_phyla", "species")])
species_list <- arrange(species_list, species_phyla, species)

#view(species_list)
#nrow(species_list)
#table(species_list)

write.table(as.data.frame(species_list),file="Species list.csv", quote=F,sep=",",row.names=F)

# Genera
data.all$genera <- sapply(strsplit(data.all$species, " "), "[",1)

species.genera <- data.frame(table(data.all$genera))
str(species.genera)

#write.csv(species.genera, file="Species genera.csv", sep = ",")
#species.genera  <- read_csv("~/Documents/IMBRSea/Thesis S4/RStudio Uptake rates/Species genera.csv")
#str(species.genera)

species.genera.phyla <- unique(data.all[,c("species_phyla", "genera")])
str(species.genera.phyla)

species_genera <- merge(species.genera,species.genera.phyla)
str(species_genera)

col_order <- c("species_phyla", "genera", "frequency")
species_genera <- species_genera[, col_order]
species_genera

species_genera %>% arrange(species_phyla) %>% write_csv("Species genera.csv")

# STAT - CORRELATIONS --------------------------------------------------------------

#DATA SET
data.mod <- data.new[,c("species_type", "species_phyla", "nutrient", "type_uptake", #fixed factors
                        "species", "genera",                                        #random factors?
                        "temperature_experiment",                                   #co-variable?
                        "Vmax", "alpha")]                                           #response variables


#Correlation Vmax and alpha
ggplot(data.mod, aes(x=alpha, y=Vmax, colour=species_type)) +
        geom_point(shape=21) +
        geom_smooth(method="lm") +
        scale_x_log10() +
        scale_y_log10()

ggplot(data.mod, aes(x=alpha, y=Vmax, colour=species_type)) +
        geom_point(shape=21) +
        geom_smooth(method="lm") +
        scale_x_continuous(limits=c(0, 30))

ggplot(data.mod, aes(x=alpha, y=Vmax, colour=species_type)) +
        geom_point(shape=21) +
        geom_smooth(method="lm") +
        facet_grid(.~type_uptake) +
        scale_x_log10() +
        scale_y_log10()

ggplot(data.mod, aes(x=alpha, y=Vmax, colour=species_phyla)) +
        geom_point(shape=21) +
        geom_smooth(method="lm") +
        scale_x_log10() +
        scale_y_log10() +
        theme_bw()

ggplot(data.mod, aes(x=alpha, y=Vmax, colour=species_phyla)) +
        geom_point(shape=21) +
        geom_smooth(method="lm") +
        scale_x_continuous(limits=c(0, 30))


# STAT - STEPWISE REGRESSION (MODEL 1)----------------------------------------------
library("MASS")

#fit the full model
mod1 <- lm(log10(Vmax)~species_type*nutrient*type_uptake, data.mod) #interactive
#mod2 <- lm(log10(Vmax)~species_type+nutrient+type_uptake, data.mod) #additive
 
step1 <- stepAIC(mod1, direction="both", trace=TRUE)
summary(mod1)
anova(step1)

# STAT - LMER (MODEL 2)-------------------------------------------------------------

names(data.mod)

mod2<- lmer(log10(Vmax)~species_type*nutrient*type_uptake+(1|species), data.mod)
mod3<- lmer(log10(Vmax)~Species_type+nutrient+type_uptake+(1|species), data.mod)

AIC(mod2, mod3)




