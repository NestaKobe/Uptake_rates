# METADATA ----------------------------------------------------------------

## SIMON VON SACHSEN-COBURG UND GOTHA'S MSc THESIS
## Created: Faro, 24th June 2021
## Last modification: 05/07/2021

## Simon Coburg
## email: simon.vonsachsencoburgundgotha@imbrsea.eu

## CODE FOR
#1 PRE-STATISTICS

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

# FILTER by species type
data.seagrass <- data.all[data.all$species_type == "Seagrass",]
data.algae <- data.all[data.all$species_type == "Algae",]

# FILTER by uptake type
data.surge <- data.all[data.all$type_uptake =="Surge",]
data.intern <- data.all[data.all$type_uptake == "Int. contr. phase",]



# STATISTICS -------------------------------------------------------------

# ALPHA
summary(data.all$alpha)
summary(data.algae$alpha)
summary(data.seagrass$alpha)

# VMAX
summary(data.all$Vmax)
summary(data.algae$Vmax)
summary(data.seagrass$Vmax)


#Testing for normality
#ALPHA
shapiro.test(data.all$alpha)
shapiro.test(data.seagrass$alpha)
shapiro.test(data.algae$alpha)

#VMAX
shapiro.test(data.all$Vmax)
shapiro.test(data.seagrass$Vmax)
shapiro.test(data.algae$Vmax)


#Kruskall wallis
#ALPHA
kruskal.test(type_uptake ~ alpha, data = data.all)

pairwise.wilcox.test(data.all$alpha, data.all$species_phyla,
                     p.adjust.method = "BH")
#VMAX
kruskal.test(type_uptake ~ Vmax, data = data.all)

pairwise.wilcox.test(data.all$Vmax, data.all$species_phyla,
                     p.adjust.method = "BH")


# DATA DISTRIBUTION

# ALPHA
#Testing for skewness - higher number = the bigger the skew
skewness(data.all$alpha, na.rm=TRUE)
#[1] 19.86958

#Visualize skewness
g <- ggplot(data.all, aes(x=alpha))+
        geom_density() +
        stat_overlay_normal_density(color = "red", linetype = "dashed") +
        ggtitle("Alpha distribution") + 
        labs(x="Alpha", y="Density") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5)) +
        geom_text(x=350, y= 0.275, label="Skewness = 19.86958", size=3.5, fontface="italic")
g
ggsave(filename = "Alpha density.png", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)

# VMAX
#Testing for skewness
skewness(data.all$Vmax, na.rm=TRUE)
#[1] 4.427202

#Visualize skewness
g <- ggplot(data.all, aes(x=Vmax))+
        geom_density() +
        stat_overlay_normal_density(color = "red", linetype = "dashed") +
        ggtitle("Vmax distribution") + 
        labs(x="Vmax", y="Density") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5)) +
        geom_text(x=825, y= 0.0235, label="Skewness = 4.427202", size=3.5, fontface="italic")
g
ggsave(filename = "Vmax density.png", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)


# DATA TRANSFORMATION

#Transform data by Log10
data.all.log <- data.all
data.all.log$alpha <- log10(data.all.log$alpha)
data.all.log$Vmax <- log10(data.all.log$Vmax)


#ALPHA
#Testing for skewness anew
skewness(data.all.log$alpha, na.rm=TRUE)
#[1] -1.159578

#Visualize data transformation
g <- ggplot(data.all.log, aes(x=alpha)) +
        geom_density() +
        stat_overlay_normal_density(color = "red", linetype = "dashed") +
        ggtitle("Alpha distribution after Log10 transformation") + 
        labs(x="Alpha", y="Density") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5)) +
        geom_text(x=2.5, y= 0.47, label="Skewness = -1.159578", size=3.5, fontface="italic")

g
ggsave(filename = "Alpha log.png", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)

#Testing for normality
shapiro.test(data.all.log$alpha)


#VMAX
#Testing for skewness anew
skewness(data.all.log$Vmax, na.rm=TRUE)
#[1] -0.9496007

#Visualize data transformation
g <- ggplot(data.all.log, aes(x=Vmax)) +
        geom_density() +
        stat_overlay_normal_density(color = "red", linetype = "dashed") +
        ggtitle("Vmax distribution after Log10 transformation") + 
        labs(x="Vmax", y="Density") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5)) +
        geom_text(x=4, y= 0.41, label="Skewness = -0.9496007", size=3.5, fontface="italic")

g
ggsave(filename = "Vmax log.png", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)

#Testing for normality
shapiro.test(data.all.log$Vmax)



# ANOVA
# ALPHA        
# ONE WAY
one.way <- aov(alpha ~ species_type, data=data.all.log)
summary(one.way)

# TWO WAY
two.way_1 <- aov(alpha ~ species_type + nutrient, data=data.all.log)
summary(two.way_1)

two.way_2 <- aov(alpha ~ species_type + type_uptake, data=data.all.log)
summary(two.way_2)

# THREE WAY
three.way <- aov(alpha ~ species_type + type_uptake + nutrient, data=data.all.log)
summary(three.way)

#using temperature
#three.way_2 <- aov(alpha ~ species_type*temperature_experiment + type_uptake*temperature_experiment + nutrient*temperature_experiment, data=data.all.log)
#summary(three.way_2)

# AIC - find best-fit model
#library(AICcmodavg)

model.set <- list(one.way, two.way_1, two.way_2, three.way_1, three.way_2)
model.names <- c("one.way", "two.way_1", "two.way_2", "three.way", "")

aictab(model.set, modnames = model.names)


par(mfrow=c(2,2))
plot(three.way)
par(mfrow=c(1,1))


# POST HOC
TukeyHSD(three.way)

# HOMOGENEITY OF VARIANCE
#library(car)
leveneTest(alpha ~ species_type, data = data.all.log)
leveneTest(alpha ~ type_uptake, data = data.all.log)
leveneTest(alpha ~ nutrient, data = data.all.log)



# ANCOVA - missing co-variate temperature
#ALPHA
alpha.model <- lm(alpha ~ species_type * type_uptake  * nutrient, data = data.all.log)
anova(alpha.model)

summary(alpha.model)

# ASSUMPTIONS

# LINEARITY
#library(ggpubr)
ggscatter(data.all.log, x = "temperature_experiment", y = "alpha",
          facet.by = c("type_uptake", "species_type"),
          short.panel.labs = FALSE) +
        stat_smooth(method = "loess", span = 0.)

# HOMOGENEITY OF REGRESSION SLOPES
#library(rstatix)
anova_test(alpha.model ~ species_type + type_uptake + nutrient +
                   species_type*type_uptake + species_type*nutrient +
                   type_uptake*nutrient, data = data.all.log)


# NORMALITY OF RESIDUALS
alpha_residuals <- residuals(object = alpha.model)
shapiro.test(alpha_residuals)


#library(lindia)
gg_diagnose(alpha.model)


# MIXED MODEL
#library(lme4)

mixed.lmer <- lmer(alpha ~ species_type + type_uptake + nutrient + (1|temperature_experiment), data = data.all.log)
summary(mixed.lmer)

0.3473/(0.3473 + 0.6192)  # ~36 %

plot(mixed.lmer)

qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))


# OUTLIERS --------------------------------------------------------------

#Grubbs test - only for single outliers
#install.packages("outliers")
#library(outliers)
#grubbs.test(data.algae$alpha)

#Rosner's test
install.packages("EnvStats")
library(EnvStats)

test <- rosnerTest(data.all$alpha)
test

test$all.stats

data.all[755,]["study_id"]
data.all[754,]["study_id"]

grubbs.test(data.all$alpha) #, opposite=TRUE if lowest value needed
#G = 23.94424, U = 0.50618, p-value < 2.2e-16
#alternative hypothesis: highest value 410 is an outlier

data.all.out <- data.all[-c(755),]
rosnerTest(data.all.out$alpha)
grubbs.test(data.all.out$alpha)
data.all.out <- data.all.out[-c(754),]
rosnerTest(data.all.out$alpha)
grubbs.test(data.all.out$alpha)

#Outliers algae
testA <- rosnerTest(data.algae$alpha, k=4)
testA



data.algae[530,]["study_id"]
data.algae[531,]["study_id"]
data.algae[209,]["study_id"]
data.algae[743,]["study_id"]

data.algae.out <- data.algae[-c(530, 531),] #Removing extreme outliers

#rosnerTest(data.algae.out$alpha)
grubbs.test(data.all.out$alpha)

#Q-Q plot - correlation between a given sample and the normal distribution
ggqqplot(data.algae.out$alpha, main ="Q-Q plot", xlab="Alpha algae")

#Testing for normality
#shapiro.test(data.algae.out$alpha)

#Outliers seagrass  
testS <- rosnerTest(data.seagrass$alpha, k=3)
testS

data.seagrass[313,]["study_id"]
data.seagrass[314,]["study_id"]
data.seagrass[315,]["study_id"]

data.seagrass.out <- data.seagrass[-c(314, 313),] #Removing extreme outliers

#rosnerTest(data.seagrass.out$alpha, k=3)

#Q-Q plot - correlation between a given sample and the normal distribution
#ggqqplot(data.seagrass.out$alpha, main ="Q-Q plot", xlab="Alpha seagrass")

#Testing for normality
#shapiro.test(data.seagrass.out$alpha)