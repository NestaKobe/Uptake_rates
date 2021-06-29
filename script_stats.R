# METADATA ----------------------------------------------------------------

## SIMON VON SACHSEN-COBURG UND GOTHA'S MSc THESIS
## Created: Faro, 24th June 2021
## Last modification: 29/06/2021

## Simon Coburg
## email: simon.vonsachsencoburgundgotha@imbrsea.eu

## CODE FOR
#1 STATISTICAL ANALYSIS
#2 PLOTS


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
              "lme4")

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

#data.exp$Vmax   <- as.numeric(data.exp$Vmax)
#data.exp$Km     <- as.numeric(data.exp$Km)
#data.exp$alpha  <- as.numeric(data.exp$alpha)
data.exp$sampling_inter <- as.numeric(data.exp$sampling_inter)

# MERGE spreadsheets
data.ee <- merge(data.env,data.exp,by="id_short")
data.all <- merge(data.sou, data.ee, by="study_id")

nrow(data.all)
        #[1] 1170

# FILTER OUT roots/rhizomes
data.all <- filter(data.all, !species_compartm %in% c("Roots/rhizoids"))
#View(data.all)

nrow(data.all)
        #[1] 1036

# FILTER by species type
data.seagrass <- data.all[data.all$species_type == "Seagrass",]
data.algae <- data.all[data.all$species_type == "Algae",]

# FILTER by uptake type
data.surge <- data.all[data.all$type_uptake =="Surge",]
data.intern <- data.all[data.all$type_uptake == "Int. contr. phase",]


# EXPLORATORY -------------------------------------------------------------

##Number of studies assessed
genv <- unique(data.all[,c("study_id", "year")]) # create data for ggplot - 
#using the unique function with study_id and year you eliminate repeated countries within the same study

gdata <- data.frame(table(genv$year)) # this is to create a frequency table by year
names(gdata) <- c("year","n")

sum(gdata$n)

rm(gdata, genv) # delete gdata

###DATA.SOURCES

##Publication year
genv <- unique(data.all[,c("study_id", "year")]) # create data for ggplot - 
#using the unique function with study_id and year you eliminate repeated countries within the same study

gdata <- data.frame(table(genv$year)) # this is to create a frequency table by year
names(gdata) <- c("year","n")
gdata$year <- as.numeric(as.character(gdata$year))

g <- ggplot(gdata, aes(x=year, y=n)) +
        geom_bar(stat="identity", na.rm=FALSE) +
        #ggtitle("Publication year") + 
        labs(x="\nYear of publication", y="Number of articles\n") +
        scale_x_continuous(limits=c(1975, 2021), breaks=seq(1975,2021,5)) +
        scale_y_continuous(limits=c(0,8), breaks=seq(0,8,2)) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(axis.text.x = element_text(angle = 0)) +
        geom_text(x=1977.5, y= 7.5, label="n = 104", size=3.5) +
        theme(axis.text=element_text(size=10), axis.title.y = element_text(size=10))
g
ggsave(filename = "Publication year.png", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)

rm(gdata, genv) # delete gdata


###DATA.ENV

#Sampling country

genv <- unique(data.all[,c("study_id", "sample_country")]) # create data for ggplot - 
#using the unique function with study_id and sample_country you eliminate repeated countries within the same study

gdata <- data.frame(table(genv$sample_country)) # this is to create a frequency table for countries
names(gdata) <- c("sample_country","n")

g <- ggplot(gdata, aes(x=reorder(sample_country,n), y=n)) +   # here I have added "reorder" so in the plot you can see countries ordered by frequency
        geom_bar(stat="identity", na.rm=FALSE) + 
        coord_flip() +
        scale_x_discrete("\nCountry") +
        scale_y_continuous("Number of studies\n", limits = c(0, 25), breaks = seq(0, 25, by = 2)) +
        ##adjust scale, if numbers higher than limits set --> excluded from plot
        theme_bw()
        
g
ggsave(filename = "Study countries.png", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)

rm(gdata, genv) # delete gdata


###DATA.EXP

###Species type

g <- ggplot(data.all, aes(x=species_type)) +
        geom_bar(stat="count",  width = .5) +
        geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
        ggtitle("Registered values by species type") + 
        labs(x="Species type", y="Number of registered values") +
        theme_bw()+
        theme(plot.title = element_text(hjust = 0.5))
        

g
ggsave(filename = "Registered values by species type.png", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)

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
        
        
        
# ANCOVA
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

# DATA DISTRIBUTION PLOTS ALPHA -----------------------------------------

length(data.algae$alpha[data.algae$alpha !="NA"]) #How many alpha values are there for algae
        #[1] 810
length(data.seagrass$alpha[data.seagrass$alpha !="NA"]) #How many alpha values are there for seagrass
        #[1] 226

g <- ggplot(data.all, aes(x=species_type, y=alpha)) +
        geom_boxplot(outlier.shape = NA, width=0.6) +
        stat_compare_means(method = "anova") +
        stat_compare_means(label = "p.signif", label.y = 4.5, label.x = 0.965) +
        scale_y_continuous(limits = c(0, 5)) +
        labs(x="", y="Alpha [l g^-1 dw h^-1]\n") + #adding [\n] to axis label to increase gap
        theme_bw() +
        theme(axis.text=element_text(size=12), axis.title.y = element_text(size=13)) +
        geom_text(x=1.3, y= 4.2, label="n = 810", size=3.7) + #Algae alpha values
        geom_text(x=2.3, y= 4.2, label="n = 226", size=3.7)  #Seagrass alpha values
        
g
ggsave(filename = "Alpha.png", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)

# by uptake types
g <- ggplot(data.all, aes(x=species_type, y=alpha, fill=species_type)) +
        geom_boxplot(outlier.shape = NA, width=0.6) +
        stat_compare_means(method="anova") +
        stat_compare_means(label = "p.signif", label.y = 7, label.x = 0.965, hide.ns = TRUE) +
        facet_grid(.~type_uptake) +
        scale_y_continuous(limits = c(0, 8)) +
        labs(x="", y="Alpha [l g^-1 dw h^-1]\n") + #adding [\n] to axis label to increase gap
        theme_bw() +
        theme(axis.text=element_text(size=12), axis.title.y = element_text(size=13)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "")

g
ggsave(filename = "Alpha uptake type.png", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)
        
# by nutrients
g <- ggplot(data.all, aes(x=species_type, y=alpha, fill=species_type)) +
        geom_boxplot(outlier.shape = NA, width=0.6) +
        stat_compare_means(method="anova", label.y = 14.5, size=2.5) +
        stat_compare_means(label = "p.signif", label.y = 10, label.x = 0.89, size=2.5, hide.ns = TRUE) +
        facet_grid(type_uptake~nutrient) +
        scale_y_continuous(limits = c(0, 15)) +
        labs(x="", y="Alpha [l g^-1 dw h^-1]\n") + #adding [\n] to axis label to increase gap
        theme_bw() +
        theme(axis.text=element_text(size=12), axis.title.y = element_text(size=13)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "bottom") +
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank())

g
ggsave(filename = "Alpha nutrients.png", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)

# by phyla
my_comparisons <- list(c("Chlorophyta", "Ochrophyta"), c("Chlorophyta", "Rhodophyta"), c("Chlorophyta", "Tracheophyta"),
                                      c("Ochrophyta", "Rhodophyta"), c("Ochrophyta", "Tracheophyta"), c("Rhodophyta", "Tracheophyta"))
#Created in order to compare between groups with command stat_compare_means(comparisons=) within facet_grid

        #Check colourblindfriendly colours for phyla (green, brown & red algae)
        #library(RColorBrewer)
        #display.brewer.all(colorblindFriendly = T)
        #display.brewer.pal(n=12, name= "RdBu")
        #brewer.pal(n=11, name= "RdBu")

g <- ggplot(data.all, aes(x=species_phyla, y=alpha, fill=species_phyla)) +
        scale_fill_manual(values = c("#A1D99B", "#B35806", "#D6604D", "#44AA99")) +
        geom_boxplot(outlier.shape = NA, width=0.5) +
        facet_grid(type_uptake~.) +
        stat_compare_means(method="anova", label.y= 17.5, size=3.5) +
        stat_compare_means(label = "p.signif", method = "t.test", ref.group = ".all.", label.y=15, hide.ns = TRUE) +
        #stat_compare_means(comparisons = my_comparisons) +
        #stat_compare_means(label = "p.signif", label.y = 10, label.x = 0.89, size=2.5, hide.ns = TRUE) +
        scale_y_continuous(limits = c(0, 18)) +
        labs(x="\nPhyla", y="Alpha [l g^-1 dw h^-1]\n") + #adding [\n] to axis label to increase gap
        theme_bw() +
        theme(axis.text=element_text(size=10), axis.title.y = element_text(size=13), axis.title.x = element_text(size=13)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "")
        
g
ggsave(filename = "Alpha phyla.png", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)




# DATA DISTRIBUTION PLOTS VMAX ------------------------------------------

length(data.algae$Vmax[data.algae$Vmax !="NA"]) #How many Vmax values are there for algae
        #[1] 810
length(data.seagrass$Vmax[data.seagrass$Vmax !="NA"]) #How many Vmax values are there for seagrass
        #[1] 226

g <- ggplot(data.all, aes(x=species_type, y=Vmax)) +
        geom_boxplot(outlier.shape = NA, width=0.6) +
        stat_compare_means(method = "anova") +
        stat_compare_means(label = "p.signif", label.y = 41, label.x = 0.985) +
        scale_y_continuous(limits = c(0, 45)) +
        labs(x="", y="Vmax [µmol g^-1 dw h^-1]\n") + #adding [\n] to axis label to increase gap
        theme_bw() +
        theme(axis.text=element_text(size=12), axis.title.y = element_text(size=13)) +
        geom_text(x=1.3, y= 37.5, label="n = 810", size=3.7) + #Algae alpha values
        geom_text(x=2.3, y= 37.5, label="n = 226", size=3.7)  #Seagrass alpha values

g
ggsave(filename = "Vmax.png", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)


# by uptake types
g <- ggplot(data.all, aes(x=species_type, y=Vmax, fill=species_type)) +
        geom_boxplot(outlier.shape = NA, width=0.6) +
        stat_compare_means(method="anova", size=3.5, label.x= 1.75) +
        stat_compare_means(label = "p.signif", label.y = 50, label.x = 0.95, hide.ns = TRUE) +
        facet_grid(.~type_uptake) +
        scale_y_continuous(limits = c(0, 50)) +
        labs(x="", y="Vmax [µmol g^-1 dw h^-1]\n") + #adding [\n] to axis label to increase gap
        theme_bw() +
        theme(axis.text=element_text(size=12), axis.title.y = element_text(size=13)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "")

g
ggsave(filename = "Vmax uptake type.png", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)

# by nutrients
g <- ggplot(data.all, aes(x=species_type, y=Vmax, fill=species_type)) +
        geom_boxplot(outlier.shape = NA, width=0.6) +
        stat_compare_means(method="anova", label.y = 95, size=2.5) +
        stat_compare_means(label = "p.signif", label.y = 75, label.x = 0.925, size=2.5, hide.ns = TRUE) +
        facet_grid(type_uptake~nutrient) +
        scale_y_continuous(limits = c(0, 100)) +
        labs(x="", y="Vmax [µmol g^-1 dw h^-1]\n") + #adding [\n] to axis label to increase gap
        theme_bw() +
        theme(axis.text=element_text(size=12), axis.title.y = element_text(size=13)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "bottom") +
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank())

g
ggsave(filename = "Vmax nutrients.png", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)

# by phyla
my_comparisons <- list(c("Chlorophyta", "Ochrophyta"), c("Chlorophyta", "Rhodophyta"), c("Chlorophyta", "Tracheophyta"),
                       c("Ochrophyta", "Rhodophyta"), c("Ochrophyta", "Tracheophyta"), c("Rhodophyta", "Tracheophyta"))
#Created in order to compare between groups with command stat_compare_means(comparisons=) within facet_grid

        #Check colourblindfriendly colours for phyla (green, brown & red algae)
        #library(RColorBrewer)
        #display.brewer.all(colorblindFriendly = T)
        #display.brewer.pal(n=12, name= "RdBu")
        #brewer.pal(n=11, name= "RdBu")

g <- ggplot(data.all, aes(x=species_phyla, y=Vmax, fill=species_phyla)) +
        scale_fill_manual(values = c("#A1D99B", "#B35806", "#D6604D", "#44AA99")) +
        geom_boxplot(outlier.shape = NA, width=0.5) +
        facet_grid(type_uptake~.) +
        stat_compare_means(method="anova", label.y= 95, label.x = 3.5, size=3.5) +
        stat_compare_means(label = "p.signif", method = "t.test", ref.group = ".all.", label.y=95, hide.ns = TRUE) +
        stat_compare_means(comparisons = my_comparisons) +
        #stat_compare_means(label = "p.signif", label.y = 10, label.x = 0.89, size=2.5, hide.ns = TRUE) +
        scale_y_continuous(limits = c(0, 100)) +
        labs(x="\nPhyla", y="Vmax [µmol g^-1 dw h^-1]\n") + #adding [\n] to axis label to increase gap
        theme_bw() +
        theme(axis.text=element_text(size=10), axis.title.y = element_text(size=13), axis.title.x = element_text(size=13)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "")

g
ggsave(filename = "Vmax phyla.png", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)





# # OUTLIERS --------------------------------------------------------------


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
