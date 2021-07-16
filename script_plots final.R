# METADATA ----------------------------------------------------------------

## SIMON VON SACHSEN-COBURG UND GOTHA'S MSc THESIS
## Created: Faro, 24th June 2021
## Last modification: 16/07/2021

## Simon Coburg
## email: simon.vonsachsencoburgundgotha@imbrsea.eu

## CODE FOR
#1 DATA STRUCTURE
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

# quality controls
length(unique(data.env$id_short))
length(unique(data.exp$id_short))

# MERGE spreadsheets
data.ee <- merge(data.env,data.exp,by="id_short")
data.all <- merge(data.sou, data.ee, by="study_id")


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


# DATA SUMMARY (data.lea) ------------------------------------------------------------

# data structure
table(data.lea$species_type,data.lea$nutrient, data.lea$type_uptake)
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

# Summary
summary(log10(data.exp$Vmax))
summary(log10(data.exp$alpha))


# clean
rm(data,species)


# DATA SUMMARY INORGANIC (data.lea) ------------------------------------------------------------

# FILTER OUT inorganic nutrients
data.lea.org <- data.lea[data.lea$nutrient_group=="organic",]
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



# EXPLORATORY PLOTS -------------------------------------------------------------

###DATA.SOURCES

## Number of studies assessed
genv <- unique(data.all[,c("study_id", "year")]) # create data for ggplot
#using the unique function with study_id and year you eliminate repeated countries within the same study

gdata <- data.frame(table(genv$year)) # this is to create a frequency table by year
names(gdata) <- c("year","n")

sum(gdata$n)

rm(gdata, genv) # delete gdata


## Publication year
genv <- unique(data.all[,c("study_id", "year")])# create data for ggplot
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
        theme(axis.text=element_text(size=12), axis.title.y = element_text(size=12)) 
g
ggsave(filename = "Publication year.svg", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)

rm(gdata, genv) # delete gdata


###DATA.ENV

## Sampling country
genv <- unique(data.all[,c("study_id", "sample_country")]) # create data for ggplot - 
#using the unique function with study_id and sample_country you eliminate repeated countries within the same study

gdata <- data.frame(table(genv$sample_country)) # this is to create a frequency table for countries
names(gdata) <- c("sample_country","n")

g <- ggplot(gdata, aes(x=reorder(sample_country,n), y=n)) +   # here I have added "reorder" so in the plot you can see countries ordered by frequency
        geom_bar(stat="identity", na.rm=FALSE) + 
        coord_flip() +
        scale_x_discrete("\n Sampling countries") +
        scale_y_continuous("Number of studies\n", limits = c(0, 28), breaks = seq(0, 28, by = 2)) +
        ##adjust scale, if numbers higher than limits set --> excluded from plot
        theme_bw() +
        theme(axis.text=element_text(size=11), axis.title.y = element_text(size=11))
        
g
ggsave(filename = "Study sites.svg", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)

rm(gdata, genv) # delete gdata


# DATA DISTRIBUTION PLOTS ------------------------------------------------

#Original data distribution
#both Vmax & Alpha are highly skewed - non normal distributed residuals (see script_pre-stats.R)

#Log transformed
my_comparisons <- list(c("Algae", "Seagrass"))

#VMAX
g <- ggplot(data.lea, aes(x=species_type, y=log10(Vmax), fill=species_type)) +
        geom_boxplot(width=0.6) +
        #stat_compare_means(label.y=3.75, label.x=1.25) +
        #stat_compare_means(label = "p.signif", label.y=3.25, label.x = 0.965) +
        stat_compare_means(comparisons=my_comparisons) +
        scale_y_continuous(limits = c(-3.5, 3.5)) +
        labs(x="", y="(log10) Vmax [µmol g^-1 dw h^-1]\n") + #adding [\n] to axis label to increase gap
        theme_bw() +
        theme(axis.text=element_text(size=12), axis.title.y = element_text(size=13)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "")
        #geom_text(x=1.3, y= -4.2, label="n = ", size=3.7) + #Algae alpha values
        #geom_text(x=2.3, y= -4.2, label="n = 220", size=3.7)  #Seagrass alpha values

g
ggsave(filename = "Vmax inorganic.svg", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)

#ALPHA
g <- ggplot(data.lea, aes(x=species_type, y=log10(alpha), fill=species_type)) +
        geom_boxplot(width=0.6) +
        #stat_compare_means(label.y=4, label.x=1.25) +
        #stat_compare_means(label = "p.signif", label.y = 2.8, label.x = 0.965) +
        stat_compare_means(comparisons = my_comparisons) +
        scale_y_continuous(limits = c(-3.5, 3.5)) +
        labs(x="", y="(log10) Alpha [l g^-1 dw h^-1]\n") + #adding [\n] to axis label to increase gap
        theme_bw() +
        theme(axis.text=element_text(size=12), axis.title.y = element_text(size=13)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "")
        #geom_text(x=1.3, y= -4.2, label="n = 801", size=3.7) + #Algae alpha values
        #geom_text(x=2.3, y= -4.2, label="n = 226", size=3.7)  #Seagrass alpha values

g
ggsave(filename = "Alpha inorganic.svg", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)


#---#

# by uptake types
#my_comparisons <- list(c("Algae", "Seagrass"))

#VMAX
g <- ggplot(data.lea, aes(x=species_type, y=log10(Vmax), fill=species_type)) +
        geom_boxplot(width=0.6) +
        #stat_compare_means(size=3.5, label.y = 3.5) +
        #stat_compare_means(label = "p.signif", label.y = 3, label.x = 0.95, hide.ns = TRUE) +
        stat_compare_means(comparisons=my_comparisons) +
        facet_grid(.~type_uptake) +
        scale_y_continuous(limits = c(-4, 4)) +
        labs(x="", y="(log10) Vmax [µmol g^-1 dw h^-1]\n") + #adding [\n] to axis label to increase gap
        theme_bw() +
        theme(axis.text=element_text(size=12), axis.title.y = element_text(size=13)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "")

g
ggsave(filename = "Vmax inorg. uptake type.svg", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)

#ALPHA
g <- ggplot(data.lea, aes(x=species_type, y=log10(alpha), fill=species_type)) +
        geom_boxplot(width=0.6) +
        stat_compare_means(comparisons = my_comparisons) +
        facet_grid(.~type_uptake) +
        scale_y_continuous(limits = c(-4, 4)) +
        labs(x="", y="(log10) Alpha [l g^-1 dw h^-1]\n") + #adding [\n] to axis label to increase gap
        theme_bw() +
        theme(axis.text=element_text(size=12), axis.title.y = element_text(size=13)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "")
        #geom_text(x=0.5, y= -4.2, label="n = 361", size=3) + #Surge algae values
        #geom_text(x=2.3, y= -4.2, label="n = 130", size=3)  #Surge seagrass  values

g
ggsave(filename = "Alpha inorg. uptake type.svg", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)

#---#

#by nutrients
#my_comparisons <- list(c("Algae", "Seagrass"))

#INORGANIC
#VMAX
g <- ggplot(data.lea, aes(x=species_type, y=log10(Vmax), fill=species_type)) +
        geom_boxplot(width=0.6) +
        #stat_compare_means(label.y = 3.5, size=2.1) +
        stat_compare_means(comparisons = my_comparisons, size=2.5) +
        #stat_compare_means(label = "p.signif", label.y = 3, label.x = 0.925, size=2.5, hide.ns = TRUE) +
        facet_grid(type_uptake~nutrient) +
        scale_y_continuous(limits = c(-4, 4)) +
        labs(x="", y="(log10) Vmax [µmol g^-1 dw h^-1]\n") + #adding [\n] to axis label to increase gap
        theme_bw() +
        theme(axis.text=element_text(size=12), axis.title.y = element_text(size=13)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "bottom") +
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank())

g
ggsave(filename = "Vmax inorg. nutrients.svg", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)

#ALPHA
g <- ggplot(data.lea, aes(x=species_type, y=log10(alpha), fill=species_type)) +
        geom_boxplot(width=0.6) +
        #stat_compare_means(label.y = 2.6, size=2.1) +
        #stat_compare_means(label = "p.signif", label.y = 2, label.x = 0.89, size=2.5, hide.ns = TRUE) +
        facet_grid(type_uptake~nutrient) +
        stat_compare_means(comparisons = my_comparisons, size=2.5) +
        scale_y_continuous(limits = c(-4, 4)) +
        labs(x="", y="(log10) Alpha [l g^-1 dw h^-1]\n") + #adding [\n] to axis label to increase gap
        theme_bw() +
        theme(axis.text=element_text(size=12), axis.title.y = element_text(size=13)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "bottom") +
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank())

g
ggsave(filename = "Alpha inorg. nutrients.svg", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)


#ORGANIC
#VMAX
g <- ggplot(data.lea.org, aes(x=species_type, y=log10(Vmax), fill=species_type)) +
        geom_boxplot(width=0.6) +
        #stat_compare_means(label.y = 3.5, size=2.1) +
        stat_compare_means(comparisons = my_comparisons, size=2.5) +
        #stat_compare_means(label = "p.signif", label.y = 3, label.x = 0.925, size=2.5, hide.ns = TRUE) +
        facet_grid(type_uptake~nutrient) +
        scale_y_continuous(limits = c(-4, 4)) +
        labs(x="", y="(log10) Vmax [µmol g^-1 dw h^-1]\n") + #adding [\n] to axis label to increase gap
        theme_bw() +
        theme(axis.text=element_text(size=12), axis.title.y = element_text(size=13)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "bottom") +
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank())

g
ggsave(filename = "Vmax org. nutrients.svg", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)

#ALPHA
g <- ggplot(data.lea.org, aes(x=species_type, y=log10(alpha), fill=species_type)) +
        geom_boxplot(width=0.6) +
        #stat_compare_means(label.y = 2.6, size=2.1) +
        #stat_compare_means(label = "p.signif", label.y = 2, label.x = 0.89, size=2.5, hide.ns = TRUE) +
        facet_grid(type_uptake~nutrient) +
        stat_compare_means(comparisons = my_comparisons, size=2.5) +
        scale_y_continuous(limits = c(-4, 4)) +
        labs(x="", y="(log10) Alpha [l g^-1 dw h^-1]\n") + #adding [\n] to axis label to increase gap
        theme_bw() +
        theme(axis.text=element_text(size=12), axis.title.y = element_text(size=13)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "bottom") +
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank())

g
ggsave(filename = "Alpha org. nutrients.svg", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)
#---#


# by phyla ----------------------------------------------------------------

my_comparisons <- list(c("Chlorophyta", "Ochrophyta"), c("Chlorophyta", "Rhodophyta"), c("Chlorophyta", "Tracheophyta"),
                                      c("Ochrophyta", "Rhodophyta"), c("Ochrophyta", "Tracheophyta"), c("Rhodophyta", "Tracheophyta"))
#Created in order to compare between groups with command stat_compare_means(comparisons=) within facet_grid

        #Check colourblindfriendly colours for phyla (green, brown & red algae)
        #library(RColorBrewer)
        #display.brewer.all(colorblindFriendly = T)
        #display.brewer.pal(n=12, name= "RdBu")
        #brewer.pal(n=11, name= "RdBu")

#VMAX
g <- ggplot(data.lea, aes(x=species_phyla, y=log10(Vmax), fill=species_phyla)) +
        scale_fill_manual(values = c("#A1D99B", "#B35806", "#D6604D", "#44AA99")) +
        geom_boxplot(width=0.5) +
        facet_grid(type_uptake~.) +
        #stat_compare_means(label.y= 3.8, size=3.5) +
        #stat_compare_means(label = "p.signif", ref.group = ".all.", label.y=3, hide.ns = TRUE) +
        stat_compare_means(comparisons = my_comparisons, size=2.8) +
        #stat_compare_means(label = "p.signif", label.y = 10, label.x = 0.89, size=2.5, hide.ns = TRUE) +
        scale_y_continuous(limits = c(-4, 7.5)) +
        labs(x="\nPhyla", y="(log10) Vmax [µmol g^-1 dw h^-1]\n") + #adding [\n] to axis label to increase gap
        theme_bw() +
        theme(axis.text=element_text(size=10), axis.title.y = element_text(size=13), axis.title.x = element_text(size=13)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "")

g
ggsave(filename = "Vmax inorg. phyla.svg", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)


#ALPHA
g <- ggplot(data.lea, aes(x=species_phyla, y=log10(alpha), fill=species_phyla)) +
        scale_fill_manual(values = c("#A1D99B", "#B35806", "#D6604D", "#44AA99")) +
        geom_boxplot(width=0.5) +
        facet_grid(type_uptake~.) +
        #stat_compare_means(label.y=3.9, size=3.5) +
        #stat_compare_means(label = "p.signif", ref.group = ".all.", label.y=2, hide.ns = TRUE) +
        stat_compare_means(comparisons = my_comparisons, size=2.8) +
        scale_y_continuous(limits = c(-4, 7.5)) +
        labs(x="\nPhyla", y="(log10) Alpha [l g^-1 dw h^-1]\n") + #adding [\n] to axis label to increase gap
        theme_bw() +
        theme(axis.text=element_text(size=10), axis.title.y = element_text(size=13), axis.title.x = element_text(size=13)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "")
        
g
ggsave(filename = "Alpha inorg. phyla.svg", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)

