# METADATA ----------------------------------------------------------------

## SIMON VON SACHSEN-COBURG UND GOTHA'S MSc THESIS
## Created: Faro, 24th June 2021
## Last modification: 05/07/2021

## Simon Coburg
## email: simon.vonsachsencoburgundgotha@imbrsea.eu

## CODE FOR
#1 PLOTS


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
#str(data.sou)
#names(data.sou)

# load ENVIRONMENTAL
data.env  <- read_excel("~/Documents/IMBRSea/Thesis S4/Database uptake rate_final.xlsx",
                        sheet="environmental",na="NA",skip=3)
#str(data.env)
#names(data.env)

# load EXPERIMENTAL
data.exp  <- read_excel("~/Documents/IMBRSea/Thesis S4/Database uptake rate_final.xlsx",
                        sheet="experimental",na="NA",skip=3)
#str(data.exp)
#names(data.exp)

data.exp$Vmax   <- as.numeric(data.exp$Vmax)
data.exp$Km     <- as.numeric(data.exp$Km)
data.exp$alpha  <- as.numeric(data.exp$alpha)
data.exp$sampling_inter <- as.numeric(data.exp$sampling_inter)

# MERGE spreadsheets
data.ee <- merge(data.env,data.exp,by="id_short")
data.all <- merge(data.sou, data.ee, by="study_id")

#nrow(data.all)
#[1] 1170

# FILTER OUT roots/rhizomes
data.new <- filter(data.all, !species_compartm %in% c("Roots/rhizoids"))
#View(data.new)
nrow(data.new)
#[1] 1036
        
# FILTER FOR ORGANIC nutrients ONLY
data.amino <- filter(data.new[data.new$nutrient == "Amino acid",])
nrow(data.amino)
#[1] 58
data.urea <-  filter(data.new[data.new$nutrient == "Urea",])
nrow(data.urea)
#[1] 20
        # MERGE datasets
        data.organic <- merge(data.amino, data.urea, by="id_short")
        rm(data.amino, data.urea)

# FILTER OUT nutrients AMINO ACID + UREA
data.inorganic <- filter(data.new, !nutrient %in% c("Amino acid"))
nrow(data.inorganic)
#[1] 978
data.inorganic <- filter(data.inorganic, !nutrient %in% c("Urea"))
nrow(data.inorganic)
#[1] 958


# EXPLORATORY PLOTS -------------------------------------------------------------

##Number of studies assessed
genv <- unique(data.all[,c("study_id", "year")]) # create data for ggplot
#using the unique function with study_id and year you eliminate repeated countries within the same study

gdata <- data.frame(table(genv$year)) # this is to create a frequency table by year
names(gdata) <- c("year","n")

sum(gdata$n)

rm(gdata, genv) # delete gdata

#---#

###DATA.SOURCES

##Publication year
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

#---#

###DATA.ENV

#Sampling country
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


# DATA DISTRIBUTION PLOTS ALPHA ------------------------------------------------

#Original data distribution
#both Vmax & Alpha are highly skewed - non normal distributed residuals (see script_pre-stats.R)

#Log transformed
my_comparisons <- list(c("Algae", "Seagrass"))

#VMAX
g <- ggplot(data.inorganic, aes(x=species_type, y=log10(Vmax), fill=species_type)) +
        geom_boxplot(width=0.6) +
        #stat_compare_means(label.y=3.75, label.x=1.25) +
        #stat_compare_means(label = "p.signif", label.y=3.25, label.x = 0.965) +
        stat_compare_means(comparisons=my_comparisons) +
        scale_y_continuous(limits = c(-4.5, 4)) +
        labs(x="", y="(log10) Vmax [µmol g^-1 dw h^-1]\n") + #adding [\n] to axis label to increase gap
        theme_bw() +
        theme(axis.text=element_text(size=12), axis.title.y = element_text(size=13)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "") +
        geom_text(x=1.3, y= -4.2, label="n = 709", size=3.7) + #Algae alpha values
        geom_text(x=2.3, y= -4.2, label="n = 220", size=3.7)  #Seagrass alpha values

g
ggsave(filename = "Vmax inorganic.svg", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)

#ALPHA
g <- ggplot(data.new, aes(x=species_type, y=log10(alpha), fill=species_type)) +
        geom_boxplot(width=0.6) +
        #stat_compare_means(label.y=4, label.x=1.25) +
        #stat_compare_means(label = "p.signif", label.y = 2.8, label.x = 0.965) +
        stat_compare_means(comparisons = my_comparisons) +
        scale_y_continuous(limits = c(-4.5, 4)) +
        labs(x="", y="(log10) Alpha [l g^-1 dw h^-1]\n") + #adding [\n] to axis label to increase gap
        theme_bw() +
        theme(axis.text=element_text(size=12), axis.title.y = element_text(size=13)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "") +
        geom_text(x=1.3, y= -4.2, label="n = 801", size=3.7) + #Algae alpha values
        geom_text(x=2.3, y= -4.2, label="n = 226", size=3.7)  #Seagrass alpha values

g
ggsave(filename = "Alpha inorganic.svg", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)


#---#

# by uptake types
#my_comparisons <- list(c("Algae", "Seagrass"))

#VMAX
g <- ggplot(data.inorganic, aes(x=species_type, y=log10(Vmax), fill=species_type)) +
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
g <- ggplot(data.inorganic, aes(x=species_type, y=log10(alpha), fill=species_type)) +
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

#VMAX
g <- ggplot(data.inorganic, aes(x=species_type, y=log10(Vmax), fill=species_type)) +
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
g <- ggplot(data.inorganic, aes(x=species_type, y=log10(alpha), fill=species_type)) +
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

#---#

# by phyla
my_comparisons <- list(c("Chlorophyta", "Ochrophyta"), c("Chlorophyta", "Rhodophyta"), c("Chlorophyta", "Tracheophyta"),
                                      c("Ochrophyta", "Rhodophyta"), c("Ochrophyta", "Tracheophyta"), c("Rhodophyta", "Tracheophyta"))
#Created in order to compare between groups with command stat_compare_means(comparisons=) within facet_grid

        #Check colourblindfriendly colours for phyla (green, brown & red algae)
        #library(RColorBrewer)
        #display.brewer.all(colorblindFriendly = T)
        #display.brewer.pal(n=12, name= "RdBu")
        #brewer.pal(n=11, name= "RdBu")
#VMAX
g <- ggplot(data.inorganic, aes(x=species_phyla, y=log10(Vmax), fill=species_phyla)) +
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
ggsave(filename = "Vmax inorg. phyla.png", g) +
        theme(plot.title = element_text(hjust = 0.5), dpi = 600, limitsize = TRUE)



#ALPHA
g <- ggplot(data.inorganic, aes(x=species_phyla, y=log10(alpha), fill=species_phyla)) +
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



# VALUES PER INDEPENDENT FACTOR -------------------------------------------

#Overall
sum(!is.na(data.surge$alpha))
        #[1] 491
sum(!is.na(data.intern$alpha))
        #[1] 536

## BY UPTAKE TYPE
#SURGE
#SEAGRASS
data.surge.seagrass <- data.surge[data.surge$species_type == "Seagrass",]

        #VMAX 
        sum(!is.na(data.surge.seagrass$Vmax))
                #[1]
        #ALPHA
        sum(!is.na(data.surge.seagrass$alpha))
                #[1] 130
#ALGAE
data.surge.algae <- data.surge[data.surge$species_type == "Algae",]
        #VMAX
        sum(!is.na(data.surge.algae$Vmax))
                #[1]
        #ALPHA
        sum(!is.na(data.surge.algae$alpha))
                #[1] 361
                

#INT. CONTR. PHASE
#SEAGRASS
data.intern.seagrass <- data.intern[data.intern$species_type == "Seagrass",]
        #VMAX
        sum(!is.na(data.intern.seagrass$Vmax))
                #[1] 96
        #ALPHA
        sum(!is.na(data.intern.seagrass$alpha))
                #[1] 96
#ALGAE
data.intern.algae <- data.intern[data.intern$species_type == "Algae",]
        #VMAX
        sum(!is.na(data.intern.algae$Vmax))
                #[1] 398
        #ALPHA
        sum(!is.na(data.intern.algae$alpha))
                #[1] 440
        
                
#SURGE
        #VMAX
        #SEAGRASS
        sum(!is.na(data.surge.seagrass$Vmax))
                #[1] 128
        #ALGAE
        sum(!is.na(data.surge.algae$Vmax))
                #[1] 311
        

## BY NUTRIENTS
#SURGE
data.surge.seagrass.amin <- data.surge.seagrass[data.surge.seagrass$nutrient == "Amino acid",]
data.surge.seagrass.ammo <- data.surge.seagrass[data.surge.seagrass$nutrient == "Ammonium",]
data.surge.seagrass.nitr <- data.surge.seagrass[data.surge.seagrass$nutrient == "Nitrate",]
data.surge.seagrass.phos <- data.surge.seagrass[data.surge.seagrass$nutrient == "Phosphate",]
data.surge.seagrass.urea <- data.surge.seagrass[data.surge.seagrass$nutrient == "Urea",]

data.surge.algae.amin <- data.surge.algae[data.surge.algae$nutrient == "Amino acid",]
data.surge.algae.ammo <- data.surge.algae[data.surge.algae$nutrient == "Ammonium",]
data.surge.algae.nitr <- data.surge.algae[data.surge.algae$nutrient == "Nitrate",]
data.surge.algae.phos <- data.surge.algae[data.surge.algae$nutrient == "Phosphate",]
data.surge.algae.urea <- data.surge.algae[data.surge.algae$nutrient == "Urea",]

        #SEAGRASS
        #ALPHA
        sum(!is.na(data.surge.seagrass.amin$alpha))
        sum(!is.na(data.surge.seagrass.ammo$alpha))
        sum(!is.na(data.surge.seagrass.nitr$alpha))
        sum(!is.na(data.surge.seagrass.phos$alpha))
        sum(!is.na(data.surge.seagrass.urea$alpha))
        
        #SEAGRASS
        #VMAX
        sum(!is.na(data.surge.seagrass.amin$Vmax))
        sum(!is.na(data.surge.seagrass.ammo$Vmax))
        sum(!is.na(data.surge.seagrass.nitr$Vmax))
        sum(!is.na(data.surge.seagrass.phos$Vmax))
        sum(!is.na(data.surge.seagrass.urea$Vmax))

        #ALGAE
        #ALPHA
        sum(!is.na(data.surge.algae.amin$alpha))
        sum(!is.na(data.surge.algae.ammo$alpha))
        sum(!is.na(data.surge.algae.nitr$alpha))
        sum(!is.na(data.surge.algae.phos$alpha))
        sum(!is.na(data.surge.algae.urea$alpha))
        
        #ALGAE
        #VMAX
        sum(!is.na(data.surge.algae.amin$Vmax))
        sum(!is.na(data.surge.algae.ammo$Vmax))
        sum(!is.na(data.surge.algae.nitr$Vmax))
        sum(!is.na(data.surge.algae.phos$Vmax))
        sum(!is.na(data.surge.algae.urea$Vmax))
        

#INT. CONTR. PHASE
data.intern.seagrass.amin <- data.intern.seagrass[data.intern.seagrass$nutrient == "Amino acid",]
data.intern.seagrass.ammo <- data.intern.seagrass[data.intern.seagrass$nutrient == "Ammonium",]
data.intern.seagrass.nitr <- data.intern.seagrass[data.intern.seagrass$nutrient == "Nitrate",]
data.intern.seagrass.phos <- data.intern.seagrass[data.intern.seagrass$nutrient == "Phosphate",]
data.intern.seagrass.urea <- data.intern.seagrass[data.intern.seagrass$nutrient == "Urea",]

data.intern.algae.amin <- data.intern.algae[data.intern.algae$nutrient == "Amino acid",]
data.intern.algae.ammo <- data.intern.algae[data.intern.algae$nutrient == "Ammonium",]
data.intern.algae.nitr <- data.intern.algae[data.intern.algae$nutrient == "Nitrate",]
data.intern.algae.phos <- data.intern.algae[data.intern.algae$nutrient == "Phosphate",]
data.intern.algae.urea <- data.intern.algae[data.intern.algae$nutrient == "Urea",]
        
        #SEAGRASS
        #VMAX
        sum(!is.na(data.intern.seagrass.amin$Vmax))
        sum(!is.na(data.intern.seagrass.ammo$Vmax))
        sum(!is.na(data.intern.seagrass.nitr$Vmax))
        sum(!is.na(data.intern.seagrass.phos$Vmax))
        sum(!is.na(data.intern.seagrass.urea$Vmax))

        #SEAGRASS
        #ALPHA
        sum(!is.na(data.intern.seagrass.amin$alpha))
        sum(!is.na(data.intern.seagrass.ammo$alpha))
        sum(!is.na(data.intern.seagrass.nitr$alpha))
        sum(!is.na(data.intern.seagrass.phos$alpha))
        sum(!is.na(data.intern.seagrass.urea$alpha))
        
        
        #ALGAE
        #VMAX
        sum(!is.na(data.intern.algae.amin$Vmax))
        sum(!is.na(data.intern.algae.ammo$Vmax))
        sum(!is.na(data.intern.algae.nitr$Vmax))
        sum(!is.na(data.intern.algae.phos$Vmax))
        sum(!is.na(data.intern.algae.urea$Vmax))
        
        #ALGAE
        #ALPHA
        sum(!is.na(data.intern.algae.amin$alpha))
        sum(!is.na(data.intern.algae.ammo$alpha))
        sum(!is.na(data.intern.algae.nitr$alpha))
        sum(!is.na(data.intern.algae.phos$alpha))
        sum(!is.na(data.intern.algae.urea$alpha))
        

## BY PHYLA
        
#SURGE
data.surge.chl <- data.surge[data.surge$species_phyla == "Chlorophyta",]
data.surge.och <- data.surge[data.surge$species_phyla == "Ochrophyta",]
data.surge.rho <- data.surge[data.surge$species_phyla == "Rhodophyta",]
data.surge.tra <- data.surge[data.surge$species_phyla == "Tracheophyta",]

        #VMAX
        sum(!is.na(data.surge.chl$Vmax))
        sum(!is.na(data.surge.och$Vmax))
        sum(!is.na(data.surge.rho$Vmax))
        sum(!is.na(data.surge.tra$Vmax))
        
        #ALPHA
        sum(!is.na(data.surge.chl$alpha))
        sum(!is.na(data.surge.och$alpha))
        sum(!is.na(data.surge.rho$alpha))
        sum(!is.na(data.surge.tra$alpha))
        

#INT. CONT. PHASE
data.intern.chl <- data.intern[data.intern$species_phyla == "Chlorophyta",]
data.intern.och <- data.intern[data.intern$species_phyla == "Ochrophyta",]
data.intern.rho <- data.intern[data.intern$species_phyla == "Rhodophyta",]
data.intern.tra <- data.intern[data.intern$species_phyla == "Tracheophyta",]        
        
        #VMAX
        sum(!is.na(data.intern.chl$Vmax))
        sum(!is.na(data.intern.och$Vmax))
        sum(!is.na(data.intern.rho$Vmax))
        sum(!is.na(data.intern.tra$Vmax))
        
        #ALPHA
        sum(!is.na(data.intern.chl$alpha))
        sum(!is.na(data.intern.och$alpha))
        sum(!is.na(data.intern.rho$alpha))
        sum(!is.na(data.intern.tra$alpha))
        
