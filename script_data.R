# METADATA ----------------------------------------------------------------

## SIMON VON SACHSEN-COBURG UND GOTHA'S MSc THESIS
## Created: Faro, 16th April 2021
## Last modification: 05/07/2021

## Simon Coburg and Carmen dos Santos
## email: simon.vonsachsencoburgundgotha@imbrsea.eu / cbsantos@ualg.pt

## CODE FOR
#1 HANDLING DATA
#2 DATA EXPLORATORY ANALYSIS DURING COMPILATION


# SETTINGS ----------------------------------------------------------------

# load libraries
packages <- c("tidyverse",      # for data science (general) - includes ggplot2
              "readxl",         # for reading xlsx files
              "devtools",
              "ggpubr")


for (i in seq_along(packages)) {
  if(!do.call(require, list(package = packages[i]))) {
    do.call(install.packages, list(pkgs = packages[i]))
    do.call(require, list(package = packages[i]))
  }
}


# clean working environment
rm(list=ls())

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

# quality controls
length(unique(data.env$id_short))
length(unique(data.exp$id_short))

#x <- unique(data.env$id_short)
#y <- unique(data.exp$id_short)

#x <- as.list(x)
#y <- as.list(y)

#capture.output(x, file = "env.csv")
#capture.output(y, file = "exp.csv")

# MERGE spreadsheets
data.ee <- merge(data.env,data.exp,by="id_short")
data.all <- merge(data.sou, data.ee, by="study_id")

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


###DATA.SOU

##Publication type

ggplot(data.sou, aes(x=publication_type)) +
      geom_bar(stat="count", width=0.5) +
      ggtitle("Publication types") + 
      labs(x="Type of publication", y="Number of articles") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))


##Publication year
genv <- unique(data.all[,c("study_id", "year")]) # create data for ggplot - 
#using the unique function with study_id and year you eliminate repeated countries within the same study

gdata <- data.frame(table(genv$year)) # this is to create a frequency table by year
names(gdata) <- c("year","n")
gdata$year <- as.numeric(as.character(gdata$year))

ggplot(gdata, aes(x=year, y=n)) +
      geom_bar(stat="identity", na.rm=FALSE) +
      ggtitle("Publication year") + 
      labs(x="Year of publication", y="Number of articles") +
      scale_x_continuous(limits=c(1975, 2021), breaks=seq(1975,2021,5)) +
      scale_y_continuous(limits=c(0,8), breaks=seq(0,8,2)) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.text.x = element_text(angle = 0))


rm(gdata, genv) # delete gdata


###DATA.ENV

#Sampling country

genv <- unique(data.all[,c("study_id", "sample_country")]) # create data for ggplot - 
#using the unique function with study_id and sample_country you eliminate repeated countries within the same study

gdata <- data.frame(table(genv$sample_country)) # this is to create a frequency table for countries
names(gdata) <- c("sample_country","n")

ggplot(gdata, aes(x=reorder(sample_country,n), y=n)) +   # here I have added "reorder" so in the plot you can see countries ordered by frequency
      geom_bar(stat="identity", na.rm=FALSE) + 
      coord_flip() +
      scale_x_discrete("Country") +
      scale_y_continuous("Number of studies", limits = c(0, 28), breaks = seq(0, 28, by = 2)) +
      ##adjust scale, if numbers higher than limits set --> excluded from plot
      theme_bw()
      

rm(gdata, genv) # delete gdata
        

###DATA.EXP

###Species type

ggplot(data.all, aes(x=species_type)) +
      geom_bar(stat="count",  width = .5) +
      geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
      ggtitle("Registered values by species type") + 
      labs(x="Species type", y="Number of registered values") +
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5))

  
ggplot(data.all, aes(x=species_type, fill=species_phyla)) +
      scale_fill_manual(values = c("forestgreen", "salmon4", "red2", "springgreen4")) +
      geom_bar(stat="count", position=position_dodge(),  width = .6)+
      ggtitle("Registered values by species phyla") + 
      labs(x="Species type", y="Number of registered values") +
      scale_y_continuous(limits = c(0, 350), breaks = seq(0, 350, by =50)) + 
      #adjust scale, if numbers higher than limits set --> excluded from plot
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5), legend.position="right") +
      theme(legend.title = element_blank()) +
      theme(legend.position = "bottom")

ggplot(data.all, aes(x=species_type, fill=species_phyla)) +
      scale_fill_manual(values = c("forestgreen", "salmon4", "red2", "springgreen4")) +
      geom_bar(position="stack", stat="count", width = 0.4, color="black") +
      ggtitle("Registered values by species phyla") + 
      labs(x="Species type", y="Number of registered values") +
      scale_y_continuous(limits = c(0, 850), breaks = seq(0, 850, by =50)) + 
      #adjust scale, if numbers higher than limits set --> excluded from plot
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5), legend.position="right") +
      theme(legend.title = element_blank()) +
      theme(legend.position = "bottom")
 

#data.range[!complete.cases(data.range$species_phyla),]

ggplot(data.all, aes(x=species_phyla, colour=species_compartm, fill=species_compartm)) +
      geom_bar(stat="count", position=position_dodge(), width= .6) +
      ggtitle("Registered values by species compartment for phyla") + 
      labs(x="", y="Number of registered values") +
      scale_y_continuous(limits = c(0, 250), breaks = seq(0, 250, by = 25)) + 
      #adjust scale, if numbers higher than limits set --> excluded from plot
      facet_grid(species_type~.) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
      theme(legend.title = element_blank())

#library(ggpubr)
#ggarrange(spcomp, spphyl, ncol=2, nrow=1)



# Alpha plots -------------------------------------------------------------------

ggplot(data.all, aes(x=alpha))+
      geom_histogram(binwidth=0.1, 
                     breaks = seq(0, 15, by = 0.2), 
                     colour = "black", 
                     fill = "white") +
      geom_rug() +
      geom_density(aes(y=..density..*10), colour="blue") +
      scale_x_continuous(limits=c(0, 15)) +
      #scale_y_continuous(limits=c(0, 20)) +
      facet_grid(type_uptake~species_type) +
      ggtitle("Alpha frequency distribution") + 
      labs(x="Alpha [l g^-1 dw h^-1 µM^-1]", y="Frequency") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))


      #Overlapping densities
      ggplot(data.all, aes(x=alpha, group=species_type, fill=species_type))+
            geom_density(alpha=0.3) +
            facet_grid(.~type_uptake) +
            scale_x_continuous(limits=c(0, 10), breaks=seq(0, 10, by=2.5)) +
            ggtitle("Alpha frequency distribution for Algae vs Seagrass") + 
            labs(x="Alpha [l g^-1 dw h^-1 µM^-1]", y="Frequency") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(legend.title = element_blank()) +
            theme(legend.position = c(.9,.88))
      
      
#Seasonality
ggplot(data=subset(data.all, !is.na(sample_season)), aes(x=alpha)) +
      geom_histogram(binwidth=0.1, 
                     breaks = seq(0, 15, by = 0.2), 
                     colour = "black", 
                     fill = "white") +
      geom_rug() +
      geom_density(aes(y=..density..*10), colour="blue") +
      facet_grid(sample_season~species_type) +
      scale_x_continuous(limits=c(0, 10)) +
      #scale_y_continuous(limits=c(0, 20)) +
      ggtitle("Alpha frequency distribution by season") + 
      labs(x="Alpha [l g^-1 dw h^-1 µM^-1]", y="Frequency") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))


      #Overlapping densities
      ggplot(data=subset(data.all, !is.na(sample_season)), aes(x=alpha, group=species_type, fill=species_type))+
        geom_density(alpha=0.3) +
        facet_grid(sample_season~type_uptake) +
        scale_x_continuous(limits=c(0, 5), breaks=seq(0, 5, by=1)) +
        scale_y_continuous(limits=c(0,2)) +
        ggtitle("Alpha frequency distribution by season and uptake") + 
        labs(x="Alpha [l g^-1 dw h^-1 µM^-1]", y="Frequency") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "bottom")
      


#by nutrients
ggplot(data.all, aes(x=species_type, y=alpha)) +
      geom_boxplot(outlier.shape=NA) +
      geom_jitter(width=0.3, shape=21) +
      facet_grid(type_uptake~nutrient) +
      scale_y_continuous(limits=c(0, 50), breaks=seq(0, 50, by=10)) +
      ggtitle("Alpha values for nutrients") + 
      labs(x="Species type", y="Alpha [l g^-1 dw h^-1 µM^-1]") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))

      #Overlapping densities
      ggplot(data.all, aes(x=alpha, group=species_type, fill=species_type))+
            geom_density(alpha=0.3) +
            facet_grid(type_uptake~nutrient) +
            ggtitle("Alpha frequency distribution by nutrient") + 
            labs(x="Alpha [l g^-1 dw h^-1 µM^-1]", y="Frequency") +
            scale_x_continuous(limits=c(0, 0.75), breaks=seq(0, 1, by=0.25)) +
            #scale_y_continuous(breaks = seq(0, 12, by =5)) +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(legend.title = element_blank()) +
            theme(legend.position = "bottom") +
            theme(axis.text.x = element_text(angle = 90))
      
            #Overlapping densities by seasonality for surge uptake & int. controlled phase
            ggplot(data=subset(data.surge, !is.na(sample_season)), aes(x=alpha, group=species_type, fill=species_type))+
                  geom_density(alpha=0.3) +
                  facet_grid(sample_season~nutrient) +
                  scale_x_continuous(limits=c(0, 3), breaks=seq(0, 3, by=1)) +
                  scale_y_continuous(limits=c(0, 3)) +
                  ggtitle("Alpha surge uptake by nutrient & seasonality") + 
                  labs(x="Alpha [l g^-1 dw h^-1 µM^-1]", y="Frequency") +
                  theme_bw() +
                  theme(plot.title = element_text(hjust = 0.5)) +
                  theme(legend.title = element_blank()) +
                  theme(legend.position = "bottom")
            
            ggplot(data=subset(data.intern, !is.na(sample_season)), aes(x=alpha, group=species_type, fill=species_type))+
                  geom_density(alpha=0.3) +
                  facet_grid(sample_season~nutrient) +
                  scale_x_continuous(limits=c(0, 3), breaks=seq(0, 3, by=1)) +
                  scale_y_continuous(limits=c(0, 3)) +
                  ggtitle("Alpha intern. contr. phase by nutrient & seasonality") + 
                  labs(x="Alpha [l g^-1 dw h^-1 µM^-1]", y="Frequency") +
                  theme_bw() +
                  theme(plot.title = element_text(hjust = 0.5)) +
                  theme(legend.title = element_blank()) +
                  theme(legend.position = "bottom")
      
      
      #Outliers
      out <- boxplot.stats(data.all$alpha)$out #look for outliers
      out_ind <- which(data.all$alpha %in% c(out))
      out_ind
      
      outliers <- data.all[out_ind, ]
      outliers <- unique(outliers[,c("study_id", "id_short", "species", "species_type", 
                                     "species_phyla", "species_compartm", "temperature_experiment", 
                                     "lumination_experiment", "nutrient", "time", "type_uptake", 
                                     "Vmax", "Km", "alpha", "annotations_exp")])
      
      write_csv(outliers, file = "Outliers alpha.csv")
      

#by incubation time
ggplot(data.all, aes(x=time, y=alpha, colour=nutrient)) +
      geom_point(outlier.shape=NA) +
      facet_grid(species_phyla~nutrient) +
      scale_y_continuous(limits = c(0,50), breaks=seq(0,50,by=10)) +
      ggtitle("Alpha values within 60 minutes") + 
      labs(x="Time [minutes]", y="Alpha [l g^-1 dw h^-1 µM^-1]") +
      scale_x_continuous(limits=c(0, 60)) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.position = "none")

  

# Vmax plots --------------------------------------------------------------------

ggplot(data.all, aes(x=Vmax))+
      geom_histogram(binwidth=0.1, 
                     breaks = seq(0, 15, by = 0.2), 
                     colour = "black", 
                     fill = "white") +
      geom_rug() +
      geom_density(aes(y=..density..*10), colour="blue") +
      #scale_x_continuous(limits=c(0, 20)) +
      #scale_y_continuous(limits=c(0, 10)) +
      facet_grid(type_uptake~species_type) +
      ggtitle("Vmax frequency distribution") + 
      labs(x="Vmax [µmol g^-1 dw h^-1]", y="Frequency") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))

      #Overlapping densities
      ggplot(data.all, aes(x=Vmax, group=species_type, fill=species_type))+
            geom_density(alpha=0.4) +
            scale_x_continuous(limits=c(0,100)) +
            facet_grid(.~type_uptake) +
            ggtitle("Vmax frequency distribution for Algae vs Seagrass") + 
            labs(x="Vmax [µmol g^-1 dw h^-1]", y="Frequency") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(legend.title = element_blank()) +
            theme(legend.position = c(.9,.88))

            #Log transformation
            ggplot(data.all, aes(x=Vmax, group=species_type, fill=species_type))+
                  geom_density(alpha=0.5) +
                  scale_x_log10() +
                  facet_grid(.~type_uptake) +
                  ggtitle("Log10 Vmax values (Range)") + 
                  labs(x="(log) Vmax [µmol g^-1 dw h^-1]", y="Frequency") +
                  theme_bw() +
                  theme(plot.title = element_text(hjust = 0.5)) +
                  theme(legend.title = element_blank()) +
                  theme(legend.position = c(.9,.9))
            
#Seasonality
ggplot(data=subset(data.all, !is.na(sample_season)), aes(x=Vmax)) +
      geom_histogram(binwidth=0.1, 
                     breaks = seq(0, 15, by = 0.2), 
                     colour = "black", 
                     fill = "white") +
      geom_rug() +
      geom_density(aes(y=..density..*10), colour="blue") +
      facet_grid(sample_season~species_type) +
      scale_x_continuous(limits=c(0, 10)) +
      #scale_y_continuous(limits=c(0, 20)) +
      ggtitle("Vmax frequency distribution by season") + 
      labs(x="Vmax [µmol g^-1 dw h^-1]", y="Frequency") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))

            
            #Overlapping densities
            ggplot(data=subset(data.all, !is.na(sample_season)), aes(x=Vmax, group=species_type, fill=species_type))+
                  geom_density(alpha=0.3) +
                  facet_grid(sample_season~type_uptake) +
                  scale_x_continuous(limits=c(0, 5), breaks=seq(0, 5, by=1)) +
                  scale_y_continuous(limits=c(0, 4)) +
                  ggtitle("Vmax frequency distribution by season and uptake") + 
                  labs(x="Vmax [µmol g^-1 dw h^-1]", y="Frequency") +
                  theme_bw() +
                  theme(plot.title = element_text(hjust = 0.5)) +
                  theme(legend.title = element_blank()) +
                  theme(legend.position = "bottom")
                
#by nutrients
ggplot(data.all, aes(x=species_type, y=Vmax)) +
      geom_boxplot(outlier.shape=NA) +
      geom_jitter(width=0.3, shape=21) +
      facet_grid(type_uptake~nutrient) +
      scale_y_continuous(limits = c(0, 400)) +
      ggtitle("Vmax values for nutrients") + 
      labs(x="Species type", y="Vmax [µmol g^-1 dw h^-1]") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))


      #Overlapping densities
      ggplot(data.all, aes(x=Vmax, group=species_type, fill=species_type))+
            geom_density(alpha=0.3) +
            facet_grid(type_uptake~nutrient) +
            ggtitle("Vmax frequency distribution by nutrient for Algae vs Seagrass") + 
            labs(x="Vmax [µmol g^-1 dw h^-1]", y="Frequency") +
            scale_x_continuous(limits=c(0, 15), breaks=seq(0, 15, by=5)) +
            #scale_y_continuous(breaks = seq(0, 1.5, by =0.5)) +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(legend.title = element_blank()) +
            theme(legend.position = "bottom") +
            theme(axis.text.x = element_text(angle = 0))
    
    
    #Outliers
    out <- boxplot.stats(data.all$Vmax)$out #look for outliers
    out_ind <- which(data.all$Vmax %in% c(out))
    out_ind
    
    outliers <- data.all[out_ind, ]
    outliers <- unique(outliers[,c("study_id", "id_short", "species", "species_type", 
                                   "species_phyla", "species_compartm", "temperature_experiment", 
                                   "lumination_experiment", "nutrient", "time", "type_uptake", 
                                   "Vmax", "Km", "alpha", "annotations_exp")])
    
    write_csv(outliers, file = "Outliers Vmax.csv")
  


#by incubation time
ggplot(data.all, aes(x=time, y=Vmax, colour=nutrient)) +
      geom_point(outlier.shape=NA) +
      facet_grid(species_phyla~nutrient) +
      ggtitle("Vmax values within 60 minutes") + 
      labs(x="Time [minutes]", y="Vmax [µmol g^-1 dw h^-1]") +
      scale_x_continuous(limits=c(0, 60)) +
      scale_y_continuous(limits=c(0,600)) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.position = "none")

  