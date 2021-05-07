# METADATA ----------------------------------------------------------------

## SIMON VON SACHSEN-COBURG UND GOTHA'S MSc THESIS
## Created: Faro, 16th April 2021
## Last modification: 07/05/2021

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
data.sou  <- read_excel("~/Documents/IMBRSea/Thesis S4/Database uptake rate_07.05.2021.xlsx",
                        sheet="sources",na="NA",skip=3)
str(data.sou)
names(data.sou)

# load ENVIRONMENTAL
data.env  <- read_excel("~/Documents/IMBRSea/Thesis S4/Database uptake rate_07.05.2021.xlsx",
                        sheet="environmental",na="NA",skip=3)
str(data.env)
names(data.env)

# load EXPERIMENTAL
data.exp  <- read_excel("~/Documents/IMBRSea/Thesis S4/Database uptake rate_07.05.2021.xlsx",
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

# MERGE data.exp and data.env
data.all <- merge(data.env,data.exp,by="id_short")



# EXPLORATORY -------------------------------------------------------------

###DATA.SOU

##Publication type
ggplot(data.sou, aes(x=publication_type)) +
      geom_bar(stat="count", width=0.5) +
      ggtitle("Publication types") + 
      labs(x="Type of publication", y="Number of articles") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))

##Publication year
ggplot(data.sou, aes(x=year)) +
      geom_bar(stat="count") +
      ggtitle("Publication year") + 
      labs(x="Year of publication", y="Number of articles") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_continuous(limits=c(1980, 2021), breaks=seq(1980,2021,5))
      

###DATA.ENV

#Sampling country
#gexp <- unique(data.exp[,c("id_short", "species_type")])
genv <- unique(data.env[,c("id_short", "sample_country")]) # create data for ggplot - 

#using the unique function with study_id and sample_country you eliminate repeated countries within the same study
gdata <- data.frame(table(genv$sample_country)) # this is to create a frequency table for countries
names(gdata) <- c("sample_country","n")

ggplot(gdata, aes(x=reorder(sample_country,n), y=n)) +   # here I have added "reorder" so in the plot you can see countries ordered by frequency
      geom_bar(stat="identity", na.rm=FALSE) + 
      coord_flip() +
      scale_x_discrete("Country") +
      scale_y_continuous("Number of studies") +
      theme_bw() 

rm(gdata, genv, gexp) # delete gdata
        

###DATA.EXP

#Incubation type / surge or int. contr. phase
ggplot(data.exp, aes(x=type_uptake, fill=type_uptake))+
      geom_bar(stat="count") +
      facet_grid(type_incub~species_type) +
      scale_size_manual(values=c(5))+
      ggtitle("Surge uptake vs. Internally controlled phase") + 
      labs(x="Uptake type", y="Number of registered values") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.position = "none")


###Species type

ggplot(data.exp, aes(x=species_type)) +
      geom_bar(stat="count",  width = .5) +
      geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
      ggtitle("Registered values by species type") + 
      labs(x="Species type", y="Number of registered values") +
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5))

  
ggplot(data.exp, aes(x=species_type, fill=species_phyla)) +
      scale_fill_manual(values = c("forestgreen", "salmon4", "red2", "springgreen4")) +
      geom_bar(stat="count", position=position_dodge(),  width = .6)+
      ggtitle("Species phyla") + 
      labs(x="Species type", y="Number of registered values") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5), legend.position="right") +
      theme(legend.title = element_blank()) +
      theme(legend.position = c(.85,.8))


ggplot(data.exp, aes(x=species_type, colour=species_compartm, fill=species_compartm)) +
      geom_bar(stat="count", position=position_dodge(), width= .6) +
      ggtitle("Species compartment") + 
      labs(x="Species type", y="Number of registered values") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5), legend.position="right") +
      theme(legend.title = element_blank()) +
      theme(legend.position = c(.85,.8))

#library(ggpubr)
#ggarrange(spcomp, spphyl, ncol=2, nrow=1)


# Vmax Range -------------------------------------------------------------------

#Distribution of recorded values - Histogram

#Vmax Range
ggplot(data.exp, aes(x=Vmax))+
      geom_histogram(binwidth=10, 
                     breaks = seq(0, 500, by = 10), 
                     colour = "black", 
                     fill = "white") +
      geom_rug() +
      geom_density(aes(y=..density..*750), colour="blue") +
      scale_x_continuous(limits=c(0, 500)) +
      scale_y_continuous(limits=c(0, 50)) +
      facet_grid(.~species_type) +
      ggtitle("Vmax values (Range)") + 
      labs(x="Vmax", y="Frequency") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))

#Vmax Single
ggplot(data.exp, aes(x=uptake_rate_dw))+
      geom_histogram(binwidth=10, 
                     breaks = seq(-100, 450, by = 15), 
                     colour = "black", 
                     fill = "white") +
      geom_rug() +
      geom_density(aes(y=..density..*1000), colour="blue") +
      scale_x_continuous(limits=c(-100, 500)) +
      scale_y_continuous(limits=c(0, 400)) +
      facet_grid(.~species_type) +
      ggtitle("Vmax values (Single)") + 
      labs(x="Vmax", y="Frequency") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))

#Alpha Range
ggplot(data.exp, aes(x=alpha))+
      geom_histogram(binwidth=0.1, 
                     breaks = seq(0, 15, by = 0.2), 
                     colour = "black", 
                     fill = "white") +
      geom_rug() +
      geom_density(aes(y=..density..*10), colour="blue") +
      scale_x_continuous(limits=c(0, 15)) +
      scale_y_continuous(limits=c(0, 20)) +
      facet_grid(.~species_type) +
      ggtitle("Alpha values (Range)") + 
      labs(x="Alpha", y="Frequency") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))

##Overlapping densities
#Vmax Range
ggplot(data.exp, aes(x=Vmax, group=species_type, fill=species_type))+
      geom_density(alpha=0.4) +
      ggtitle("RANGE Vmax values") + 
      labs(x="Vmax [µmol g^-1 dw h^-1]", y="Frequency") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.title = element_blank()) +
      theme(legend.position = c(.9,.9))

      #Log transformation
      ggplot(data.exp, aes(x=Vmax, group=species_type, fill=species_type))+
            geom_density(alpha=0.5) +
            scale_x_log10() +
            ggtitle("Log10 Vmax values (Range)") + 
            labs(x="(log) Vmax [µmol g^-1 dw h^-1]", y="Frequency") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(legend.title = element_blank()) +
            theme(legend.position = c(.9,.9))
            
#Vmax Single
ggplot(data.exp, aes(x=uptake_rate_dw, group=species_type, fill=species_type))+
      geom_density(alpha=0.5) +
      ggtitle("Vmax values (Single)") +
      labs(x="Vmax [µmol g^-1 dw h^-1]", y="Frequency") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.title = element_blank()) +
      theme(legend.position = c(.9,.9))

      #Log transformed
      ggplot(data.exp, aes(x=uptake_rate_dw, group=species_type, fill=species_type))+
            geom_density(alpha=0.5) +
            scale_x_log10() +
            ggtitle("Log10 Vmax values (Single)") +
            labs(x="(log) Vmax [µmol g^-1 dw h^-1]", y="Frequency") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5))



#Vmax alpha
ggplot(data.exp, aes(x=alpha, group=species_type, fill=species_type))+
      geom_density(alpha=0.4) +
      ggtitle("Alpha values (Range)") + 
      labs(x="Alpha [l g^-1 dw h^-1 µM^-1]", y="Frequency") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.title = element_blank()) +
      theme(legend.position = c(.9,.9))


      #Log transformation
      ggplot(data.exp, aes(x=alpha, group=species_type, fill=species_type))+
            geom_density(alpha=0.4) +
            scale_x_log10() +
            ggtitle("Alpha values (Range)") + 
            labs(x="(log) Alpha [l g^-1 dw h^-1 µM^-1]", y="Frequency") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(legend.title = element_blank()) +
            theme(legend.position = c(.9,.9))

      
##Vmax for species compartment
ggplot(data.exp, aes(x=Vmax))+
  geom_histogram(binwidth=10) +
  facet_grid(species_compartm~species_type) +
  theme_bw()


##Vmax for int. contr. phase / surge
ggplot(data=subset(data.exp, !is.na(Vmax)), aes(x=Vmax), fill=type_uptake)+
  geom_histogram(binwidth=10) +
  facet_grid(type_uptake~species_type) +
  theme_bw()


##Scatterplot for temperature
ggplot(data.exp, aes(x=temperature_experiment, y=Vmax)) +
  geom_point() +
  facet_grid(.~species_type) +
  theme_bw()


##Boxplot + jitter for nutrients
r1 <- ggplot(data.exp, aes(x=species_type, y=Vmax)) +
            geom_boxplot(outlier.shape=NA) +
            geom_jitter(width=0.3, shape=21) +
            facet_grid(.~nutrient) +
            ggtitle("RANGE Vmax values for nutrients") + 
            labs(x="Species type", y="Vmax") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5))

##Boxplot + jitter for nutrients coloured
r2 <- ggplot(data.exp, aes(x=species_type, y=Vmax)) +
            geom_boxplot(aes(colour = factor(type_uptake)),outlier.shape = NA) +
            geom_jitter(aes(colour = factor(type_uptake)), width=0.3, shape=21) +
            facet_grid(.~nutrient) +
            ggtitle("RANGE Vmax values for nutrients") + 
            labs(x="Species type", y="Vmax") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(legend.title = element_blank()) +
            theme(legend.position = "bottom")


##Scatterplot alpha
ggplot(data.exp, aes(x=alpha, y=temperature_experiment, colour=species_type)) +
      geom_point() +
      facet_grid(.~nutrient) +
      ggtitle("Alpha values for nutrients") + 
      labs(x="Alpha", y="Temperature") +
      scale_y_continuous(limits=c(0, 30), breaks=seq(0,30,5)) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.title = element_blank()) +
      theme(legend.position = "bottom")


ggplot(data.exp, aes(x=alpha, y=temperature_experiment, colour=species_type)) +
      geom_point() +
      facet_grid(species_compartm~nutrient) +
      ggtitle("Alpha values by nutrient & species compartment") + 
      labs(x="Alpha", y="Temperature") +
      scale_y_continuous(limits=c(0, 30), breaks=seq(0,30,5)) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.title = element_blank()) +
      theme(legend.position = "bottom")


ggplot(data.exp, aes(x=alpha, y=temperature_experiment, colour=species_type)) +
  geom_point() +
  facet_grid(type_uptake~nutrient) +
  ggtitle("Alpha values by nutrient & uptake types") + 
  labs(x="Alpha", y="Temperature") +
  scale_y_continuous(limits=c(0, 30), breaks=seq(0,30,5)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")

# Vmax Single -------------------------------------------------------------

#Distribution - Hist
ggplot(data.exp, aes(x=uptake_rate_dw))+
        geom_histogram(binwidth=10) +
        facet_grid(.~species_type) +
        theme_bw()


#Vmax for species compartment
ggplot(data.exp, aes(x=uptake_rate_dw))+
        geom_histogram(binwidth=10) +
        facet_grid(species_compartm~species_type) +
        theme_bw()


#Vmax for int. contr. phase / surge
ggplot(data=subset(data.exp, !is.na(uptake_rate_dw)), aes(x=uptake_rate_dw))+
        geom_histogram(binwidth=10) +
        facet_grid(type_uptake~species_type) +
        theme_bw()


#Scatterplot for temperature
ggplot(data.exp, aes(x=temperature_experiment, y=uptake_rate_dw)) +
        geom_point() +
        facet_grid(.~species_type) +
        theme_bw()

ggplot(data.exp, aes(x=temperature_experiment, y=uptake_rate_dw)) +
        geom_point() +
        facet_grid(species_type~type_uptake) +
        theme_bw()


#Boxplot + jitter for nutrients
ggplot(data.exp, aes(x=species_type, y=uptake_rate_dw)) +
      geom_boxplot(outlier.shape=NA) +
      geom_jitter(width=0.3, shape=21) +
      facet_grid(.~nutrient) +
      ggtitle("SINGLE Vmax values for nutrients") + 
      labs(x="Species type", y="Vmax") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))

#Boxplot + jitter for nutrients coloured
ggplot(data.exp, aes(x=species_type, y=uptake_rate_dw)) +
      geom_boxplot(aes(colour = factor(type_uptake)),outlier.shape = NA) +
      geom_jitter(aes(colour = factor(type_uptake)), width=0.3, shape=21) +
      facet_grid(.~nutrient) +
      ggtitle("SINGLE Vmax values for nutrients") + 
      labs(x="Species type", y="Vmax") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.title = element_blank()) +
      theme(legend.position = "bottom")
        
        
        