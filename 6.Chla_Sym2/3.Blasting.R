# Libraries and sources
library(ggplot2)
library(plotly)
library(GGally)
library(ggthemes)

# Read data
  data<-read.csv("3.All_Blast.csv")
  summary (data)
  
# Organize factors  
  data$Colony <- as.factor(paste(data$Spp, data$Colony, sep = "_"))
  data$Core <- as.factor(paste(data$Spp, data$Core, sep = "_"))
  data$Treatment <- as.factor(data$Treatment)
  data$Rep <- as.factor(data$Rep)
  data$Date<-as.Date(data$Date, "%Y-%m-%d")
  data$Days<-(as.numeric(data$Date) -17485)
  summary (data)

# Remove the sick guy
data <- data[!(data$Date == "2018-02-15"), ]

# Chl by Symbiont

data$Chl.Sym<-((data$Chl_a)/(data$Sym.cm2))

# Graphs to initially explore data
SymDen<- ggplot(data, aes(Colony, Sym.cm2, colour=factor(Treatment))) +
  geom_jitter(aes(colour=factor(Treatment))) +
  stat_summary(fun.data = "mean_cl_boot",geom = "errorbar", width = 0.2)+
  theme_gdocs() + facet_grid(Date~.)
SymDen

Chl<-ggplot(data, aes(Colony, Chl_a, colour=factor(Treatment))) +
  geom_jitter(aes(colour=factor(Treatment))) +
  stat_summary(fun.data = "mean_cl_boot",geom = "errorbar", width = 0.2)+
  theme_gdocs() + facet_grid(Date~.)
Chl
  
Chl.S<-ggplot(data, aes(Colony, Chl.Sym, colour=factor(Treatment))) +
  geom_jitter(aes(colour=factor(Treatment))) +
  stat_summary(fun.data = "mean_cl_boot",geom = "errorbar", width = 0.2)+
  theme_gdocs() + facet_grid(Date~.)
Chl.S

  Chl.Sym<- ggplot(data, aes(factor(Treatment), Chl.Sym))
  Chl.Sym + geom_boxplot(aes(fill=factor(Treatment)))+ facet_grid(Spp~.)

dataB<-data
  dataB$Spp<- NULL 
ggpairs(dataB)


# Libraries 
    library(lme4)
    library(multcomp)
    library(multcompView)
    library(lsmeans)
    library(effects)
    library(lmerTest)

# Symbiont density 

    SYM.CM<-lmerTest::lmer(Sym.cm2~ Treatment * Date + (1|Colony), REML=TRUE, data= data)
    
    
    SYM.CM_1 <- lmer(Sym.cm2 ~ Treatment  * Date + 
                   (1 + Treatment|Colony), REML=TRUE, data= data)
    
    SYM.CM_2 <- lmer(Sym.cm2 ~ Treatment * Date +  
                   (Treatment|Colony), REML=TRUE,  data= data)
    
    SYM.CM_3 <- lmer(Sym.cm2 ~ Treatment  * Date + 
                   (1|Colony), REML=TRUE, data= data)
    
    SYM.CM_4 <- lm(Sym.cm2 ~ Treatment*Date, data= data)
    
    anova(SYM.CM_1, SYM.CM_2, refit=FALSE)
    anova(SYM.CM_2, SYM.CM_3, refit=FALSE)
    anova(SYM.CM_3, SYM.CM_4)
    
    anova(SYM.CM_3)

    plot(Effect(c("Treatment", "Date"), SYM.CM_3), x.var="Treatment", multiline=T, ci.style="bars")
    
# Chl-a 
    
    Chl.CM_1 <- lmer(Chl_a ~ Treatment  * Date + 
                       (1 + Treatment|Colony), REML=TRUE, data= data)
    
    Chl.CM_2 <- lmer(Chl_a ~ Treatment  * Date +  
                       (Treatment|Colony), REML=TRUE,  data= data)
    
    Chl.CM_3 <- lmer(Chl_a ~ Treatment * Date  + 
                       (1|Colony), REML=TRUE, data= data)
    
    Chl.CM_4 <- lm(Chl_a ~ Treatment * Date, data= data)
    
    anova(Chl.CM_1, Chl.CM_2, refit=FALSE)
    anova(Chl.CM_2, Chl.CM_3, refit=FALSE)
    anova(Chl.CM_3, Chl.CM_4)

anova(Chl.CM_3)
summary(Chl.CM_3)
coef(Chl.CM_3)
fitted(Chl.CM_3)

layout(matrix(1:4,2,2))  
plot(Chl.CM_3)  

plot(Effect(c("Treatment", "Date"), Chl.CM_3), x.var="Treatment", multiline=T, ci.style="bars")

#  Pair-wise comparisons
cld(lsmeans(Chl.CM_3, "Treatment"))
cld(lsmeans(Chl.CM_3, specs = c("Treatment", "Date")))

## Correlation 

Ch_a.Sym <- ggplot(data, aes(Sym.cm2, Chl_a, colour=factor(Treatment))) +   
  geom_smooth(method=lm, colour="black", se=T) + 
  geom_point(aes(shape=Colony)) + facet_grid(~Date) 
ggExtra::ggMarginal(Ch_a.Sym, type="histogram")


# Test normality

  shapiro.test(data$Zoox.cm2) # Not normal
  ggplot() + geom_histogram(aes(x=Zoox.cm2), data=data)
  
  shapiro.test(data$Chlorophyll.AF8.Content.ug.cm2.) # Not normal
  ggplot() + geom_histogram(aes(x=Chlorophyll.AF8.Content.ug.cm2.), data=data)
  
  shapiro.test(data$ Lipid.ug.cm2.) # Not normal
  ggplot() + geom_histogram(aes(x=Lipid.ug.cm2.), data=data)

  shapiro.test(data$Chl.Sym) # Not normal
  ggplot() + geom_histogram(aes(x=Chl.Sym), data=data)



# Transformations 
    data$Zoox.sq<-sqrt(data$Zoox.cm2)
    data$Chl.sq<-sqrt(data$Chlorophyll.AF8.Content.ug.cm2.)
    data$Lipds.sq<-sqrt(data$Lipid.ug.cm2.)
    data$Chl.Sym.sq<-sqrt(data$Chl.Sym)

# Normality again

  shapiro.test(data$Zoox.sq)# Still not Normal
  ggplot() + geom_histogram(aes(x=Zoox.sq), data=data)
  
  shapiro.test(data$Chl.sq)# Normal
  ggplot() + geom_histogram(aes(x=Chl.sq), data=data)
  
  shapiro.test(data$Lipds.sq) # Still not Normal
  ggplot() + geom_histogram(aes(x=Lipds.sq), data=data)

  shapiro.test(data$Chl.Sym.sq) # Normal
  ggplot() + geom_histogram(aes(x=Chl.Sym.sq), data=data)

# Hocedasticity cannot be tested with one replicate for Mcav-NPF

  
# ANOVAs

  # Zoo
    ZooANOVA<-aov(Zoox.sq ~ Treatment*Species,data=data)
    ZooANOVA
    summary(ZooANOVA)
  
  # Chl
    ChlANOVA<-aov(Chl.sq ~ Treatment*Species,data=data)
    ChlANOVA
    summary(ChlANOVA)
  
  # Lipids
    LipANOVA<-aov(Lipds.sq ~ Treatment*Species,data=data)
    LipANOVA
    summary(LipANOVA)

# PostHoc
  TukeyHSD(ZooANOVA, "Treatment")

  TukeyHSD(ChlANOVA, "Treatment")
 
  TukeyHSD(LipANOVA, "Treatment")

# Graphs with transformed data
#   SymDen<- ggplot(data, aes(factor(Treatment), Zoox.sq))
#   SymDen + geom_boxplot(aes(fill=factor(Treatment)))+ facet_grid(Species~., margins=TRUE)
#   SymDen + geom_boxplot(aes(fill=factor(Species)))
#   
#   Chl<- ggplot(data, aes(factor(Treatment), Chlorophyll.AF8.Content.ug.cm2.))
#   Chl + geom_boxplot(aes(fill=factor(Treatment)))+ facet_grid(Species~., margins=TRUE)
#   Chl + geom_boxplot(aes(fill=factor(Species)))
#   
#   Lipids<- ggplot(data, aes(factor(Treatment), Lipid.ug.cm2.))
#   Lipids + geom_boxplot(aes(fill=factor(Species)))



# Correlation Chl Symbionts
  chl.sym <-ggplot(data, aes(Sym6, Chlorophyll.AF8.Content.ug.cm2.))
  chl.sym + geom_point (aes(colour=Species))
  
  chl.sym.cell <-ggplot(data, aes(Sym6, Chl.Sym))
  chl.sym.cell + geom_point (aes(colour=Treatment, shape=Species), size=5)


                            # Drop data points

# Mcav in NPF
NotUse <- data[which((data$Treatment=="NPF") & (data$Species=="Ss") ), ]
data2 <- droplevels(data[!rownames(data) %in% rownames(NotUse), ])

summary(data2)

                            # Drop data points

#-----------------------------------
# DATA ANALYSIS WITHOUT MCav-NPF 

# Graphs 

# Graphs to initially explore data
  SymDen<- ggplot(data2, aes(factor(Treatment), Zoox.cm2))
  SymDen + stat_boxplot(geom ='errorbar') +  
  geom_boxplot(aes(fill=factor(Treatment)))+ facet_wrap(~Species)+ 
  expand_limits(y=0)+
  labs(list(x = "Treatment", y = "Chlorophylle A / cm2", colour = "Treatment"))
  
  
  Chl<- ggplot(data2, aes(factor(Treatment), Chlorophyll.AF8.Content.ug.cm2.))
  Chl + stat_boxplot(geom ='errorbar') +  geom_boxplot(aes(fill=factor(Treatment)))+ facet_grid(Species~.) + expand_limits(y=0)
  
  Lipids<- ggplot(data2, aes(factor(Treatment), Lipid.ug.cm2.))
  Lipids + stat_boxplot(geom ='errorbar') +  geom_boxplot(aes(fill=factor(Treatment)))+ facet_grid(Species~.,)+ expand_limits(y=0)

  Chl.Sym<- ggplot(data2, aes(factor(Treatment), Chl.Sym))
  Chl.Sym + stat_boxplot(geom ='errorbar') + geom_boxplot(aes(fill=factor(Treatment)))+ facet_grid(Species~.) + expand_limits(y=0)

# Normality

  shapiro.test(data2$Zoox.sq)# Not normal
  ggplot() + geom_histogram(aes(x=Zoox.sq), data=data2)
  
  shapiro.test(data$Chl.sq)# Normal
  ggplot() + geom_histogram(aes(x=Chl.sq), data=data)
  
  shapiro.test(data$Lipds.sq) # Still not Normal
  ggplot() + geom_histogram(aes(x=Lipds.sq), data=data)
  
  shapiro.test(data$Chl.Sym.sq) # Normal
  ggplot() + geom_histogram(aes(x=Chl.Sym.sq), data=data)

# Hocedasticity (was OK for all of them)

  bartlett.test(Zoox.sq ~ interaction(Species,Treatment), data=data2)
  bartlett.test(Chl.sq ~ interaction(Species,Treatment), data=data2)
  bartlett.test(Lipds.sq ~ interaction(Species,Treatment), data=data2)
  bartlett.test(Chl.Sym.sq ~ interaction(Species,Treatment), data=data2)

# ANOVAs

# Zoo
  ZooANOVA2<-aov(Zoox.sq ~ Treatment*Species,data=data2)
  ZooANOVA2
  summary(ZooANOVA2)

# Chl
  ChlANOVA2<-aov(Chl.sq ~ Treatment*Species,data=data2)
  ChlANOVA2
  summary(ChlANOVA2)

# Lipids
  LipANOVA2<-aov(Lipds.sq ~ Treatment*Species,data=data2)
  LipANOVA2
  summary(LipANOVA2)

# Chl by symbiont
  Chl.symANOVA2<-aov(Chl.Sym.sq ~ Treatment*Species,data=data2)
  Chl.symANOVA2
  summary(Chl.symANOVA2)

# PostHoc
TukeyHSD(ZooANOVA2, "Treatment")
TukeyHSD(ChlANOVA2, "Treatment")

par(mfrow=c(1,2))
  plot(TukeyHSD(ZooANOVA2, "Treatment"))
  plot(TukeyHSD(ChlANOVA2, "Treatment"))
par(mfrow=c(1,1))

  TukeyHSD(Chl.symANOVA2, "Treatment")

# ---------------------------------

# Dark treatment
Dark <- data[which(data$Treatment=="Dark"), ]
data <- droplevels(data[!rownames(data) %in% rownames(Dark), ])

Ss <- data[which(data$Species=="Ss"), ]
data <- droplevels(data[!rownames(data) %in% rownames(Ss), ])
#-----------------------------------
            # DATA ANALYSIS WITHOUT DARK- non significant

# Graphs

SymDen3<- ggplot(data, aes(factor(Treatment), Zoox.cm2.millions.))

SymDen3 + geom_boxplot(aes(colour=Treatment)) + theme(legend.position = "none") +labs(x = "Treatment", y = "Symbiodinium density (10^6 cells/cm2)")
Sym<-aov(Zoox.cm2.millions.~Treatment, data = data)
summary(Sym)

Chl3<- ggplot(data.NoDark, aes(factor(Treatment), Chlorophyll.AF8.Content.ug.cm2.))
Chl3 + geom_boxplot(aes(fill=factor(Treatment)))+ facet_grid(Species~.)

Lipids3<- ggplot(data.NoDark, aes(factor(Treatment), Lipid.ug.cm2.))
Lipids3 + geom_boxplot(aes(fill=factor(Treatment)))+ facet_grid(Species~.)

ChlByCell3<- ggplot(data.NoDark, aes(factor(Treatment), Chlorophyll.cell))
ChlByCell3 + geom_boxplot(aes(fill=factor(Treatment)))+ facet_grid(Species~.)


# ANOVAs
  # Zoo
  ZooANOVA3<-aov(Zoox.sq ~ Treatment*Species,data=data.NoDark)
  ZooANOVA3
  summary(ZooANOVA3)
  
  # Chl
  ChlANOVA3<-aov(Chl.sq ~ Treatment*Species,data=data.NoDark)
  ChlANOVA3
  summary(ChlANOVA3)
  
  # Lipids
  LipANOVA3<-aov(Lipds.sq ~ Treatment*Species,data=data.NoDark)
  LipANOVA3
  summary(LipANOVA3)

# Chl/cell
ChlCell3<-aov(Chlorophyll.cell ~ Treatment*Species,data=data.NoDark)
ChlCell3
summary(ChlCell3)

# -------------------DATA ANALYSIS BY SPP

# Separate by spp

mcav.data<-data[which(data$Species=="M.cavernosa"), ]
Ssid.data <- droplevels(data[!rownames(data) %in% rownames(mcav.data), ])

# Mcav

  # ANOVAs
  
    # Zoo
    ZooANOVA4<-aov(Zoox.sq ~ Treatment,data=mcav.data)
    ZooANOVA4
    summary(ZooANOVA4)
    TukeyHSD(ZooANOVA4, "Treatment")
    
    # Chl
    ChlANOVA4<-aov(Chl.sq ~ Treatment,data=mcav.data)
    ChlANOVA4
    summary(ChlANOVA4)
    TukeyHSD(ChlANOVA4, "Treatment")
    
    # Lipids
    LipANOVA4<-aov(Lipds.sq ~ Treatment,data=mcav.data)
    LipANOVA4
    summary(LipANOVA4)
    
    # Chl by symbiont
    Chl.symANOVA4<-aov(Chl.Sym.sq ~ Treatment,data=mcav.data)
    Chl.symANOVA4
    summary(Chl.symANOVA4)
    TukeyHSD(Chl.symANOVA4, "Treatment")
  
  # PostHoc graph
   
    par(mfrow=c(1,2))
    plot(TukeyHSD(ZooANOVA4, "Treatment"))
    plot(TukeyHSD(Chl.symANOVA4, "Treatment"))
    par(mfrow=c(1,1))

# Ssid

# ANOVAs

  # Zoo
  ZooANOVA5<-aov(Zoox.sq ~ Treatment,data=Ssid.data)
  ZooANOVA5
  summary(ZooANOVA5)

  
  # Chl
  ChlANOVA5<-aov(Chl.sq ~ Treatment,data=Ssid.data)
  ChlANOVA5
  summary(ChlANOVA5)
  TukeyHSD(ChlANOVA5, "Treatment")
  
  # Lipids
  LipANOVA5<-aov(Lipds.sq ~ Treatment,data=Ssid.data)
  LipANOVA5
  summary(LipANOVA5)
  
  # Chl by symbiont
  Chl.symANOVA5<-aov(Chl.Sym.sq ~ Treatment,data=Ssid.data)
  Chl.symANOVA5
  summary(Chl.symANOVA5)

  
  # PostHoc graph
  
  plot(TukeyHSD(ChlANOVA5, "Treatment"))

    
   
  

# FINAL GRAPHS

# Graphs to initially explore data
SymDen<- ggplot(data2, aes(factor(Treatment), Sym6))
  SymDen + stat_boxplot(geom ='errorbar') +  # Chanhe the error bars
  geom_boxplot(aes(fill=factor(Treatment)))+ facet_wrap(~Species)+ # Group horizontally
  expand_limits(y=0)+ 
  labs(list(x = "Treatment", y = "Symbiodinium density (millons of cells/cm2)")) +
  theme(legend.position="none")


Chl<- ggplot(data2, aes(factor(Treatment), Chlorophyll.AF8.Content.ug.cm2.))
Chl + stat_boxplot(geom ='errorbar') +  
  geom_boxplot(aes(fill=factor(Treatment)))+ facet_wrap(~Species)+ 
  expand_limits(y=0)+
  labs(list(x = "Treatment", y = "Chlorophyll a (ug/cm2)")) +
  theme(legend.position="none")

Lipids<- ggplot(data2, aes(factor(Treatment), Lipid.ug.cm2.))
Lipids +  stat_boxplot(geom ='errorbar') +  
  geom_boxplot(aes(fill=factor(Treatment)))+ facet_wrap(~Species)+ 
  expand_limits(y=0)+
  labs(list(x = "Treatment", y = "Lipids (ug/cm2)")) +
  theme(legend.position="none")

Chl.Sym<- ggplot(data2, aes(factor(Treatment), Chl.Sym))
Chl.Sym +  stat_boxplot(geom ='errorbar') +  
  geom_boxplot(aes(fill=factor(Treatment)))+ facet_wrap(~Species)+ 
  expand_limits(y=0)+
  labs(list(x = "Treatment", y = "Chlorophyll a / Symbiodinium (ug/cell)")) +
  theme(legend.position="none")