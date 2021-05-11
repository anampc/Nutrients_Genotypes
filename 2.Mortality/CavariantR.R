
# Data
data<-read.csv("Data/Acer_Mortality_CoVariate.csv", header = TRUE)
data<-data[(data$Treatment!="Ambient"),]
summary(data)
summary(data$Genotype)
data$Genotype<-factor(data$Genotype, 
                                levels=c("G_48", "G_62","G_31", "G_08","G_07", "G_50"))
summary(data$Genotype)
summary(data$Treatment)
data$Treatment<-factor(data$Treatment, 
                                 levels=c("N", "N_P"))


surv_object <- Surv(time=data$Time1, time2=data$Time2, data$Fu.stat_exp)
surv_object 


Model1<-coxph(Surv(time=Time1, time2=Time2, Fu.stat_exp)~ Treatment *  HeatStress, data)
Model1
summary(Model1)

fit1 <- survfit(surv_object ~ Treatment+HeatStress, data = data)
summary(fit1)

Ac_Treatment_Only<-ggsurvplot(surv_object, data = data, pval = TRUE, 
                              conf.int = T, risk.table=T,
                              break.time.by=15, xlim=c(0,115), risk.table.y.text = FALSE,
                              risk.table.title="Number of fragments at risk") +
  ggtitle("Treatment (all genotypes)")
Ac_Treatment_Only
