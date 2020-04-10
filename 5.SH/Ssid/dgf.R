logC_Treatment <- ggplot(qPCR.variables, aes(Date, logC.SH, colour=factor(Treatment))) +
  #geom_jitter(aes(colour=factor(Replicate))) +
  stat_summary(fun.data = "mean_cl_boot",geom = "errorbar", width = 0.2)+
  stat_summary(fun.y=mean, geom="line", size =1, alpha=0.5) + 
  facet_grid(~Colony) +
  theme_gdocs() 
logC_Treatment

logD_Treatment <- ggplot(qPCR.variables, aes(Date, logD.SH, colour=factor(Treatment))) +
  #geom_jitter(aes(colour=factor(Replicate))) +
  stat_summary(fun.data = "mean_cl_boot",geom = "errorbar", width = 0.2)+
  stat_summary(fun.y=mean, geom="line", size =1, alpha=0.5) + 
  facet_grid(~Colony) +
  theme_gdocs() 
logD_Treatment

Colony30<-dplyr::filter(qPCR.variables, qPCR.variables$Colony==30)

DPrp_Treatment <- ggplot(Colony30, aes(Date, D.Prp, colour=factor(Core))) +
  geom_jitter(aes(colour=factor(Core))) +
  stat_summary(fun.data = "mean_cl_boot",geom = "errorbar", width = 0.2)+
  stat_summary(fun.y=mean, geom="line", size =1, alpha=0.5) + 
  facet_grid(Treatment~.) +
  theme_gdocs() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 2), legend.position = "none")
DPrp_Treatment


DPrp_Treatment <- ggplot(qPCR.variables, aes(Date, D.Prp, colour=factor(Core))) +
  geom_jitter(aes(colour=factor(Core))) +
  stat_summary(fun.data = "mean_cl_boot",geom = "errorbar", width = 0.2)+
  stat_summary(fun.y=mean, geom="line", size =1, alpha=0.5) + 
  facet_grid(Colony~Treatment) +
  theme_gdocs() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 2), legend.position = "none")
DPrp_Treatment

logC_Treatment <- ggplot(qPCR.variables, aes(Date, logC.SH, colour=factor(Core))) +
  geom_jitter(aes(colour=factor(Core))) +
  stat_summary(fun.data = "mean_cl_boot",geom = "errorbar", width = 0.2)+
  stat_summary(fun.y=mean, geom="line", size =1, alpha=0.5) + 
  facet_grid(Colony~Treatment) +
  theme_gdocs() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 2), legend.position = "none")
logC_Treatment

logD_Treatment <- ggplot(qPCR.variables, aes(Date, logD.SH, colour=factor(Core))) +
  geom_jitter(aes(colour=factor(Core))) +
  stat_summary(fun.data = "mean_cl_boot",geom = "errorbar", width = 0.2)+
  stat_summary(fun.y=mean, geom="line", size =1, alpha=0.5) + 
  facet_grid(Colony~Treatment) +
  theme_gdocs() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 2), legend.position = "none")
logD_Treatment


logSHTreatment <- ggplot(qPCR.variables, aes(Date, logSH, colour=factor(Community))) +
  # geom_jitter(aes(colour=factor(Replicate))) +
  stat_summary(fun.data = "mean_cl_boot",geom = "errorbar", width = 0.2)+
  # stat_summary(fun.y=mean, geom="point", size =3, alpha=0.5) +
  stat_summary(fun.y=mean, geom="line", size =1, alpha=0.5) + 
  facet_grid(Colony~Treatment) +
  theme_gdocs() 

logSHTreatment + ylab("Relative log10 (S:H)") + xlab("Treatment") +  
  theme(axis.title.y=element_text(size=12))

logSHTreatment <- ggplot(qPCR.variables, aes(Treatment, logSH, colour=factor(Community))) +
  # geom_jitter(aes(colour=factor(Replicate))) +
  stat_summary(fun.data = "mean_cl_boot",geom = "errorbar", width = 0.2)+
  # stat_summary(fun.y=mean, geom="point", size =3, alpha=0.5) +
  stat_summary(fun.y=mean, geom="line", size =1, alpha=0.5) + 
  facet_grid(~Date) +
  theme_gdocs() 

logSHTreatment + ylab("Relative log10 (S:H)") + xlab("Treatment") +  
  theme(axis.title.y=element_text(size=12))