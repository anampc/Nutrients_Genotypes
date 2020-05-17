SetUP<- ggplot(YII.data, aes (Days, YII, colour=Treatment)) +
  ggthe_bw+ Fill.colour +
  # stat_summary(fun.y=mean, geom="point")  +
  theme(legend.position="bottom",
        strip.background = element_rect(fill="white"))+
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, 0.25),  
                     expand = c(0, 0),
                     name=("")) +
  scale_x_continuous(name="Days in the experiment",
                     limits = c(-1,113),
                     breaks = seq(0, 113, 15),  
                     expand = c(0, 0))+
  annotate("segment", x = 2, xend = 91, y = 0.12, yend = 0.12,
           colour = "gray90", linetype=2)+
  annotate("segment", x = 79, xend = 91, y = 0.12, yend = 0.20,
           colour = "gray90", linetype=4)+
  annotate("segment", x = 91, xend = 110, y = 0.20, yend = 0.20,
           colour = "gray90", linetype=3)+
  annotate("text", x = 45, y = 0.15, label = "Nutrients", size=3)+
  annotate("text", x = 99, y = 0.15, label = "H", size=3)
SetUP

ggsave(file="Outputs/SetUP.svg", plot=SetUP, width=2, height=3)

```
