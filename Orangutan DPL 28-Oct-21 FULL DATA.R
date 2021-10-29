#combining all the data for export

all<-rbind(TE_distances2, TA_distances2, WA_distances2, BIB_distances2, VI_distances2, 
           IN_distances2, BT_distances2, KB_distances2, JT_distances2, DY_distances2, 
           DE_distances2, AY_distances2)
#write.csv(all, "all_distances.csv")

reg1 <- lm(dist_btw_bouts~Age.of.Youngest.Offspring,data=all) 
summary(reg1)

mean(all$dist_btw_bouts)
#110m

plot(all$Age.of.Youngest.Offspring, all$dist_btw_bouts, 
     ylim=c(0, 500))
abline(reg1, col=c("blue"))

plot(all$Age.of.Youngest.Offspring, all$dist_btw_bouts, 
     ylim=c(0, 1000))
abline(reg1, col=c("blue"))

library(ggplot2)

ggplot(all, aes(x=Age.of.Youngest.Offspring, y=dist_btw_bouts, ))+
  geom_point(aes(color=OH))+theme_bw()+ 
  geom_smooth(method="gam", color="black")+
  scale_color_brewer(palette="Paired")+
  labs(y="Interpatch Distance (m)", x="Age of Youngest Offspring (years)")

ggplot(all, aes(x=Age.of.Youngest.Offspring, y=dist_btw_bouts, ))+
  geom_point(aes(color=OH))+theme_bw()+ 
  geom_smooth(method="lm", color="black")+
  scale_color_brewer(palette="Paired")+
  ylim(0, 700)+
  labs(y="Interpatch Distance (m)", x="Age of Youngest Offspring (years)")


