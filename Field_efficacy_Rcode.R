#ABC Efficacy Study

library(readr)
library(ggplot2)
all_data<-read.csv("all_data.csv")

#### Population growth 2023 ####

### Analyze via population growth

#Calculate Population Growth
all_data$R<-log(all_data$final_cmbsd+1)-log(all_data$initial_cmbsd+1)
summary(all_data$R)

all_data$R_check<-log( (all_data$final_cmbsd+1)/(all_data$initial_cmbsd+1) )
summary(all_data$R_check)


#create new data frame to reorder treatments
new.map1<-all_data
new.map1$treatment<-as.factor(new.map1$treatment)
new.map1$ant_rating<-as.factor(new.map1$ant_rating)
new.map1$treatment <- factor(new.map1$treatment, c('clean','untreated','untreated_netted','lw','lw_netted','sdb','sdb_netted','both','both_netted'))


#Plot population growth as a function of treatment
raw1<-ggplot(new.map1, mapping=aes(x=treatment, y=R))+
  xlab("Treatment")+
  ylab("Population Growth (R)")+
  geom_boxplot(fill='indianred1')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 40, vjust = 0.5, hjust=0.5), axis.title=element_text(size=15),axis.text=element_text(size=10))+
  scale_x_discrete(labels=c('Clean', 'No predator, Unnetted', 'No predator, Netted', 'C.r., Unnetted','C.r., Netted','R.l., Unnetted', 'R.l., Netted','C.r./R.l., Unnetted','C.r./R.l., Netted'))
raw1


#Perform statistical analysis
library(car)
Fit1<-lm(R ~ treatment + ant_rating + log(initial_cmbsd+1), data=new.map1)
summary(Fit1)


#Check assumptions
#data normality
#plot(Fit1) 
Res1<-residuals(Fit1)
shapiro.test(Res1) #data is normal

#Homogeneity
leveneTest(R~treatment, data=new.map1) #unequal variance due to low variance in "clean plants"

#perform Anova
Anova(Fit1, type='II')


#Post-hoc
library(emmeans)
em_fit1<-emmeans(Fit1, pairwise~treatment)
em_fit1

### Graphing Year 1 Population Growth

#Convert emmeans output to df
df.year1<-as.data.frame(em_fit1$emmeans)

#reorder treatments
df.year1$treatment<-as.factor(df.year1$treatment)
df.year1$treatment <- factor(df.year1$treatment, c('clean','untreated','untreated_netted','lw','lw_netted','sdb','sdb_netted','both','both_netted'))

# Define variance
min<-df.year1$lower.CL
low<-df.year1$emmean-df.year1$SE
mid<-df.year1$emmean
top<-df.year1$emmean+df.year1$SE
max<-df.year1$upper.CL

# Plot
emmeans1<-ggplot(df.year1, aes(x=treatment, ymin= min, lower = low, middle = mid, upper = top, ymax = max)) +
  geom_boxplot(stat = "identity", width=0.35,fill='indianred1')+
  xlab("Treatment")+
  ylab("EMM of Population Growth (R)")+
  theme_bw()+
  scale_x_discrete(labels=c('Clean', 'No predator, Unnetted', 'No predator, Netted', 'C.r., Unnetted','C.r., Netted','R.l., Unnetted', 'R.l., Netted','C.r./R.l., Unnetted','C.r./R.l., Netted'))+
  theme(plot.title.position = 'plot', plot.title = element_text(hjust = 0.5, size=13))+
  theme(axis.text.x = element_text(angle = 40, vjust = 0.5, hjust=0.5),axis.title=element_text(size=15),axis.text=element_text(size=10))+
  annotate("text", x=1, y=1.02, label= "ac")+
  annotate("text", x=2, y=1.680, label= "ab")+
  annotate("text", x=3, y=1.61, label= "ab")+
  annotate("text", x=4, y=1.740, label= "b")+
  annotate("text", x=5, y=1.75, label= "b")+
  annotate("text", x=6, y=1.40, label= "abc")+
  annotate("text", x=7, y=0.80, label= "c")+
  annotate("text", x=8, y=1.43, label= "abc")+
  annotate("text", x=9, y=1.87, label= "b")
emmeans1

#above plot accounts for SE as boxplots, remove boxes
emmeans1<-ggplot(df.year1, aes(x=treatment, y=emmean))+
  geom_point(size=3, color="indianred2")+
  geom_errorbar(aes(ymin=min, ymax=max), color="black")+
  xlab("Treatment")+
  ylab("EMM of Population Growth (R)")+
  theme_bw()+
  scale_x_discrete(labels=c('Clean', 'No predator, Unnetted', 'No predator, Netted', 'C.r., Unnetted','C.r., Netted','R.l., Unnetted', 'R.l., Netted','C.r./R.l., Unnetted','C.r./R.l., Netted'))+
  theme(plot.title.position = 'plot', plot.title = element_text(hjust = 0.5, size=13))+
  theme(axis.text.x = element_text(angle = 40, vjust = 0.5, hjust=0.5),axis.title=element_text(size=15),axis.text=element_text(size=10))+
  annotate("text", x=1, y=1.02, label= "ac")+
  annotate("text", x=2, y=1.680, label= "ab")+
  annotate("text", x=3, y=1.61, label= "ab")+
  annotate("text", x=4, y=1.740, label= "b")+
  annotate("text", x=5, y=1.75, label= "b")+
  annotate("text", x=6, y=1.40, label= "abc")+
  annotate("text", x=7, y=0.80, label= "c")+
  annotate("text", x=8, y=1.43, label= "abc")+
  annotate("text", x=9, y=1.87, label= "b")


#Combine raw population growth and emmeans population growth
library(ggpubr)
year1 <- ggarrange(raw1, emmeans1,
                   labels = c("A", "B"),
                   ncol = 2, nrow = 1)
year1


#### Final densities 2023 ####

#Plot population growth as a function of treatment
raw_dens1<-ggplot(new.map1, mapping=aes(x=treatment, y=initial_cmbsd))+
  xlab("Treatment")+
  ylab("Final density")+
  geom_boxplot(fill='indianred1')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 40, vjust = 0.5, hjust=0.5),axis.title=element_text(size=15), axis.text=element_text(size=10))+
  scale_x_discrete(labels=c('Clean', 'No predator, Unnetted', 'No predator, Netted', 'C.r., Unnetted','C.r., Netted','R.l., Unnetted', 'R.l., Netted','C.r./R.l., Unnetted','C.r./R.l., Netted'))
raw_dens1

#Graph initial and final densities
library(readr)
library(ggplot2)

year1_density_tp <- read_csv("year1_density_tp.csv")
year1_density_tp$timepoint <- factor(year1_density_tp$timepoint, c("initial","final"))

year1_density_tp$treatment<-as.factor(year1_density_tp$treatment)
year1_density_tp$treatment <- factor(year1_density_tp$treatment, c('clean','untreated','untreated_netted','lw','lw_netted','sdb','sdb_netted','both','both_netted'))


ggplot(year1_density_tp, aes(x=treatment, y=density, fill=timepoint)) + 
  scale_fill_manual(values=c("gray","indianred1"))+
  xlab("Treatment")+
  ylab("A. lagerstroemiae density")+
  geom_boxplot()+
  theme_bw()+
  scale_x_discrete(labels=c('Clean', 'No predator, Unnetted', 'No predator, Netted', 'C.r., Unnetted','C.r., Netted','R.l., Unnetted', 'R.l., Netted','C.r./R.l., Unnetted','C.r./R.l., Netted'))+
  theme(axis.text.x = element_text(angle = 40, vjust = 0.5, hjust=0.5), legend.position="none", axis.title=element_text(size=15),axis.text=element_text(size=10))


#Perform statistical analysis
library(car)
lm_dens1<- lm(log(final_cmbsd+1)~ treatment + ant_rating + log(initial_cmbsd+1), data=new.map1)
summary(lm_dens1)

#Check assumptions
#data normality
#plot(lm_dens1) 
Res_dens1<-residuals(lm_dens1)
shapiro.test(Res_dens1) #data is normal

#Homogeneity
leveneTest(log(final_cmbsd+1)~treatment, data=new.map1) #unequal variance due to low variance in "clean plants"

#perform Anova
Anova(lm_dens1, type='II')


#Post-hoc
em_dens1<-emmeans(lm_dens1, pairwise~treatment)
em_dens1

### Graphing Year 1 Final Densities

#Convert emmeans output to df
dens.year1<-as.data.frame(em_dens1$emmeans)

#reorder treatments
dens.year1$treatment<-as.factor(dens.year1$treatment)
dens.year1$treatment <- factor(dens.year1$treatment, c('clean','untreated','untreated_netted','lw','lw_netted','sdb','sdb_netted','both','both_netted'))

# Define variance
min<-dens.year1$emmean-dens.year1$SE
low<-dens.year1$lower.CL
mid<-dens.year1$emmean
top<-dens.year1$upper.CL
max<-dens.year1$emmean+dens.year1$SE

# Plot
emmeans_dens1<-ggplot(dens.year1, aes(x=treatment, ymin= min, lower = low, middle = mid, upper = top, ymax = max)) +
  geom_boxplot(stat = "identity", width=0.35,fill='indianred1')+
  xlab("Treatment")+
  ylab("EMM of final density")+
  theme_bw()+
  scale_x_discrete(labels=c('Clean', 'No predator, Unnetted', 'No predator, Netted', 'C.r., Unnetted','C.r., Netted','R.l., Unnetted', 'R.l., Netted','C.r./R.l., Unnetted','C.r./R.l., Netted'))+
  theme(plot.title.position = 'plot', plot.title = element_text(hjust = 0.5, size=13),axis.title=element_text(size=15), axis.text=element_text(size=10))+
  theme(axis.text.x = element_text(angle = 40, vjust = 0.5, hjust=0.5))+
  annotate("text", x=1, y=1.30, label= "ac")+
  annotate("text", x=2, y=1.960, label= "b")+
  annotate("text", x=3, y=1.90, label= "ab")+
  annotate("text", x=4, y=2.03, label= "b")+
  annotate("text", x=5, y=2.04, label= "b")+
  annotate("text", x=6, y=1.70, label= "abc")+
  annotate("text", x=7, y=1.08, label= "c")+
  annotate("text", x=8, y=1.72, label= "abc")+
  annotate("text", x=9, y=2.15, label= "b")
emmeans_dens1

#Combine raw population growth and emmeans population growth
library(ggpubr)
year1_dens <- ggarrange(raw_dens1, emmeans_dens1,
                        labels = c("A", "B"),
                        ncol = 2, nrow = 1)
year1_dens


#### Ant tending 2023 ####

#Statistical analysis
Anova(Fit1, type='II') 
summary(Fit1)

em.ant1<-emmeans(Fit1, pairwise~ant_rating)
em.ant1

#graph emmeans ant rating for year 1

df.ant1<-as.data.frame(em.ant1$emmeans)

# Define variance
low<-df.ant1$emmean-df.ant1$SE
min<-df.ant1$lower.CL
mid<-df.ant1$emmean
max<-df.ant1$upper.CL
top<-df.ant1$emmean+df.ant1$SE

df.ant1$ant_rating<-as.factor(df.ant1$ant_rating)

em_ant_graph1<-ggplot(df.ant1, aes(x=ant_rating, ymin= min, lower = low, middle = mid, upper = top, ymax = max)) +
  geom_boxplot(stat = "identity", width=0.35,fill='indianred1')+
  xlab("Ant Rating")+
  ylab("EMM of Population Growth (R)")+
  theme_bw()+
  theme(axis.title=element_text(size=15),axis.text=element_text(size=10))+
  ylim(0.0,2.5)+
  annotate("text", x=1, y=.982, label= "a")+
  annotate("text", x=2, y=1.234, label= "ab")+
  annotate("text", x=3, y=1.479, label= "bc")+
  annotate("text", x=4, y=1.704, label= "c")+
  annotate("text", x=5, y=1.922, label= "abc")
em_ant_graph1

#above plot accounts for SE as boxplots, remove boxes
em_ant_graph1<-ggplot(df.ant1, aes(x=ant_rating, y=emmean))+
  geom_point(size=3, color="indianred2")+
  geom_errorbar(aes(ymin=min, ymax=max), color="black")+
  xlab("Ant Rating")+
  ylab("EMM of Population Growth (R)")+
  theme_bw()+
  theme(plot.title.position = 'plot', plot.title = element_text(hjust = 0.5, size=13), axis.title=element_text(size=15), axis.text=element_text(size=12))+
  annotate("text", x=1, y=.982, label= "a")+
  annotate("text", x=2, y=1.234, label= "ab")+
  annotate("text", x=3, y=1.479, label= "bc")+
  annotate("text", x=4, y=1.704, label= "c")+
  annotate("text", x=5, y=1.922, label= "abc")
em_ant_graph1


### 2024 ###

library(readr)
all_data_2024 <- read_csv("all_data_2024.csv")

# Calculate final and initial densities

all_data_2024$initial_cmbsd<-all_data_2024$initial_cmbs/all_data_2024$twig_length
all_data_2024$final_cmbsd<-all_data_2024$final_cmbs/all_data_2024$twig_length

#calculate population growth
all_data_2024$R<-log(all_data_2024$final_cmbsd+1)-log(all_data_2024$initial_cmbsd+1)

# Create new data frame to restructure data
new.map2<-all_data_2024
new.map2$treatment<-as.factor(new.map2$treatment)
new.map2$final_antrating<-as.factor(new.map2$final_antrating)
new.map2$treatment <- factor(new.map2$treatment, c('clean','untreated','untreated_netted','lw_10','lw_netted_10','lw_20','lw_netted_20','lw_30','lw_netted_30','sdb','sdb_netted'))

#### Population growth 2024 ####

raw2<-ggplot(new.map2, mapping=aes(x=treatment,y=R))+
  geom_boxplot()+
  geom_boxplot(fill='indianred1')+
  xlab("Treatment")+
  ylab("Population Growth (R)")+
  theme_bw()+
  scale_x_discrete(labels=c('Clean','No predator', 'No predator/netted', 'C.r. x 10', 'C.r. x 10/netted','C.r. x 20','C.r. x 20/netted','C.r. x 30','C.r. x 30/netted','R.l.','R.l./netted'))+
  theme(plot.title.position = 'plot', plot.title = element_text(hjust = 0.5, size=13))+
  theme(axis.text.x = element_text(angle = 40, vjust = 0.6, hjust=0.5), axis.title=element_text(size=15),axis.text=element_text(size=10))
raw2

#Fit model
library(car)
Fit2<-lm(R~treatment + final_antrating + log(initial_cmbsd+1), data=new.map2)
summary(Fit2)

#Check Assumptions
#data normality
#plot(Fit2) #data is normal
Res2<-residuals(Fit2)
shapiro.test(Res2)
# homogeneity
leveneTest(R~treatment, data=new.map2) #unequal variance

#Perform Anova
Anova(Fit2, type='II')

#Post-hoc analysis
em_fit2<-emmeans(Fit2, pairwise~treatment)
em_fit2

df.year2<-as.data.frame(em_fit2$emmeans)


# Define variance
low<-df.year2$emmean-df.year2$SE
min<-df.year2$lower.CL
mid<-df.year2$emmean
max<-df.year2$upper.CL
top<-df.year2$emmean+df.year2$SE

df.year2$treatment<-as.factor(df.year2$treatment)
df.year2$treatment <- factor(df.year2$treatment, c('clean','untreated','untreated_netted','lw_10','lw_netted_10','lw_20','lw_netted_20','lw_30','lw_netted_30','sdb','sdb_netted'))

emmeans2<-ggplot(df.year2, aes(x=treatment, ymin= min, lower = low, middle = mid, upper = top, ymax = max)) +
  geom_boxplot(stat = "identity", width=0.35,fill='indianred1')+
  xlab("Treatment")+
  ylab("EMM of Population Growth (R)")+
  theme_bw()+
  scale_x_discrete(labels=c('Clean','No predator, Unnetted', 'No predator, Netted', 'C.r. x 10, Unnetted', 'C.r. x 10, Netted','C.r. x 20, Unnetted','C.r. x 20, Netted','C.r. x 30, Unnetted','C.r. x 30, Netted','R.l., Unnetted','R.l., Netted'))+
  theme(plot.title.position = 'plot', plot.title = element_text(hjust = 0.5, size=13))+
  theme(axis.text.x = element_text(angle = 40, vjust = 0.6, hjust=0.6),axis.title=element_text(size=15),axis.text=element_text(size=10))+
  annotate("text", x=1, y=1.13, label= "a")+
  annotate("text", x=2, y=2.112, label= "ab")+
  annotate("text", x=3, y=2.3, label= "b")+
  annotate("text", x=4, y=2.0, label= "ab")+
  annotate("text", x=5, y=1.76, label= "ab")+
  annotate("text", x=6, y=1.76, label= "ab")+
  annotate("text", x=7, y=1.78, label= "ab")+
  annotate("text", x=8, y=1.87, label= "ab")+
  annotate("text", x=9, y=1.52, label= "ab")+
  annotate("text", x=10, y=2.082, label= "ab")+
  annotate("text", x=11, y=0.88, label= "a")
emmeans2

emmeans2<-ggplot(df.year2, aes(x=treatment, y=emmean))+
  geom_point(size=3, color="indianred2")+
  geom_errorbar(aes(ymin=min, ymax=max), color="black")+
  xlab("Treatment")+
  ylab("EMM of Population Growth (R)")+
  theme_bw()+
  scale_x_discrete(labels=c('Clean','No predator, Unnetted', 'No predator, Netted', 'C.r. x 10, Unnetted', 'C.r. x 10, Netted','C.r. x 20, Unnetted','C.r. x 20, Netted','C.r. x 30, Unnetted','C.r. x 30, Netted','R.l., Unnetted','R.l., Netted'))+
  theme(plot.title.position = 'plot', plot.title = element_text(hjust = 0.5, size=13))+
  theme(axis.text.x = element_text(angle = 40, vjust = 0.6, hjust=0.6),axis.title=element_text(size=15),axis.text=element_text(size=10))+
  annotate("text", x=1, y=1.13, label= "a")+
  annotate("text", x=2, y=2.112, label= "ab")+
  annotate("text", x=3, y=2.3, label= "b")+
  annotate("text", x=4, y=2.0, label= "ab")+
  annotate("text", x=5, y=1.76, label= "ab")+
  annotate("text", x=6, y=1.76, label= "ab")+
  annotate("text", x=7, y=1.78, label= "ab")+
  annotate("text", x=8, y=1.87, label= "ab")+
  annotate("text", x=9, y=1.52, label= "ab")+
  annotate("text", x=10, y=2.082, label= "ab")+
  annotate("text", x=11, y=0.88, label= "a")
emmeans2


library(ggpubr)
year2 <- ggarrange(raw2, emmeans2,
                   labels = c("A", "B"),
                   ncol = 2, nrow = 1)
year2

#### Final densities 2024 ####

#Plot population growth as a function of treatment
raw_dens2<-ggplot(new.map2, mapping=aes(x=treatment, y=initial_cmbsd))+
  xlab("Treatment")+
  ylab("Final density")+
  geom_boxplot(fill='indianred1')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 40, vjust = 0.5, hjust=0.5),axis.title=element_text(size=15), axis.text=element_text(size=10))+
  scale_x_discrete(labels=c('Clean','No predator, Unnetted', 'No predator, Netted', 'C.r. x 10, Unnetted', 'C.r. x 10, Netted','C.r. x 20, Unnetted','C.r. x 20, Netted','C.r. x 30, Unnetted','C.r. x 30, Netted','R.l., Unnetted','R.l., Netted'))
raw_dens2

#Graphing initial and final densities
library(readr)
library(ggplot2)

year2_density_tp <- read_csv("year2_density_tp.csv")
year2_density_tp$timepoint <- factor(year2_density_tp$timepoint, c("initial","final"))

year2_density_tp$treatment<-as.factor(year2_density_tp$treatment)
year2_density_tp$treatment <- factor(year2_density_tp$treatment, c('clean','untreated','untreated_netted','lw_10','lw_netted_10','lw_20','lw_netted_20','lw_30','lw_netted_30','sdb','sdb_netted'))


ggplot(year2_density_tp, aes(x=treatment, y=density, fill=timepoint)) + 
  scale_fill_manual(values=c("gray","indianred1"))+
  xlab("Treatment")+
  ylab("A. lagerstroemiae density")+
  geom_boxplot()+
  theme_bw()+
  scale_x_discrete(labels=c('Clean','No predator, Unnetted', 'No predator, Netted', 'C.r. x 10, Unnetted', 'C.r. x 10, Netted','C.r. x 20, Unnetted','C.r. x 20, Netted','C.r. x 30, Unnetted','C.r. x 30, Netted','R.l., Unnetted','R.l., Netted'))+
  theme(axis.text.x = element_text(angle = 40, vjust = 0.5, hjust=0.5), legend.position="none",axis.title=element_text(size=15),axis.text=element_text(size=10) )


#Perform statistical analysis
library(car)
lm_dens2<- lm(log(final_cmbsd+1)~ treatment + final_antrating+ log(initial_cmbsd+1), data=new.map2)
summary(lm_dens2)

#Check assumptions
#data normality
#plot(lm_dens2) 
Res_dens2<-residuals(lm_dens2)
shapiro.test(Res_dens2) #data is normal

#Homogeneity
leveneTest(log(final_cmbsd+1)~treatment, data=new.map2) #unequal variance due to low variance in "clean plants"

#perform Anova
Anova(lm_dens2, type='II')


#Post-hoc
em_dens2<-emmeans(lm_dens2, pairwise~treatment)
em_dens2

### Graphing Year 1 Final Densities

#Convert emmeans output to df
dens.year2<-as.data.frame(em_dens2$emmeans)

#reorder treatments
dens.year2$treatment<-as.factor(dens.year2$treatment)
dens.year2$treatment <- factor(dens.year2$treatment, c('clean','untreated','untreated_netted','lw_10','lw_netted_10','lw_20','lw_netted_20','lw_30','lw_netted_30','sdb','sdb_netted'))

# Define variance
min<-dens.year2$emmean-dens.year2$SE
low<-dens.year2$lower.CL
mid<-dens.year2$emmean
top<-dens.year2$upper.CL
max<-dens.year2$emmean+dens.year2$SE

# Plot
emmeans_dens2<-ggplot(dens.year2, aes(x=treatment, ymin= min, lower = low, middle = mid, upper = top, ymax = max)) +
  geom_boxplot(stat = "identity", width=0.35,fill='indianred1')+
  xlab("Treatment")+
  ylab("EMM of final density")+
  theme_bw()+
  scale_x_discrete(labels=c('Clean','No predator, Unnetted', 'No predator, Netted', 'C.r. x 10, Unnetted', 'C.r. x 10, Netted','C.r. x 20, Unnetted','C.r. x 20, Netted','C.r. x 30, Unnetted','C.r. x 30, Netted','R.l., Unnetted','R.l., Netted'))+
  theme(plot.title.position = 'plot', plot.title = element_text(hjust = 0.5, size=13), axis.title=element_text(size=15), axis.text=element_text(size=10))+
  theme(axis.text.x = element_text(angle = 40, vjust = 0.5, hjust=0.5))+
  annotate("text", x=1, y=1.53, label= "a")+
  annotate("text", x=2, y=2.5, label= "ab")+
  annotate("text", x=3, y=2.7, label= "b")+
  annotate("text", x=4, y=2.4, label= "ab")+
  annotate("text", x=5, y=2.16, label= "ab")+
  annotate("text", x=6, y=2.16, label= "ab")+
  annotate("text", x=7, y=2.18, label= "ab")+
  annotate("text", x=8, y=2.27, label= "ab")+
  annotate("text", x=9, y=1.92, label= "ab")+
  annotate("text", x=10, y=2.48, label= "ab")+
  annotate("text", x=11, y=1.28, label= "a")
emmeans_dens2

#Combine raw population growth and emmeans population growth
library(ggpubr)
year2_dens <- ggarrange(raw_dens2, emmeans_dens2,
                        labels = c("A", "B"),
                        ncol = 2, nrow = 1)
year2_dens

#### Ant tending 2024 ####

#Statistical analysis
Anova(Fit2, type='II') 
summary(Fit2)

em.ant2<-emmeans(Fit2, pairwise~final_antrating)
em.ant2

#graph emmeans ant rating for year 2

df.ant2<-as.data.frame(em.ant2$emmeans)

# Define variance
low2<-df.ant2$emmean-df.ant2$SE
min2<-df.ant2$lower.CL
mid2<-df.ant2$emmean
max2<-df.ant2$upper.CL
top2<-df.ant2$emmean+df.ant1$SE

df.ant2$final_antrating<-as.factor(df.ant2$final_antrating)

em_ant_graph2<-ggplot(df.ant2, aes(x=final_antrating, ymin= min2, lower = low2, middle = mid2, upper = top2, ymax = max2)) +
  geom_boxplot(stat = "identity", width=0.35,fill='indianred1')+
  xlab("Ant Rating")+
  ylab("EMM of Population Growth (R)")+
  theme_bw()+
  theme(axis.title=element_text(size=15), axis.text=element_text(size=10))+
  ylim(0.0,2.5)+
  annotate("text",x=1, y=0.889, label='a')+
  annotate("text",x=2, y=1.083, label='a')+
  annotate("text",x=3, y=1.452, label='a')+
  annotate("text",x=4, y=2.317, label='b')+
  annotate("text",x=5, y=2.204, label='ab')
em_ant_graph2

em_ant_graph2<-ggplot(df.ant2, aes(x=final_antrating, y=emmean))+
  geom_point(size=3, color="indianred2")+
  geom_errorbar(aes(ymin=min2, ymax=max2), color="black")+
  xlab("Ant Rating")+
  ylab("EMM of Population Growth (R)")+
  theme_bw()+
  theme(plot.title.position = 'plot', plot.title = element_text(hjust = 0.5, size=13), axis.title=element_text(size=15), axis.text=element_text(size=12))+
  annotate("text",x=1, y=0.889, label='a')+
  annotate("text",x=2, y=1.083, label='a')+
  annotate("text",x=3, y=1.452, label='a')+
  annotate("text",x=4, y=2.317, label='b')+
  annotate("text",x=5, y=2.204, label='ab')
em_ant_graph2




#### ABC Retention ####

#create graph
#Year One

ret_year1<-ggplot(new.map1, mapping=aes(treatment, abc_retention))+
  geom_boxplot(fill='indianred1')+
  geom_point()+
  ylab("Predator Recovery")+
  xlab("Treatment")+
  theme_bw()+
  scale_x_discrete(labels=c('Clean', 'No predator, Unnetted', 'No predator, Netted', 'C.r., Unnetted','C.r., Netted','R.l., Unnetted', 'R.l., Netted','C.r./R.l., Unnetted','C.r./R.l., Netted'))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6, hjust=0.6),
        plot.margin=margin(t=15, b=15, l=15, r=5), axis.title=element_text(size=15), axis.text=element_text(size=10)) +
  annotate("text", x=1, y=3, label= "2023")

ret_year1


#Year Two

ret_year2<-ggplot(new.map2, mapping=aes(treatment, predator_retention))+
  geom_boxplot(fill='indianred1')+
  geom_point()+
  xlab("Treatment")+
  scale_x_discrete(labels=c('Clean','No predator, Unnetted', 'No predator, Netted', 'C.r. x 10, Unnetted', 'C.r. x 10, Netted','C.r. x 20, Unnetted','C.r. x 20, Netted','C.r. x 30, Unnetted','C.r. x 30, Netted','R.l., Unnetted','R.l., Netted'))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6, hjust=0.5),
        plot.margin=margin(t=15, b=15, l=15, r=15), axis.title.y=element_blank(), axis.title=element_text(size=15), axis.text=element_text(size=10))+
  annotate("text", x=1.2, y=3, label= "2024")
ret_year2



#combine the two plots
library(ggpubr)
figure <- ggarrange(ret_year1, ret_year2,
                    labels = c("A", "B"),
                    ncol = 2, nrow = 1)
figure
