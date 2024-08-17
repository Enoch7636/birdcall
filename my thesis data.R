##PLOTS FOR RESEARCH ON COMPARATIVE ACCOUNT OF COMMON MYNA CALLS IN DIFFERENT HUMAN INFLUENCED ENVIRONMENTS
########################
#Correlation Graphs 
setwd()
getwd()
#packages loaded=ggpubr
library("ggpubr")
library(readxl)
season<- read_excel(my.thesis.whole.data.xlsx)
season<- read_excel("Desktop/R SCRIPTS/data/my.thesis.whole.data.xlsx",sheet = "sheet1")
attach(season)
View(season)
print(season)
#1.Correlation between temperature and High frequency for all cities in a faceted correlation graph
ggscatter(season,x="tempe.rature",y="high.frequency",size = 0.3, palette = "jco",
          facet.by = "cities.data", #scales = "free_x",
          add = "reg.line", conf.int = TRUE) +
  stat_cor( method = "pearson", label.y = 6)+labs(x='TEMPERATURE',y='HIGH FREQUENCY(Hz)')+theme(text = element_text(family = "serif"))+theme(text = element_text(size = 15,hjust = 0.5))

#2.Correlation between temperature and Low frequency for all cities in a faceted correlation graph
ggscatter(season,x="tempe.rature",y="low.frequency",size = 0.3, palette = "jco",
          facet.by = "cities.data", #scales = "free_x",
          add = "reg.line", conf.int = TRUE) +
  stat_cor( method = "pearson", label.y = 6)+labs(x='TEMPERATURE',y='LOW FREQUENCY(Hz)')+theme(text = element_text(family = "serif"))+theme(text = element_text(size = 15,hjust = 0.5))

#3.Correlation between temperature and Peak frequency for all cities in a faceted correlation graph
ggscatter(season,x="tempe.rature",y="peak.frequency",size = 0.3, palette = "jco",
          facet.by = "cities.data", #scales = "free_x",
          add = "reg.line", conf.int = TRUE) +
  stat_cor( method = "pearson", label.y = 6)+labs(x='TEMPERATURE',y='PEAK FREQUENCY(Hz)')+theme(text = element_text(family = "serif"))+theme(text = element_text(size = 15,hjust = 0.5))

#4.Correlation between temperature and Syllable Duration for all cities in a faceted correlation graph
ggscatter(season,x="tempe.rature",y="syllable.duration",size = 0.3, palette = "jco",
          facet.by = "cities.data", #scales = "free_x",
          add = "reg.line", conf.int = TRUE) +
  stat_cor( method = "pearson", label.y = 6)+labs(x='TEMPERATURE',y='SYLLABLE DURATION (Secs)')+theme(text = element_text(family = "serif"))+theme(text = element_text(size = 15,hjust = 0.5))

#5.Correlation between Humidity and High Frequency for all cities in a faceted correlation graph
ggscatter(season,x="Humidity",y="high.frequency",size = 0.3, palette = "jco",
          facet.by = "cities.data", #scales = "free_x",
          add = "reg.line", conf.int = TRUE) +
  stat_cor( method = "pearson", label.y = 6)+labs(x='HUMIDITY',y='HIGH FREQUENCY(Hz)')+theme(text = element_text(family = "serif"))+theme(text = element_text(size = 15,hjust = 0.5))

#6.Correlation between Humidity and Low Frequency for all cities in a faceted correlation graph
ggscatter(season,x="Humidity",y="low.frequency",size = 0.3, palette = "jco",
          facet.by = "cities.data", #scales = "free_x",
          add = "reg.line", conf.int = TRUE) +
  stat_cor( method = "pearson", label.y = 6)+labs(x='HUMIDITY',y='LOW FREQUENCY(Hz)')+theme(text = element_text(family = "serif"))+theme(text = element_text(size = 15,hjust = 0.5))

#7.Correlation between Humidity and Peak Freqeuncy for all cities in a faceted correlation graph
ggscatter(season,x="Humidity",y="peak.frequency",size = 0.3, palette = "jco",
          facet.by = "cities.data", #scales = "free_x",
          add = "reg.line", conf.int = TRUE) +
  stat_cor( method = "pearson", label.y = 6)+labs(x='HUMIDITY',y='PEAK FREQUENCY(Hz)')+theme(text = element_text(family = "serif"))+theme(text = element_text(size = 15,hjust = 0.5))

#8.Correlation between Humidity and Syllable Duration for all cities in a faceted correlation graph
ggscatter(season,x="Humidity",y="syllable.duration",size = 0.3, palette = "jco",
          facet.by = "cities.data", #scales = "free_x",
          add = "reg.line", conf.int = TRUE) +
  stat_cor( method = "pearson", label.y = 6)+labs(x='HUMIDITY',y='SYLLABLE DURATION (Secs)')+theme(text = element_text(family = "serif"))+theme(text = element_text(size = 15,hjust = 0.5))

################################
#ANOVA RESULTS FOR COMPARING DIFFRENCES IN VARIOUS PARAMETERS OF CALLS BETWEEN ALL 3 AREAS

#1.ANOVA for Comparing differences in Low Frequency between 3 cities
attach(season)
v<-aov(city.number~low.frequency,data=season)
summary(v)

#2.ANOVA for Comparing differences in High Frequency between 3 cities
attach(season)
u<-aov(city.number~high.frequency,data=season)
summary(u)

#3.ANOVA for Comparing differences in Peak Frequency between 3 cities
attach(season)
w<-aov(city.number~peak.frequency,data=season)
summary(w)

#4.ANOVA for Comparing differences in Syllable duration between 3 cities
attach(season)
w<-aov(city.number~syllable.duration,data=season)
summary(w)

##################################
#BOX PLOTS FOR COMPARING DIFFRENCES IN VARIOUS PARAMETERS OF CALLS BETWEEN ALL 3 AREAS

#1.BOXPLOT for comparing low frequency between 3 cities
attach(season)
library(ggpubr)
ggboxplot(season,x="cities.data",y="low.frequency",xlab = "CITIES",ylab = "LOW FREQUENCY(Hz)",fill = "gray",palette = "jco")+theme(text = element_text(size=13,family = "serif"))

#2.BOXPLOT for comparing high frequency between 3 cities
attach(season)
ggboxplot(season,x="cities.data",y="high.frequency",xlab = "CITIES",ylab = "HIGH FREQUENCY(Hz)",fill = "gray",palette = "jco")+theme(text = element_text(size=13,family = "serif"))

#3.BOXPLOT for comparing peak frequency between 3 cities
attach(season)
ggboxplot(season,x="cities.data",y="peak.frequency",xlab = "CITIES",ylab = "PEAK FREQUENCY(Hz)",fill = "gray",palette = "jco")+theme(text = element_text(size=13,family = "serif"))

#4.BOXPLOT for comparing Syllable Duration between 3 cities
attach(season)
ggboxplot(season,x="cities.data",y="syllable.duration",xlab = "CITIES",ylab = "SYLLABLE DURATION(secs)",fill = "gray",palette = "jco")+theme(text = element_text(size=13,family = "serif"))


#ANOVA BETWEEN CITIES ON THE BASIS OF AVERAGE SYLLABLE DURATION FOR EACH MONTH
attach(my_thesis_whole_data2)
c<-aov(city.number~syllable.duration,data=my_thesis_whole_data)
summary(c)


#Finding median high frequency for all cities

#High frequency
Ahmedabad=subset(season$high.frequency, cities.data=="Ahmedabad")
summary(Ahmedabad)

Guwahati=subset(season$high.frequency, cities.data=="Guwahati")
summary(Guwahati)

ADBU=subset(season$high.frequency, cities.data=="ADBU")
summary(ADBU)

#Low frequency
Ahmedabad=subset(season$low.frequency, cities.data=="Ahmedabad")
summary(Ahmedabad)

Guwahati=subset(season$low.frequency, cities.data=="Guwahati")
summary(Guwahati)

ADBU=subset(season$low.frequency, cities.data=="ADBU")
summary(ADBU)

#peak freqeuncy
Ahmedabad=subset(season$peak.frequency, cities.data=="Ahmedabad")
summary(Ahmedabad)

Guwahati=subset(season$peak.frequency, cities.data=="Guwahati")
summary(Guwahati)

ADBU=subset(season$peak.frequency, cities.data=="ADBU")
summary(ADBU)

#syllable duration
Ahmedabad=subset(season$syllable.duration, cities.data=="Ahmedabad")
summary(Ahmedabad)

Guwahati=subset(season$syllable.duration, cities.data=="Guwahati")
summary(Guwahati)

ADBU=subset(season$syllable.duration, cities.data=="ADBU")
summary(ADBU)

