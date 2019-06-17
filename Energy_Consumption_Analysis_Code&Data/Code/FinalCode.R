library(tidyverse)
library(MASS)
library(readr)
library(modelr)
library(class)
library (dplyr)
install.packages("lubridate")
library("lubridate")
getwd()
Energy1<- read.csv("energydata_daynight.csv")
Total_Energy <- Energy1$Appliances+Energy1$lights
#new variable addition -
pred_dew <- (243.12*(log(Energy1$RH_out/100)+(17.62*Energy1$T_out/243.12+Energy1$T_out)))/(17.62-(log(Energy1$RH_out/100)+(17.62*Energy1$T_out/243.12+Energy1$T_out)))
Energy1<- Energy1%>%
  mutate(pred_dew)
tempdiff <- Energy1$T_out - Energy1$T6
rhdiff<- Energy1$RH_out - Energy1$RH_6
Energy1<- Energy1%>%
  mutate(Total_Energy)
Energy_new <- Energy1[,-1]
Energy_new<- Energy_new[,-1]
Energy_new<- Energy_new[,-1]
summary(Energy1)
#data for day
Enew_day<-filter(Energy1t,Day_Night==1)
summary(Enew_day)
#data for Night
Enew_night<- filter(Energy1,Day_Night==0)
summary(Enew_night)
#Data for January
Enew_Jan<- Energy1%>%
  filter(Day_Night,Day_Night==1 & Month=="Jan")
#Data for February
Enew_Feb<- Energy1%>%
  filter(Day_Night,Day_Night==1 & Month=="Feb")
#Data for March
Enew_Mar<- Energy1%>%
  filter(Day_Night,Day_Night==1 & Month=="Mar")
#Data for April
Enew_Apr<- Energy1%>%
  filter(Day_Night,Day_Night==1 & Month=="Apr")
#Data for May
Enew_May<- Energy1%>%
  filter(Day_Night,Day_Night==1 & Month=="May")
#Exploration for Day Energy Comsumption along with Visibility for Room1 for temp.
ggplot(Enew, aes(Appliances,Visibility))+
  geom_point(aes(T1))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Visibility for Room1 for Humidity.
ggplot(Enew, aes(Appliances,Visibility))+
  geom_point(aes(RH_1))
#Exploration for Day Light Comsumption along with Visibility for Room1 for temp.
ggplot(Enew, aes(lights,Visibility))+
  geom_point(aes(T1))+
  geom_smooth()
#Exploration for Day Light Comsumption along with Visibility for Room1 for Humidity.
ggplot(Enew, aes(lights,Visibility))+
  geom_point(aes(RH_1))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Visibility for Room2 for temp.
ggplot(Enew, aes(Appliances,Visibility))+
  geom_point(aes(T2))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Visibility for Room2 for Humidity.
ggplot(Enew, aes(Appliances,Visibility))+
  geom_point(aes(RH_2))
#Exploration for Day Light Comsumption along with Visibility for Room2 for temp.
ggplot(Enew, aes(lights,Visibility))+
  geom_point(aes(T2))+
  geom_smooth()
#Exploration for Day Light Comsumption along with Visibility for Room2 for Humidity.
ggplot(Enew, aes(lights,Visibility))+
  geom_point(aes(RH_2))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Visibility for Room3 for temp.
ggplot(Enew, aes(Appliances,Visibility))+
  geom_point(aes(T3))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Visibility for Room3 for Humidity.
ggplot(Enew, aes(Appliances,Visibility))+
  geom_point(aes(RH_3))
#Exploration for Day Light Comsumption along with Visibility for Room3 for temp.
ggplot(Enew, aes(lights,Visibility))+
  geom_point(aes(T3))+
  geom_smooth()
#Exploration for Day Light Comsumption along with Visibility for Room3 for Humidity.
ggplot(Enew, aes(lights,Visibility))+
  geom_point(aes(RH_3))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Visibility for Room4 for temp.
ggplot(Enew, aes(Appliances,Visibility))+
  geom_point(aes(T4))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Visibility for Room4 for Humidity.
ggplot(Enew, aes(Appliances,Visibility))+
  geom_point(aes(RH_4))
#Exploration for Day Light Comsumption along with Visibility for Room4 for temp.
ggplot(Enew, aes(lights,Visibility))+
  geom_point(aes(T4))+
  geom_smooth()
#Exploration for Day Light Comsumption along with Visibility for Room4 for Humidity.
ggplot(Enew, aes(lights,Visibility))+
  geom_point(aes(RH_4))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Visibility for Room5 for temp.
ggplot(Enew, aes(Appliances,Visibility))+
  geom_point(aes(T5))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Visibility for Room5 for Humidity.
ggplot(Enew, aes(Appliances,Visibility))+
  geom_point(aes(RH_5))
#Exploration for Day Light Comsumption along with Visibility for Room5 for temp.
ggplot(Enew, aes(lights,Visibility))+
  geom_point(aes(T5))+
  geom_smooth()
#Exploration for Day Light Comsumption along with Visibility for Room5 for Humidity.
ggplot(Enew, aes(lights,Visibility))+
  geom_point(aes(RH_5))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Visibility for Room6 for temp.
ggplot(Enew, aes(Appliances,Visibility))+
  geom_point(aes(T6))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Visibility for Room6 for Humidity.
ggplot(Enew, aes(Appliances,Visibility))+
  geom_point(aes(RH_6))
#Exploration for Day Light Comsumption along with Visibility for Room6 for temp.
ggplot(Enew, aes(lights,Visibility))+
  geom_point(aes(T6))+
  geom_smooth()
#Exploration for Day Light Comsumption along with Visibility for Room6 for Humidity.
ggplot(Enew, aes(lights,Visibility))+
  geom_point(aes(RH_6))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Visibility for Room7 for temp.
ggplot(Enew, aes(Appliances,Visibility))+
  geom_point(aes(T7))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Visibility for Room7 for Humidity.
ggplot(Enew, aes(Appliances,Visibility))+
  geom_point(aes(RH_7))
#Exploration for Day Light Comsumption along with Visibility for Room7 for temp.
ggplot(Enew, aes(lights,Visibility))+
  geom_point(aes(T7))+
  geom_smooth()
#Exploration for Day Light Comsumption along with Visibility for Room7 for Humidity.
ggplot(Enew, aes(lights,Visibility))+
  geom_point(aes(RH_7))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Visibility for Room8 for temp.
ggplot(Enew, aes(Appliances,Visibility))+
  geom_point(aes(T8))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Visibility for Room8 for Humidity.
ggplot(Enew, aes(Appliances,Visibility))+
  geom_point(aes(RH_8))
#Exploration for Day Light Comsumption along with Visibility for Room8 for temp.
ggplot(Enew, aes(lights,Visibility))+
  geom_point(aes(T8))+
  geom_smooth()
#Exploration for Day Light Comsumption along with Visibility for Room8 for Humidity.
ggplot(Enew, aes(lights,Visibility))+
  geom_point(aes(RH_8))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Visibility for Room9 for temp.
ggplot(Enew, aes(Appliances,Visibility))+
  geom_point(aes(T9))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Visibility for Room9 for Humidity.
ggplot(Enew, aes(Appliances,Visibility))+
  geom_point(aes(RH_9))
#Exploration for Day Light Comsumption along with Visibility for Room9 for temp.
ggplot(Enew, aes(lights,Visibility))+
  geom_point(aes(T9))+
  geom_smooth()
#Exploration for Day Light Comsumption along with Visibility for Room9 for Humidity.
ggplot(Enew, aes(lights,Visibility))+
  geom_point(aes(RH_9))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Tdewpoint for Room1 for temp.
ggplot(Enew, aes(Appliances,Tdewpoint))+
  geom_point(aes(T1))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Tdewpoint for Room1 for Humidity.
ggplot(Enew, aes(Appliances,Tdewpoint))+
  geom_point(aes(RH_1))
#Exploration for Day Light Comsumption along with Tdewpoint for Room1 for temp.
ggplot(Enew, aes(lights,Tdewpoint))+
  geom_point(aes(T1))+
  geom_smooth()
#Exploration for Day Light Comsumption along with Tdewpoint for Room1 for Humidity.
ggplot(Enew, aes(lights,Tdewpoint))+
  geom_point(aes(RH_1))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Tdewpoint for Room2 for temp.
ggplot(Enew, aes(Appliances,Tdewpoint))+
  geom_point(aes(T2))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Tdewpoint for Room2 for Humidity.
ggplot(Enew, aes(Appliances,Tdewpoint))+
  geom_point(aes(RH_2))
#Exploration for Day Light Comsumption along with Tdewpoint for Room2 for temp.
ggplot(Enew, aes(lights,Tdewpoint))+
  geom_point(aes(T2))+
  geom_smooth()
#Exploration for Day Light Comsumption along with Tdewpoint for Room2 for Humidity.
ggplot(Enew, aes(lights,Tdewpoint))+
  geom_point(aes(RH_2))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Tdewpoint for Room3 for temp.
ggplot(Enew, aes(Appliances,Tdewpoint))+
  geom_point(aes(T3))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Tdewpoint for Room3 for Humidity.
ggplot(Enew, aes(Appliances,Tdewpoint))+
  geom_point(aes(RH_3))
#Exploration for Day Light Comsumption along with Tdewpoint for Room3 for temp.
ggplot(Enew, aes(lights,Tdewpoint))+
  geom_point(aes(T3))+
  geom_smooth()
#Exploration for Day Light Comsumption along with Tdewpoint for Room3 for Humidity.
ggplot(Enew, aes(lights,Tdewpoint))+
  geom_point(aes(RH_3))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Tdewpoint for Room4 for temp.
ggplot(Enew, aes(Appliances,Tdewpoint))+
  geom_point(aes(T4))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Tdewpoint for Room4 for Humidity.
ggplot(Enew, aes(Appliances,Tdewpoint))+
  geom_point(aes(RH_4))
#Exploration for Day Light Comsumption along with Tdewpoint for Room4 for temp.
ggplot(Enew, aes(lights,Tdewpoint))+
  geom_point(aes(T4))+
  geom_smooth()
#Exploration for Day Light Comsumption along with Tdewpoint for Room4 for Humidity.
ggplot(Enew, aes(lights,Tdewpoint))+
  geom_point(aes(RH_4))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Tdewpoint for Room5 for temp.
ggplot(Enew, aes(Appliances,Tdewpoint))+
  geom_point(aes(T5))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Tdewpoint for Room5 for Humidity.
ggplot(Enew, aes(Appliances,Tdewpoint))+
  geom_point(aes(RH_5))
#Exploration for Day Light Comsumption along with Tdewpoint for Room5 for temp.
ggplot(Enew, aes(lights,Tdewpoint))+
  geom_point(aes(T5))+
  geom_smooth()
#Exploration for Day Light Comsumption along with Tdewpoint for Room5 for Humidity.
ggplot(Enew, aes(lights,Tdewpoint))+
  geom_point(aes(RH_5))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Tdewpoint for Room6 for temp.
ggplot(Enew, aes(Appliances,Tdewpoint))+
  geom_point(aes(T6))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Tdewpoint for Room6 for Humidity.
ggplot(Enew, aes(Appliances,Tdewpoint))+
  geom_point(aes(RH_6))
#Exploration for Day Light Comsumption along with Tdewpoint for Room6 for temp.
ggplot(Enew, aes(lights,Tdewpoint))+
  geom_point(aes(T6))+
  geom_smooth()
#Exploration for Day Light Comsumption along with Tdewpoint for Room6 for Humidity.
ggplot(Enew, aes(lights,Tdewpoint))+
  geom_point(aes(RH_6))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Tdewpoint for Room7 for temp.
ggplot(Enew, aes(Appliances,Tdewpoint))+
  geom_point(aes(T7))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Tdewpoint for Room7 for Humidity.
ggplot(Enew, aes(Appliances,Tdewpoint))+
  geom_point(aes(RH_7))
#Exploration for Day Light Comsumption along with Tdewpoint for Room7 for temp.
ggplot(Enew, aes(lights,Tdewpoint))+
  geom_point(aes(T7))+
  geom_smooth()
#Exploration for Day Light Comsumption along with Tdewpoint for Room7 for Humidity.
ggplot(Enew, aes(lights,Tdewpoint))+
  geom_point(aes(RH_7))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Tdewpoint for Room8 for temp.
ggplot(Enew, aes(Appliances,Tdewpoint))+
  geom_point(aes(T8))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Tdewpoint for Room8 for Humidity.
ggplot(Enew, aes(Appliances,Tdewpoint))+
  geom_point(aes(RH_8))
#Exploration for Day Light Comsumption along with Tdewpoint for Room8 for temp.
ggplot(Enew, aes(lights,Tdewpoint))+
  geom_point(aes(T8))+
  geom_smooth()
#Exploration for Day Light Comsumption along with Tdewpoint for Room8 for Humidity.
ggplot(Enew, aes(lights,Tdewpoint))+
  geom_point(aes(RH_8))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Tdewpoint for Room9 for temp.
ggplot(Enew, aes(Appliances,Tdewpoint))+
  geom_point(aes(T9))+
  geom_smooth()
#Exploration for Day Energy Comsumption along with Tdewpoint for Room9 for Humidity.
ggplot(Enew, aes(Appliances,Tdewpoint))+
  geom_point(aes(RH_9))
#Exploration for Day Light Comsumption along with Tdewpoint for Room9 for temp.
ggplot(Enew, aes(lights,Tdewpoint))+
  geom_point(aes(T9))+
  geom_smooth()
#Exploration for Day Light Comsumption along with Tdewpoint for Room9 for Humidity.
ggplot(Enew, aes(lights,Tdewpoint))+
  geom_point(aes(RH_9))+
  geom_smooth()
#modelling
Total_Energy <- Energy1$Appliances+Energy1$lights
class(Total_Energy)
mod1<- lm(Appliances~T1+RH_1+T2+RH_2+T3+RH_3+T4+RH_4+T5+RH_5+T6+RH_6+T7+RH_7+T8+RH_8+T9+RH_9+Windspeed+Tdewpoint+Visibility+RH_out+T_out+Press_mm_hg, data = Energy1)
summary(mod1)
mod2<- lm(lights~T1+RH_1+T2+RH_2+T3+RH_3+T4+RH_4+T5+RH_5+T6+RH_6+T7+RH_7+T8+RH_8+T9+RH_9+Windspeed+Tdewpoint+Visibility+RH_out+T_out+Press_mm_hg, data = Energy1)
summary(mod2)
modroom1<- lm(Appliances~T1+RH_1+Windspeed+T_out*Tdewpoint+Visibility+RH_out+Press_mm_hg, data = Energy1)
summary(modroom1)
modroom2<- lm(Appliances~T2+RH_2+Windspeed+Tdewpoint+Visibility+RH_out+T_out+Press_mm_hg, data = Energy1)
summary(modroom2)
modroom3<- lm(Appliances~T3+RH_3+Windspeed+Tdewpoint+Visibility+RH_out+T_out+Press_mm_hg, data = Energy1)
summary(modroom3)
modroom4<- lm(Appliances~T4+RH_4+Windspeed+Tdewpoint+Visibility+RH_out+T_out+Press_mm_hg, data = Energy1)
summary(modroom4)
modroom5<- lm(Appliances~T5+RH_5+Windspeed+Tdewpoint+Visibility+RH_out+T_out+Press_mm_hg, data = Energy1)
summary(modroom5)
modroom6<- lm(Appliances~T6+RH_6+Windspeed+Tdewpoint+Visibility+RH_out+T_out+Press_mm_hg, data = Energy1)
summary(modroom6)
modroom7<- lm(Appliances~T7+RH_7+Windspeed+Tdewpoint+Visibility+RH_out+T_out+Press_mm_hg, data = Energy1)
summary(modroom7)
modroom8<- lm(Appliances~T8+RH_8+Windspeed+Tdewpoint+Visibility+RH_out+T_out+Press_mm_hg, data = Energy1)
summary(modroom8)
modroom9<- lm(Appliances~T9+RH_9+Windspeed+Tdewpoint+Visibility+RH_out+T_out+Press_mm_hg, data = Energy1)
summary(modroom9)
modlightroom1<- lm(lights~T1+RH_1+Windspeed+Tdewpoint+Visibility+RH_out+T_out+Press_mm_hg, data = Energy1)
summary(modlightroom1)
modlightroom2<- lm(lights~T2+RH_2+Windspeed+Tdewpoint+Visibility+RH_out+T_out+Press_mm_hg, data = Energy1)
summary(modlightroom2)
modlightroom3<- lm(lights~T3+RH_3+Windspeed+Tdewpoint+Visibility+RH_out+T_out+Press_mm_hg, data = Energy1)
summary(modlightroom3)
modlightroom4<- lm(lights~T4+RH_4+Windspeed+Tdewpoint+Visibility+RH_out+T_out+Press_mm_hg, data = Energy1)
summary(modlightroom4)
modlightroom5<- lm(lights~T5+RH_5+Windspeed+Tdewpoint+Visibility+RH_out+T_out+Press_mm_hg, data = Energy1)
summary(modlightroom5)
modlightroom6<- lm(lights~T6+RH_6+Windspeed+Tdewpoint+Visibility+RH_out+T_out+Press_mm_hg, data = Energy1)
summary(modlightroom6)
modlightroom7<- lm(lights~T7+RH_7+Windspeed+Tdewpoint+Visibility+RH_out+T_out+Press_mm_hg, data = Energy1)
summary(modlightroom7)
modlightroom8<- lm(lights~T8+RH_8+Windspeed+Tdewpoint+Visibility+RH_out+T_out+Press_mm_hg, data = Energy1)
summary(modlightroom8)
modlightroom9<- lm(lights~T9+RH_9+Windspeed+Tdewpoint+Visibility+RH_out+T_out+Press_mm_hg, data = Energy1)
summary(modlightroom9)
model1<- lm(Appliances~T_out*Tdewpoint*RH_out*Visibility*Windspeed*Press_mm_hg, data = Energy1)
summary(model1)
meandiff<- Energy1$T6 - Energy1$T_out
meandiff
mean(meandiff)
ggplot(Energy1)+
  geom_histogram(aes(meandiff))
model2<- lm(Appliances~T1+RH_1+T2+RH_2+T3+RH_3+T4+RH_4+T5+RH_5+T7+RH_7+T8+RH_8+T6*RH_6+T_out*Tdewpoint*RH_out*Windspeed*Press_mm_hg+Visibility, data = Energy1)
summary(model2)
meandiff1<- Energy1$T1 - Energy1$T6
meandiff2<- Energy1$T2 - Energy1$T6
meandiff3<- Energy1$T3 - Energy1$T6
meandiff4<- Energy1$T4 - Energy1$T6
meandiff5<- Energy1$T5 - Energy1$T6
meandiff7<- Energy1$T7 - Energy1$T6
meandiff8<- Energy1$T8 - Energy1$T6
meandiff9<- Energy1$T9 - Energy1$T6
RHDIFF1<- Energy1$RH_1 -Energy1$RH_6
RHDIFF2<- Energy1$RH_2 -Energy1$RH_6
RHDIFF3<- Energy1$RH_3 -Energy1$RH_6
RHDIFF4<- Energy1$RH_4 -Energy1$RH_6
RHDIFF5<- Energy1$RH_5 -Energy1$RH_6
RHDIFF7<- Energy1$RH_7 -Energy1$RH_6
RHDIFF8<- Energy1$RH_8 -Energy1$RH_6
RHDIFF9<- Energy1$RH_9 -Energy1$RH_6
model3<- lm(Appliances~meandiff1*RHDIFF1+meandiff2*RHDIFF2+meandiff3*RHDIFF3+meandiff4*RHDIFF4+meandiff5+RHDIFF5*meandiff7+RHDIFF7*meandiff8+RHDIFF8+meandiff9*RHDIFF9+T6*RH_6+T_out*Tdewpoint*RH_out*Windspeed*Press_mm_hg+Visibility, data = Energy1)
summary(model3)
totalenergy<- Appliances+lights
view(totalenergy)
count(totalenergy)
Energy1<- Energy1%>%
  mutate(Total_Energy)
view(Energy1)
count(Energy1)
#Dewpoint
ggplot((Energy1))+
  geom_histogram(aes(Tdewpoint))
#new variable addition -
pred_dew <- (243.12*(log(Energy1$RH_out/100)+(17.62*Energy1$T_out/243.12+Energy1$T_out)))/(17.62-(log(Energy1$RH_out/100)+(17.62*Energy1$T_out/243.12+Energy1$T_out)))
hist(pred_dew)
view(pred_dew)
class(pred_dew)
count(pred_dew)
Energy1$pred_dew <- Energy1
ggplot(Energy1)+
  geom_point(aes(pred_dew,Tdewpoint))
Energy1<- Energy1%>%
  mutate(pred_dew)
ggplot(Energy_new)+
  geom_point(aes(Tdewpoint,pred_dew, color = Month))+
  facet_wrap(~Month, nrow = 3)
#why difference in apr and may
ggplot(Energy_new)+
  geom_smooth(aes(Windspeed,Appliances))+
  facet_wrap(~Day_Night)
#data for day
Enew_day<- Energy_new%>%
  filter(Day_Night,Day_Night==1)
#data for Night
Enew_night<- Energy_new%>%
  filter(Day_Night,Day_Night==0)
#temp diff outside and weather station
tempdiff <- Energy1$T_out - Energy1$T6
rhdiff<- Energy1$RH_out - Energy1$RH_6
Energy1 <- Energy1%>%
  mutate(tempdiff)%>%
  mutate(rhdiff)
#Relationship between temp and humidity
ggplot(Energy_new)+
  geom_smooth(aes(RH_out,tempdiff))+
  facet_wrap(~Month)
ggplot(Energy_new)+
  geom_smooth(aes(rhdiff,tempdiff))+
  facet_wrap(~Month)
summary(rhdiff)
# taking out values
Energy_new2<- filter(Energy_new,Energy_new$RH_out>60& Energy_new$RH_out<90)
Energy_new <- Energy1[,-1]
Energy_new<- Energy_new[,-1]
Energy_new<- Energy_new[,-1]
#modelrun
mod_new1 <- lm(Total_Energy~., data = Energy_new)
summary(mod_new1)
ggplot(Energy_new2)+
  geom_smooth(aes(lights,tempdiff))
#PCA
install.packages("pls")
library(pls)
pcr.fit.model=pcr(Total_Energy~., data=Energy_new, scale=TRUE, validation="CV")
summary(pcr.fit.model)
validationplot(pcr.fit.model, val.type="MSEP")
#To run the model again with the optimal number of components
pcr.fit.model1=pcr(Total_Energy~., data=Energy_new, scale=TRUE, ncomp = 7)
summary(pcr.fit.model1)
validationplot(pcr.fit.model1,val.type="R2")
pcr.fit.model1=pcr(Total_Energy~., data=Energy_new, scale=TRUE, ncomp = 10)
summary(pcr.fit.model1)
validationplot(pcr.fit.model1, val.type="MSEP")
#New PCA Method
install.packages("FactoMineR")
library(FactoMineR)
Energy_new1<- Energy_new[,-28]
pca.model= PCA(Energy_new1, graph = FALSE)
summary(pca.model)
pca.model$eig
pca.model$var$coord
head(pca.model$ind$coord)
#New PCA Method
library(stats)
pca1 = prcomp(Energy_new1, scale. = TRUE)
# sqrt of eigenvalues
pca1$sdev
head(pca1$rotation)
head(pca1$x)
scores = as.data.frame(pca1$x)
# plot of observations
ggplot(data = scores, aes(x = PC1, y = PC2, label = rownames(scores))) +
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  geom_text(colour = "tomato", alpha = 0.3, size = 1) +
  ggtitle("PCA plot ")
circle <- function(center = c(0, 0), npoints = 100) {
  r = 1
  tt = seq(0, 2 * pi, length = npoints)
  xx = center[1] + r * cos(tt)
  yy = center[1] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
corcir = circle(c(0, 0), npoints = 100)
# create data frame with correlations between variables and PCs
correlations = as.data.frame(cor(Energy_new1, pca1$x))
# data frame with arrows coordinates
arrows = data.frame(x1 = c(0, 0, 0, 0), y1 = c(0, 0, 0, 0), x2 = correlations$PC1,
                    y2 = correlations$PC2)
# geom_path will do open circles
ggplot() + geom_path(data = corcir, aes(x = x, y = y), colour = "gray65") +
  geom_segment(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2), colour = "gray65") +
  geom_text(data = correlations, aes(x = PC1, y = PC2, label = rownames(correlations))) +
  geom_hline(yintercept = 0, colour = "gray65") + geom_vline(xintercept = 0,
                                                             colour = "gray65") + xlim(-1.1, 1.1) + ylim(-1.1, 1.1) + labs(x = "pc1 aixs",
                                                                                                                           y = "pc2 axis") + ggtitle("Circle of correlations")
library(ggplot2)
# create data frame with scores
scores = as.data.frame(pca.model$ind$coord)
# plot of observations
ggplot(data = scores, aes(x = Dim.1, y = Dim.2, label = rownames(scores))) +
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  geom_text(colour = "tomato", alpha = 0.8, size = 4) +
  ggtitle("PCA plot")
#Best subset selection
install.packages('leaps')
library(leaps)
regfit.full2=regsubsets(Total_Energy~., Energy_new)
summary(regfit.full2)
regfit.max = regsubsets(Total_Energy~., Energy_new, nvmax=2)
regsummary = summary(regfit.max)
regsummary$rsq
?regsubsets
#Annotate the one with highest value
which.max(regsummary$adjr2)
points(11,regsummary$adjr2[11],col='red')
#We can plot cp, bic the same way
plot(regfit.max, scale="r2")
plot(regfit.max, scale="adjr2")
plot(regfit.max, scale="bic")
coef(regfit.max, 6)
coef(regfit.max, 8)
#Forward and backward selection
regfit.fwd=regsubsets(Total_Energy~., data=Energy_new, nvmax=19, method="forward")
r<- summary(regfit.fwd)
regfit.bwd=regsubsets(Total_Energy~., data=Energy_new, nvmax=19, method="backward")
r1<- summary(regfit.bwd)
# Check the best seven-variable models
coef(regfit.full2, 7)
coef(regfit.fwd, 9)
coef(regfit.bwd, 9)
plot(r$bic, xlab ="Parameter", ylab ="bic")
plot(r1$bic, xlab ="Parameter", ylab ="bic")
model1<- lm(Total_Energy~RH_1+RH_2+T3+T8+RH_8+T9+Month, data = data_train)
x_test <- data_test[,1:29]
y_test <- data_test[,30]
predictions <- predict(model1, x_test)
# summarize results
confusionMatrix(predictions$class, y_test)
summary(model1)
install.pacakages("klaR")
install.packages("caret")
library(caret)
split=0.80
trainIndex <- createDataPartition(Energy_new$Total_Energy, p=split, list=FALSE)
data_train <- Energy_new[ trainIndex,]
data_test <- Energy_new[-trainIndex,]
#lasso and Ridge
install.packages("glmnet")
library(glmnet)
#We need to create predictor matrix and y vector because glmnet() requires such input format
X=model.matrix(Total_Energy~., Energy_new)
y=Energy_new$Total_Energy
#ridge regression: alpha=0; lasso regression: alpha = 1
#We run it for 100 lamda values
lambdas=10^seq(10, -2, length=100)
ridge.fit=glmnet(X, y, alpha=0, lambda = lambdas)
summary(ridge.fit)
dim(Energy_new)
#The coeffients are a 30X100 matrix. Each column is corresponding to each lamda. We take a look at one.
coef(ridge.fit)[,50]
#cross_valudation - cv.glmnet to select the optimal model
set.seed(1)
ridge.cv = cv.glmnet(X, y, alpha = 0)
plot(ridge.cv)
ridge.cv$lambda
opt_lambda = ridge.cv$lambda.min
opt_lambda
opt_lambda = ridge.cv$lambda.1se
opt_lambda
#predict the estimated y value with the model we choose by setting "s" to be equal to the best lambda
y_predicted = predict(ridge.fit, s = opt_lambda, newx = X)
TSS = sum((y - mean(y))^2)
RSS = sum((y_predicted - y)^2)
Rsq <- 1 - RSS / TSS
Rsq
#The lasso, alpha=1
lasso.fit=glmnet(X, y, alpha=1, lambda = lambdas)
plot(lasso.fit)
set.seed(1)
lasso.cv = cv.glmnet(X, y, alpha = 1)
plot(lasso.cv)
bestlambda = lasso.cv$lambda.min
bestlambda
lasso.coef=predict(lasso.fit, type = "coefficients", s = bestlambda)[1:30,]
lasso.coef
y_predicted = predict(lasso.fit, s = bestlambda, newx = X)
TSS = sum((y - mean(y))^2)
RSS = sum((y_predicted - y)^2)
Rsq <- 1 - RSS / TSS
Rsq
install.packages('tree')
library(tree)
library(ISLR)
library(MASS)
#To turn the "Appliances" into a binary variable.
summary(Energy_new$Total_Energy)
High = ifelse(Energy_new$Total_Energy<=101.5, "No", "Yes")
Energy_new = data.frame(Energy_new, High)
#Fitting the tree
tree.energy = tree(High~.-Total_Energy, Energy_new)
summary(tree.energy)
tree.energy
#plot the tree
plot(tree.energy)
text(tree.energy, pretty = 0)
#Pruning the tree
set.seed(3)
cv.energy = cv.tree(tree.energy, FUN = prune.misclass)
cv.energy
#plot the error rate as a function of both size and k; type "b" means both line and points
plot(cv.energy$size, cv.energy$dev, type = "b")
plot(cv.energy$k, cv.energy$dev, type = "b")
#Get the tree that gives the lowest error
prune.energy = prune.misclass(tree.energy, best = 5)
plot(prune.energy)
text(prune.energy, pretty = 0)
prune.energy
#For predictions of the tree, we use predict function. You may want to separate the dataset into train and test for this
tree.pred = predict(prune.energy, Energy_new, type = "class")
table(tree.pred, Energy_new$High)
#fitting regression tree
#Random FOrest
install.packages('randomForest')
library(randomForest)
train = sample(nrow(Energy_new), nrow(Energy_new)/2)
set.seed(1)
bag.energy = randomForest(Total_Energy~., data = Energy_new, subset = train, mtry = 31, importance = TRUE)
bag.energy
bag.y = predict(bag.energy, newdata = Energy_new3[-train, ])
energy.test = Energy_new3[-train, "medv"]
plot(bag.y, energy.test)
abline(0,1)
mean((bag.y - energy.test)^2)
#random forests
set.seed(1)
rf.energy = randomForest(Appliances~., data = Energy_new3, subset = train, mtry = 6, importance = TRUE)
rf.energy
rf.y = predict(rf.energy, newdata = Energy_new3[-train, ])
mean((rf.y - energy.test)^2)
#change mtry value
set.seed(1)
rf.energy = randomForest(Appliances~., data = Energy_new3, subset = train, mtry = 10, importance = TRUE)
rf.energy
rf.y = predict(rf.energy, newdata = Energy_new3[-train, ])
mean((rf.y - energy.test)^2)
importance(rf.energy)
varImpPlot(rf.energy)
# GBM
# We use gbm library
install.packages("gbm")
library(gbm)
set.seed(1)
boost.energy = gbm(Appliances~., data = Energy_new3[train,], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
summary(boost.energy)
boost.y = predict(boost.energy, newdata = Energy_new3[-train, ], n.trees = 5000)
mean((boost.y - energy.test)^2)
#For Light
# GBM
install.packages("rpart")
library(rpart)
data(kyphosis)
y <- as.numeric(kyphosis$Kyphosis)-1
y
x <- kyphosis$Age
glm1 <- glm(y~poly(x,2),family=binomial)
p <- predict(glm1,type="response")
calibrate.plot(y, p, xlim=c(0,0.6), ylim=c(0,0.6))
library(rpart)
install.packages("pls")
library(pls)
pcr.fit=pcr(Lights~., data=Enew_Jan, scale=FALSE, validation="CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type="MSEP")
#dewpoint
ggplot((Energy1))+
  geom_histogram(aes(Tdewpoint))
#new variable addition -
pred_dew <- (243.12*(log(Energy1$RH_out/100)+(17.62*Energy1$T_out/243.12+Energy1$T_out)))/(17.62-(log(Energy1$RH_out/100)+(17.62*Energy1$T_out/243.12+Energy1$T_out)))
hist(pred_dew)
view(pred_dew)
class(pred_dew)
count(pred_dew)
Energy1$pred_dew <- Energy1
ggplot(Energy1)+
  geom_point(aes(pred_dew,Tdewpoint))
Energy_new<- Energy1%>%
  mutate(pred_dew)
ggplot(Energy_new)+
  geom_point(aes(Tdewpoint,pred_dew, color = Month))+
  facet_wrap(~Month, nrow = 3)
#why difference in apr and may
ggplot(Energy_new)+
  geom_smooth(aes(Windspeed,Lights))+
  facet_wrap(~Day_Night)
#data for day
Enew_day<- Energy_new%>%
  filter(Day_Night,Day_Night==1)
#data for Night
Enew_night<- Energy_new%>%
  filter(Day_Night,Day_Night==0)
#temp diff outside and weather station
tempdiff <- Energy_new$T_out - Energy_new$T6
rhdiff<- Energy_new$RH_out - Energy_new$RH_6
Energy_new <- Energy_new%>%
  mutate(tempdiff)%>%
  mutate(rhdiff)
#Relationship between temp and humidity
ggplot(Energy_new)+
  geom_smooth(aes(RH_out,tempdiff))+
  facet_wrap(~Month)
ggplot(Energy_new)+
  geom_smooth(aes(rhdiff,tempdiff))+
  facet_wrap(~Month)
summary(rhdiff)
# taking out values
Energy_new2<- filter(Energy_new,Energy_new$RH_out>60& Energy_new$RH_out<90)
Energy_new3 <- Energy_new2[,-1]
#modelrun
mod_new1 <- lm(Lights~pred_dew*Windspeed*Press_mm_hg+T1*RH_1+T2*RH_2+T3*RH_3+T4*RH_4+T5*RH_5+T6*RH_6+T7*RH_7+T8*RH_8+T9*RH_9, data = Energy_new2)
summary(mod_new1)
ggplot(Energy_new2)+
  geom_smooth(aes(lights,tempdiff))
#PCA
install.packages("pls")
library(pls)
pcr.fit.model=pcr(Lights~., data=Energy_new3, scale=TRUE, validation="CV")
summary(pcr.fit.model)
validationplot(pcr.fit.model, val.type="MSEP")
#To run the model again with the optimal number of components
pcr.fit.model1=pcr(Lights~., data=Energy_new3, scale=TRUE, ncomp = 7)
summary(pcr.fit.model1)
pcr.fit.model1=pcr(Lights~., data=Energy_new3, scale=TRUE, ncomp = 10)
summary(pcr.fit.model1)
#Best subset selection
install.packages('leaps')
library(leaps)
regfit.full2=regsubsets(Lights~., Energy_new3)
summary(regfit.full2)
#Annotate the one with highest value
which.max(regsummary$adjr2)
points(11,regsummary$adjr2[11],col='red')
#We can plot cp, bic the same way
plot(regfit.max, scale="r2")
plot(regfit.max, scale="adjr2")
plot(regfit.max, scale="bic")
coef(regfit.max, 6)
#Forward and backward selection
regfit.fwd=regsubsets(Lights~., data=Energy_new3, nvmax=19, method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(Lights~., data=Energy_new3, nvmax=19, method="backward")
summary(regfit.bwd)
# Check the best seven-variable models
coef(regfit.full2, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)
#lasso and Ridge
install.packages("glmnet")
library(glmnet)
#We need to create predictor matrix and y vector because glmnet() requires such input format
X=model.matrix(Lights~., Energy_new3)
y=Energy_new3$Lights
#ridge regression: alpha=0; lasso regression: alpha = 1
#We run it for 100 lamda values
lambdas=10^seq(10, -2, length=100)
ridge.fit=glmnet(X, y, alpha=0, lambda = lambdas)
summary(ridge.fit)
dim(Energy_new3)
#The coeffients are a 32X100 matrix. Each column is corresponding to each lamda. We take a look at one.
coef(ridge.fit)[,50]
#cross_valudation - cv.glmnet to select the optimal model
set.seed(1)
ridge.cv = cv.glmnet(X, y, alpha = 0)
plot(ridge.cv)
ridge.cv$lambda
opt_lambda = ridge.cv$lambda.min
opt_lambda
opt_lambda = ridge.cv$lambda.1se
opt_lambda
#predict the estimated y value with the model we choose by setting "s" to be equal to the best lambda
y_predicted = predict(ridge.fit, s = opt_lambda, newx = X)
TSS = sum((y - mean(y))^2)
RSS = sum((y_predicted - y)^2)
Rsq <- 1 - RSS / TSS
Rsq
#The lasso, alpha=1
lasso.fit=glmnet(X, y, alpha=1, lambda = lambdas)
plot(lasso.fit)
set.seed(1)
lasso.cv = cv.glmnet(X, y, alpha = 1)
plot(lasso.cv)
bestlambda = lasso.cv$lambda.min
bestlambda
lasso.coef=predict(lasso.fit, type = "coefficients", s = bestlambda)[1:32,]
lasso.coef
install.packages('tree')
library(tree)
library(ISLR)
library(MASS)
#To turn the "Lights" into a binary variable.
summary(Energy_new3$Lights)
High = ifelse(Energy_new3$Lights<=100.3, "No", "Yes")
Energy_new3 = data.frame(Energy_new3, High)
#Fitting the tree
tree.energy = tree(High~.-Lights, Energy_new3)
summary(tree.energy)
tree.energy
#plot the tree
plot(tree.energy)
text(tree.energy, pretty = 0)
#Pruning the tree
set.seed(3)
cv.energy = cv.tree(tree.energy, FUN = prune.misclass)
cv.energy
#plot the error rate as a function of both size and k; type "b" means both line and points
plot(cv.energy$size, cv.energy$dev, type = "b")
plot(cv.energy$k, cv.energy$dev, type = "b")
#Get the tree that gives the lowest error
prune.energy = prune.misclass(tree.energy, best = 5)
plot(prune.energy)
text(prune.energy, pretty = 0)
prune.energy
#For predictions of the tree, we use predict function. You may want to separate the dataset into train and test for this
tree.pred = predict(prune.energy, Energy_new3, type = "class")
table(tree.pred, Energy_new3$High)
#fitting regression tree
#Random FOrest
install.packages('randomForest')
library(randomForest)
train = sample(nrow(Energy_new3), nrow(Energy_new3)/2)
set.seed(1)
bag.energy = randomForest(Lights~., data = Energy_new3, subset = train, mtry = 31, importance = TRUE)
bag.energy
bag.y = predict(bag.energy, newdata = Energy_new3[-train, ])
energy.test = Energy_new3[-train, "medv"]
plot(bag.y, energy.test)
abline(0,1)
mean((bag.y - energy.test)^2)
#random forests
set.seed(1)
rf.energy = randomForest(Lights~., data = Energy_new3, subset = train, mtry = 6, importance = TRUE)
rf.energy
rf.y = predict(rf.energy, newdata = Energy_new3[-train, ])
mean((rf.y - energy.test)^2)
#change mtry value
set.seed(1)
rf.energy = randomForest(Lights~., data = Energy_new3, subset = train, mtry = 10, importance = TRUE)
rf.energy
rf.y = predict(rf.energy, newdata = Energy_new3[-train, ])
mean((rf.y - energy.test)^2)
importance(rf.energy)
varImpPlot(rf.energy)
# GBM
# We use gbm library
install.packages("gbm")
library(gbm)
set.seed(1)
boost.energy = gbm(Lights~., data = Energy_new[train,], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
summary(boost.energy)
boost.y = predict(boost.energy, newdata = Energy_new3[-train, ], n.trees = 5000)
mean((boost.y - energy.test)^2)
With Time Series -
install.packages('tidyverse', lib = ('C:/Program Files/Microsoft/R Client/R_SERVER/library'))
install.packages('ggplot2', lib = ('C:/Program Files/Microsoft/R Client/R_SERVER/library'))
install.packages('ISLR', lib = ('C:/Program Files/Microsoft/R Client/R_SERVER/library'))
install.packages('hms', lib = ('C:/Program Files/Microsoft/R Client/R_SERVER/library'))
install.packages('dplyr', lib = ('C:/Program Files/Microsoft/R Client/R_SERVER/library'))
install.packages('tibble', lib = ('C:/Program Files/Microsoft/R Client/R_SERVER/library'))
install.packages('lubridate', lib = ('C:/Program Files/Microsoft/R Client/R_SERVER/library'))
install.packages('xts', lib = ('C:/Program Files/Microsoft/R Client/R_SERVER/library'))
install.packages('zoo', lib = ('C:/Program Files/Microsoft/R Client/R_SERVER/library'))
install.packages('tseries')
library(tidyverse)
library(ggplot2)
library(tseries)
library(hms)
library(dplyr)
library(tibble)
library(lubridate)
library(xts)
.libPaths()
#Importing the CSV file
Enerdata2<-read.csv( 'energydata_daynight.csv')
attach(Enerdata2)
#View and summary data set
View(Enerdata2)
summary(Enerdata2)
str(Enerdata2)
#Extract Month, Weekday, Day of Year, Week of year, and Time from Local Time
Enerdata2$Dates<-as.Date(Enerdata2$date, "%m/%d/%Y")
str(Enerdata2)
View(Enerdata2)
Enerdata2$date2<-as.POSIXct(Enerdata2$date, format = "%m/%d/%Y %H:%M")
class(Enerdata2$date2)
Enerdata2$Wday<-format(as.Date(Enerdata2$date2, by = 'day', len = 7), "%a")
Enerdata2$Month<-format(as.Date(Enerdata2$date2, by = 'month', len = 12), "%b")
Enerdata2$Week<-format(as.Date(Enerdata2$date2, by = 'week', len = 7), "%W")
Enerdata2$NDay<-format(as.Date(Enerdata2$date2, by = 'day', len = 366), "%j")
Enerdata2$Time2<-format(as.POSIXlt(Enerdata2$date2), "%R")
Enerdata2$Hour<-format(as.POSIXlt(Enerdata2$date2), "%H")
Enerdata2$Min<-format(as.POSIXlt(Enerdata2$date2), "%M")
#Check for stationarity
Y<-Enerdata2$T1
d.Y<-diff(Y)
t<-Enerdata2$Time2
summary(Y)
summary(d.Y)
plot(Y)
plot(d.Y)
# Dickey-Fuller test for variable
adf.test(Y, alternative="stationary", k=0)
adf.test(Y, alternative="explosive", k=0)
adf.test(d.Y, k=0)
adf.test(d.Y)
# ACF and PACF
acf(Y)
pacf(Y)
acf(d.Y)
pacf(d.Y)
# ARIMA on differenced variable
# ARIMA(1,1,0)
arima(d.Y, order = c(1,0,0))
# ARIMA(0,1,1)
arima(d.Y, order = c(0,0,1))
# ARIMA(1,1,1)
arima(d.Y, order = c(1,0,1))
# ARIMA(1,1,3)
arima(d.Y, order = c(1,0,3))
# ARIMA(2,1,3)
arima(d.Y, order = c(2,0,3))
#Definitely First Difference for Temperature. ARIMA(1,1,1) performed best.
Enerdata2$T1N<-c(0,diff(Y))
#Now, check for humidity.
Y2<-Enerdata2$RH_1
d.Y2<-diff(Y2)
t<-Enerdata2$Time2
summary(Y2)
summary(d.Y2)
plot(Y2)
plot(d.Y2)
# Dickey-Fuller test for variable
adf.test(Y2, alternative="stationary", k=0)
adf.test(Y2, alternative="explosive", k=0)
adf.test(d.Y2, k=0)
adf.test(d.Y)
# ACF and PACF
acf(Y2)
pacf(Y2)
acf(d.Y2)
pacf(d.Y)
# ARIMA on differenced variable
# ARIMA(1,1,0)
arima(d.Y2, order = c(1,0,0))
# ARIMA(0,1,1)
arima(d.Y2, order = c(0,0,1))
# ARIMA(1,1,1)
arima(d.Y2, order = c(1,0,1))
# ARIMA(1,1,3)
arima(d.Y2, order = c(1,0,3))
# ARIMA(2,1,3)
arima(d.Y2, order = c(2,0,3))
#Definitely First Difference for Humidity. ARIMA(1,1,3) performed best.
RH_1M<-Enerdata2$RH_1 - mean(Enerdata2$RH_1)#Normalize
RH_1M2 <- ((RH_1M)^2)
cs<- cos(2*pi*RH_1M2) # estimate curve
sn<- sin(2*pi*RH_1M2) # estimate curve
trend <- Enerdata2$Time # time
reg1<- lm(Enerdata2$RH_1 ~ RH_1M + cs + sn, na.action=NULL)
plot(Y2,lines(fitted(reg1)))
plot(residuals(reg1) ~ fitted(reg1))
par(mfrow=c(3,1)) # plot ACFs
acf(Y2, 20, main="RH_1 (Original)")
acf(resid(reg1), 20, main="Detrended")
acf(d.Y2, 20, main="First Difference")
par(mfrow=c(1,1)) # plot ACFs
Enerdata2$RH_1M<-resid(reg1)
Enerdata2$RH_1N<-c(0,diff(Enerdata2$RH_1))
#First Difference of all the inside room temp
Enerdata2$T2N<-c(0,diff(Enerdata2$T2))
Enerdata2$T3N<-c(0,diff(Enerdata2$T3))
Enerdata2$T4N<-c(0,diff(Enerdata2$T4))
Enerdata2$T5N<-c(0,diff(Enerdata2$T5))
Enerdata2$T6N<-c(0,diff(Enerdata2$T6))
Enerdata2$T7N<-c(0,diff(Enerdata2$T7))
Enerdata2$T8N<-c(0,diff(Enerdata2$T8))
Enerdata2$T9N<-c(0,diff(Enerdata2$T9))
#First Difference of all the inside room humidity
Enerdata2$RH_2N<-c(0,diff(Enerdata2$RH_2))
Enerdata2$RH_3N<-c(0,diff(Enerdata2$RH_3))
Enerdata2$RH_4N<-c(0,diff(Enerdata2$RH_4))
Enerdata2$RH_5N<-c(0,diff(Enerdata2$RH_5))
Enerdata2$RH_6N<-c(0,diff(Enerdata2$RH_6))
Enerdata2$RH_7N<-c(0,diff(Enerdata2$RH_7))
Enerdata2$RH_8N<-c(0,diff(Enerdata2$RH_8))
Enerdata2$RH_9N<-c(0,diff(Enerdata2$RH_9))
#Check Outside Temperature, Humidity, Pressure, Windspeed, Visibility, Dewpoint outside for stationarity
Y3<-Enerdata2$Appliances
d.Y3<-diff(Y3)
t<-Enerdata2$Time2
summary(Y3)
summary(d.Y3)
plot(Y3)
plot(d.Y3)
# Dickey-Fuller test for variable
adf.test(Y3, alternative="stationary", k=0)
adf.test(Y3, alternative="explosive", k=0)
adf.test(d.Y3, k=0)
adf.test(d.Y3)
# ACF and PACF
acf(Y3)
pacf(Y3)
acf(d.Y3)
pacf(d.Y3)
#Definitely First Difference for RH_Out, T_Out, Pressure, Windspeed, Visibility, Tdewpoint, and Appliances
Enerdata2$T_Nout<-c(0,diff(Enerdata2$T_out))
Enerdata2$RH_Nout<-c(0,diff(Enerdata2$RH_out))
Enerdata2$Press_Nout<-c(0,diff(Enerdata2$Press_mm_hg))
Enerdata2$Wind_Nout<-c(0,diff(Enerdata2$Windspeed))
Enerdata2$Vis_Nout<-c(0,diff(Enerdata2$Visibility))
Enerdata2$Dew_Nout<-c(0,diff(Enerdata2$Tdewpoint))
#Another Vector for total Energy consumption
Enerdata2$Tot_KWH<- Enerdata2$lights + Enerdata2$Appliances
#First Difference of Appliance because Fridge, AC, and other continously running appliance may set the baseline
Enerdata2$Tot_NKWH<-abs(c(0,diff(Enerdata2$Tot_KWH)))
#Subtract first row before regression
Enerdata4<-Enerdata2[-1,]
attach(Enerdata4)
View(Enerdata4)
write.csv((Enerdata4), file = 'Energydata4.csv')
