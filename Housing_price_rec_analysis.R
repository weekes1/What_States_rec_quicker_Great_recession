library(mosaic)
library(tidyverse)
library(ggplot2)
install.packages('writexl')
library(writexl)
library(readr)
##########Checking HPI Data Frame clean##################
library(readr)
HPI_state <- read_csv("Economics with GIS/Inflation project/HPI_state.csv")
View(HPI_state)

head(HPI_state)

HPI_state<-HPI_state%>%
  select(State,Abbreviation,Year,Annual_Change,HPI)%>%#HPI with 1975 base year. Data from 2000-2021
  filter(Year>=2000)%>%
  mutate(Abbreviation=as.factor(Abbreviation),
         Annual_Change=as.numeric(Annual_Change))

head(HPI_state)
print(n=22,HPI_state)
#Data frame for house prices cleaned. 
Annual_change_by_year<-favstats(Annual_Change~Year,data=HPI_state)
Annual_change_by_year<-Annual_change_by_year%>%
select(Year,mean,n)
Annual_change_by_year

write_csv(Annual_change_by_year,"C:\\Users\\weeke\\OneDrive\\Documents\\Economics with GIS\\Inflation project\\Annual_change_by_yr.csv")


Annual_change_by_state<-favstats(Annual_Change~Abbreviation,data=HPI_state)
Annual_change_by_state<-Annual_change_by_state%>%
  select(Abbreviation,mean,n)
Annual_change_by_state

write_csv(Annual_change_by_state,"C:\\Users\\weeke\\OneDrive\\Documents\\Economics with GIS\\Inflation project\\Annual_change_by_st.csv")


Price_change_plot<-ggplot(HPI_state,aes(Year, Annual_Change,color=Abbreviation))+
                            geom_line()+
  labs(title = "Annual Change in Housing Prices for each State",
       subtitle = "Reference for House Price Over Time",
       caption = "Data source: Federal Housing Finance Agency (HPI for states (All-Transactions Index))",
       x = "Year", y = "Percent Change in Housing Price",
       tag = "Fig 1")
                        
Price_change_plot

HPI_state<-HPI_state%>%
  mutate(Year_char=as.character(Year))

HPI_state<-HPI_state%>%
  mutate(State_Year=paste(State,Year_char,sep=""))

head(HPI_state)
#############Medium Income per State Data Frame clean############################

library(readr)
Med_income_state <- read_csv("Economics with GIS/Inflation project/Med_income_state.csv")
View(Med_income_state)

head(Med_income_state)
Med_income_state<-Med_income_state%>%
  rename(State = ...1)

Med_income_state<-Med_income_state%>%
pivot_longer(!State, names_to = "Year", values_to = "Income")
 
Med_income_state<-Med_income_state%>%
  mutate(Year_num=as.numeric(Year))%>%
  filter(Year_num>=2000)

print(n=22,Med_income_state)

Med_income_state<-Med_income_state%>%
   mutate(State_Year=paste(State,Year,sep=""))%>%
filter(State!="United States")
  
print(n=22,Med_income_state)

Income_mean_by_state<-favstats(Income~State,data = Med_income_state)

Income_mean_by_state<-Income_mean_by_state%>%
  select(State,mean,n)

Income_mean_by_state

write_csv(Income_mean_by_state,"C:\\Users\\weeke\\OneDrive\\Documents\\Economics with GIS\\Inflation project\\Income_mean_by_state.csv")

favstats(Income~Year,data = Med_income_state)
Income_mean_by_year<-favstats(Income~Year,data = Med_income_state)

Income_mean_by_year<-Income_mean_by_year%>%
  select(Year,mean, Q1,Q3)%>%
  filter(Year>=2000)

Income_mean_by_year

write_csv(Income_mean_by_year,"C:\\Users\\weeke\\OneDrive\\Documents\\Economics with GIS\\Inflation project\\Income_mean_by_year.csv")
###################################Population Density By State######################

library(readr)
population_density<- read_csv("Economics with GIS/Inflation project/population_density_by_state.csv")
View(population_density)

head(population_density)

population_density<-population_density%>%
  select(Name, Year, 'Resident Population Density')%>%
  rename(Pop_density='Resident Population Density')

population_density<-population_density%>%
  filter(Year==2010)%>%
  rename(State=Name)

print(n=57,population_density)

write_csv(population_density,"C:\\Users\\weeke\\OneDrive\\Documents\\Economics with GIS\\Inflation project\\population_densities.csv")

density_stats<-fav_stats(population_density$Pop_density)

density_stats<-density_stats%>%
  select(Q1,median,Q3,mean)

density_stats

write_csv(density_stats,"C:\\Users\\weeke\\OneDrive\\Documents\\Economics with GIS\\Inflation project\\Density_stats.csv")
################################################Joining Data Frames#######################################


Price_and_Income<-inner_join(HPI_state,Med_income_state, by = "State_Year")
Price_and_Income

Price_and_Income<-Price_and_Income%>%
  select(State.x,Abbreviation,Year.x,Annual_Change,HPI,Income)

Price_and_Income<-Price_and_Income%>%
  rename(State=State.x,Year=Year.x,Annual_Change_HPI=Annual_Change)
print(n=22,Price_and_Income)

Final_Data<-left_join(Price_and_Income,population_density, by = "State")
Final_Data


Final_Data<-Final_Data%>%
  select(!Year.y)

Final_Data<-Final_Data%>%
  rename(Year=Year.x)

df_for_drops<-Final_Data%>%
  select(State,Year,Annual_Change_HPI)%>%
  filter(Year==2005|Year==2008)%>%
  mutate(Drop_05_08= 0)%>%
  mutate(Drop_05_08=c(0,diff(Annual_Change_HPI)))%>%
  filter(Year==2008)%>%
  select(State,Drop_05_08)

df_for_drops

Final_Data<-left_join(Final_Data,df_for_drops, by = "State")

Final_Data
print(n=22,Final_Data)


#adding Hack

Hachman_index <- read_csv("Economics with GIS/Inflation project/Hachman_index.csv")
head(Hachman_index)

Hachman_index<-Hachman_index%>%
 rename( Hachman_Score=`Hachman Index`)
Hachman_index

Final_Data<-left_join(Final_Data,Hachman_index, by = "State")

print(n=22,Final_Data)

write_csv(Final_Data,"C:\\Users\\weeke\\OneDrive\\Documents\\Economics with GIS\\Inflation project\\Final_data.csv")

####################################################ANOVA Analysis###################################################
downswings<-c()

for( i in 1:1071){
  if(Final_Data$Annual_Change_HPI[i]<0){
    downswings[i]=Final_Data$Annual_Change_HPI[i]
  }
  else{
    downswings[i]=0
  }
}
downswings

recovery_changes<-c()

for( i in 1:1071){
  if(Final_Data$Year[i]>=2009 & Final_Data$Year[i]<2017){
    recovery_changes[i]=Final_Data$Annual_Change_HPI[i]
  }
  else{
    recovery_changes[i]=0
  }
}

recovery_changes


ANOVA_df<-Final_Data%>%
  mutate(Recovery_swings=recovery_changes, 
         Down_swings = downswings)
  
ANOVA_df_<-ANOVA_df%>%
  filter(Recovery_swings!=0)

ANOVA_df_

nrow(ANOVA_df_)
income_level<-c()

for( i in 1:408){
  
  if(ANOVA_df_$Year[i]==2009 & ANOVA_df_$Income[i]< 44625.5){
    income_level[i]="Low_I"
  }
  
  if(ANOVA_df_$Year[i]==2009 & ANOVA_df_$Income[i]>=44625.5 & ANOVA_df_$Income[i]<55571.0){
    income_level[i]="Mid_I"
  }
  
  if(ANOVA_df_$Year[i]==2009 & ANOVA_df_$Income[i]>=55571.0){
    income_level[i]="High_I"
  }
  
  if(ANOVA_df_$Year[i]==2010 & ANOVA_df_$Income[i]< 45155.5){
    income_level[i]="Low_I"
  }
  
  if(ANOVA_df_$Year[i]==2010 & ANOVA_df_$Income[i]>=45155.5 & ANOVA_df_$Income[i]<55554.5){
    income_level[i]="Mid_I"
  }
  
  if(ANOVA_df_$Year[i]==2010 & ANOVA_df_$Income[i]>=55554.5){
    income_level[i]="High_I"
  }
  if(ANOVA_df_$Year[i]==2011 & ANOVA_df_$Income[i]< 46114.5){
    income_level[i]="Low_I"
  }
  
  if(ANOVA_df_$Year[i]==2011 & ANOVA_df_$Income[i]>=46114.5 & ANOVA_df_$Income[i]<57137.5){
    income_level[i]="Mid_I"
  }
  
  if(ANOVA_df_$Year[i]==2011 & ANOVA_df_$Income[i]>=57137.5){
    income_level[i]="High_I"
  }
  
  if(ANOVA_df_$Year[i]==2012 & ANOVA_df_$Income[i]< 45883.5){
    income_level[i]="Low_I"
  }
  
  if(ANOVA_df_$Year[i]==2012 & ANOVA_df_$Income[i]>=45883.5 & ANOVA_df_$Income[i]<58959.0){
    income_level[i]="Mid_I"
  }
  
  if(ANOVA_df_$Year[i]==2012 & ANOVA_df_$Income[i]>=58959.0){
    income_level[i]="High_I"
  }
  
  if(ANOVA_df_$Year[i]==2013 & ANOVA_df_$Income[i]< 47156.0){
    income_level[i]="Low_I"
  }
  
  if(ANOVA_df_$Year[i]==2013 & ANOVA_df_$Income[i]>= 47156.0 & ANOVA_df_$Income[i]<61788.0){
    income_level[i]="Mid_I"
  }
  
  if(ANOVA_df_$Year[i]==2013 & ANOVA_df_$Income[i]>=61788.0){
    income_level[i]="High_I"
  }
  
  if(ANOVA_df_$Year[i]==2014 & ANOVA_df_$Income[i]< 48657.0){
    income_level[i]="Low_I"
  }
  
  if(ANOVA_df_$Year[i]==2014 & ANOVA_df_$Income[i]>= 48657.0 & ANOVA_df_$Income[i]<60719.0){
    income_level[i]="Mid_I"
  }
  
  if(ANOVA_df_$Year[i]==2014 & ANOVA_df_$Income[i]>=60719.0){
    income_level[i]="High_I"
  }
  
  if(ANOVA_df_$Year[i]==2015 & ANOVA_df_$Income[i]< 50782.5){
    income_level[i]="Low_I"
  }
  
  if(ANOVA_df_$Year[i]==2015 & ANOVA_df_$Income[i]>=50782.5 & ANOVA_df_$Income[i]<62561.0){
    income_level[i]="Mid_I"
  }
  
  if(ANOVA_df_$Year[i]==2015 & ANOVA_df_$Income[i]>=62561.0){
    income_level[i]="High_I"
  }
  
  if(ANOVA_df_$Year[i]==2016 & ANOVA_df_$Income[i]<53874.5){
    income_level[i]="Low_I"
  }
  
  if(ANOVA_df_$Year[i]==2016 & ANOVA_df_$Income[i]>=53874.5 & ANOVA_df_$Income[i]<66544.0){
    income_level[i]="Mid_I"
  }
  
  if(ANOVA_df_$Year[i]==2016 & ANOVA_df_$Income[i]>=66544.0){
    income_level[i]="High_I"
  }
  
}

density_level<-c()
for( i in 1:408){
  if(ANOVA_df_$Pop_density[i]<48.5){
    density_level[i]="Low_D"
  }
  
  if(ANOVA_df_$Pop_density[i]>=48.5 & ANOVA_df_$Pop_density[i] <231.1){
    density_level[i]="Mid_D"
  }
  
  if(ANOVA_df_$Pop_density[i]>=231.1){
    density_level[i]="High_D"
  }
  
}

income_level

ANOVA_df_<-ANOVA_df_%>%
  mutate(Income_level=as.factor(income_level),
         Density_level=as.factor(density_level))

ANOVA_df_


write_csv(ANOVA_df_,"C:\\Users\\weeke\\OneDrive\\Documents\\Economics with GIS\\Inflation project\\Final_workable_data.csv")

interaction.plot(ANOVA_df_$Density_level,ANOVA_df_$Income_level,ANOVA_df_$Recovery_swings)
Recovery_mod = aov(Recovery_swings~Income_level*Density_level+Year, data=ANOVA_df_)
ANOVA_for_recovery_swings<-anova(Recovery_mod)
ANOVA_for_recovery_swings
plot(Recovery_mod,1:2)

write_csv(ANOVA_for_recovery_swings,"C:\\Users\\weeke\\OneDrive\\Documents\\Economics with GIS\\Inflation project\\ANOVA_table.csv")


Recovery_mod_log = aov(Recovery_swings~exp(Income_level)*exp(Density_level)+exp(Year), data=ANOVA_df_)
ANOVA_for_recovery_swings_log<-anova(Recovery_mod_log)
ANOVA_for_recovery_swings_log
plot(Recovery_mod_log,1:2)

write_csv(ANOVA_for_recovery_swings_log,"C:\\Users\\weeke\\OneDrive\\Documents\\Economics with GIS\\Inflation project\\ANOVA_table_log.csv")


install.packages("foreign")
library(foreign)
int.trt <- with(ANOVA_df_, interaction(Income_level, Density_level))
amod <- aov(Recovery_swings ~ int.trt, data=ANOVA_df_)
library(agricolae)
(a<-LSD.test(amod, "int.trt", group=TRUE))
plot(a)

log_amod <- aov(log(Recovery_swings) ~ int.trt, data=ANOVA_df_)
(b<-LSD.test(log_amod, "int.trt", group=TRUE))
plot(b)

ANOVA_df_
States_and_their_categories<-ANOVA_df_%>%
  #group_by(State)%>%
  group_by(Density_level,Income_level)%>%
  summarize(State=State)%>%
  unique()


print(n=66,States_and_their_categories)

write_csv(States_and_their_categories,"C:\\Users\\weeke\\OneDrive\\Documents\\Economics with GIS\\Inflation project\\States_and_Categories.csv")



###########################################################Regression#########################################
head(ANOVA_df_)

Recovery_reg_levels = lm(Recovery_swings~Income_level*Density_level+Year+Drop_05_08+Hachman_Score, data=ANOVA_df_)
summary(Recovery_reg_levels)
plot(Recovery_reg_levels,1:2)

dfl<- coef(summary(Recovery_reg_levels))
dfl
write.csv(dfl, "C:\\Users\\weeke\\OneDrive\\Documents\\Economics with GIS\\Inflation project\\regression_levels.csv")

Recovery_reg_levels_log = lm(log(Recovery_swings)~Income_level*Density_level+Year, data=ANOVA_df_)
summary(Recovery_reg_levels_log)
plot(Recovery_reg_levels_log,1:2)

Recovery_reg = lm(Recovery_swings~Income*Pop_density+Year+Drop_05_08+Hachman_Score, data=ANOVA_df_)
summary(Recovery_reg)
plot(Recovery_reg,1:2)

df<- coef(summary(Recovery_reg))
df
write.csv(df, "C:\\Users\\weeke\\OneDrive\\Documents\\Economics with GIS\\Inflation project\\regression.csv")

head(ANOVA_df_)
Resilience_reg = lm(Recovery_swings~Income*Pop_density+Year+Drop_05_08+Hachman_Score, data=ANOVA_df_)
summary(Resilience_reg)
plot(Resilience_reg,1:2)

df3<- coef(summary(Recovery_reg))
df
write.csv(df3, "C:\\Users\\weeke\\OneDrive\\Documents\\Economics with GIS\\Inflation project\\resilience_regression.csv")

Recovery_reg_log = lm(log(Recovery_swings)~Income*Pop_density+Year, data=ANOVA_df_)
summary(Recovery_reg_log)
plot(Recovery_reg_log,1:2)



#########################################################different years for symbology#########################################

ANOVA_df_wide<-ANOVA_df_%>%
  select(State,Year,Recovery_swings)%>%
  pivot_wider(names_from = Year,values_from = Recovery_swings)
 

ANOVA_df_wide

write_csv(ANOVA_df_wide,"C:\\Users\\weeke\\OneDrive\\Documents\\Economics with GIS\\Inflation project\\ANOVA_df_wide.csv")
