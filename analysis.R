#Loading packages
library(readxl)
library(ggplot2)
library(dplyr)

#Importing data
setwd("~/Processos Seletivos/Voxy")
dados_voxy <- read_xlsx("Data_Processing_Intern_Activity.xlsx", sheet="Data")

#Correcting data types
dados_voxy$total_minutes_studied <- as.numeric(dados_voxy$total_minutes_studied)
dados_voxy$total_minutes_in_lessons <- as.numeric(dados_voxy$total_minutes_in_lessons)
dados_voxy$total_minutes_in_word_bank <- as.numeric(dados_voxy$total_minutes_in_word_bank)
dados_voxy$total_minutes_in_grammar_guide <- as.numeric(dados_voxy$total_minutes_in_grammar_guide)

#Group Sizes
ggplot(dados_voxy)+
  geom_bar(aes(group_name), fill="chocolate")+
  theme_light()+
  labs(x="Group Name", y="Number of Students", title="Group Sizes", subtitle="Voxy 2021")

#Níveis
dados_voxy$current_level_name <- factor(dados_voxy$current_level_name, levels=c("Beginner", "High Beginner", "Low Intermediate", "Intermediate", "High Intermediate", "Low Advanced"), ordered=TRUE)
ggplot(dados_voxy%>%filter(!is.na(current_level_name)))+
  geom_bar(aes(current_level_name), fill="chocolate1")+
  theme_light()+
  labs(x="Current Level", y="Number of Students", title="English Level", subtitle="Voxy 2021")
#NA's:
sum(is.na(dados_voxy$current_level_name))

#Níveis por grupo
ggplot(dados_voxy%>%filter(!is.na(current_level_name)))+
  geom_bar(aes(current_level_name), fill="chocolate2")+
  facet_wrap(~group_name)+
  theme_light()+
  labs(x="Current Level", y="Number of Students", title="Level Distribution per Group", subtitle="Voxy 2021")+
  theme(axis.text.x=element_text(angle=10))

#Days without login
dados_voxy <- dados_voxy%>%
  mutate(days_without_activity=round(as.POSIXct("2022-01-04", format="%Y-%m-%d")-last_login_date))
ggplot(dados_voxy)+
  geom_histogram(aes(days_without_activity), bins=20, fill="chocolate3")+
  theme_light()+
  labs(x="Days Without Login", y="Number of Students", title="Distribution of Days Without Login", subtitle="Voxy 2021")

dados_voxy%>%
  summarize(Mean=mean(days_without_activity, na.rm=TRUE), Min=min(days_without_activity, na.rm=TRUE), Q1=quantile(days_without_activity, 0.25, na.rm=TRUE),
                                   Median=median(days_without_activity, na.rm=TRUE), Q3=quantile(days_without_activity, 0.75, na.rm=TRUE), Max=max(days_without_activity, na.rm=TRUE))
#Total activities (causes)
ggplot(dados_voxy)+
  geom_jitter(aes(total_minutes_studied, total_activities_completed), alpha=0.5, color="chocolate4")+
  theme_light()+
  labs(x="Minutes Spent Studying", y="Number of Activities", title="Activities Completed vs Study Time", subtitle="Voxy 2021")
mod1 <- lm(total_activities_completed~total_minutes_studied, data=dados_voxy)
coef(mod1)

ggplot(dados_voxy)+
  geom_jitter(aes(total_minutes_in_lessons, total_activities_completed), alpha=0.5, color="deepskyblue")+
  theme_light()+
  labs(x="Minutes Spent in Lessons", y="Number of Activities", title="Activities Completed vs Time in Lessons", subtitle="Voxy 2021")
mod2 <- lm(total_activities_completed~total_minutes_in_lessons, data=dados_voxy)
coef(mod2)

ggplot(dados_voxy)+
  geom_jitter(aes(total_minutes_in_word_bank, total_activities_completed), alpha=0.5, color="deepskyblue1")+
  theme_light()+
  labs(x="Minutes Spent in Word Bank", y="Number of Activities", title="Activities Completed vs Time in Word Bank", subtitle="Voxy 2021")
mod3 <- lm(total_activities_completed~total_minutes_in_word_bank, data=dados_voxy)
coef(mod3)

ggplot(dados_voxy)+
  geom_jitter(aes(total_minutes_in_grammar_guide, total_activities_completed), alpha=0.5, color="deepskyblue2")+
  theme_light()+
  labs(x="Minutes Spent in Word Bank", y="Number of Activities", title="Activities Completed vs Time in Grammar Guide", subtitle="Voxy 2021")
mod4 <- lm(total_activities_completed~total_minutes_in_grammar_guide, data=dados_voxy)
coef(mod4)

#Scores
ggplot(dados_voxy)+
  geom_histogram(aes(last_vpa_score_total_adjusted), fill="deepskyblue3")+
  theme_light()+
  labs(x="Last VPA Score", y="Number of Students", title="Distribution of Scores in the Voxy Proficiency Assessment (VPA)", subtitle="Voxy 2021")
dados_voxy%>%
  summarize(Mean=mean(last_vpa_score_total_adjusted, na.rm=TRUE), Min=min(last_vpa_score_total_adjusted, na.rm=TRUE), Q1=quantile(last_vpa_score_total_adjusted, 0.25, na.rm=TRUE),
            Median=median(last_vpa_score_total_adjusted, na.rm=TRUE), Q3=quantile(last_vpa_score_total_adjusted, 0.75, na.rm=TRUE), Max=max(last_vpa_score_total_adjusted, na.rm=TRUE))

#Scores per level
ggplot(dados_voxy%>%filter(!is.na(current_level_name)))+
  geom_histogram(aes(last_vpa_score_total_adjusted), fill="deepskyblue4")+
  facet_wrap(~current_level_name)+
  theme_light()+
  labs(x="Last VPA Score", y="Number of Students", title="Distribution of VPA Scores per English Level", subtitle="Voxy 2021")
