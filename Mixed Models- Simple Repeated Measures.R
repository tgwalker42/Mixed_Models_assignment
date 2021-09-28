install.packages("devtools")
devtools::install_github("easystats/easystats")

install.packages('Rcpp')
library(Rcpp)
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(performance)
library(ggplot2)


bat_data<-read.csv("BatsInNoiseTW.csv")

bat_data<-na.omit(bat_data)

View(bat_data)

bat_data$Bat

lmeModel<-lmer(Search_time ~ Treatment*Night + (1|Bat), data=bat_data)
anova(lmeModel)


Bat<- as.character(bat_data$Bat)

Bat_sound.plot1<- ggplot(bat_data, aes(x=Treatment, y=Search_time, group=Bat, color=Bat)) + 
  geom_point(size=4, position = position_dodge2(width=.33, preserve = "total")) +
  #geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title="Bat Search Time with Varying Sound Treatments", x= "Treatment", y = "Search Time")
Bat_sound.plot1
##############################################
search_means <- bat_data %>%
  group_by(Treatment) %>%
  summarise(mean_search_time=mean(Search_time),
            se_search=sd(Search_time)/sqrt(n()))
search_means

mixed_bat_searchtime <- lmer(Search_time~(Treatment*Bat)+(1|Bat), data = bat_data)
anova(mixed_bat_searchtime)

summary(mixed_bat_searchtime)

performance::check_model(mixed_bat_searchtime)


mixed_bat_searchtime_emm <- emmeans(mixed_bat_searchtime, "Treatment")

mixed_bat_searchtime_emm

###################################

emmeans(mixed_bat_searchtime, "Bat")

emmeans(mixed_bat_searchtime, "Treatment","Bat")

emmeans(mixed_bat_searchtime, "Bat", "Treatment")

###########################Raw Means Vs. model Adjusted Means

bat_data_emm <- as.data.frame(mixed_bat_searchtime_emm)
bat_data_emm
search_means

Bat_data_plot2<- ggplot(bat_data_emm, aes(x=Treatment, y=emmean)) + 
  geom_point(size=4) +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=.2) +
  labs(title="Bat Data Comparing Raw Vs. Model Adjusted Means", x= NULL, y = "Search Time") +
  geom_point(data=search_means, size=4, x=search_means$Treatment, y=search_means$mean_search_time, color="blue") +
  theme(axis.text.x = ggtext::element_markdown(color = "blue", size = 12)) +
  scale_x_discrete(name = NULL, labels = labels)
Bat_data_plot2



pairs(emmeans(mixed_bat_searchtime, "Treatment"))
pairs(emmeans(mixed_bat_searchtime, "Bat"))


emmeans(mixed_bat_searchtime, specs = pairwise ~ Treatment:Bat)




############################PART 2 Nested Design

Alligator_weight<- read.csv("Alligator.csv")

Alligator.plot<-ggplot(Alligator_weight, aes(Beach, Body_weight, colour = as.factor(Male), shape=as.factor(measures))) + 
  geom_jitter(width =0.15, size=5) +
  annotate("text", x = 2, y = 82.5, label = "13 Males") +
  annotate("text", x = 2, y = 80, label = "3 measures per Male") +
  annotate("text", x = 2, y = 77.5, label = "39 total measurements", size=5, color="blue")

Alligator.plot

alligators.1 <- lmer(Body_weight ~ Beach + (1|Beach/Male), data = Alligator_weight )
summary(alligators.1)

anova(alligators.1)


alligators.2 <- lmer(Body_weight ~ (1|Beach/Male), data = Alligator_weight)
anova(alligators.2)
summary(alligators.2)


anova(alligators.2,alligators.1, test = "Chisq")
