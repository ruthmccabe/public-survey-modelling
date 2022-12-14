## Trust in government advice

## Questions covered:
## How much did you trust government advice regarding public health issues? (prior and during)
## How do you feel when government advice changes based on new scientific evidence?

library(readxl)
library(tidyverse)

prolific <- read_excel("Data/responses_prolific.xlsx") %>% data.frame() %>%
  select(-c(Start.time,Completion.time,Email,consent_1,consent_2))

twitter <- read_excel("Data/responses_twitter.xlsx") %>% data.frame() %>%
  select(-c(Start.time,Completion.time,Email,consent_1,consent_2))

all_responses <- rbind(prolific,twitter) %>% group_by(platform) %>%
  mutate("platform_label"=ifelse(platform=="twitter","Twitter","Prolific Academic"),
         #"platform_sample_size"=length(platform),
         "unique_ID"=paste0(platform,"_",ID)) %>% #data.frame() %>%
  filter(!is.na(gender),!is.na(age_group),!is.na(sector),!is.na(vaccinated)) %>%
  group_by(platform) %>%
  mutate(platform_sample_size=length(platform)) %>% data.frame()


## Table 1

table(all_responses$platform_label,all_responses$prior_government_trust)
prop.table(table(all_responses$platform_label,all_responses$prior_government_trust),1)*100

table(all_responses$platform_label,all_responses$during_government_trust)
prop.table(table(all_responses$platform_label,all_responses$during_government_trust),1)*100


## Supplementary Figure 14

government_trust_melt <- rbind(all_responses %>%
                                 dplyr::select(platform_label,platform_sample_size,
                                               prior_government_trust,
                                               age_group,gender,sector,vaccinated) %>%
                                 dplyr::rename(government_trust = prior_government_trust) %>%
                                 mutate(time = "Prior to the COVID-19 pandemic"),
                               all_responses %>%
                                 dplyr::select(platform_label,platform_sample_size,
                                               during_government_trust,
                                               age_group,gender,sector,vaccinated) %>%
                                 dplyr::rename(government_trust = during_government_trust) %>%
                                 mutate(time = "During the COVID-19 pandemic"))

government_trust_melt_summary <- government_trust_melt %>% group_by(platform_label,time,government_trust) %>%
  summarise(count = length(government_trust)) %>%
  mutate(platform_sample_size = ifelse(platform_label=="Prolific Academic",504,202),
         percentage = count/platform_sample_size *100,
         government_trust = ifelse(is.na(government_trust),"Did not answer",government_trust),
         government_trust = factor(government_trust,
                                   levels=c("No trust whatsoever","Moderate level of trust",
                                            "High level of trust","Did not answer")))

ggplot(government_trust_melt_summary,
       aes(x=time,fill=government_trust))+
  geom_col(aes(y=percentage))+
  theme_bw()+
  facet_wrap(~platform_label)+
  scale_fill_viridis_d(option="magma",end=0.6)+
  scale_size_area(max_size = 20)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12))+
  labs(x="Time period",
       y="Percentage of responses (%)",
       fill="How much did you \ntrust government advice \nregarding public health \nissues?")+
  theme(strip.background = element_rect(fill = "white", color = "black"))+
  guides(color=guide_legend(order=1),fill=guide_legend(order=1))
ggsave("Plots/SupFig14.png",width=7,height=5)


## Supplementary Figure 15
government_trust_reliability <- rbind(all_responses %>%
                                        dplyr::select(platform_label,
                                                      prior_government_trust,prior_reliability_1_10) %>%
                                        rename(government_trust = prior_government_trust,
                                               reliability_1_10 = prior_reliability_1_10) %>%
                                        mutate(time = "Prior to the COVID-19 pandemic"),
                                      all_responses %>%
                                        dplyr::select(platform_label,
                                                      during_government_trust,during_reliability_1_10) %>%
                                        rename(government_trust = during_government_trust,
                                               reliability_1_10 = during_reliability_1_10) %>%
                                        mutate(time = "During the COVID-19 pandemic")) %>%
  mutate(time = factor(time,
                       levels=c("Prior to the COVID-19 pandemic","During the COVID-19 pandemic")),
         government_trust = factor(government_trust,
                                   levels=c("No trust whatsoever","Moderate level of trust",
                                            "High level of trust")))

ggplot(government_trust_reliability %>% filter(!is.na(reliability_1_10),
                                               !is.na(government_trust)),
       aes(x=time,y=reliability_1_10,col=time))+
  geom_jitter(shape=1)+
  geom_boxplot(lwd=1.5,outlier.shape = NA,fill=NA)+
  theme_bw()+
  facet_grid(platform_label~government_trust)+
  theme(strip.background = element_rect(fill="white"),
        legend.position="bottom")+
  scale_colour_manual(values=c("#a8c66c","#1b6535"))+
  scale_y_continuous(n.breaks=10) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  labs(x="Time period",
       y="On a scale of 1-10 with 1 being “extremely unreliable” \nand 10 being “extremely reliable” how do you feel \nabout the use of transmission models in informing \npublic health policy?",col="")
ggsave("Plots/SupFig15.png",width=7,height=5)


## Supplementary Table 10

government_trust_melt <- government_trust_melt %>%
  mutate(government_trust_numeric = ifelse(government_trust=="High level of trust",3,
                                           ifelse(government_trust=="Moderate level of trust",2,
                                                  ifelse(government_trust=="No trust whatsoever",1,NA))))

m1 <- glm(government_trust_numeric~age_group+gender,
          data=government_trust_melt %>% filter(platform_label=="Prolific Academic",
                                                time=="Prior to the COVID-19 pandemic"),
          family="poisson")
summary(m1)

m2 <- glm(government_trust_numeric~age_group+gender,
          data=government_trust_melt %>% filter(platform_label=="Twitter",
                                                time=="Prior to the COVID-19 pandemic"),
          family="poisson")
summary(m2)

m3 <- glm(government_trust_numeric~age_group+gender,
          data=government_trust_melt %>% filter(platform_label=="Prolific Academic",
                                                time=="During the COVID-19 pandemic"),
          family="poisson")
summary(m3)

m4 <- glm(government_trust_numeric~age_group+gender,
          data=government_trust_melt %>% filter(platform_label=="Twitter",
                                                time=="During the COVID-19 pandemic"),
          family="poisson")
summary(m4)



## how many people changed their opinion?
changing_opinion <- all_responses %>% select(platform_label,prior_government_trust,during_government_trust) %>%
  mutate(change = ifelse(prior_government_trust==during_government_trust,FALSE,TRUE))

table(changing_opinion$platform_label,changing_opinion$change)


## Supplementary Table 11
government_trust_awareness <- rbind(all_responses %>%
                                      dplyr::select(platform_label,
                                                    prior_government_trust,prior_awareness_use_in_policy) %>%
                                      rename(government_trust = prior_government_trust,
                                             awareness_use_in_policy = prior_awareness_use_in_policy) %>%
                                      mutate(time = "Prior to the COVID-19 pandemic"),
                                    all_responses %>%
                                      dplyr::select(platform_label,
                                                    during_government_trust,during_awareness_use_in_policy) %>%
                                      rename(government_trust =during_government_trust,
                                             awareness_use_in_policy = during_awareness_use_in_policy) %>%
                                      mutate(time = "During the COVID-19 pandemic"))

government_trust_awareness_summary <- government_trust_awareness %>%
  group_by(platform_label,time,awareness_use_in_policy,government_trust) %>%
  summarise(count = length(platform_label)) %>%
  ungroup() %>% group_by(platform_label,time,awareness_use_in_policy) %>%
  mutate(total = sum(count),
         perc = count/total *100) %>%
  mutate(time= factor(time,
                      levels=c("Prior to the COVID-19 pandemic","During the COVID-19 pandemic")),
         awareness_use_in_policy_label = factor(ifelse(awareness_use_in_policy=="Yes","Aware",
                                                       ifelse(awareness_use_in_policy=="Unsure","Unsure",
                                                              ifelse(awareness_use_in_policy=="No","Not aware",NA))),
                                                levels=c("Not aware","Unsure","Aware")),
         government_trust = ifelse(is.na(government_trust),"Did not answer",government_trust),
         government_trust = factor(government_trust,
                                   levels=c("No trust whatsoever","Moderate level of trust",
                                            "High level of trust","Did not answer")))

## Supplementary Figure 16
ggplot(government_trust_awareness_summary %>% filter(!is.na(awareness_use_in_policy)),
       aes(x=time,y=perc,fill=government_trust))+
  geom_col()+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"))+
  facet_grid(platform_label~awareness_use_in_policy_label)+
  scale_fill_viridis_d(option="magma",end=0.6)+
  scale_size_area(max_size = 20)+
  labs(x="Time",y="Percentage of respondents (%)",
       fill="How much did you \ntrust government \nadvice regarding \npublic health issues?")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
ggsave("Plots/SupFig16.png",width=7,height=5)


## Table 1

table(all_responses$platform_label,all_responses$trust_changing_advice)
prop.table(table(all_responses$platform_label,all_responses$trust_changing_advice),1)*100

## Supplementary Figure 17
trust_changing_advice_summary <- all_responses %>%
  group_by(platform_label,trust_changing_advice) %>%
  summarise(count = length(trust_changing_advice)) %>%
  mutate(trust_changing_advice = ifelse(is.na(trust_changing_advice),"Did not answer",trust_changing_advice),
         total = ifelse(platform_label=="Prolific Academic",504,202),
         trust_changing_advice = factor(trust_changing_advice,
                                        levels=c("I have less trust in the advice.",
                                                 "My level of trust remains unchanged.",
                                                 "I have more trust in the advice.",
                                                 "Did not answer")),
         percentage = count/total *100)

ggplot(trust_changing_advice_summary,
       aes(x=platform_label,fill=trust_changing_advice))+
  geom_col(aes(y=percentage))+
  theme_bw()+
  scale_fill_viridis_d(option="magma",end=0.6)+
  scale_size_area(max_size = 20)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12))+
  labs(x="Sample",
       y="Percentage of responses (%)",
       fill="How do you feel when government \nadvice changes based on new \nscientific evidence?")+
  theme(strip.background = element_rect(fill = "white", color = "black"))+
  guides(color=guide_legend(order=1),fill=guide_legend(order=1))
ggsave("Plots/SupFig17.png",width=5,height=4)


## Supplementary Table 12
trust_changing_advice <- all_responses %>% dplyr::select(platform_label,
                                                         trust_changing_advice,
                                                         age_group,gender,sector,vaccinated) %>%
  mutate(trust_changing_advice_numeric = ifelse(trust_changing_advice=="I have more trust in the advice.",3,
                                                ifelse(trust_changing_advice=="My level of trust remains unchanged.",
                                                       2,
                                                       ifelse(trust_changing_advice=="I have less trust in the advice.",
                                                              1,NA))))

m5 <- glm(trust_changing_advice_numeric~age_group+gender,
          data=trust_changing_advice %>% filter(platform_label=="Prolific Academic"),
          family="poisson")
summary(m5)

m6 <- glm(trust_changing_advice_numeric~age_group+gender,
          data=trust_changing_advice %>% filter(platform_label=="Twitter"),
          family="poisson")
summary(m6)


## Supplementary Figure 18
trust_df <- rbind(all_responses %>% dplyr::select(platform_label,prior_government_trust,trust_changing_advice) %>%
                    mutate(time="Prior to the COVID-19 pandemic") %>%
                    rename(government_trust = prior_government_trust),
                  all_responses %>% dplyr::select(platform_label,during_government_trust,trust_changing_advice) %>%
                    mutate(time="During the COVID-19 pandemic") %>%
                    rename(government_trust = during_government_trust))

trust_df_summary <- trust_df %>% group_by(platform_label,time,government_trust,
                                          trust_changing_advice) %>%
  summarise(count = length(trust_changing_advice)) %>%
  mutate(trust_changing_advice = ifelse(is.na(trust_changing_advice),
                                        "Did not answer",trust_changing_advice),
         government_trust = ifelse(is.na(government_trust),
                                   "Did not answer",government_trust)) %>%
  group_by(platform_label,time,government_trust) %>%
  mutate(total = sum(count),
         percentage = count/total *100) %>%
  mutate(trust_changing_advice = factor(trust_changing_advice,
                                        levels=c("I have less trust in the advice.",
                                                 "My level of trust remains unchanged.",
                                                 "I have more trust in the advice.",
                                                 "Did not answer")),
         government_trust = factor(government_trust,
                                   levels=c("No trust whatsoever","Moderate level of trust","High level of trust",
                                            "Did not answer")),
         time = factor(time,
                       levels=c("Prior to the COVID-19 pandemic","During the COVID-19 pandemic")))

ggplot(trust_df_summary %>% filter(government_trust!="Did not answer"),
       aes(x=time,fill=trust_changing_advice))+
  geom_col(aes(y=percentage))+
  theme_bw()+
  facet_grid(platform_label~government_trust)+
  scale_fill_viridis_d(option="magma",end=0.6)+
  scale_size_area(max_size = 20)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  labs(x=" ",
       y="Percentage of responses (%)",
       fill="How do you feel when government \nadvice changes based on new \nscientific evidence?")+
  theme(strip.background = element_rect(fill = "white", color = "black"))+
  guides(color=guide_legend(order=1),fill=guide_legend(order=1))
ggsave("Plots/SupFig18.png",width=8,height=5)


## Supplementary Table 13
changing_advice_awareness <- rbind(all_responses %>%
                                     dplyr::select(platform_label,
                                                   trust_changing_advice,prior_awareness_use_in_policy) %>%
                                     rename(awareness_use_in_policy = prior_awareness_use_in_policy) %>%
                                     mutate(time = "Prior to the COVID-19 pandemic"),
                                   all_responses %>%
                                     dplyr::select(platform_label,
                                                   trust_changing_advice,during_awareness_use_in_policy) %>%
                                     rename(awareness_use_in_policy = during_awareness_use_in_policy) %>%
                                     mutate(time = "During the COVID-19 pandemic"))

changing_advice_awareness_summary <- changing_advice_awareness %>%
  group_by(platform_label,time,awareness_use_in_policy,trust_changing_advice) %>%
  summarise(count = length(platform_label)) %>%
  ungroup() %>% group_by(platform_label,time,awareness_use_in_policy) %>%
  mutate(total = sum(count),
         perc = count/total *100) %>%
  mutate(time= factor(time,
                      levels=c("Prior to the COVID-19 pandemic","During the COVID-19 pandemic")),
         awareness_use_in_policy_label = factor(ifelse(awareness_use_in_policy=="Yes","Aware",
                                                       ifelse(awareness_use_in_policy=="Unsure","Unsure",
                                                              ifelse(awareness_use_in_policy=="No","Not aware",NA))),
                                                levels=c("Not aware","Unsure","Aware")),
         trust_changing_advice = ifelse(is.na(trust_changing_advice),"Did not answer",trust_changing_advice),
         trust_changing_advice = factor(trust_changing_advice,
                                        levels=c("I have less trust in the advice.",
                                                 "My level of trust remains unchanged.",
                                                 "I have more trust in the advice.",
                                                 "Did not answer")))

ggplot(changing_advice_awareness_summary %>% filter(!is.na(awareness_use_in_policy)),
       aes(x=time,y=perc,fill=trust_changing_advice))+
  geom_col()+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"))+
  facet_grid(platform_label~awareness_use_in_policy_label)+
  scale_fill_viridis_d(option="magma",end=0.6)+
  scale_size_area(max_size = 20)+
  labs(x="Time",y="Percentage of respondents (%)",
       fill="How do you feel when government \nadvice changes based on new \nscientific evidence?")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
ggsave("Plots/SupFig19.png",width=8,height=5)


## Supplementary Figure 20
changing_advice_reliability <- rbind(all_responses %>% dplyr::select(platform_label,
                                                                     trust_changing_advice,prior_reliability_1_10) %>%
                                       rename(reliability_1_10 = prior_reliability_1_10) %>%
                                       mutate(time = "Prior to the COVID-19 pandemic"),
                                     all_responses %>%
                                       dplyr::select(platform_label,
                                                     trust_changing_advice,during_reliability_1_10) %>%
                                       rename(reliability_1_10 = during_reliability_1_10) %>%
                                       mutate(time = "During the COVID-19 pandemic")) %>%
  mutate(time = factor(time,
                       levels=c("Prior to the COVID-19 pandemic","During the COVID-19 pandemic")),
         trust_changing_advice = factor(trust_changing_advice,
                                        levels=c("I have less trust in the advice.",
                                                 "My level of trust remains unchanged.",
                                                 "I have more trust in the advice.")))

ggplot(changing_advice_reliability %>% filter(!is.na(reliability_1_10),
                                              !is.na(trust_changing_advice)),
       aes(x=time,y=reliability_1_10,col=time))+
  geom_jitter(shape=1)+
  geom_boxplot(lwd=1.5,outlier.shape = NA,fill=NA)+
  theme_bw()+
  facet_grid(platform_label~trust_changing_advice)+
  theme(strip.background = element_rect(fill="white"),
        legend.position="bottom")+
  scale_colour_manual(values=c("#a8c66c","#1b6535"))+
  scale_y_continuous(n.breaks=10) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  labs(x="Time period",
       y="On a scale of 1-10 with 1 being “extremely unreliable” \nand 10 being “extremely reliable” how do you feel \nabout the use of transmission models in informing \npublic health policy?",col="")
ggsave("Plots/SupFig20.png",width=8,height=5)






#