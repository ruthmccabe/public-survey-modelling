### How would you describe a transmission model?

## Questions covered:
## How would you describe a transmission model?
## What level of confidence do you have in your description?


library(tidyverse)
library(readxl)
library(cowplot)

##prolific
prolific <- read_excel("Data/responses_prolific.xlsx") %>% data.frame() %>%
  select(-c(Start.time,Completion.time,Email,consent_1,consent_2))

twitter <- read_excel("Data/responses_twitter.xlsx") %>% data.frame() %>%
  select(-c(Start.time,Completion.time,Email,consent_1,consent_2))

all_responses <- rbind(prolific,twitter) %>% group_by(platform) %>%
  mutate("platform_label"=ifelse(platform=="twitter","Twitter","Prolific Academic"),
         "platform_sample_size"=length(platform),
         "unique_ID"=paste0(platform,"_",ID)) %>% data.frame() %>%
  filter(!is.na(gender),!is.na(age_group),!is.na(sector),!is.na(vaccinated)) 



### read in the written answers that have been categorised 

model_description <- read_excel("Data/transmission_models.xlsx")

model_description <- model_description %>% mutate(relevant = ifelse(theme %in% c("Epi","Maths/stats","Other related"),
                                                                    "More relevant","Less relevant")) %>%
  mutate(model_description_confidence = factor(model_description_confidence,
                                               levels=c("Very confident","Moderately confident",
                                                        "Not at all confident")))

### Table 1
model_description %>% group_by(model_description_confidence,platform) %>%
  summarise(total = length(model_description)) %>% ungroup %>%
  group_by(platform) %>%
  mutate(perc = 100*total/sum(total)) %>% arrange(platform)

### Supplementary Table 1
description_confidence_summary <- model_description %>% group_by(model_description_confidence,relevant,platform) %>%
  summarise(total = length(model_description))

description_confidence_summary %>% ungroup() %>% group_by(platform) %>% mutate(perc = 100*total/sum(total)) %>%
  arrange(platform)


### Supplementary Figure 2
description_confidence_summary <- description_confidence_summary %>% ungroup() %>% group_by(platform,relevant) %>% mutate(perc = 100*total/sum(total)) %>%
  arrange(platform) %>%
  mutate(model_description_confidence = factor(model_description_confidence,
                                               levels=c("Not at all confident","Moderately confident",
                                                        "Very confident")),
         relevant=ifelse(relevant=="More relevant","More relevant model description",
                         "Less relevant model description"))


ggplot(description_confidence_summary,
       aes(x=platform,fill=model_description_confidence,group=platform))+
  geom_col(aes(y=perc))+
  theme_bw()+
  facet_wrap(~relevant)+
  scale_fill_viridis_d(option="magma",end=0.4)+
  scale_size_area(max_size = 20)+
  labs(y="Percentage of respondents (%)",x="Sample",
       fill="What level of \nconfidence do \nyou have in \nyour description?")+
  theme(strip.background = element_rect(fill = "white", color = "black"))+
  guides(color=guide_legend(order=1),fill=guide_legend(order=1))
ggsave("Plots/SupFig2.png",width=7,height=5)


### Supplementary Table 2

all_responses_lm <- all_responses %>% dplyr::select(model_description_confidence,age_group,gender,platform) %>%
  mutate(confidence_number = ifelse(model_description_confidence=="Very confident",3,
                                    ifelse(model_description_confidence=="Moderately confident",2,
                                           ifelse(model_description_confidence=="Not at all confident",1,NA))))

m1 <- glm(confidence_number~age_group+gender,data=all_responses_lm %>% filter(platform=="prolific"),
            family="poisson")
summary(m1)

m2 <- glm(confidence_number~age_group+gender,data=all_responses_lm %>% filter(platform=="twitter"),
            family="poisson")
summary(m2)

