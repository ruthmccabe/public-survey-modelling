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
         "platform_label_generic"=ifelse(platform=="twitter","Social media","Online panel"),
         "platform_sample_size"=length(platform),
         "unique_ID"=paste0(platform,"_",ID)) %>% data.frame() %>%
  filter(!is.na(gender),!is.na(age_group),!is.na(sector),!is.na(vaccinated)) 



### read in the written answers that have been categorised 

model_description <- read_excel("Data/transmission_models.xlsx") %>%
  mutate(platform_label_generic = ifelse(platform=="Twitter","Social media","Online panel"))

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



### Supplementary Figure 16
# description_confidence_summary <- model_description %>% 
#   group_by(platform_label_generic,theme,relevant,model_description_confidence) %>%
#   summarise(number = length(platform_label_generic)) %>% ungroup() %>%
#   group_by(platform_label_generic,relevant,model_description_confidence) %>% 
#   mutate(total = sum(number)) %>% ungroup() %>%
#   mutate(sample_size = ifelse(platform_label_generic=="Online panel",504,202),
#          perc = total/sample_size*100)


description_confidence_summary <- model_description %>% 
  group_by(platform_label_generic,relevant,model_description_confidence) %>%
  summarise(count = length(platform_label_generic)) %>%
  ungroup() %>%
  group_by(platform_label_generic,relevant) %>%
  mutate(total = sum(count),
         perc = 100*count/total) %>%
  mutate(model_description_confidence = factor(model_description_confidence,
                                               levels=c("Not at all confident","Moderately confident",
                                                        "Very confident")),
         relevant=ifelse(relevant=="More relevant","More relevant model description",
                         "Less relevant model description"))
# 
# 
# description_confidence_summary <- description_confidence_summary %>% ungroup() %>% 
#   group_by(platform_label_generic,relevant) %>% mutate(perc = 100*total/sum(total)) %>%
#   arrange(platform_label_generic) %>%
#   mutate(model_description_confidence = factor(model_description_confidence,
#                                                levels=c("Not at all confident","Moderately confident",
#                                                         "Very confident")),
#          relevant=ifelse(relevant=="More relevant","More relevant model description",
#                          "Less relevant model description"))


ggplot(description_confidence_summary,
       aes(x=platform_label_generic,fill=model_description_confidence,group=platform_label_generic))+
  geom_col(aes(y=perc))+
  theme_bw()+
  facet_wrap(~relevant)+
  scale_fill_viridis_d(option="magma",end=0.4)+
  scale_size_area(max_size = 20)+
  labs(y="Percentage of respondents (%)",x="Sample",
       fill="What level of \nconfidence do \nyou have in \nyour description?")+
  theme(strip.background = element_rect(fill = "white", color = "black"))+
  guides(color=guide_legend(order=1),fill=guide_legend(order=1))
ggsave("outputs/SupFig16.png",width=7,height=5)


### Supplementary Table 27
description_confidence_summary <- model_description %>% group_by(model_description_confidence,relevant,platform) %>%
  summarise(total = length(model_description))

description_confidence_summary %>% ungroup() %>% group_by(platform) %>% mutate(perc = 100*total/sum(total)) %>%
  arrange(platform)


## tests within text
### stat sig differences?
prop.test(x = c(model_description %>% filter(platform=="Prolific Academic",relevant=="More relevant") %>% nrow(),
                model_description %>% filter(platform=="Twitter",relevant=="More relevant") %>% nrow()),
          n = c(model_description %>% filter(platform=="Prolific Academic") %>% nrow(),
                model_description %>% filter(platform=="Twitter") %>% nrow()))

# opinion
prop.test(x = c(model_description %>% filter(platform=="Prolific Academic",theme=="Opinion") %>% nrow(),
                model_description %>% filter(platform=="Twitter",theme=="Opinion") %>% nrow()),
          n = c(model_description %>% filter(platform=="Prolific Academic") %>% nrow(),
                model_description %>% filter(platform=="Twitter") %>% nrow()))


### significant
prop.test(x=c(model_description %>% filter(platform=="Prolific Academic",
                                           model_description_confidence!="Not at all confident") %>% nrow(),
              model_description %>% filter(platform=="Twitter",
                                           model_description_confidence!="Not at all confident") %>% nrow()),
          n = c(model_description %>% filter(platform=="Prolific Academic") %>% nrow(),
                model_description %>% filter(platform=="Twitter") %>% nrow()))


## relevance among high confidence
prop.test(x = c(model_description %>% filter(platform=="Prolific Academic",model_description_confidence=="Very confident",
                             relevant=="More relevant") %>% nrow(),
                model_description %>% filter(platform=="Twitter",model_description_confidence=="Very confident",
                                             relevant=="More relevant") %>% nrow()),
          n = c(model_description %>% filter(platform=="Prolific Academic",
                                             model_description_confidence=="Very confident") %>% nrow(),
                model_description %>% filter(platform=="Twitter",
                                             model_description_confidence=="Very confident") %>% nrow()))


### Supplementary Table 28

all_responses_lm <- all_responses %>% dplyr::select(model_description_confidence,age_group,gender,platform) %>%
  mutate(confidence_number = ifelse(model_description_confidence=="Very confident",1,
                                    ifelse(model_description_confidence=="Moderately confident",0,
                                           ifelse(model_description_confidence=="Not at all confident",-1,NA))))

m1 <- lm(confidence_number~age_group+gender,data=all_responses_lm %>% filter(platform=="prolific"))
summary(m1)
anova(m1,test="Chisq")

m2 <- lm(confidence_number~age_group+gender,data=all_responses_lm %>% filter(platform=="twitter"))
summary(m2)
anova(m2,test="Chisq")

