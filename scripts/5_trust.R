## Trust in government advice

## Questions covered:
## How much did you trust government advice regarding public health issues? (prior and during)
## How do you feel when government advice changes based on new scientific evidence?

library(readxl)
library(tidyverse)
library(cowplot)
library(ggalluvial)

prolific <- read_excel("Data/responses_prolific.xlsx") %>% data.frame() %>%
  select(-c(Start.time,Completion.time,Email,consent_1,consent_2))

twitter <- read_excel("Data/responses_twitter.xlsx") %>% data.frame() %>%
  select(-c(Start.time,Completion.time,Email,consent_1,consent_2))

all_responses <- rbind(prolific,twitter) %>% group_by(platform) %>%
  mutate("platform_label"=ifelse(platform=="twitter","Twitter","Prolific Academic"),
         "platform_label_generic"=ifelse(platform=="twitter","Social media","Online panel"),
         #"platform_sample_size"=length(platform),
         "unique_ID"=paste0(platform,"_",ID)) %>% #data.frame() %>%
  filter(!is.na(gender),!is.na(age_group),!is.na(sector),!is.na(vaccinated)) %>%
  group_by(platform) %>%
  mutate(platform_sample_size=length(platform)) %>% data.frame()


## Table 1

table(all_responses$platform_label,all_responses$prior_government_trust,useNA="ifany")
round(prop.table(table(all_responses$platform_label,all_responses$prior_government_trust,useNA="ifany"),1)*100)

table(all_responses$platform_label,all_responses$during_government_trust,useNA="ifany")
round(prop.table(table(all_responses$platform_label,all_responses$during_government_trust,useNA = "ifany"),1)*100)


## Figure 4
government_trust_melt <- rbind(all_responses %>%
                                 dplyr::select(platform_label,platform_sample_size,
                                               prior_government_trust,
                                               prior_awareness_use_in_policy,
                                               age_group,gender,sector,vaccinated) %>%
                                 dplyr::rename(government_trust = prior_government_trust,
                                               awareness_use_in_policy = prior_awareness_use_in_policy) %>%
                                 mutate(time = "Prior to the COVID-19 pandemic"),
                               all_responses %>%
                                 dplyr::select(platform_label,platform_sample_size,
                                               during_government_trust,
                                               during_awareness_use_in_policy,
                                               age_group,gender,sector,vaccinated) %>%
                                 dplyr::rename(government_trust = during_government_trust,
                                               awareness_use_in_policy = during_awareness_use_in_policy) %>%
                                 mutate(time = "During the COVID-19 pandemic"))

government_trust_melt_summary <- government_trust_melt %>% group_by(platform_label,time,government_trust) %>%
  summarise(count = length(government_trust)) %>%
  mutate(platform_sample_size = ifelse(platform_label=="Prolific Academic",504,202),
         percentage = count/platform_sample_size *100,
         government_trust = ifelse(is.na(government_trust),"Did not answer",government_trust),
         government_trust = factor(government_trust,
                                   levels=c("No trust whatsoever","Moderate level of trust",
                                            "High level of trust","Did not answer")),
         time = factor(time,levels=c("Prior to the COVID-19 pandemic","During the COVID-19 pandemic")))

trust_changing_advice_summary <- all_responses %>%
  group_by(platform_label_generic,trust_changing_advice) %>%
  summarise(count = length(trust_changing_advice)) %>%
  mutate(trust_changing_advice = ifelse(is.na(trust_changing_advice),"Did not answer",trust_changing_advice),
         total = ifelse(platform_label_generic=="Online panel",504,202),
         trust_changing_advice = factor(trust_changing_advice,
                                        levels=c("I have more trust in the advice.",
                                                 "My level of trust remains unchanged.",
                                                 "I have less trust in the advice.",
                                                 "Did not answer")),
         percentage = count/total *100)

# 
# plot_grid(ggplot(government_trust_melt_summary,
#                  aes(x=time,fill=government_trust))+
#             geom_col(aes(y=percentage))+
#             theme_bw()+
#             facet_wrap(~platform_label)+
#             #scale_fill_viridis_d(option="magma",end=0.6)+
#             scale_fill_manual(values=c("#000004","#3b0f70","#8c2981","grey"))+
#             scale_size_area(max_size = 20)+
#             scale_x_discrete(labels = function(x) str_wrap(x, width = 12))+
#             labs(x="Time period",
#                  y="Percentage of responses (%)",
#                  fill="How much did you trust government advice regarding public health issues?")+
#             theme(strip.background = element_rect(fill = "white", color = "black"),legend.position="bottom",
#                   #legend.title=element_text(size=10), 
#                   #legend.text=element_text(size=8),
#                   legend.title.align=0.5)+
#             guides(color=guide_legend(order=1,nrow=1,title.position = "top"),
#                    fill=guide_legend(order=1,nrow=1,title.position = "top")),
#           plot_grid(ggplot(trust_changing_advice_summary,
#                  aes(x=platform_label,fill=trust_changing_advice))+
#             geom_col(aes(y=percentage))+
#             theme_bw()+
#             facet_wrap(~platform_label,scales="free_x")+
#             #scale_fill_viridis_d(option="magma",end=0.6)+
#               scale_fill_manual(values=c("#000004","#3b0f70","#8c2981","grey"))+
#             scale_size_area(max_size = 20)+
#             scale_x_discrete(labels = function(x) str_wrap(x, width = 12))+
#             labs(x="Sample",
#                  y="Percentage of responses (%)",
#                  fill="How do you feel when government advice \nchanges based on new scientific evidence?")+
#             theme(strip.background = element_rect(fill = "white", color = "black"),
#                   legend.position="none",
#                   #legend.title=element_text(size=10), 
#                   #legend.text=element_text(size=8),
#                   legend.title.align=0.5)+
#             guides(color=guide_legend(order=1,nrow=4,title.position = "top"),
#                    fill=guide_legend(order=1,nrow=4,title.position = "top")),
#             get_legend(ggplot(trust_changing_advice_summary,
#                                aes(x=platform_label,fill=trust_changing_advice))+
#                           geom_col(aes(y=percentage))+
#                           theme_bw()+
#                           facet_wrap(~platform_label,scales="free_x")+
#                           #scale_fill_viridis_d(option="magma",end=0.6)+
#                          scale_fill_manual(values=c("#000004","#3b0f70","#8c2981","grey"))+
#                           scale_size_area(max_size = 20)+
#                           scale_x_discrete(labels = function(x) str_wrap(x, width = 12))+
#                           labs(x="Sample",
#                                y="Percentage of responses (%)",
#                                fill="How do you feel when government advice \nchanges based on new scientific evidence?")+
#                           theme(strip.background = element_rect(fill = "white", color = "black"),
#                                 legend.position="right",
#                                 #legend.title=element_text(size=10), 
#                                 #legend.text=element_text(size=8),
#                                 legend.title.align=0.5)+
#                           guides(color=guide_legend(order=1,nrow=4,title.position = "top"),
#                                  fill=guide_legend(order=1,nrow=4,title.position = "top"))),rel_widths=c(1.05,1))
#        ,
#           nrow=2,
#           align="h",axis="l",labels="AUTO",rel_heights=c(1.2,1))
# ggsave("outputs/Fig4.png",height=8,width=7)
# 


#### try alluvial plot

trust_alluvial <- all_responses %>% 
  dplyr::select(platform_label_generic,prior_government_trust,during_government_trust) %>%
  group_by(platform_label_generic,prior_government_trust,during_government_trust) %>%
  summarise(freq = length(platform_label_generic)) %>%
  ungroup() %>%
  group_by(platform_label_generic,prior_government_trust) %>%
  mutate(total = sum(freq),
         freq2 = freq) %>%
  ungroup() %>%
  group_by(platform_label_generic,during_government_trust) %>%
  mutate(total_during = sum(freq)) %>%
  mutate(prior_short = ifelse(prior_government_trust=="High level of trust","High",
                              ifelse(prior_government_trust=="Moderate level of trust","Moderate",
                                     ifelse(prior_government_trust=="No trust whatsoever","None",NA))),
         during_short = ifelse(during_government_trust=="High level of trust","High",
                              ifelse(during_government_trust=="Moderate level of trust","Moderate",
                                     ifelse(during_government_trust=="No trust whatsoever","None",NA))))


plot_grid(ggplot(data = trust_alluvial %>% filter(!is.na(prior_government_trust),!is.na(during_government_trust)),
       aes(axis1 = prior_short, axis2 = during_short,
           y = freq, freq2 = freq2))+
  scale_x_discrete(limits = c("Level of trust prior to the COVID-19 pandemic",
                              "Level of trust during the COVID-19 pandemic"), 
                   expand = c(.2, .05),
                   labels = function(x) str_wrap(x, width = 16)) +
  labs(x="Time period",y="Number of respondents",fill="Trust during the pandemic")+
  theme_classic()+
  facet_wrap(~platform_label_generic,scales="free_y")+
  geom_alluvium(aes(fill=during_government_trust))+
  geom_stratum()+
  #scale_fill_manual(values=c("#C54B8C","#4d5198","#000004","grey"))+##f5beb4
    scale_fill_manual(values=c("#70A9A1","#40798C","#0B2027"))+
  geom_text(stat = "stratum", size=4,aes(label = stringr::str_wrap(after_stat(paste0(stratum)),10)),
            min.y=10) +
  # geom_label(stat = "alluvium", aes(label = after_stat(freq2),
  #                                   fill = prior_awareness_use_in_policy),alpha = 0.4,
  #                                               position = "dodge",vjust = 0.75, size = 5) +
  theme(legend.position = "bottom",
        axis.line = element_blank(),strip.text = element_text(size=12),
        legend.text = element_text(size=11),
        axis.text.x = element_text(colour="black",size=10))+
    guides(color=guide_legend(order=1,nrow=4,title.position = "top"),
             fill=guide_legend(order=1,nrow=4,title.position = "top"))+
  ggtitle("How much did you trust government advice regarding \npublic health issues?\n"),
  ggplot(trust_changing_advice_summary, #%>% filter(trust_changing_advice!="Did not answer"),
         aes(x=platform_label_generic,fill=trust_changing_advice))+
    geom_col(aes(y=percentage),alpha=0.75)+
    theme_classic()+
    facet_wrap(~platform_label_generic,scales="free_x")+
    #scale_fill_manual(values=c("#C54B8C","#4d5198","#000004","grey"))+
    scale_fill_manual(values=c("#70A9A1","#40798C","#0B2027","grey"))+
    scale_size_area(max_size = 20)+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 12))+
    labs(x="Sample",
         y="Percentage of responses (%)",
         fill="")+
    theme(strip.background = element_rect(fill = "white", color = "black"),
          legend.position="bottom",
          #legend.title=element_text(size=10), 
          #legend.text=element_text(size=8),
          legend.title.align=0.5,axis.line = element_blank(),strip.text = element_text(size=12),
          plot.title = element_text(size=12),
          legend.text = element_text(size=10.5),
          axis.text.x = element_text(colour="black",size=10))+
    guides(color=guide_legend(order=1,nrow=4,title.position = "top"),
           fill=guide_legend(order=1,nrow=4,title.position = "top"))+
    ggtitle("How do you feel when government \nadvice changes based on new \nscientific evidence?"),
  align="hv",axis="tb",rel_widths = c(2.25,1),labels="AUTO"
  )
ggsave("outputs/Fig4_raw.png",width=10.5,height=6.5)



## Supplementary Figure 12
government_trust_reliability <- rbind(all_responses %>%
                                        dplyr::select(platform_label_generic,
                                                      prior_government_trust,prior_reliability_1_10) %>%
                                        rename(government_trust = prior_government_trust,
                                               reliability_1_10 = prior_reliability_1_10) %>%
                                        mutate(time = "Prior to the COVID-19 pandemic"),
                                      all_responses %>%
                                        dplyr::select(platform_label_generic,
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
  geom_jitter(shape=1,height=0.15)+
  geom_boxplot(lwd=1.5,outlier.shape = NA,fill=NA)+
  theme_bw()+
  facet_grid(platform_label_generic~government_trust)+
  theme(strip.background = element_rect(fill="white"),
        legend.position="bottom")+
  scale_colour_manual(values=c("#a8c66c","#1b6535"))+
  scale_y_continuous(n.breaks=10) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  labs(x="Time period",
       y="On a scale of 1-10 with 1 being “extremely unreliable” \nand 10 being “extremely reliable” how do you feel \nabout the use of transmission models in informing \npublic health policy?",col="")
ggsave("outputs/SupFig12.png",width=7,height=5)


## Supplementary Table 14

government_trust_reliability %>% group_by(platform_label_generic,time,government_trust) %>%
  summarise(median = median(reliability_1_10,na.rm = TRUE),
            variance = var(reliability_1_10,na.rm=TRUE))



### significant differences?
# prop.test(x=c(all_responses %>% filter(platform_label=="Prolific Academic",
#                                        prior_government_trust=="Moderate level of trust") %>% nrow(),
#               all_responses %>% filter(platform_label=="Prolific Academic",
#                                        during_government_trust=="Moderate level of trust") %>% nrow()),
#           n=c(all_responses %>% filter(platform_label=="Prolific Academic") %>% nrow(),
#               all_responses %>% filter(platform_label=="Prolific Academic") %>% nrow()))
# 
# prop.test(x=c(all_responses %>% filter(platform_label=="Twitter",
#                                        prior_government_trust=="Moderate level of trust") %>% nrow(),
#               all_responses %>% filter(platform_label=="Twitter",
#                                        during_government_trust=="Moderate level of trust") %>% nrow()),
#           n=c(all_responses %>% filter(platform_label=="Twitter") %>% nrow(),
#               all_responses %>% filter(platform_label=="Twitter") %>% nrow()))


## significant differences in those changing trust levels across samples?
changing_opinion <- all_responses %>% select(platform_label,prior_government_trust,during_government_trust,
                                             age_group,gender) %>%
  mutate(change = ifelse(prior_government_trust==during_government_trust,FALSE,TRUE))

table(changing_opinion$platform_label,changing_opinion$change)

prop.test(x=c(changing_opinion %>% filter(platform_label=="Prolific Academic",
                                          change==TRUE) %>% nrow(),
              changing_opinion %>% filter(platform_label=="Twitter",
                                          change==TRUE) %>% nrow()),
          n=c(changing_opinion %>% filter(platform_label=="Prolific Academic") %>% nrow(),
              changing_opinion %>% filter(platform_label=="Twitter") %>% nrow()))

## significant differences in those selecting lowest trust level?

trust_numeric <- all_responses %>% dplyr::select(platform_label,prior_government_trust,
                                                                     during_government_trust) %>%
  mutate(prior_none = ifelse(prior_government_trust=="No trust whatsoever",1,0),
         prior_moderate = ifelse(prior_government_trust=="Moderate level of trust",1,0),
         prior_high = ifelse(prior_government_trust=="High level of trust",1,0),
         during_none = ifelse(during_government_trust=="No trust whatsoever",1,0),
         during_moderate = ifelse(during_government_trust=="Moderate level of trust",1,0),
         during_high = ifelse(during_government_trust=="High level of trust",1,0),)

## no trust 
wilcox.test(x = trust_numeric %>% filter(platform_label=="Prolific Academic") %>%
              dplyr::select(prior_none) %>% unlist() %>% as.numeric(),
            y = trust_numeric %>% filter(platform_label=="Prolific Academic") %>%
              dplyr::select(during_none) %>% unlist() %>% as.numeric(),
            paired = TRUE,
            alternative = "two.sided")

wilcox.test(x = trust_numeric %>% filter(platform_label=="Twitter") %>%
              dplyr::select(prior_none) %>% unlist() %>% as.numeric(),
            y = trust_numeric %>% filter(platform_label=="Twitter") %>%
              dplyr::select(during_none) %>% unlist() %>% as.numeric(),
            paired = TRUE,
            alternative = "two.sided")



## significantly more people saying no trust than high trust?
wilcox.test(x = trust_numeric %>% filter(platform_label=="Prolific Academic") %>%
              dplyr::select(during_none) %>% unlist() %>% as.numeric(),
            y = trust_numeric %>% filter(platform_label=="Prolific Academic") %>%
              dplyr::select(during_high) %>% unlist() %>% as.numeric(),
            paired = TRUE,
            alternative = "two.sided")

wilcox.test(x = trust_numeric %>% filter(platform_label=="Twitter") %>%
              dplyr::select(during_none) %>% unlist() %>% as.numeric(),
            y = trust_numeric %>% filter(platform_label=="Twitter") %>%
              dplyr::select(during_high) %>% unlist() %>% as.numeric(),
            paired = TRUE,
            alternative = "two.sided")



## significantly more people changing from high to no in the twitter sample?
prop.test(x=c(changing_opinion %>% filter(platform_label=="Prolific Academic",
                                          prior_government_trust=="High level of trust",
                                          during_government_trust=="No trust whatsoever") %>% nrow(),
              changing_opinion %>% filter(platform_label=="Twitter",
                                          prior_government_trust=="High level of trust",
                                          during_government_trust=="No trust whatsoever") %>% nrow()),
          n=c(changing_opinion %>% filter(platform_label=="Prolific Academic",
                                          prior_government_trust=="High level of trust") %>% nrow(),
              changing_opinion %>% filter(platform_label=="Twitter",
                                          prior_government_trust=="High level of trust") %>% nrow()))


## which sample more likely to select high trust?
prop.test(x=c(changing_opinion %>% filter(platform_label=="Prolific Academic",
                                          during_government_trust=="High level of trust") %>% nrow(),
              changing_opinion %>% filter(platform_label=="Twitter",
                                          during_government_trust=="High level of trust") %>% nrow()),
          n=c(changing_opinion %>% filter(platform_label=="Prolific Academic") %>% nrow(),
              changing_opinion %>% filter(platform_label=="Twitter") %>% nrow()))


prop.test(x=c(changing_opinion %>% filter(platform_label=="Prolific Academic",
                                          prior_government_trust=="High level of trust") %>% nrow(),
              changing_opinion %>% filter(platform_label=="Twitter",
                                          prior_government_trust=="High level of trust") %>% nrow()),
          n=c(changing_opinion %>% filter(platform_label=="Prolific Academic") %>% nrow(),
              changing_opinion %>% filter(platform_label=="Twitter") %>% nrow()))



## Supplementary Table 15
changing_opinion <- changing_opinion %>%
  mutate(prior_government_trust_numeric = ifelse(prior_government_trust=="High level of trust",1,
                                                 ifelse(prior_government_trust=="Moderate level of trust",0,
                                                        ifelse(prior_government_trust=="No trust whatsoever",-1,
                                                               NA))),
         during_government_trust_numeric = ifelse(during_government_trust=="High level of trust",1,
                                                  ifelse(during_government_trust=="Moderate level of trust",0,
                                                         ifelse(during_government_trust=="No trust whatsoever",-1,
                                                                NA))),
         diff = during_government_trust_numeric - prior_government_trust_numeric)


table(changing_opinion$platform_label,changing_opinion$diff)

## Supplementary Table 16

government_trust_melt <- government_trust_melt %>%
  mutate(government_trust_numeric = ifelse(government_trust=="High level of trust",1,
                                           ifelse(government_trust=="Moderate level of trust",0,
                                                  ifelse(government_trust=="No trust whatsoever",-1,NA))))

m1 <- lm(government_trust_numeric~age_group+gender,
          data=government_trust_melt %>% filter(platform_label=="Prolific Academic",
                                                time=="Prior to the COVID-19 pandemic"))
summary(m1)
anova(m1,test="Chisq")


m2 <- lm(government_trust_numeric~age_group+gender,
          data=government_trust_melt %>% filter(platform_label=="Twitter",
                                                time=="Prior to the COVID-19 pandemic"))
summary(m2)
anova(m2,test="Chisq")

m3 <- lm(government_trust_numeric~age_group+gender,
          data=government_trust_melt %>% filter(platform_label=="Prolific Academic",
                                                time=="During the COVID-19 pandemic"))
summary(m3)
anova(m3,test="Chisq")

m4 <- lm(government_trust_numeric~age_group+gender,
          data=government_trust_melt %>% filter(platform_label=="Twitter",
                                                time=="During the COVID-19 pandemic"))
summary(m4)
anova(m4,test="Chisq")


## Supplementary Table 17

m5 <- lm(diff~age_group+gender,
         data=changing_opinion %>% filter(platform_label=="Prolific Academic"))
summary(m5)
anova(m5,test="Chisq")


m6 <- lm(diff~age_group+gender,
         data=changing_opinion %>% filter(platform_label=="Twitter"))
summary(m6)
anova(m6,test="Chisq")


## Supplementary Table 18
### now accounting for awareness
government_trust_melt <- government_trust_melt %>%
  mutate(government_trust_numeric = ifelse(government_trust=="High level of trust",1,
                                           ifelse(government_trust=="Moderate level of trust",0,
                                                  ifelse(government_trust=="No trust whatsoever",-1,NA))),
         awareness_use_in_policy_numeric = ifelse(awareness_use_in_policy=="Yes",1,
                                                  ifelse(awareness_use_in_policy=="Unsure",0,
                                                         ifelse(awareness_use_in_policy=="No",-1,NA))))

m7 <- lm(government_trust_numeric~age_group+gender+awareness_use_in_policy_numeric,
          data=government_trust_melt %>% filter(platform_label=="Prolific Academic",
                                                time=="Prior to the COVID-19 pandemic"))
summary(m7)
anova(m7,test="Chisq")


m8 <- lm(government_trust_numeric~age_group+gender+awareness_use_in_policy_numeric,
          data=government_trust_melt %>% filter(platform_label=="Twitter",
                                                time=="Prior to the COVID-19 pandemic"))
summary(m8)
anova(m8,test="Chisq")


m9 <- lm(government_trust_numeric~age_group+gender+awareness_use_in_policy_numeric,
          data=government_trust_melt %>% filter(platform_label=="Prolific Academic",
                                                time=="During the COVID-19 pandemic"))
summary(m9)
anova(m9,test="Chisq")


m10 <- lm(government_trust_numeric~age_group+gender+awareness_use_in_policy_numeric,
          data=government_trust_melt %>% filter(platform_label=="Twitter",
                                                time=="During the COVID-19 pandemic"))
summary(m10)
anova(m10,test="Chisq")




## Supplementary Table 19
government_trust_awareness <- rbind(all_responses %>%
                                      dplyr::select(platform_label_generic,
                                                    prior_government_trust,prior_awareness_use_in_policy) %>%
                                      rename(government_trust = prior_government_trust,
                                             awareness_use_in_policy = prior_awareness_use_in_policy) %>%
                                      mutate(time = "Prior to the COVID-19 pandemic"),
                                    all_responses %>%
                                      dplyr::select(platform_label_generic,
                                                    during_government_trust,during_awareness_use_in_policy) %>%
                                      rename(government_trust =during_government_trust,
                                             awareness_use_in_policy = during_awareness_use_in_policy) %>%
                                      mutate(time = "During the COVID-19 pandemic"))

government_trust_awareness_summary <- government_trust_awareness %>%
  group_by(platform_label_generic,time,awareness_use_in_policy,government_trust) %>%
  summarise(count = length(platform_label_generic)) %>%
  ungroup() %>% group_by(platform_label_generic,time,awareness_use_in_policy) %>%
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

government_trust_awareness_summary


## Supplementary Figure 13
ggplot(government_trust_awareness_summary %>% filter(!is.na(awareness_use_in_policy)),
       aes(x=time,y=perc,fill=government_trust))+
  geom_col()+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"))+
  facet_grid(platform_label_generic~awareness_use_in_policy_label)+
  #scale_fill_viridis_d(option="magma",end=0.6)+
  scale_fill_manual(values=c("#000004","#3b0f70","#8c2981","grey"))+
  scale_size_area(max_size = 20)+
  labs(x="Time",y="Percentage of respondents (%)",
       fill="How much did you \ntrust government \nadvice regarding \npublic health issues?")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
ggsave("outputs/SupFig13.png",width=7,height=5)


# ## make into alluvial plot
# 
# trust_awareness_alluvial <- all_responses %>% 
#   dplyr::select(platform_label_generic,
#                 prior_government_trust,during_government_trust,
#                 prior_awareness_use_in_policy,during_awareness_use_in_policy) %>%
#   group_by(platform_label_generic,prior_government_trust,during_government_trust,
#            prior_awareness_use_in_policy,during_awareness_use_in_policy) %>%
#   summarise(freq = length(platform_label_generic)) %>%
#   ungroup() %>%
#   group_by(platform_label_generic,prior_government_trust,prior_awareness_use_in_policy) %>%
#   mutate(total = sum(freq),
#          freq2 = freq) %>%
#   ungroup() %>%
#   group_by(platform_label_generic,during_government_trust,during_awareness_use_in_policy) %>%
#   mutate(total_during = sum(freq)) %>%
#   mutate(prior_short = ifelse(prior_government_trust=="High level of trust","High",
#                               ifelse(prior_government_trust=="Moderate level of trust","Moderate",
#                                      ifelse(prior_government_trust=="No trust whatsoever","None",NA))),
#          during_short = ifelse(during_government_trust=="High level of trust","High",
#                                ifelse(during_government_trust=="Moderate level of trust","Moderate",
#                                       ifelse(during_government_trust=="No trust whatsoever","None",NA))))
# 
# 
# ggplot(data = trust_awareness_alluvial %>% 
#          filter(!is.na(prior_government_trust),!is.na(during_government_trust)),
#                  aes(axis1 = prior_short, axis2 = during_short,
#                      y = freq, freq2 = freq2))+
#             scale_x_discrete(limits = c("Level of trust prior to the COVID-19 pandemic",
#                                         "Level of trust during the COVID-19 pandemic"), 
#                              expand = c(.2, .05),
#                              labels = function(x) str_wrap(x, width = 16)) +
#             labs(x="Time period",y="Number of respondents",fill="Trust during the pandemic")+
#             theme_classic()+
#             facet_grid(platform_label_generic~during_awareness_use_in_policy,scales="free_y")+
#             geom_alluvium(aes(fill=during_government_trust))+
#             geom_stratum()+
#             #scale_fill_manual(values=c("#C54B8C","#4d5198","#000004","grey"))+##f5beb4
#             scale_fill_manual(values=c("#70A9A1","#40798C","#0B2027"))+
#             geom_text(stat = "stratum", size=4,aes(label = stringr::str_wrap(after_stat(paste0(stratum)),10)),
#                       min.y=10) +
#             # geom_label(stat = "alluvium", aes(label = after_stat(freq2),
#             #                                   fill = prior_awareness_use_in_policy),alpha = 0.4,
#             #                                               position = "dodge",vjust = 0.75, size = 5) +
#             theme(legend.position = "bottom",
#                   axis.line = element_blank(),strip.text = element_text(size=12),
#                   legend.text = element_text(size=11),
#                   axis.text.x = element_text(colour="black",size=10))+
#             guides(color=guide_legend(order=1,nrow=4,title.position = "top"),
#                    fill=guide_legend(order=1,nrow=4,title.position = "top"))+
#             ggtitle("How much did you trust government advice regarding \npublic health issues?\n")


## Supplementary Tables 20 - 22
trust_changing_advice <- all_responses %>% dplyr::select(platform_label,
                                                          trust_changing_advice,
                                                         prior_awareness_use_in_policy,
                                                          during_awareness_use_in_policy,
                                                          prior_government_trust,
                                                          during_government_trust,
                                                          age_group,gender,sector,vaccinated) %>%
  mutate(trust_changing_advice_numeric = ifelse(trust_changing_advice=="I have more trust in the advice.",1,
                                                ifelse(trust_changing_advice=="My level of trust remains unchanged.",
                                                       0,
                                                       ifelse(trust_changing_advice=="I have less trust in the advice.",
                                                              -1,NA))),
         prior_awareness_use_in_policy_numeric = ifelse(prior_awareness_use_in_policy=="Yes",1,
                                                         ifelse(prior_awareness_use_in_policy=="Unsure",0,
                                                                ifelse(prior_awareness_use_in_policy=="No",-1,
                                                                       NA))),
         during_awareness_use_in_policy_numeric = ifelse(during_awareness_use_in_policy=="Yes",1,
                                                         ifelse(during_awareness_use_in_policy=="Unsure",0,
                                                                ifelse(during_awareness_use_in_policy=="No",-1,
                                                                       NA))),
         prior_government_trust_numeric = ifelse(prior_government_trust=="High level of trust",1,
                                                 ifelse(prior_government_trust=="Moderate level of trust",0,
                                                        ifelse(prior_government_trust=="No trust whatsoever",-1,
                                                               NA))),
         during_government_trust_numeric = ifelse(during_government_trust=="High level of trust",1,
                                                  ifelse(during_government_trust=="Moderate level of trust",0,
                                                         ifelse(during_government_trust=="No trust whatsoever",-1,
                                                                NA))))

## Supplementary Table 20
m11 <- lm(trust_changing_advice_numeric~age_group+gender,
          data=trust_changing_advice %>% filter(platform_label=="Prolific Academic"))
summary(m11)
anova(m11,test="Chisq")

m12 <- lm(trust_changing_advice_numeric~age_group+gender,
          data=trust_changing_advice %>% filter(platform_label=="Twitter"))
summary(m12)
anova(m12,test="Chisq")

## Supplementary Table 21
m13 <- lm(trust_changing_advice_numeric~age_group+gender+prior_awareness_use_in_policy_numeric+during_awareness_use_in_policy_numeric,
          data=trust_changing_advice %>% filter(platform_label=="Prolific Academic"))
summary(m13)
anova(m13,test="Chisq")

m14 <- lm(trust_changing_advice_numeric~age_group+gender+prior_awareness_use_in_policy_numeric+during_awareness_use_in_policy_numeric,
          data=trust_changing_advice %>% filter(platform_label=="Twitter"))
summary(m14)
anova(m14,test="Chisq")

## Supplementary Table 22
m15 <- lm(trust_changing_advice_numeric~age_group+gender+prior_government_trust_numeric+during_government_trust_numeric,
          data=trust_changing_advice %>% filter(platform_label=="Prolific Academic"))
summary(m15)
anova(m15,test="Chisq")

m16 <- lm(trust_changing_advice_numeric~age_group+gender+prior_government_trust_numeric+during_government_trust_numeric,
          data=trust_changing_advice %>% filter(platform_label=="Twitter"))
summary(m16)
anova(m16,test="Chisq")

## Supplementary Figure 14
changing_advice_reliability <- rbind(all_responses %>% 
                                       dplyr::select(platform_label_generic,
                                                     trust_changing_advice,prior_reliability_1_10) %>%
                                       rename(reliability_1_10 = prior_reliability_1_10) %>%
                                       mutate(time = "Prior to the COVID-19 pandemic"),
                                     all_responses %>%
                                       dplyr::select(platform_label_generic,
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
  geom_jitter(shape=1,height=0.15)+
  geom_boxplot(lwd=1.5,outlier.shape = NA,fill=NA)+
  theme_bw()+
  facet_grid(platform_label_generic~trust_changing_advice)+
  theme(strip.background = element_rect(fill="white"),
        legend.position="bottom")+
  scale_colour_manual(values=c("#a8c66c","#1b6535"))+
  scale_y_continuous(n.breaks=10) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  labs(x="Time period",
       y="On a scale of 1-10 with 1 being “extremely unreliable” \nand 10 being “extremely reliable” how do you feel \nabout the use of transmission models in informing \npublic health policy?",col="")
ggsave("outputs/SupFig14.png",width=8,height=5)



## Supplementary Figure 15
trust_df <- rbind(all_responses %>% 
                    dplyr::select(platform_label_generic,prior_government_trust,trust_changing_advice) %>%
                    mutate(time="Prior to the COVID-19 pandemic") %>%
                    rename(government_trust = prior_government_trust),
                  all_responses %>% 
                    dplyr::select(platform_label_generic,during_government_trust,trust_changing_advice) %>%
                    mutate(time="During the COVID-19 pandemic") %>%
                    rename(government_trust = during_government_trust))

trust_df_summary <- trust_df %>% group_by(platform_label_generic,time,government_trust,
                                          trust_changing_advice) %>%
  summarise(count = length(trust_changing_advice)) %>%
  mutate(trust_changing_advice = ifelse(is.na(trust_changing_advice),
                                        "Did not answer",trust_changing_advice),
         government_trust = ifelse(is.na(government_trust),
                                   "Did not answer",government_trust)) %>%
  group_by(platform_label_generic,time,government_trust) %>%
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
  facet_grid(platform_label_generic~government_trust)+
  #scale_fill_viridis_d(option="magma",end=0.6)+
  scale_fill_manual(values=c("#000004","#3b0f70","#8c2981","grey"))+
  scale_size_area(max_size = 20)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  labs(x=" ",
       y="Percentage of responses (%)",
       fill="How do you feel when government \nadvice changes based on new \nscientific evidence?")+
  theme(strip.background = element_rect(fill = "white", color = "black"))+
  guides(color=guide_legend(order=1),fill=guide_legend(order=1))
ggsave("outputs/SupFig15.png",width=8,height=5)

## Supplementary Table 23
trust_df_summary









