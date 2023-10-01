### Reliability of transmission modelling in informing policy

## Questions covered:
## On a scale of 1 - 10 with 1 being "extremely unreliable" and 10 being "extremely reliable", how did you feel about the use of transmission models in informing public health policy? (prior and during)

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
         "platform_label_generic"=ifelse(platform=="prolific","Online panel","Social media"),
         "platform_sample_size"=length(platform),
         "unique_ID"=paste0(platform,"_",ID)) %>% data.frame() %>%
  filter(!is.na(gender),!is.na(age_group),!is.na(sector),!is.na(vaccinated)) 


## Table 1
table(all_responses$platform_label,all_responses$prior_reliability_1_10,useNA="ifany")
round(prop.table(table(all_responses$platform_label,all_responses$prior_reliability_1_10,useNA="ifany"),1)*100)

table(all_responses$platform_label,all_responses$during_reliability_1_10,useNA="ifany")
round(prop.table(table(all_responses$platform_label,all_responses$during_reliability_1_10,useNA="ifany"),1)*100)


## summary statistics and tests for significance between the samples
all_responses %>% filter(platform=="prolific") %>% select(prior_reliability_1_10) %>% 
  summarise(mean = mean(prior_reliability_1_10,na.rm=TRUE),
            sd = sd(prior_reliability_1_10,na.rm=TRUE))

all_responses %>% filter(platform=="twitter") %>% select(prior_reliability_1_10) %>% 
  summarise(mean = mean(prior_reliability_1_10,na.rm=TRUE),
            sd = sd(prior_reliability_1_10,na.rm=TRUE))

wilcox.test(x= all_responses %>% filter(platform=="prolific") %>% select(prior_reliability_1_10) %>%
              unlist() %>% as.numeric(),
            y= all_responses %>% filter(platform=="twitter") %>% select(prior_reliability_1_10) %>%
              unlist() %>% as.numeric(),
            paired=FALSE,
            alternative="two.sided")

all_responses %>% filter(platform=="prolific") %>% select(during_reliability_1_10) %>% 
  summarise(mean = mean(during_reliability_1_10,na.rm=TRUE),
            sd = sd(during_reliability_1_10,na.rm=TRUE))

all_responses %>% filter(platform=="twitter") %>% select(during_reliability_1_10) %>% 
  summarise(mean = mean(during_reliability_1_10,na.rm=TRUE),
            sd = sd(during_reliability_1_10,na.rm=TRUE))

wilcox.test(x= all_responses %>% filter(platform=="prolific") %>% select(during_reliability_1_10) %>%
              unlist() %>% as.numeric(),
            y= all_responses %>% filter(platform=="twitter") %>% select(during_reliability_1_10) %>%
              unlist() %>% as.numeric(),
            paired=FALSE,
            alternative="two.sided")

## test for significance within samples across time periods
wilcox.test(x= all_responses %>% filter(platform=="prolific") %>% select(prior_reliability_1_10) %>%
              unlist() %>% as.numeric(),
            y= all_responses %>% filter(platform=="prolific") %>% select(during_reliability_1_10) %>%
              unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")

wilcox.test(x= all_responses %>% filter(platform=="twitter") %>% select(prior_reliability_1_10) %>%
              unlist() %>% as.numeric(),
            y= all_responses %>% filter(platform=="twitter") %>% select(during_reliability_1_10) %>%
              unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")


## Figure 3
all_responses_reliability_melt <- rbind(all_responses %>%
                                          dplyr::select(prior_awareness_use_in_policy,prior_reliability_1_10,
                                                        platform_label,platform_label_generic,
                                                        age_group,gender,sector,vaccinated) %>%
                                          mutate("time" = "Prior to the COVID-19 pandemic") %>%
                                          rename(awareness_use_in_policy = prior_awareness_use_in_policy,
                                                 reliability_1_10 = prior_reliability_1_10),
                                        all_responses %>%
                                          dplyr::select(during_awareness_use_in_policy,during_reliability_1_10,
                                                        platform_label,platform_label_generic,
                                                        age_group,gender,sector,vaccinated) %>%
                                          mutate("time" = "During the COVID-19 pandemic") %>%
                                          rename(awareness_use_in_policy = during_awareness_use_in_policy,
                                                 reliability_1_10 = during_reliability_1_10)) %>%
  group_by(platform_label,time) %>%
  mutate(platform_sample_size = length(platform_label),
         time = factor(time,
                       levels=c("Prior to the COVID-19 pandemic",
                                "During the COVID-19 pandemic")),
         reliability_mean = mean(reliability_1_10,na.rm=TRUE)) %>% data.frame() 

reliability_time <- all_responses %>% dplyr::select(ID,platform_label,platform_label_generic,platform_sample_size,
                                                    prior_awareness_use_in_policy,prior_reliability_1_10,
                                                    during_awareness_use_in_policy,during_reliability_1_10,
                                                    during_level_of_awareness,
                                                    age_group,gender,sector,vaccinated) %>%
  mutate(difference = during_reliability_1_10 - prior_reliability_1_10)

reliability_time_summary <- reliability_time %>% group_by(platform_label_generic) %>%
  mutate(platform_sample_size = length(platform_label_generic)) %>%
  group_by(platform_label_generic,difference) %>%
  summarise(count = length(difference)) %>%
  mutate(platform_sample_size = ifelse(platform_label_generic=="Online panel",
                                       504,202),
         perc = count/platform_sample_size * 100)


ggplot(all_responses_reliability_melt,
                        aes(y=reliability_1_10,x=time,group=time,col=time))+
                   geom_jitter(alpha=0.5,shape=1,height=0.15)+
                   geom_boxplot(position="dodge",lwd=1.5,outlier.shape = NA,fill=NA)+
                   stat_summary(fun=mean, geom="crossbar",  linetype="dashed") +
                   facet_wrap(~platform_label_generic)+
                   theme_bw()+
                   scale_colour_manual(values=c("#a8c66c","#1b6535"))+
                   scale_y_continuous(n.breaks=10) +
                   scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
                   theme(legend.position="top",strip.background.x = element_rect(fill="white"))+
                   labs(x="Time period",y="On a scale of 1-10 with 1 being “extremely \nunreliable” and 10 being “extremely reliable” \nhow do you feel about the use of transmission \nmodels in informing public health policy?",col="")
ggsave("outputs/SupFig4.png",height=4,width=6)

# plot_grid(ggplot(all_responses_reliability_melt,
#                  aes(y=reliability_1_10,x=time,group=time,col=time))+
#             geom_jitter(alpha=0.5,shape=1,height=0.15)+
#             geom_boxplot(position="dodge",lwd=1.5,outlier.shape = NA,fill=NA)+
#             stat_summary(fun=mean, geom="crossbar",  linetype="dashed") +
#             facet_wrap(~platform_label_generic)+
#             theme_bw()+
#             scale_colour_manual(values=c("#a8c66c","#1b6535"))+
#             scale_y_continuous(n.breaks=10) +
#             scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
#             theme(legend.position="top",strip.background.x = element_rect(fill="white"))+
#             labs(x="Time period",y="On a scale of 1-10 with 1 being “extremely \nunreliable” and 10 being “extremely reliable” \nhow do you feel about the use of transmission \nmodels in informing public health policy?",col=""),
#           ggplot(reliability_time_summary,aes(x=difference,y=perc,#x=platform_label,
#                                               group=platform_label_generic,col=platform_label_generic,
#                                               fill=platform_label_generic))+
#             geom_col(position="dodge")+
#             theme_bw()+
#             scale_colour_manual(values=c("#1e2761","#408ec6"))+
#             scale_fill_manual(values=c("#1e2761","#408ec6"))+
#             scale_x_continuous(breaks = seq(-10,10,1))+
#             theme(legend.position="bottom",strip.background.x = element_rect(fill="white"))+
#             labs(x="Difference in reliability scores (during - before)",y="Percentage of respondents (%)",
#                  col="",fill="")+
#             geom_vline(xintercept = 0.5,linetype="dashed")+
#             geom_vline(xintercept = -0.5,linetype="dashed")+
#             annotate("text",x=5,y=40,label="Reliability score increased \nduring the pandemic")+
#             annotate("segment", x = 0.5, xend = 10, y = 40, yend = 40, colour = "black", size=1, alpha=0.6, arrow=arrow())+
#             annotate("text",x=-4.8,y=40,label="Reliability score decreased \nduring the pandemic")+
#             annotate("segment", x = -0.5, xend = -10, y = 40, yend = 40, colour = "black", size=1, alpha=0.6, arrow=arrow()),
#           nrow=2,rel_heights = c(1.5,1),align="hv",axis="l",labels="AUTO")
#ggsave("outputs/Fig3.png",height=7,width=6)


reliability_alluvial <- all_responses %>% 
  dplyr::select(platform_label_generic,prior_reliability_1_10,during_reliability_1_10) %>%
  group_by(platform_label_generic,prior_reliability_1_10,during_reliability_1_10) %>%
  summarise(freq = length(platform_label_generic)) %>% ungroup() %>%
  group_by(platform_label_generic,prior_reliability_1_10) %>%
  mutate(total = sum(freq),
         freq2 = freq) %>% ungroup() %>%
  group_by(platform_label_generic,during_reliability_1_10) %>%
  mutate(during_total = sum(freq)) %>%
  mutate(prior_reliability_1_10 = factor(ifelse(is.na(prior_reliability_1_10),"Did not answer",
                                         prior_reliability_1_10),
                                         levels=c("1","2","3","4","5","6","7","8","9","10","Did not answer")),
         during_reliability_1_10 = factor(ifelse(is.na(during_reliability_1_10),"Did not answer",
                                          during_reliability_1_10),
         levels=c("1","2","3","4","5","6","7","8","9","10","Did not answer"))) %>%
  ungroup()

# ggplot(data = reliability_alluvial%>% filter(prior_reliability_1_10!="Did not answer",
#                                            during_reliability_1_10!="Did not answer"),
#        aes(axis1 = prior_reliability_1_10, axis2 = during_reliability_1_10,
#            y = freq, freq2 = freq2))+
#   scale_x_discrete(limits = c("Prior to the COVID-19 pandemic", "During the COVID-19 pandemic"), 
#                    expand = c(.2, .05),
#                    labels = function(x) str_wrap(x, width = 12)) +
#   labs(x="Time period",y="Number of respondents",fill="Reliability score during the pandemic")+
#   theme_classic()+
#   facet_wrap(~platform_label_generic,scales="free_y")+
#   geom_alluvium(aes(fill=during_reliability_1_10))+
#   geom_stratum()+
#   scale_fill_viridis_d(begin=1,end=0)+
#   geom_text(stat = "stratum", aes(label = after_stat(paste0(stratum))),
#             min.y=7) +
#   theme(legend.position = "bottom",
#         axis.line = element_blank(),strip.text = element_text(size=12),
#         axis.text.x = element_text(colour="black"))+
#   ggtitle("Were you aware of the use of transmission models in informing public health policy?")

# plot_grid(ggplot(data = reliability_alluvial%>% filter(prior_reliability_1_10!="Did not answer",
#                                                        during_reliability_1_10!="Did not answer"),
#                  aes(axis1 = prior_reliability_1_10, axis2 = during_reliability_1_10,
#                      y = freq, freq2 = freq2))+
#             scale_x_discrete(limits = c("Prior to the COVID-19 pandemic", "During the COVID-19 pandemic"), 
#                              expand = c(.2, .05),
#                              labels = function(x) str_wrap(x, width = 12)) +
#             labs(x="Time period",y="Number of respondents",fill="Reliability score during the pandemic")+
#             theme_classic()+
#             facet_wrap(~platform_label_generic,scales="free_y")+
#             geom_alluvium(aes(fill=during_reliability_1_10))+
#             geom_stratum()+
#             scale_fill_viridis_d(begin=1,end=0)+
#             geom_text(stat = "stratum", aes(label = after_stat(paste0(stratum))),
#                       size=2.5,min.y=4) +
#             theme(legend.position = "bottom",
#                   axis.line = element_blank(),strip.text = element_text(size=12),
#                   axis.text.x = element_text(colour="black"))+
#             ggtitle("On a scale of 1-10 with 1 being “extremely unreliable” and 10 being “extremely \nreliable” how do you feel about the use of transmission models in informing \npublic health policy?"),
#           ggplot(reliability_time_summary,aes(x=difference,y=perc,#x=platform_label,
#                                               group=platform_label_generic,col=platform_label_generic,
#                                               fill=platform_label_generic))+
#             geom_col(position="dodge")+
#             theme_bw()+
#             scale_colour_manual(values=c("#1e2761","#408ec6"))+
#             scale_fill_manual(values=c("#1e2761","#408ec6"))+
#             scale_x_continuous(breaks = seq(-10,10,1))+
#             theme(legend.position="bottom",strip.background.x = element_rect(fill="white"))+
#             labs(x="Difference in reliability scores (during - before)",y="Percentage of \nrespondents (%)",
#                  col="",fill="")+
#             geom_vline(xintercept = 0.5,linetype="dashed")+
#             geom_vline(xintercept = -0.5,linetype="dashed")+
#             annotate("text",x=5,y=40,label="Reliability score increased \nduring the pandemic")+
#             annotate("segment", x = 0.5, xend = 10, y = 40, yend = 40, colour = "black", size=1, alpha=0.6, arrow=arrow())+
#             annotate("text",x=-4.8,y=40,label="Reliability score decreased \nduring the pandemic")+
#             annotate("segment", x = -0.5, xend = -10, y = 40, yend = 40, colour = "black", size=1, alpha=0.6, arrow=arrow()),
#           nrow=2,rel_heights = c(2,1),align="hv",axis="l",labels="AUTO")
# ggsave("outputs/Fig3_updated.png",height=9,width=7)



ggplot(data = reliability_alluvial%>% filter(prior_reliability_1_10!="Did not answer",
                                             during_reliability_1_10!="Did not answer"),
       aes(axis1 = prior_reliability_1_10, axis2 = during_reliability_1_10,
           y = freq, freq2 = freq2))+
  scale_x_discrete(limits = c("Prior to the COVID-19 pandemic", "During the COVID-19 pandemic"), 
                   expand = c(.2, .05),
                   labels = function(x) str_wrap(x, width = 12)) +
  labs(x="Time period",y="Number of respondents",fill="Reliability score during the pandemic")+
  theme_classic()+
  facet_wrap(~platform_label_generic,scales="free_y")+
  geom_alluvium(aes(fill=during_reliability_1_10))+
  geom_stratum()+
  scale_fill_viridis_d(begin=1,end=0)+
  geom_text(stat = "stratum", aes(label = after_stat(paste0(stratum))),
            size=2.5,min.y=4) +
  theme(legend.position = "bottom",
        axis.line = element_blank(),strip.text = element_text(size=12),
        axis.text.x = element_text(colour="black"))+
  ggtitle("On a scale of 1-10 with 1 being “extremely unreliable” and 10 being “extremely \nreliable” how do you feel about the use of transmission models in informing \npublic health policy?")
ggsave("outputs/SupFig5.png",height=6,width=7)



reliability_alluvial_grouped <- all_responses %>% 
  dplyr::select(platform_label_generic,prior_reliability_1_10,during_reliability_1_10) %>%
    mutate(prior_reliability_1_10 = case_when(prior_reliability_1_10 %in% c(1,2) ~ "1-2",
                                              prior_reliability_1_10 %in% c(3,4) ~ "3-4",
                                              prior_reliability_1_10 %in% c(5,6) ~ "5-6",
                                              prior_reliability_1_10 %in% c(7,8) ~ "7-8",
                                              prior_reliability_1_10 %in% c(9,10) ~ "9-10",
                                              is.na(prior_reliability_1_10) ~ "NA"),
           during_reliability_1_10 = case_when(during_reliability_1_10 %in% c(1,2) ~ "1-2",
                                              during_reliability_1_10 %in% c(3,4) ~ "3-4",
                                              during_reliability_1_10 %in% c(5,6) ~ "5-6",
                                              during_reliability_1_10 %in% c(7,8) ~ "7-8",
                                              during_reliability_1_10 %in% c(9,10) ~ "9-10",
                                              is.na(during_reliability_1_10) ~ "NA")) %>%
    group_by(platform_label_generic,prior_reliability_1_10,during_reliability_1_10) %>%
    summarise(freq = length(platform_label_generic)) %>% ungroup() %>%
    group_by(platform_label_generic,prior_reliability_1_10) %>%
    mutate(total = sum(freq),
          freq2 = freq) %>% ungroup() %>%
    group_by(platform_label_generic,during_reliability_1_10) %>%
    mutate(during_total = sum(freq)) %>%
  mutate(prior_reliability_1_10 = factor(ifelse(is.na(prior_reliability_1_10),"Did not answer",
                                                prior_reliability_1_10),
                                         levels=c("1-2","3-4","5-6","7-8","9-10","Did not answer")),
         during_reliability_1_10 = factor(ifelse(is.na(during_reliability_1_10),"Did not answer",
                                                 during_reliability_1_10),
                                          levels=c("1-2","3-4","5-6","7-8","9-10","Did not answer"))) %>%
  ungroup()



ggplot(data = reliability_alluvial_grouped%>% filter(prior_reliability_1_10!="Did not answer",
                                           during_reliability_1_10!="Did not answer"),
       aes(axis1 = prior_reliability_1_10, axis2 = during_reliability_1_10,
           y = freq, freq2 = freq2))+
  scale_x_discrete(limits = c("Prior to the COVID-19 pandemic", "During the COVID-19 pandemic"),
                   expand = c(.2, .05),
                   labels = function(x) str_wrap(x, width = 12)) +
  labs(x="Time period",y="Number of respondents",fill="Reliability score during the pandemic")+
  theme_classic()+
  facet_wrap(~platform_label_generic,scales="free_y")+
  geom_alluvium(aes(fill=during_reliability_1_10))+
  geom_stratum()+
  scale_fill_viridis_d(begin=1,end=0)+
  geom_text(stat = "stratum", aes(label = after_stat(paste0(stratum))),
            min.y=7) +
  theme(legend.position = "bottom",
        axis.line = element_blank(),strip.text = element_text(size=12),
        axis.text.x = element_text(colour="black"))+
  ggtitle("Were you aware of the use of transmission models in informing public health policy?")

plot_grid(ggplot(data = reliability_alluvial_grouped %>% filter(prior_reliability_1_10!="Did not answer",
                                                       during_reliability_1_10!="Did not answer"),
                 aes(axis1 = prior_reliability_1_10, axis2 = during_reliability_1_10,
                     y = freq, freq2 = freq2))+
            scale_x_discrete(limits = c("Prior to the COVID-19 pandemic", "During the COVID-19 pandemic"),
                             expand = c(.2, .05),
                             labels = function(x) str_wrap(x, width = 12)) +
            labs(x="Time period",y="Number of respondents",fill="Reliability score during the pandemic")+
            theme_classic()+
            facet_wrap(~platform_label_generic,scales="free_y")+
            geom_alluvium(aes(fill=during_reliability_1_10))+
            geom_stratum()+
            scale_fill_viridis_d(begin=1,end=0)+
            geom_text(stat = "stratum", aes(label = after_stat(paste0(stratum))),
                      size=2.5,min.y=4) +
            theme(legend.position = "bottom",
                  axis.line = element_blank(),strip.text = element_text(size=12),
                  axis.text.x = element_text(colour="black"))+
            ggtitle("On a scale of 1-10 with 1 being “extremely unreliable” and 10 being “extremely \nreliable” how do you feel about the use of transmission models in informing \npublic health policy?"),
          ggplot(reliability_time_summary,aes(x=difference,y=perc,#x=platform_label,
                                              group=platform_label_generic,col=platform_label_generic,
                                              fill=platform_label_generic))+
            geom_col(position="dodge")+
            theme_bw()+
            scale_colour_manual(values=c("#1e2761","#408ec6"))+
            scale_fill_manual(values=c("#1e2761","#408ec6"))+
            scale_x_continuous(breaks = seq(-10,10,1))+
            theme(legend.position="bottom",strip.background.x = element_rect(fill="white"))+
            labs(x="Difference in reliability scores (during - before)",y="Percentage of \nrespondents (%)",
                 col="",fill="")+
            geom_vline(xintercept = 0.5,linetype="dashed")+
            geom_vline(xintercept = -0.5,linetype="dashed")+
            annotate("text",x=5,y=40,label="Reliability score increased \nduring the pandemic")+
            annotate("segment", x = 0.5, xend = 10, y = 40, yend = 40, colour = "black", size=1, alpha=0.6, arrow=arrow())+
            annotate("text",x=-4.8,y=40,label="Reliability score decreased \nduring the pandemic")+
            annotate("segment", x = -0.5, xend = -10, y = 40, yend = 40, colour = "black", size=1, alpha=0.6, arrow=arrow()),
          nrow=2,rel_heights = c(2,1),align="hv",axis="l",labels="AUTO")
ggsave("outputs/Fig3.png",height=9,width=7)



## Supplementary Table 8

reliability_time_summary %>%
  mutate(difference_label = ifelse(difference==0,"Same score",
                                   ifelse(difference>0,"Increased reliability",
                                          ifelse(difference<0,"Decreased reliability",NA)))) %>%
  group_by(platform_label_generic,difference_label) %>%
  summarise(count = sum(count)) %>%
  mutate(platform_sample_size = ifelse(platform_label_generic=="Online panel",504,202),
         perc = count/platform_sample_size *100)




## Supplementary Table 9
m1 <- lm(prior_reliability_1_10~age_group+gender,
         data = reliability_time %>% filter(platform_label=="Prolific Academic"))
summary(m1)
anova(m1,test="Chisq")

m2 <- lm(prior_reliability_1_10~age_group+gender,
         data = reliability_time %>% filter(platform_label=="Twitter"))
summary(m2)
anova(m2,test="Chisq")

m3 <- lm(during_reliability_1_10~age_group+gender,
         data = reliability_time %>% filter(platform_label=="Prolific Academic"))
summary(m3)
anova(m3,test="Chisq")

m4 <- lm(during_reliability_1_10~age_group+gender,
         data = reliability_time %>% filter(platform_label=="Twitter"))
summary(m4)
anova(m4,test="Chisq")

## Supplementary Table 10 (UPDATED)

reliability_time <- reliability_time %>%
  mutate(prior_awareness_use_in_policy_numeric = ifelse(prior_awareness_use_in_policy=="Yes",1,
                                                        ifelse(prior_awareness_use_in_policy=="Unsure",0,
                                                               ifelse(prior_awareness_use_in_policy=="No",-1,NA))),
         during_awareness_use_in_policy_numeric = ifelse(during_awareness_use_in_policy=="Yes",1,
                                                        ifelse(during_awareness_use_in_policy=="Unsure",0,
                                                               ifelse(during_awareness_use_in_policy=="No",-1,NA))))

m5 <- lm(prior_reliability_1_10~age_group+gender+prior_awareness_use_in_policy_numeric,
         data = reliability_time %>% filter(platform_label=="Prolific Academic"))
summary(m5)
anova(m5,test="Chisq")

m6 <- lm(prior_reliability_1_10~age_group+gender+prior_awareness_use_in_policy_numeric,
         data = reliability_time %>% filter(platform_label=="Twitter"))
summary(m6)
anova(m6,test="Chisq")

m7 <- lm(during_reliability_1_10~age_group+gender+during_awareness_use_in_policy_numeric,
         data = reliability_time %>% filter(platform_label=="Prolific Academic"))
summary(m7)
anova(m7,test="Chisq")

m8 <- lm(during_reliability_1_10~age_group+gender+during_awareness_use_in_policy_numeric,
         data = reliability_time %>% filter(platform_label=="Twitter"))
summary(m8)
anova(m8,test="Chisq")



## Supplementary Figure 6

ggplot(reliability_time,aes(x=prior_reliability_1_10,y=during_reliability_1_10,col=platform_label_generic,
                            fill=platform_label_generic))+
  geom_jitter(shape=1)+
  geom_smooth(method="lm")+
  theme_bw()+
  facet_wrap(~platform_label_generic)+
  scale_color_manual(values=c("#1e2761","#408ec6"))+
  scale_fill_manual(values=c("#1e2761","#408ec6"))+
  theme(legend.position = "bottom",
        strip.background = element_rect(fill="white"))+
  labs(x="Prior to the COVID-19 pandemic, on a scale of 1 – 10 with 1 \nbeing “extremely unreliable” and 10 being “extremely reliable” \nhow did you feel about the use of transmission models in \ninforming public health policy?",
       y="During the COVID-19 pandemic, on a scale of 1 – 10 \nwith 1 being “extremely unreliable” and 10 being \n “extremely reliable” how did you feel about the \nuse of transmission models in informing public \nhealth policy?",
       col="",fill="")+
  scale_x_continuous(n.breaks = 10)+
  scale_y_continuous(n.breaks = 10)+
  geom_abline(slope=1,intercept = 0,linetype="dashed")
ggsave("outputs/SupFig6.png",width=6,height=5)


## Supplementary Table 11
m9 <- lm(during_reliability_1_10~prior_reliability_1_10,data=reliability_time %>%
           filter(platform_label=="Prolific Academic"))
summary(m9)

m10 <- lm(during_reliability_1_10~prior_reliability_1_10,data=reliability_time %>%
           filter(platform_label=="Twitter"))
summary(m10)




## Supplementary Table 12
m11 <- lm(difference~age_group+gender,data=reliability_time %>% filter(platform_label=="Prolific Academic"))
summary(m11)
anova(m11,test="Chisq")


m12 <- lm(difference~age_group+gender,data=reliability_time %>% filter(platform_label=="Twitter"))
summary(m12)
anova(m12,test="Chisq")


## Supplementary Figure 7
reliability_time <- reliability_time %>%
  mutate(prior_awareness_use_in_policy_label = ifelse(prior_awareness_use_in_policy=="No",
                                                      "Self-reported no awareness",
                                                      ifelse(prior_awareness_use_in_policy=="Yes",
                                                             "Self-reported awareness",
                                                             ifelse(prior_awareness_use_in_policy=="Unsure",
                                                                    "Self-reported unsure",NA))),
         during_awareness_use_in_policy_label = ifelse(during_awareness_use_in_policy=="No",
                                                       "Self-reported no awareness",
                                                       ifelse(during_awareness_use_in_policy=="Yes",
                                                              "Self-reported awareness",
                                                              ifelse(during_awareness_use_in_policy=="Unsure",
                                                                     "Self-reported unsure",NA))))

reliability_time_melt <- rbind(reliability_time %>% select(-platform_sample_size,-during_awareness_use_in_policy,
                                                           -during_awareness_use_in_policy_label,
                                                           -during_reliability_1_10) %>%
                                 rename(awareness_use_in_policy = prior_awareness_use_in_policy,
                                        reliability_1_10 = prior_reliability_1_10,
                                        awareness_use_in_policy_label = prior_awareness_use_in_policy_label) %>%
                                 mutate(time="Prior to the COVID-19 pandemic"),
                               reliability_time %>% select(-platform_sample_size,-prior_awareness_use_in_policy,
                                                           -prior_awareness_use_in_policy_label,
                                                           -prior_reliability_1_10) %>%
                                 rename(awareness_use_in_policy = during_awareness_use_in_policy,
                                        reliability_1_10 = during_reliability_1_10,
                                        awareness_use_in_policy_label = during_awareness_use_in_policy_label) %>%
                                 mutate(time="During the COVID-19 pandemic")) %>%
  mutate(time = factor(time,
                       levels=c("Prior to the COVID-19 pandemic","During the COVID-19 pandemic")),
         awareness_use_in_policy_label = factor(awareness_use_in_policy_label,
                                                levels=c("Self-reported no awareness","Self-reported unsure",
                                                         "Self-reported awareness")))


ggplot(reliability_time_melt %>% filter(!is.na(awareness_use_in_policy_label)),
       aes(y=reliability_1_10,x=time,group=time,col=time))+
  geom_jitter(alpha=0.5,shape=1,height=0.15)+
  geom_boxplot(position="dodge",lwd=1.5,outlier.shape = NA,fill=NA)+
  facet_grid(platform_label_generic~awareness_use_in_policy_label)+
  theme_bw()+
  scale_colour_manual(values=c("#a8c66c","#1b6535"))+
  scale_y_continuous(n.breaks=10) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(legend.position="bottom",strip.background = element_rect(fill="white"))+
  labs(x="Time period",y="On a scale of 1-10 with 1 being “extremely unreliable” \nand 10 being “extremely reliable” how do you feel \nabout the use of transmission models in informing \npublic health policy?",col="")
ggsave("outputs/SupFig7.png",width=6,height=5)


## tests for significance 
wilcox.test(reliability_time_melt %>% filter(platform_label=="Prolific Academic",
                                             time =="Prior to the COVID-19 pandemic",
                                             awareness_use_in_policy_label=="Self-reported awareness") %>%
              select(reliability_1_10) %>% unlist() %>% as.numeric(),
            reliability_time_melt %>% filter(platform_label=="Prolific Academic",
                                             time =="Prior to the COVID-19 pandemic",
                                             awareness_use_in_policy_label=="Self-reported no awareness") %>%
              select(reliability_1_10) %>% unlist() %>% as.numeric(),
            paired=FALSE,
            alternative="two.sided")

wilcox.test(reliability_time_melt %>% filter(platform_label=="Prolific Academic",
                                             time =="During the COVID-19 pandemic",
                                             awareness_use_in_policy_label=="Self-reported awareness") %>%
              select(reliability_1_10)  %>% unlist() %>% as.numeric(),
            reliability_time_melt %>% filter(platform_label=="Prolific Academic",
                                             time =="During the COVID-19 pandemic",
                                             awareness_use_in_policy_label=="Self-reported no awareness") %>%
              select(reliability_1_10)  %>% unlist() %>% as.numeric(),
            paired=FALSE,
            alternative="two.sided")


wilcox.test(reliability_time_melt %>% filter(platform_label=="Twitter",
                                             time =="Prior to the COVID-19 pandemic",
                                             awareness_use_in_policy_label=="Self-reported awareness") %>%
              select(reliability_1_10) %>% unlist() %>% as.numeric(),
            reliability_time_melt %>% filter(platform_label=="Twitter",
                                             time =="Prior to the COVID-19 pandemic",
                                             awareness_use_in_policy_label=="Self-reported no awareness") %>%
              select(reliability_1_10) %>% unlist() %>% as.numeric(),
            paired=FALSE,
            alternative="two.sided")



wilcox.test(reliability_time_melt %>% filter(platform_label=="Twitter",
                                             time =="During the COVID-19 pandemic",
                                             awareness_use_in_policy_label=="Self-reported awareness") %>%
              select(reliability_1_10) %>% unlist() %>% as.numeric(),
            reliability_time_melt %>% filter(platform_label=="Twitter",
                                             time =="During the COVID-19 pandemic",
                                             awareness_use_in_policy_label=="Self-reported no awareness") %>%
              select(reliability_1_10) %>% unlist() %>% as.numeric(),
            paired=FALSE,
            alternative="two.sided")

## Supplementary Figure 8
reliability_during_level_awareness <- all_responses %>% dplyr::select(platform_label_generic,
                                                                      during_reliability_1_10,
                                                                      during_level_of_awareness) %>%
  mutate(during_level_of_awareness_label = ifelse(during_level_of_awareness=="Too little","Too little knowledge",
                                                  ifelse(during_level_of_awareness=="Too much",
                                                         "Too much knowledge",
                                                         ifelse(during_level_of_awareness=="About right",
                                                                "About right knowledge",NA))),
         during_level_of_awareness_label = factor(during_level_of_awareness_label,
                                                  levels=c("Too little knowledge","About right knowledge",
                                                           "Too much knowledge")))


ggplot(reliability_during_level_awareness %>% filter(!is.na(during_level_of_awareness_label)),
       aes(y=during_reliability_1_10,x=platform_label_generic,
           group=platform_label_generic,col=platform_label_generic))+
  geom_jitter(alpha=0.5,shape=1,height=0.15)+
  geom_boxplot(position="dodge",lwd=1.5,outlier.shape = NA,fill=NA)+
  facet_grid(~during_level_of_awareness_label,scales="free_x")+
  theme_bw()+
  scale_colour_manual(values=c("#1e2761","#408ec6"))+
  scale_y_continuous(n.breaks=10) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(legend.position="bottom",strip.background = element_rect(fill="white"))+
  labs(x="Sample",y="During the pandemic, on a scale of 1-10 with 1 being \n“extremely unreliable” and 10 being “extremely reliable” \nhow do you feel about the use of transmission models \nin informing public health policy?",col="")
ggsave("outputs/SupFig8.png",width=6,height=5)


## tests for significance 
wilcox.test(reliability_during_level_awareness %>% filter(platform_label=="Prolific Academic",
                                             during_level_of_awareness_label=="About right knowledge") %>%
              select(during_reliability_1_10) %>% unlist() %>% as.numeric(),
            reliability_during_level_awareness %>% filter(platform_label=="Prolific Academic",
                                             during_level_of_awareness_label=="Too little knowledge") %>%
              select(during_reliability_1_10) %>% unlist() %>% as.numeric(),
            paired=FALSE,
            alternative="two.sided")

wilcox.test(reliability_during_level_awareness %>% 
              filter(platform_label=="Twitter",during_level_of_awareness_label=="About right knowledge") %>%
              select(during_reliability_1_10) %>% unlist() %>% as.numeric(),
            reliability_during_level_awareness %>% 
              filter(platform_label=="Twitter",during_level_of_awareness_label=="Too little knowledge") %>%
              select(during_reliability_1_10) %>% unlist() %>% as.numeric(),
            paired=FALSE,
            alternative="two.sided")



wilcox.test(reliability_during_level_awareness %>% filter(platform_label=="Prolific Academic",
                                                      during_level_of_awareness_label=="About right knowledge") %>%
              select(during_reliability_1_10) %>% unlist() %>% as.numeric(),
            reliability_during_level_awareness %>% filter(platform_label=="Prolific Academic",
                                                          during_level_of_awareness_label=="Too much knowledge") %>%
              select(during_reliability_1_10) %>% unlist() %>% as.numeric(),
            paired=FALSE,
            alternative="two.sided")

wilcox.test(reliability_during_level_awareness %>% 
              filter(platform_label=="Twitter",during_level_of_awareness_label=="About right knowledge") %>%
              select(during_reliability_1_10) %>% unlist() %>% as.numeric(),
            reliability_during_level_awareness %>% 
              filter(platform_label=="Twitter",during_level_of_awareness_label=="Too much knowledge") %>%
              select(during_reliability_1_10) %>% unlist() %>% as.numeric(),
            paired=FALSE,
            alternative="two.sided")


## Supplementary Figure 9

library(plyr)

reliability_how <- all_responses %>% select(platform_label,ID,
                                            prior_reliability_1_10,prior_awareness_how,
                                            during_reliability_1_10,during_awareness_how)


prior_how_list <- strsplit(as.matrix(reliability_how %>%
                                       select(prior_awareness_how)),";")
prior_how_list2 <- lapply(lapply(prior_how_list, unlist), function(x) {
  names(x)[names(x) == "prior_awareness_how"] <- "prior_awareness_how1"
  data.frame(t(x))
})
prior_matrix_to_bind <- rbind.fill(prior_how_list2)
colnames(prior_matrix_to_bind) <- paste("awareness_how",1:ncol(prior_matrix_to_bind),sep="")

prior_reliability_how_wide <- cbind(reliability_how %>% select(platform_label,ID,prior_reliability_1_10,
                                                               prior_awareness_how),
                                    prior_matrix_to_bind)

prior_reliability_how_wide_melt <- gather(prior_reliability_how_wide,
                                          key="awareness_how",value="how",
                                          awareness_how1,awareness_how2,awareness_how3,awareness_how4,awareness_how5) %>%
  select(-prior_awareness_how) %>%
  mutate(how = factor(how,
                      levels=c("I was not aware","Newspaper (online or print)","News show (TV or online)",
                               "Social media","Internet search","Academic reports and papers","Other")),
         time="Prior to the COVID-19 pandemic")

during_how_list <- strsplit(as.matrix(reliability_how %>%
                                        select(during_awareness_how)),";")
during_how_list2 <- lapply(lapply(during_how_list, unlist), function(x) {
  names(x)[names(x) == "during_awareness_how"] <- "during_awareness_how1"
  data.frame(t(x))
})
during_matrix_to_bind <- rbind.fill(during_how_list2)
colnames(during_matrix_to_bind) <- paste("awareness_how",1:ncol(during_matrix_to_bind),sep="")

during_reliability_how_wide <- cbind(reliability_how %>% select(platform_label,ID,during_reliability_1_10,
                                                                during_awareness_how),
                                     during_matrix_to_bind)

during_reliability_how_wide_melt <- gather(during_reliability_how_wide,
                                           key="awareness_how",value="how",
                                           awareness_how1,awareness_how2,awareness_how3,awareness_how4,awareness_how5,awareness_how6) %>%
  select(-during_awareness_how) %>%
  mutate(how = ifelse(how=="Other ","Other",how)) %>%
  mutate(how = factor(how,
                      levels=c("I was not aware","Newspaper (online or print)","News show (TV or online)",
                               "Social media","Internet search","Academic reports and papers","Other")),
         time="During the COVID-19 pandemic")

reliability_how_wide_melt <- rbind(prior_reliability_how_wide_melt %>%
                                     dplyr::rename(reliability_1_10 = prior_reliability_1_10),
                                   during_reliability_how_wide_melt %>%
                                     dplyr::rename(reliability_1_10 = during_reliability_1_10)) %>%
  mutate(time = factor(time,
                       levels=c("Prior to the COVID-19 pandemic","During the COVID-19 pandemic")))

reliability_how_wide_melt <- reliability_how_wide_melt %>% 
  mutate(platform_label_generic = ifelse(platform_label=="Prolific Academic","Online panel","Social media"))


ggplot(reliability_how_wide_melt %>% filter(!is.na(how)),
       aes(x=how,y=reliability_1_10,col=time))+
  geom_jitter(alpha=0.5,shape=1,height=0.15)+
  geom_boxplot(position="dodge",lwd=1.5,outlier.shape = NA,fill=NA)+
  theme_bw()+
  facet_grid(platform_label_generic~time)+
  scale_y_continuous(n.breaks=10) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_colour_manual(values=c("#a8c66c","#1b6535"))+
  theme(strip.background = element_rect(fill="white"),
        legend.position = "bottom")+
  labs(x="How were you aware of transmission modelling? \nPlease select all that apply.",
       y="On a scale of 1-10 with 1 being “extremely \nunreliable” and 10 being “extremely reliable” \nhow do you feel about the use of transmission \nmodels in informing public health policy?",col="")
ggsave("outputs/SupFig9.png",width=9,height=7)


detach("package:plyr", unload=TRUE)
