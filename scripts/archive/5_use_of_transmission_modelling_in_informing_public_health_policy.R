### The use of transmission modelling in informing public health policy

## Questions covered:
## Were you aware of the use of transmission models in informing public health policy? (prior and during)
## On a scale of 1 - 10 with 1 being "extremely unreliable" and 10 being "extremely reliable", how did you feel about the use of transmission models in informing public health policy? (prior and during)

library(readxl)
library(tidyverse)

prolific <- read_excel("Data/responses_prolific.xlsx") %>% data.frame() %>%
  select(-c(Start.time,Completion.time,Email,consent_1,consent_2))

twitter <- read_excel("Data/responses_twitter.xlsx") %>% data.frame() %>%
  select(-c(Start.time,Completion.time,Email,consent_1,consent_2))

all_responses <- rbind(prolific,twitter) %>% group_by(platform) %>%
  mutate("platform_label"=ifelse(platform=="twitter","Twitter","Prolific Academic"),
         "platform_sample_size"=length(platform),
         "unique_ID"=paste0(platform,"_",ID)) %>% data.frame() %>%
  filter(!is.na(gender),!is.na(age_group),!is.na(sector),!is.na(vaccinated)) 


### Supplementary Figure 7
awareness_use_in_policy <- rbind(
  all_responses %>% dplyr::select(platform_label,prior_awareness_use_in_policy) %>%
    mutate(time = "Prior to the COVID-19 pandemic") %>%
    rename(awareness_use_in_policy = prior_awareness_use_in_policy),
  all_responses %>% dplyr::select(platform_label,during_awareness_use_in_policy) %>%
    mutate(time = "During the COVID-19 pandemic") %>%
    rename(awareness_use_in_policy = during_awareness_use_in_policy)
) %>% group_by(platform_label,time,awareness_use_in_policy) %>%
  summarise(count = length(awareness_use_in_policy)) %>%
  group_by(platform_label,time) %>%
  mutate(total = sum(count),
         percentage = count/total *100,
         awareness_use_in_policy = ifelse(is.na(awareness_use_in_policy),
                                          "Did not answer",awareness_use_in_policy),
         time = factor(time,
                       levels=c("Prior to the COVID-19 pandemic","During the COVID-19 pandemic")),
         awareness_use_in_policy = factor(awareness_use_in_policy,
                                          levels=c("No","Unsure","Yes","Did not answer")))


ggplot(awareness_use_in_policy,
       aes(x=time,fill=awareness_use_in_policy))+
  geom_col(aes(y=percentage))+
  theme_bw()+
  facet_wrap(~platform_label)+
  scale_fill_viridis_d(option="magma",end=0.6)+
  scale_size_area(max_size = 20)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12))+
  labs(x="Time period",
       y="Percentage of respondents (%)",
       fill="Were you are aware of \nthe use of transmission \nmodels in informing \npublic health policy?")+
  theme(strip.background = element_rect(fill = "white", color = "black"))+
  guides(color=guide_legend(order=1),fill=guide_legend(order=1))
ggsave("Plots/SupFig7.png",width=7,height=5)


## Table 1
table(all_responses$platform_label,all_responses$prior_reliability_1_10)
table(all_responses$platform_label,all_responses$during_reliability_1_10)

prop.table(table(all_responses$platform_label,all_responses$prior_reliability_1_10),1)*100
prop.table(table(all_responses$platform_label,all_responses$during_reliability_1_10),1)*100

## Supplementary Figure 8
all_responses_reliability_melt <- rbind(all_responses %>%
                                          dplyr::select(prior_awareness_use_in_policy,prior_reliability_1_10,
                                                        platform_label,
                                                        age_group,gender,sector,vaccinated) %>%
                                          mutate("time" = "Prior to the COVID-19 pandemic") %>%
                                          rename(awareness_use_in_policy = prior_awareness_use_in_policy,
                                                 reliability_1_10 = prior_reliability_1_10),
                                        all_responses %>%
                                          dplyr::select(during_awareness_use_in_policy,during_reliability_1_10,
                                                        platform_label,
                                                        age_group,gender,sector,vaccinated) %>%
                                          mutate("time" = "During the COVID-19 pandemic") %>%
                                          rename(awareness_use_in_policy = during_awareness_use_in_policy,
                                                 reliability_1_10 = during_reliability_1_10)) %>%
  group_by(platform_label,time) %>%
  mutate(platform_sample_size = length(platform_label),
         time = factor(time,
                       levels=c("Prior to the COVID-19 pandemic",
                                "During the COVID-19 pandemic"))) %>% data.frame()

ggplot(all_responses_reliability_melt,
       aes(y=reliability_1_10,x=time,group=time,col=time))+
  geom_jitter(alpha=0.5,shape=1)+
  geom_boxplot(position="dodge",lwd=1.5,outlier.shape = NA,fill=NA)+
  facet_wrap(~platform_label)+
  theme_bw()+
  scale_colour_manual(values=c("#a8c66c","#1b6535"))+
  scale_y_continuous(n.breaks=10) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(legend.position="bottom",strip.background.x = element_rect(fill="white"))+
  labs(x="Time period",y="On a scale of 1-10 with 1 being “extremely unreliable” and 10 \nbeing “extremely reliable” how do you feel about the use of \ntransmission models in informing public health policy?",col="")
ggsave("Plots/SupFig8.png",width=6,height=6)


## summary statistics and tests for significance
all_responses %>% filter(platform=="prolific") %>% select(prior_reliability_1_10) %>% summary()
all_responses %>% filter(platform=="twitter") %>% select(prior_reliability_1_10) %>% summary()

wilcox.test(x= all_responses %>% filter(platform=="prolific") %>% select(prior_reliability_1_10) %>%
              unlist() %>% as.numeric(),
            y= all_responses %>% filter(platform=="twitter") %>% select(prior_reliability_1_10) %>%
              unlist() %>% as.numeric(),
            paired=FALSE,
            alternative="two.sided")

all_responses %>% filter(platform=="prolific") %>% select(during_reliability_1_10) %>% summary()
all_responses %>% filter(platform=="twitter") %>% select(during_reliability_1_10) %>% summary()

wilcox.test(x= all_responses %>% filter(platform=="prolific") %>% select(during_reliability_1_10) %>%
              unlist() %>% as.numeric(),
            y= all_responses %>% filter(platform=="twitter") %>% select(during_reliability_1_10) %>%
              unlist() %>% as.numeric(),
            paired=FALSE,
            alternative="two.sided")

## Supplementary Table 6
reliability_time <- all_responses %>% dplyr::select(ID,platform_label,platform_sample_size,
                                                    prior_awareness_use_in_policy,prior_reliability_1_10,
                                                    during_awareness_use_in_policy,during_reliability_1_10,
                                                    during_level_of_awareness,
                                                    age_group,gender,sector,vaccinated) %>%
  mutate(difference = during_reliability_1_10 - prior_reliability_1_10)

m1 <- lm(prior_reliability_1_10~age_group+gender,
         data = reliability_time %>% filter(platform_label=="Prolific Academic"))
summary(m1)

m2 <- lm(prior_reliability_1_10~age_group+gender,
         data = reliability_time %>% filter(platform_label=="Twitter"))
summary(m2)

m3 <- lm(during_reliability_1_10~age_group+gender,
         data = reliability_time %>% filter(platform_label=="Prolific Academic"))
summary(m3)

m4 <- lm(during_reliability_1_10~age_group+gender,
         data = reliability_time %>% filter(platform_label=="Twitter"))
summary(m4)


## Supplementary Figure 9
ggplot(reliability_time,aes(x=prior_reliability_1_10,y=during_reliability_1_10,col=platform_label,
                            fill=platform_label))+
  geom_jitter(shape=1)+
  geom_smooth(method="lm")+
  theme_bw()+
  facet_wrap(~platform_label)+
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
ggsave("Plots/SupFig9.png",width=6,height=5)


## Supplementary Table 7
m5 <- lm(during_reliability_1_10~prior_reliability_1_10,data=reliability_time %>%
           filter(platform_label=="Prolific Academic"))
summary(m5)

m6 <- lm(during_reliability_1_10~prior_reliability_1_10,data=reliability_time %>%
           filter(platform_label=="Twitter"))
summary(m6)

## Supplementary Figure 10
reliability_time_summary <- reliability_time %>% group_by(platform_label) %>%
  mutate(platform_sample_size = length(platform_label)) %>%
  group_by(platform_label,difference) %>%
  summarise(count = length(difference)) %>%
  mutate(platform_sample_size = ifelse(platform_label=="Prolific Academic",
                                       504,202),
         perc = count/platform_sample_size * 100)


ggplot(reliability_time_summary,aes(x=difference,y=perc,#x=platform_label,
                                    group=platform_label,col=platform_label,fill=platform_label))+
  geom_col(position="dodge")+
  theme_bw()+
  scale_colour_manual(values=c("#1e2761","#408ec6"))+
  scale_fill_manual(values=c("#1e2761","#408ec6"))+
  scale_x_continuous(breaks = seq(-10,10,1))+
  theme(legend.position="bottom",strip.background.x = element_rect(fill="white"))+
  labs(x="Difference in reliability scores (during - before)",y="Percentage of respondents (%)",col="",fill="")+
  geom_vline(xintercept = 0.5,linetype="dashed")+
  geom_vline(xintercept = -0.5,linetype="dashed")+
  annotate("text",x=5,y=40,label="Reliability score increased \nduring the pandemic")+
  annotate("segment", x = 0.5, xend = 10, y = 40, yend = 40, colour = "black", size=1, alpha=0.6, arrow=arrow())+
  annotate("text",x=-5,y=40,label="Reliability score decreased \nduring the pandemic")+
  annotate("segment", x = -0.5, xend = -10, y = 40, yend = 40, colour = "black", size=1, alpha=0.6, arrow=arrow())
ggsave("Plots/SupFig10.png",width=6,height=4)


## summarise into neutral, positive, negative

reliability_time_summary %>%
  mutate(difference_label = ifelse(difference==0,"Same score",
                                   ifelse(difference>0,"Increased reliability",
                                          ifelse(difference<0,"Decreased reliability",NA)))) %>%
  group_by(platform_label,difference_label) %>%
  summarise(count = sum(count)) %>%
  mutate(platform_sample_size = ifelse(platform_label=="Prolific Academic",504,202),
         perc = count/platform_sample_size *100)



## Supplementary Table 8
m7 <- lm(difference~age_group+gender,data=reliability_time %>% filter(platform_label=="Prolific Academic"))
summary(m7)

m8 <- lm(difference~age_group+gender,data=reliability_time %>% filter(platform_label=="Twitter"))
summary(m8)



## Supplementary Figure 11
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
  geom_jitter(alpha=0.5,shape=1)+
  geom_boxplot(position="dodge",lwd=1.5,outlier.shape = NA,fill=NA)+
  facet_grid(platform_label~awareness_use_in_policy_label)+
  theme_bw()+
  scale_colour_manual(values=c("#a8c66c","#1b6535"))+
  scale_y_continuous(n.breaks=10) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(legend.position="bottom",strip.background = element_rect(fill="white"))+
  labs(x="Time period",y="On a scale of 1-10 with 1 being “extremely unreliable” \nand 10 being “extremely reliable” how do you feel \nabout the use of transmission models in informing \npublic health policy?",col="")
ggsave("Plots/SupFig11.png",width=6,height=5)


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




## Supplementary Table 9
m9 <- lm(reliability_1_10~platform_label+time+awareness_use_in_policy_label+platform_label*time+platform_label*awareness_use_in_policy_label+time*awareness_use_in_policy_label,
         data = reliability_time_melt)
summary(m9)



## Supplementary Figure 12
reliability_during_level_awareness <- all_responses %>% dplyr::select(platform_label,during_reliability_1_10,
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
       aes(y=during_reliability_1_10,x=platform_label,group=platform_label,col=platform_label))+
  geom_jitter(alpha=0.5,shape=1)+
  geom_boxplot(position="dodge",lwd=1.5,outlier.shape = NA,fill=NA)+
  facet_grid(~during_level_of_awareness_label,scales="free_x")+
  theme_bw()+
  scale_colour_manual(values=c("#1e2761","#408ec6"))+
  scale_y_continuous(n.breaks=10) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(legend.position="bottom",strip.background = element_rect(fill="white"))+
  labs(x="Sample",y="During the pandemic, on a scale of 1-10 with 1 being \n“extremely unreliable” and 10 being “extremely reliable” \nhow do you feel about the use of transmission models \nin informing public health policy?",col="")
ggsave("Plots/SupFig12.png",width=6,height=5)

## Supplementary Figure 13

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


ggplot(reliability_how_wide_melt %>% filter(!is.na(how)),
       aes(x=how,y=reliability_1_10,col=time))+
  geom_jitter(alpha=0.5,shape=1)+
  geom_boxplot(position="dodge",lwd=1.5,outlier.shape = NA,fill=NA)+
  theme_bw()+
  facet_grid(platform_label~time)+
  scale_y_continuous(n.breaks=10) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  scale_colour_manual(values=c("#a8c66c","#1b6535"))+
  theme(strip.background = element_rect(fill="white"),
        legend.position = "bottom")+
  labs(x="How were you aware of transmission modelling? \nPlease select all that apply.",
       y="On a scale of 1-10 with 1 being “extremely \nunreliable” and 10 being “extremely reliable” \nhow do you feel about the use of transmission \nmodels in informing public health policy?",col="")
ggsave("Plots/SupFig13.png",width=9,height=7)


detach("package:plyr", unload=TRUE)




