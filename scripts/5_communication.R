### Responsibility of communicating modelling to the public 

## Questions covered:
## Who do you think has the responsibility of ensuring that the public are informed about the use of modelling in policy decisions, particularly in the COVID-19 pandemic?

library(readxl)
library(tidyverse)

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
responsibility_to_inform <- rbind(data.frame(cbind(
  "response"=rownames(as.matrix(unlist(strsplit(as.matrix(all_responses %>% filter(platform=="prolific") %>%
                                                            select(responsibility_to_inform)),";")) %>%
                                  table(),ncol=1)),
  "count"=as.matrix(unlist(strsplit(as.matrix(all_responses %>%filter(platform=="prolific") %>%
                                                select(responsibility_to_inform)),";")) %>%
                      table(),ncol=1))) %>%
    rename("count"="V2") %>%
    mutate("platform"="prolific",
           "platform_label"="Prolific Academic",
           "platform_sample_size"=nrow(all_responses %>% filter(platform=="prolific"))),
  data.frame(cbind(
    "response"=rownames(as.matrix(unlist(strsplit(as.matrix(all_responses %>% filter(platform=="twitter") %>%
                                                              select(responsibility_to_inform)),";")) %>%
                                    table(),ncol=1)),
    "count"=as.matrix(unlist(strsplit(as.matrix(all_responses %>%filter(platform=="twitter") %>%
                                                  select(responsibility_to_inform)),";")) %>%
                        table(),ncol=1))) %>%
    rename("count"="V2") %>%
    mutate("platform"="twitter",
           "platform_label"="Twitter",
           "platform_sample_size"=nrow(all_responses %>% filter(platform=="twitter"))))
rownames(responsibility_to_inform) <- c()

responsibility_to_inform <- responsibility_to_inform %>%
  mutate(count = as.numeric(count),
         percentage = count/platform_sample_size *100,
         response = factor(response,
                           levels=c("The government","Those who develop transmission models",
                                    "Those who use transmission models","The media",
                                    "Myself, as a member of the public","None of the above")))


responsibility_to_inform


## Supplementary Figure 13
responsibility_to_inform <- responsibility_to_inform %>%
  mutate(platform_label_generic = ifelse(platform=="prolific","Online panel","Social media"))

ggplot(responsibility_to_inform,
       aes(x=response,y=percentage,fill=platform_label_generic))+
  geom_col(position = "dodge")+
  theme_bw()+
  #facet_wrap(~platform_label)+
  scale_fill_manual(values=c("#1e2761","#408ec6"))+
  scale_y_continuous(n.breaks=6)+
  theme(legend.position = "bottom",strip.background = element_rect(fill="white"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12))+
  labs(y="Percentage of respondents (%)",
       x="Who do you think has the responsibility of ensuring that the public \nare informed about the use of modelling in policy decisions, \nparticularly in the COVID-19 pandemic?",fill="")
ggsave("outputs/SupFig13.png",height=4,width=5)


## Supplementary Table 23
prop.test(x=c(responsibility_to_inform %>% filter(platform_label=="Prolific Academic",
                                                  response=="The government") %>% select(count) %>% unlist(),
              responsibility_to_inform %>% filter(platform_label=="Twitter",
                                                  response=="The government") %>% select(count) %>% unlist()),
          n=c(responsibility_to_inform %>% filter(platform_label=="Prolific Academic",
                                                  response=="The government") %>% select(platform_sample_size) %>%
                unlist(),
              responsibility_to_inform %>% filter(platform_label=="Twitter",
                                                  response=="The government") %>% select(platform_sample_size) %>%
                unlist()))

prop.test(x=c(responsibility_to_inform %>% filter(platform_label=="Prolific Academic",
                                                  response=="Those who develop transmission models") %>% 
                select(count) %>% unlist(),
              responsibility_to_inform %>% filter(platform_label=="Twitter",
                                                  response=="Those who develop transmission models") %>%
                select(count) %>% unlist()),
          n=c(responsibility_to_inform %>% filter(platform_label=="Prolific Academic",
                                                  response=="Those who develop transmission models") %>%
                select(platform_sample_size) %>% unlist(),
              responsibility_to_inform %>% filter(platform_label=="Twitter",
                                                  response=="Those who develop transmission models") %>% 
                select(platform_sample_size) %>% unlist()))

prop.test(x=c(responsibility_to_inform %>% filter(platform_label=="Prolific Academic",
                                                  response=="Those who use transmission models") %>% 
                select(count) %>% unlist(),
              responsibility_to_inform %>% filter(platform_label=="Twitter",
                                                  response=="Those who use transmission models") %>%
                select(count) %>% unlist()),
          n=c(responsibility_to_inform %>% filter(platform_label=="Prolific Academic",
                                                  response=="Those who use transmission models") %>%
                select(platform_sample_size) %>% unlist(),
              responsibility_to_inform %>% filter(platform_label=="Twitter",
                                                  response=="Those who use transmission models") %>% 
                select(platform_sample_size) %>% unlist()))

prop.test(x=c(responsibility_to_inform %>% filter(platform_label=="Prolific Academic",
                                                  response=="The media") %>% select(count) %>% unlist(),
              responsibility_to_inform %>% filter(platform_label=="Twitter",
                                                  response=="The media") %>% select(count) %>% unlist()),
          n=c(responsibility_to_inform %>% filter(platform_label=="Prolific Academic",
                                                  response=="The media") %>% select(platform_sample_size) %>%
                unlist(),
              responsibility_to_inform %>% filter(platform_label=="Twitter",
                                                  response=="The media") %>% select(platform_sample_size) %>%
                unlist()))

prop.test(x=c(responsibility_to_inform %>% filter(platform_label=="Prolific Academic",
                                                  response=="Myself, as a member of the public") %>% 
                select(count) %>% unlist(),
              responsibility_to_inform %>% filter(platform_label=="Twitter",
                                                  response=="Myself, as a member of the public") %>%
                select(count) %>% unlist()),
          n=c(responsibility_to_inform %>% filter(platform_label=="Prolific Academic",
                                                  response=="Myself, as a member of the public") %>%
                select(platform_sample_size) %>% unlist(),
              responsibility_to_inform %>% filter(platform_label=="Twitter",
                                                  response=="Myself, as a member of the public") %>% 
                select(platform_sample_size) %>% unlist()))

prop.test(x=c(responsibility_to_inform %>% filter(platform_label=="Prolific Academic",
                                                  response=="None of the above") %>% 
                select(count) %>% unlist(),
              responsibility_to_inform %>% filter(platform_label=="Twitter",
                                                  response=="None of the above") %>%
                select(count) %>% unlist()),
          n=c(responsibility_to_inform %>% filter(platform_label=="Prolific Academic",
                                                  response=="None of the above") %>%
                select(platform_sample_size) %>% unlist(),
              responsibility_to_inform %>% filter(platform_label=="Twitter",
                                                  response=="None of the above") %>% 
                select(platform_sample_size) %>% unlist()))



## Supplementary Figure 14
responsibility_to_inform_awareness <- rbind(data.frame(cbind(
  "response"=rownames(as.matrix(unlist(strsplit(as.matrix(all_responses %>%
                                                            filter(platform=="prolific",
                                                                   during_level_of_awareness=="About right") %>%
                                                            select(responsibility_to_inform)),";")) %>%
                                  table(),ncol=1)),
  "count"=as.matrix(unlist(strsplit(as.matrix(all_responses %>%filter(platform=="prolific",
                                                                      during_level_of_awareness=="About right") %>%
                                                select(responsibility_to_inform)),";")) %>%
                      table(),ncol=1))) %>%
    rename("count"="V2") %>%
    mutate("platform_label"="Prolific Academic",
           "during_level_of_awareness"="About right",
           "total"=all_responses %>% filter(platform=="prolific",during_level_of_awareness=="About right")%>% nrow()),
  data.frame(cbind(
    "response"=rownames(as.matrix(unlist(strsplit(as.matrix(all_responses %>%
                                                              filter(platform=="prolific",
                                                                     during_level_of_awareness=="Too little") %>%
                                                              select(responsibility_to_inform)),";")) %>%
                                    table(),ncol=1)),
    "count"=as.matrix(unlist(strsplit(as.matrix(all_responses %>%filter(platform=="prolific",
                                                                        during_level_of_awareness=="Too little") %>%
                                                  select(responsibility_to_inform)),";")) %>%
                        table(),ncol=1))) %>%
    rename("count"="V2") %>%
    mutate("platform_label"="Prolific Academic",
           "during_level_of_awareness"="Too little",
           "total"=all_responses %>% filter(platform=="prolific",during_level_of_awareness=="Too little")%>% nrow()),
  data.frame(cbind(
    "response"=rownames(as.matrix(unlist(strsplit(as.matrix(all_responses %>%
                                                              filter(platform=="prolific",
                                                                     during_level_of_awareness=="Too much") %>%
                                                              select(responsibility_to_inform)),";")) %>%
                                    table(),ncol=1)),
    "count"=as.matrix(unlist(strsplit(as.matrix(all_responses %>%filter(platform=="prolific",
                                                                        during_level_of_awareness=="Too much") %>%
                                                  select(responsibility_to_inform)),";")) %>%
                        table(),ncol=1))) %>%
    rename("count"="V2") %>%
    mutate("platform_label"="Prolific Academic",
           "during_level_of_awareness"="Too much",
           "total"=all_responses %>% filter(platform=="prolific",during_level_of_awareness=="Too much")%>% nrow()),
  data.frame(cbind(
    "response"=rownames(as.matrix(unlist(strsplit(as.matrix(all_responses %>%
                                                              filter(platform=="twitter",
                                                                     during_level_of_awareness=="About right") %>%
                                                              select(responsibility_to_inform)),";")) %>%
                                    table(),ncol=1)),
    "count"=as.matrix(unlist(strsplit(as.matrix(all_responses %>%filter(platform=="twitter",
                                                                        during_level_of_awareness=="About right") %>%
                                                  select(responsibility_to_inform)),";")) %>%
                        table(),ncol=1))) %>%
    rename("count"="V2") %>%
    mutate("platform_label"="Twitter",
           "during_level_of_awareness"="About right",
           "total"=all_responses %>% filter(platform=="twitter",during_level_of_awareness=="About right")%>% nrow()),
  data.frame(cbind(
    "response"=rownames(as.matrix(unlist(strsplit(as.matrix(all_responses %>%
                                                              filter(platform=="twitter",
                                                                     during_level_of_awareness=="Too little") %>%
                                                              select(responsibility_to_inform)),";")) %>%
                                    table(),ncol=1)),
    "count"=as.matrix(unlist(strsplit(as.matrix(all_responses %>%filter(platform=="twitter",
                                                                        during_level_of_awareness=="Too little") %>%
                                                  select(responsibility_to_inform)),";")) %>%
                        table(),ncol=1))) %>%
    rename("count"="V2") %>%
    mutate("platform_label"="Twitter",
           "during_level_of_awareness"="Too little",
           "total"=all_responses %>% filter(platform=="twitter",during_level_of_awareness=="Too little")%>% nrow()),
  data.frame(cbind(
    "response"=rownames(as.matrix(unlist(strsplit(as.matrix(all_responses %>%
                                                              filter(platform=="twitter",
                                                                     during_level_of_awareness=="Too much") %>%
                                                              select(responsibility_to_inform)),";")) %>%
                                    table(),ncol=1)),
    "count"=as.matrix(unlist(strsplit(as.matrix(all_responses %>%filter(platform=="twitter",
                                                                        during_level_of_awareness=="Too much") %>%
                                                  select(responsibility_to_inform)),";")) %>%
                        table(),ncol=1))) %>%
    rename("count"="V2") %>%
    mutate("platform_label"="Twitter",
           "during_level_of_awareness"="Too much",
           "total"=all_responses %>% filter(platform=="twitter",during_level_of_awareness=="Too much")%>% nrow()))
rownames(responsibility_to_inform_awareness) <- c()

responsibility_to_inform_awareness <- responsibility_to_inform_awareness %>% mutate(count = as.numeric(count),
                                                                                    percentage = count/total *100) %>%
  mutate(response = factor(response,
                           levels=c("The government","Those who develop transmission models",
                                    "Those who use transmission models","The media",
                                    "Myself, as a member of the public","None of the above")),
         during_level_of_awareness = factor(during_level_of_awareness,
                                            levels=c("Too little","About right","Too much")),
         during_level_of_awareness_label = ifelse(during_level_of_awareness=="Too little",
                                                  "Too little awareness",
                                                  ifelse(during_level_of_awareness=="Too much",
                                                         "Too much awareness",
                                                         ifelse(during_level_of_awareness=="About right",
                                                                "About right awareness",NA))),
         during_level_of_awareness_label = factor(during_level_of_awareness_label,
                                                  levels=c("Too little awareness","About right awareness",
                                                           "Too much awareness")))

responsibility_to_inform_awareness <- rbind(responsibility_to_inform_awareness,
                                            setNames(data.frame("response"="Myself, as a member of the public","count"=0,
                                                                "platform_label"="Prolific Academic","during_level_of_awareness"="Too much",
                                                                "total"=10,"percentage"=0,"during_level_of_awareness_label"="Too much awareness"),
                                                     names(responsibility_to_inform_awareness)))

responsibility_to_inform_awareness <- responsibility_to_inform_awareness %>%
  mutate(platform_label_generic = ifelse(platform_label=="Prolific Academic","Online panel","Social media"))


ggplot(responsibility_to_inform_awareness,
       aes(x=response,y=percentage,fill=platform_label_generic))+
  geom_col(position = "dodge")+
  theme_bw()+
  facet_wrap(~during_level_of_awareness_label,nrow=3)+
  scale_fill_manual(values=c("#1e2761","#408ec6"))+
  scale_y_continuous(n.breaks=6)+
  theme(legend.position = "bottom",strip.background = element_rect(fill="white"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12))+
  labs(y="Percentage of respondents (%)",
       x="Who do you think has the responsibility of ensuring that the public \nare informed about the use of modelling in policy decisions, \nparticularly in the COVID-19 pandemic?",fill="")
ggsave("outputs/SupFig14.png",height=7,width=5)

