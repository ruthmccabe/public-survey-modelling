### Awareness of transmission modelling

## Questions covered:
## How were you aware of transmission modelling? (prior and during)
## Were you aware of the use of transmission models in informing public health policy? (prior and during)
## How much do you know about how transmission modelling has been used throughout the COVID-19 pandemic?

library(tidyverse)
library(readxl)
library(cowplot)
library(ggalluvial)

prolific <- read_excel("Data/responses_prolific.xlsx") %>% data.frame() %>%
  select(-c(Start.time,Completion.time,Email,consent_1,consent_2))

twitter <- read_excel("Data/responses_twitter.xlsx") %>% data.frame() %>%
  select(-c(Start.time,Completion.time,Email,consent_1,consent_2))

all_responses <- rbind(prolific,twitter) %>% group_by(platform) %>%
  mutate("platform_label"=ifelse(platform=="twitter","Twitter","Prolific Academic"),
         "platform_label_generic" = ifelse(platform=="twitter","Social media","Online panel"),
         "platform_sample_size"=length(platform),
         "unique_ID"=paste0(platform,"_",ID)) %>% data.frame() %>%
  filter(!is.na(gender),!is.na(age_group),!is.na(sector),!is.na(vaccinated))


## How were you aware of transmission modelling? 
## format data

## prior to the pandemic
prior_awareness_how_prolific <- data.frame(cbind(
  "response"=rownames(as.matrix(unlist(strsplit(as.matrix(all_responses %>% filter(platform=="prolific") %>%
                                                            select(prior_awareness_how)),";")) %>%
                                  table(),ncol=1)),
  "count"=as.matrix(unlist(strsplit(as.matrix(all_responses %>%filter(platform=="prolific") %>%
                                                select(prior_awareness_how)),";")) %>%
                      table(),ncol=1))) %>%
  rename("count"="V2") %>%
  mutate("platform"="prolific",
         "platform_label"="Prolific Academic",
         "platform_label_generic"="Online panel",
         "platform_sample_size"=nrow(all_responses %>% filter(platform=="prolific")))
rownames(prior_awareness_how_prolific) <- c()

prior_awareness_how_twitter <- data.frame(cbind(
  "response"=rownames(as.matrix(unlist(strsplit(as.matrix(all_responses %>% filter(platform=="twitter") %>%
                                                            select(prior_awareness_how)),";")) %>%
                                  table(),ncol=1)),
  "count"=as.matrix(unlist(strsplit(as.matrix(all_responses %>%filter(platform=="twitter") %>%
                                                select(prior_awareness_how)),";")) %>%
                      table(),ncol=1))) %>%
  rename("count"="V2") %>%
  mutate("platform"="twitter",
         "platform_label"="Twitter",
         "platform_label_generic"="Social media",
         "platform_sample_size"=nrow(all_responses %>% filter(platform=="twitter")))
rownames(prior_awareness_how_twitter) <- c()

prior_awareness_how <- rbind(prior_awareness_how_prolific,prior_awareness_how_twitter) %>%
  mutate(count=as.numeric(count),
         time="Prior to the COVID-19 pandemic") %>% data.frame()
prior_awareness_how$response <- factor(prior_awareness_how$response,
                                       levels=c("I was not aware","Newspaper (online or print)",
                                                "News show (TV or online)","Social media","Internet search",
                                                "Academic reports and papers","Other"))


## during the pandemic
during_awareness_how_prolific <- data.frame(cbind(
  "response"=rownames(as.matrix(unlist(strsplit(as.matrix(all_responses %>% filter(platform=="prolific") %>%
                                                            select(during_awareness_how)),";")) %>%
                                  table(),ncol=1)),
  "count"=as.matrix(unlist(strsplit(as.matrix(all_responses %>%filter(platform=="prolific") %>%
                                                select(during_awareness_how)),";")) %>%
                      table(),ncol=1))) %>%
  rename("count"="V2") %>%
  mutate("platform"="prolific",
         "platform_label"="Prolific Academic",
         "platform_label_generic"="Online panel",
         "platform_sample_size"=nrow(all_responses %>% filter(platform=="prolific")))
rownames(during_awareness_how_prolific) <- c()

during_awareness_how_twitter <- data.frame(cbind(
  "response"=rownames(as.matrix(unlist(strsplit(as.matrix(all_responses %>% filter(platform=="twitter") %>%
                                                            select(during_awareness_how)),";")) %>%
                                  table(),ncol=1)),
  "count"=as.matrix(unlist(strsplit(as.matrix(all_responses %>%filter(platform=="twitter") %>%
                                                select(during_awareness_how)),";")) %>%
                      table(),ncol=1))) %>%
  rename("count"="V2") %>%
  mutate("platform"="twitter",
         "platform_label"="Twitter",
         "platform_label_generic"="Social media",
         "platform_sample_size"=nrow(all_responses %>% filter(platform=="twitter")))
rownames(during_awareness_how_twitter) <- c()

during_awareness_how <- rbind(during_awareness_how_prolific,during_awareness_how_twitter) %>%
  mutate(count=as.numeric(count),
         time="During the COVID-19 pandemic",
         response = ifelse(response=="Other ","Other",response)) %>% data.frame()
during_awareness_how$response <- factor(during_awareness_how$response,
                                        levels=c("I was not aware","Newspaper (online or print)",
                                                 "News show (TV or online)","Social media","Internet search",
                                                 "Academic reports and papers","Other"))


## Table 1

## How were you aware of transmission modelling? 
awareness_how <- rbind(prior_awareness_how,
                       during_awareness_how) %>%
  group_by(platform,time) %>%
  mutate(perc = count/platform_sample_size *100) %>% data.frame()
awareness_how$time <- factor(awareness_how$time,
                             levels=c("Prior to the COVID-19 pandemic","During the COVID-19 pandemic"))
awareness_how %>% mutate(perc = round(perc))

## Were you aware of the use of transmission models in informing public health policy? (prior and during)
table(all_responses$prior_awareness_use_in_policy,all_responses$platform_label,useNA="ifany")
round(prop.table(table(all_responses$prior_awareness_use_in_policy,all_responses$platform_label,
                       useNA="ifany"),2)*100)

table(all_responses$during_awareness_use_in_policy,all_responses$platform_label,useNA="ifany")
round(prop.table(table(all_responses$during_awareness_use_in_policy,all_responses$platform_label,
                       useNA="ifany"),2)*100)

## How much do you know about how transmission modelling has been used throughout the COVID-19 pandemic?
table(all_responses$during_level_of_awareness,all_responses$platform_label,useNA="ifany")
round(prop.table(table(all_responses$during_level_of_awareness,all_responses$platform_label,useNA="ifany"),2)*100)

## Testing within the text

## differences in awareness in policy within samples across time periods?

policy_comparison <- all_responses %>% dplyr::select(platform_label,
                                                     prior_awareness_use_in_policy,
                                                     during_awareness_use_in_policy) %>% 
  mutate(prior_awareness_policy = ifelse(prior_awareness_use_in_policy=="Yes",1,
                                         ifelse(prior_awareness_use_in_policy=="Unsure",0,
                                                ifelse(prior_awareness_use_in_policy=="No",-1,NA))),
         during_awareness_policy = ifelse(during_awareness_use_in_policy=="Yes",1,
                                         ifelse(during_awareness_use_in_policy=="Unsure",0,
                                                ifelse(during_awareness_use_in_policy=="No",-1,NA))))


wilcox.test(x=policy_comparison %>% filter(platform_label=="Prolific Academic") %>% 
              dplyr::select(prior_awareness_policy) %>% unlist() %>% as.numeric(),
            y=policy_comparison %>% filter(platform_label=="Prolific Academic") %>% 
              dplyr::select(during_awareness_policy) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")

wilcox.test(x=policy_comparison %>% filter(platform_label=="Twitter") %>% 
              dplyr::select(prior_awareness_policy) %>% unlist() %>% as.numeric(),
            y=policy_comparison %>% filter(platform_label=="Twitter") %>% 
              dplyr::select(during_awareness_policy) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")


## differences in awareness in policy across samples within time periods?
# prior
prop.test(x=c(all_responses %>% filter(platform_label=="Prolific Academic",
                                       prior_awareness_use_in_policy=="Yes") %>% nrow(),
              all_responses %>% filter(platform_label=="Twitter",
                                       prior_awareness_use_in_policy=="Yes") %>% nrow()),
          n=c(all_responses %>% filter(platform_label=="Prolific Academic") %>% nrow(),
              all_responses %>% filter(platform_label=="Twitter") %>% nrow()))

# during
prop.test(x=c(all_responses %>% filter(platform_label=="Prolific Academic",
                                       during_awareness_use_in_policy=="Yes") %>% nrow(),
              all_responses %>% filter(platform_label=="Twitter",
                                       during_awareness_use_in_policy=="Yes") %>% nrow()),
          n=c(all_responses %>% filter(platform_label=="Prolific Academic") %>% nrow(),
              all_responses %>% filter(platform_label=="Twitter") %>% nrow()))




## differences in levels of knowledge during the pandemic?
prop.test(x=c(all_responses %>% filter(platform_label=="Prolific Academic",
                         during_level_of_awareness=="About right") %>% nrow(),
              all_responses %>% filter(platform_label=="Twitter",
                                       during_level_of_awareness=="About right") %>% nrow()),
          n=c(all_responses %>% filter(platform_label=="Prolific Academic") %>% nrow(),
              all_responses %>% filter(platform_label=="Twitter") %>% nrow()))

prop.test(x=c(all_responses %>% filter(platform_label=="Prolific Academic",
                                       during_level_of_awareness=="Too little") %>% nrow(),
              all_responses %>% filter(platform_label=="Twitter",
                                       during_level_of_awareness=="Too little") %>% nrow()),
          n=c(all_responses %>% filter(platform_label=="Prolific Academic") %>% nrow(),
              all_responses %>% filter(platform_label=="Twitter") %>% nrow()))

prop.test(x=c(all_responses %>% filter(platform_label=="Prolific Academic",
                                       during_level_of_awareness=="Too much") %>% nrow(),
              all_responses %>% filter(platform_label=="Twitter",
                                       during_level_of_awareness=="Too much") %>% nrow()),
          n=c(all_responses %>% filter(platform_label=="Prolific Academic") %>% nrow(),
              all_responses %>% filter(platform_label=="Twitter") %>% nrow()))



## Figure 2
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


# ggplot(awareness_use_in_policy,
#        aes(x=time,fill=awareness_use_in_policy))+
#   geom_col(aes(y=percentage))+
#   theme_bw()+
#   facet_wrap(~platform_label)+
#   #scale_fill_viridis_d(option="magma",end=0.6)+
#   scale_fill_manual(values=c("#000004","#3b0f70","#8c2981","grey"))+
#   scale_size_area(max_size = 20)+
#   scale_x_discrete(labels = function(x) str_wrap(x, width = 12))+
#   labs(x="Time period",
#        y="Percentage of respondents (%)",
#        fill="Were you are aware of \nthe use of transmission \nmodels in informing \npublic health policy?")+
#   theme(strip.background = element_rect(fill = "white", color = "black"))+
#   guides(color=guide_legend(order=1),fill=guide_legend(order=1))
# ggsave("outputs/Fig2.png",width=7,height=5)


# awareness_alluvial <- merge(all_responses %>% dplyr::select(platform_label,prior_awareness_use_in_policy) %>%
#   group_by(platform_label,prior_awareness_use_in_policy) %>%
#   summarise(prior_freq = length(prior_awareness_use_in_policy)),
#   all_responses %>% dplyr::select(platform_label,during_awareness_use_in_policy) %>%
#   group_by(platform_label,during_awareness_use_in_policy) %>%
#   summarise(during_freq = length(during_awareness_use_in_policy)),
#   by="platform_label",all=TRUE
#   )

awareness_alluvial <- all_responses %>% 
  dplyr::select(platform_label_generic,prior_awareness_use_in_policy,during_awareness_use_in_policy) %>%
  group_by(platform_label_generic,prior_awareness_use_in_policy,during_awareness_use_in_policy) %>%
  summarise(freq = length(platform_label_generic)) %>% ungroup() %>%
  group_by(platform_label_generic,prior_awareness_use_in_policy) %>%
  mutate(total = sum(freq),
         freq2 = freq) %>% ungroup() %>%
  group_by(platform_label_generic,during_awareness_use_in_policy) %>%
  mutate(during_total = sum(freq)) %>%
  mutate(prior_awareness_use_in_policy = factor(ifelse(is.na(prior_awareness_use_in_policy),
                                                "Did not answer",prior_awareness_use_in_policy),
                                                levels=c("Yes","Unsure","No","Did not answer")),
         during_awareness_use_in_policy = factor(ifelse(is.na(during_awareness_use_in_policy),
                                                "Did not answer",during_awareness_use_in_policy),
                                                levels=c("Yes","Unsure","No","Did not answer")))


# 
# test <- all_responses %>% dplyr::select(platform_label,prior_awareness_use_in_policy,
#                                 during_awareness_use_in_policy) %>% 
#   group_by(platform_label,prior_awareness_use_in_policy,
#            during_awareness_use_in_policy) %>%
#   summarise(during_freq = length(platform_label)) %>%
#   ungroup() %>%
#   group_by(platform_label,prior_awareness_use_in_policy) %>%
#   mutate(prior_freq = sum(during_freq))


ggplot(data = awareness_alluvial %>% filter(prior_awareness_use_in_policy!="Did not answer",
                                            during_awareness_use_in_policy!="Did not answer"),
       aes(axis1 = prior_awareness_use_in_policy, axis2 = during_awareness_use_in_policy,
           y = freq, freq2 = freq2))+
  scale_x_discrete(limits = c("Prior to the COVID-19 pandemic", "During the COVID-19 pandemic"), 
                   expand = c(.2, .05),
                   labels = function(x) str_wrap(x, width = 12)) +
  labs(x="Time period",y="Number of respondents",fill="Awareness during the pandemic")+
  theme_classic()+
  facet_wrap(~platform_label_generic,scales="free_y")+
  geom_alluvium(aes(fill=during_awareness_use_in_policy))+
  geom_stratum()+
  #scale_fill_manual(values=c("#C54B8C","#4d5198","#000004","grey"))+##f5beb4
  scale_fill_manual(values=c("#70A9A1","#40798C","#0B2027"))+
  #scale_fill_manual(values=c("#C38D94","#00A5CF","#004E64"))+
  #scale_fill_manual(values=c("#C38D94","#6C3A5C","#004e64"))+
  #scale_fill_manual(values=c("#0094C6","#1282A2","#000022"))+
  #scale_fill_manual(values=c("#0094C6","#000022","#C33C54"))+
  geom_text(stat = "stratum", aes(label = after_stat(paste0(stratum))),
            min.y=10) +
  # geom_label(stat = "alluvium", aes(label = after_stat(freq2),
  #                                   fill = prior_awareness_use_in_policy),alpha = 0.4,
  #                                               position = "dodge",vjust = 0.75, size = 5) +
  theme(legend.position = "bottom",
        axis.line = element_blank(),strip.text = element_text(size=12),
        axis.text.x = element_text(colour="black"))+
  ggtitle("Were you aware of the use of transmission models in informing public health policy?")
ggsave("outputs/Fig2.png",height=6,width=8)
# can then use paint to get the finish that we want 

# awareness_alluvial_2 <- gather(awareness_alluvial,
#        key = "time_period",
#        value = "response",
#        prior_awareness_use_in_policy,during_awareness_use_in_policy)
# 
# test <- rbind(all_responses %>% 
#   dplyr::select(platform_label,prior_awareness_use_in_policy) %>%
#   group_by(platform_label,prior_awareness_use_in_policy) %>%
#   summarise(freq = length(prior_awareness_use_in_policy)) %>% 
#   mutate(time_period = "Prior to the COVID-19 pandemic") %>%
#     rename(awareness_use_in_policy = prior_awareness_use_in_policy),
# all_responses %>% 
#   dplyr::select(platform_label,during_awareness_use_in_policy) %>%
#   group_by(platform_label,during_awareness_use_in_policy) %>%
#   summarise(freq = length(during_awareness_use_in_policy)) %>% 
#   mutate(time_period = "During the COVID-19 pandemic") %>%
#   rename(awareness_use_in_policy = during_awareness_use_in_policy)
# ) %>% ungroup() %>%
#   group_by(platform_label,time_period) %>%
#   mutate(platform_total = sum(freq),
#          freq2 = freq) %>%
#   mutate(platform_label = factor(platform_label,levels=c("Prolific Academic","Twitter")),
#          time_period = factor(time_period, 
#                               levels=c("Prior to the COVID-19 pandemic","During the COVID-19 pandemic")),
#          awareness_use_in_policy = factor(ifelse(is.na(awareness_use_in_policy),"Did not answer",
#                                                  awareness_use_in_policy),
#                                           levels=c("Yes","Unsure","No","Did not answer")))
#   
# 
# ggplot(data = test,
#        aes(x=time_period,stratum = awareness_use_in_policy,alluvium = awareness_use_in_policy,
#            y = freq2))+
#   # scale_x_discrete(limits = c("Prior to the COVID-19 pandemic", "During the COVID-19 pandemic"), 
#   #                  expand = c(.2, .05)) +
#   labs(x="Time period",y="Number of respondents",fill="Awareness during the pandemic")+
#   theme_classic()+
#   facet_wrap(~platform_label,scales="free_y")+
#   geom_alluvium(aes(fill=awareness_use_in_policy))+
#   geom_stratum()+
#   scale_fill_manual(values=c("skyblue","#3b0f70","#000004","grey"))+
#   geom_text(stat = "stratum", aes(label = after_stat(paste0(stratum))),
#             min.y=10) +
#   # geom_label(stat = "alluvium", aes(label = after_stat(freq2),
#   #                                    fill = awareness_use_in_policy),alpha = 0.4,
#   #                                                position = "dodge",vjust = 0.75, size = 5) +
#   theme(legend.position = "bottom",
#         axis.line = element_blank())+
#   ggtitle("Were you aware of the use of transmission models in informing public health policy?")



# 
# ggplot(data = awareness_alluvial %>% filter(platform_label=="Prolific Academic"),
#        aes(axis1 = prior_awareness_use_in_policy, axis2 = during_awareness_use_in_policy,
#            y = prior_freq/4, freq2 = during_freq))+
#   # scale_x_discrete(limits = c("Selection for full-text review", "Inclusion in in-depth discussion"), expand = c(.2, .05)) +
#   # xlab("Review Stage")+ylab("Count")+ theme_minimal()+
#   # theme(text = element_text(size = 16),
#   #       plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
#   #       plot.subtitle = element_text(hjust = 0.5, size = 16)) +
#   #geom_alluvium(aes(fill = in_out))+
#   geom_alluvium(aes(fill=during_awareness_use_in_policy))+
#   geom_stratum()+
#   geom_text(stat = "stratum", aes(label = after_stat(paste0(stratum))),
#             fontface = "bold", size = 5) +
#   geom_label(stat = "alluvium", aes(label = after_stat(freq2),
#                                     fill = in_out),alpha = 0.4,
#              position = "dodge",vjust = 0.75, size = 5) 
#   # labs(title = paste0("Gathering Restrictions: Full-Text Review Process (n = ", total_title,")"))+
#   # scale_fill_manual(values = c(green_grey),
#   #                   breaks = c("Included", "Excluded"))+
#   # theme(legend.position = "bottom")+
#   # guides(fill = guide_legend(title = "Full-Text Verdict",
#   #                            override.aes = aes(label = "")))


## Supplementary Table 2

# make into numeric so can do a wilcoxon signed rank test
policy_awareness_comparison <- all_responses %>% dplyr::select(platform_label,platform_label_generic,
                                prior_awareness_how,prior_awareness_use_in_policy,
                                during_awareness_how,during_awareness_use_in_policy) %>%
  mutate(prior_awareness_general = ifelse(grepl( "I was not aware",prior_awareness_how, fixed = TRUE),0,1),
         prior_awareness_policy = ifelse(prior_awareness_use_in_policy=="Yes",1,
                                         ifelse(prior_awareness_use_in_policy=="No"|prior_awareness_use_in_policy=="Unsure",0,NA)),
         during_awareness_general = ifelse(grepl( "I was not aware", during_awareness_how, fixed = TRUE),0,1),
         during_awareness_policy = ifelse(during_awareness_use_in_policy=="Yes",1,0),)

## sense check and all good (remember not everyone answered the question)
policy_awareness_comparison %>% group_by(platform_label_generic) %>%
  summarise(sum_prior_general = sum(prior_awareness_general,na.rm = TRUE),
            sum_prior_policy = sum(prior_awareness_policy,na.rm = TRUE),
            sum_during_general = sum(during_awareness_general,na.rm=TRUE),
            sum_during_policy = sum(during_awareness_policy,na.rm = TRUE))

wilcox.test(x = policy_awareness_comparison %>% filter(platform_label=="Prolific Academic") %>% 
                dplyr::select(prior_awareness_general) %>% unlist() %>% as.numeric(),
            y = policy_awareness_comparison %>% filter(platform_label=="Prolific Academic") %>% 
              dplyr::select(prior_awareness_policy) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")

  
wilcox.test(x = policy_awareness_comparison %>% filter(platform_label=="Twitter") %>% 
                dplyr::select(prior_awareness_general) %>% unlist() %>% as.numeric(),
              y = policy_awareness_comparison %>% filter(platform_label=="Twitter") %>% 
                dplyr::select(prior_awareness_policy) %>% unlist() %>% as.numeric(),
              paired=TRUE,
              alternative="two.sided")
  
wilcox.test(x = policy_awareness_comparison %>% filter(platform_label=="Prolific Academic") %>% 
              dplyr::select(during_awareness_general) %>% unlist() %>% as.numeric(),
            y = policy_awareness_comparison %>% filter(platform_label=="Prolific Academic") %>% 
              dplyr::select(during_awareness_policy) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")

wilcox.test(x = policy_awareness_comparison %>% filter(platform_label=="Twitter") %>% 
              dplyr::select(during_awareness_general) %>% unlist() %>% as.numeric(),
            y = policy_awareness_comparison %>% filter(platform_label=="Twitter") %>% 
              dplyr::select(during_awareness_policy) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")



# # prolific academic prior to the pandemic
# prop.test(x=c(awareness_how %>% filter(platform_label=="Prolific Academic",
#                                        response=="I was not aware",
#                                        time=="Prior to the COVID-19 pandemic") %>% select(count) %>% unlist(),
#               all_responses %>% filter(platform_label=="Prolific Academic",prior_awareness_use_in_policy=="No"|prior_awareness_use_in_policy=="Unsure") %>% nrow()),
#           n = c(all_responses %>% filter(platform_label=="Prolific Academic") %>% nrow(),
#                 all_responses %>% filter(platform_label=="Prolific Academic") %>% nrow()))
# 
# 
# 
# 
# # twitter prior to the pandemic
# prop.test(x=c(awareness_how %>% filter(platform_label=="Twitter",
#                                        response=="I was not aware",
#                                        time=="Prior to the COVID-19 pandemic") %>% select(count) %>% unlist(),
#               all_responses %>% filter(platform_label=="Twitter",prior_awareness_use_in_policy=="No"|prior_awareness_use_in_policy=="Unsure") %>% nrow()),
#           n = c(all_responses %>% filter(platform_label=="Twitter") %>% nrow(),
#                 all_responses %>% filter(platform_label=="Twitter") %>% nrow()))
# 
# # prolific academic during the pandemic
# prop.test(x=c(awareness_how %>% filter(platform_label=="Prolific Academic",
#                                        response=="I was not aware",
#                                        time=="During the COVID-19 pandemic") %>% select(count) %>% unlist(),
#               all_responses %>% filter(platform_label=="Prolific Academic",during_awareness_use_in_policy=="No"|during_awareness_use_in_policy=="Unsure") %>% nrow()),
#           n = c(all_responses %>% filter(platform_label=="Prolific Academic") %>% nrow(),
#                 all_responses %>% filter(platform_label=="Prolific Academic") %>% nrow()))
# # twitter during the pandemic 
# prop.test(x=c(awareness_how %>% filter(platform_label=="Twitter",
#                                        response=="I was not aware",
#                                        time=="During the COVID-19 pandemic") %>% select(count) %>% unlist(),
#               all_responses %>% filter(platform_label=="Twitter",during_awareness_use_in_policy=="No"|during_awareness_use_in_policy=="Unsure") %>% nrow()),
#           n = c(all_responses %>% filter(platform_label=="Twitter") %>% nrow(),
#                 all_responses %>% filter(platform_label=="Twitter") %>% nrow()))


## Supplementary Table 3

## linear regression for awareness in policy 

awareness_policy_lm <- all_responses %>% dplyr::select(platform_label,age_group,gender,
                                                       prior_awareness_use_in_policy,
                                                       during_awareness_use_in_policy) %>%
  mutate(prior_awareness_use_in_policy_numeric = ifelse(prior_awareness_use_in_policy=="Yes",1,
                                                        ifelse(prior_awareness_use_in_policy=="Unsure",0,
                                                               ifelse(prior_awareness_use_in_policy=="No",-1,NA))),
         during_awareness_use_in_policy_numeric = ifelse(during_awareness_use_in_policy=="Yes",1,
                                                         ifelse(during_awareness_use_in_policy=="Unsure",0,
                                                                ifelse(during_awareness_use_in_policy=="No",-1,NA))))


m1 <- lm(prior_awareness_use_in_policy_numeric~age_group+gender,
         data = awareness_policy_lm %>% filter(platform_label=="Prolific Academic"))
summary(m1)
anova(m1,test="Chisq")

m2 <- lm(prior_awareness_use_in_policy_numeric~age_group+gender,
         data = awareness_policy_lm %>% filter(platform_label=="Twitter"))
summary(m2)
anova(m2,test="Chisq")

m3 <- lm(during_awareness_use_in_policy_numeric~age_group+gender,
         data = awareness_policy_lm %>% filter(platform_label=="Prolific Academic"))
summary(m3)
anova(m3,test="Chisq")

m4 <- lm(during_awareness_use_in_policy_numeric~age_group+gender,
         data = awareness_policy_lm %>% filter(platform_label=="Twitter"))
summary(m4)
anova(m4,test="Chisq")


## Supplementary Figure 1 

ggplot(awareness_how,aes(x=response,y=count/platform_sample_size,group=time,fill=time))+
  geom_col(position="dodge")+
  facet_wrap(~platform_label_generic)+
  theme_bw()+
  scale_fill_manual(values=c("#a8c66c","#1b6535"))+
  scale_y_continuous(labels=scales::label_percent(accuracy=1),n.breaks=6) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(legend.position="bottom",strip.background.x = element_rect(fill="white"),
        axis.text.x = element_text(size=8))+
  labs(x="How were you aware of transmission modelling?",y="Percentage of responsdents (%)",
       fill=" ")
ggsave("outputs/SupFig1.png",width=7,height=4)


## Supplementary Table 4
### this all needs to change to Wilcoxon test (and will require wrangling)

awareness_how_indicators <- all_responses %>% mutate(not_aware_prior_indicator = ifelse(grepl("I was not aware",
                                                                  prior_awareness_how,fixed=TRUE),1,0),
                         newspaper_prior_indicator = ifelse(grepl("Newspaper",
                                                                  prior_awareness_how,fixed=TRUE),1,0),
                         newsshow_prior_indicator = ifelse(grepl("News show",
                                                                 prior_awareness_how,fixed=TRUE),1,0),
                         social_media_prior_indicator = ifelse(grepl("Social media",
                                                                     prior_awareness_how,fixed=TRUE),1,0),
                         internet_prior_indicator = ifelse(grepl("Internet search",
                                                                 prior_awareness_how,fixed=TRUE),1,0),
                         academic_prior_indicator = ifelse(grepl("Academic reports",
                                                                 prior_awareness_how,fixed=TRUE),1,0),
                         other_prior_indicator = ifelse(grepl("Other",
                                                              prior_awareness_how,fixed=TRUE),1,0),
                         not_aware_during_indicator = ifelse(grepl("I was not aware",
                                                                  during_awareness_how,fixed=TRUE),1,0),
                         newspaper_during_indicator = ifelse(grepl("Newspaper",
                                                                  during_awareness_how,fixed=TRUE),1,0),
                         newsshow_during_indicator = ifelse(grepl("News show",
                                                                 during_awareness_how,fixed=TRUE),1,0),
                         social_media_during_indicator = ifelse(grepl("Social media",
                                                                     during_awareness_how,fixed=TRUE),1,0),
                         internet_during_indicator = ifelse(grepl("Internet search",
                                                                 during_awareness_how,fixed=TRUE),1,0),
                         academic_during_indicator = ifelse(grepl("Academic reports",
                                                                 during_awareness_how,fixed=TRUE),1,0),
                         other_during_indicator = ifelse(grepl("Other",
                                                              during_awareness_how,fixed=TRUE),1,0))
### Prolific Academic

wilcox.test(x=awareness_how_indicators %>% filter(platform_label=="Prolific Academic") %>% 
              dplyr::select(not_aware_prior_indicator) %>% unlist() %>% as.numeric(),
            y=awareness_how_indicators %>% filter(platform_label=="Prolific Academic") %>% 
              dplyr::select(not_aware_during_indicator) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative = "two.sided")

wilcox.test(x=awareness_how_indicators %>% filter(platform_label=="Prolific Academic") %>% 
              dplyr::select(newspaper_prior_indicator) %>% unlist() %>% as.numeric(),
            y=awareness_how_indicators %>% filter(platform_label=="Prolific Academic") %>% 
              dplyr::select(newspaper_during_indicator) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative = "two.sided")

wilcox.test(x=awareness_how_indicators %>% filter(platform_label=="Prolific Academic") %>% 
              dplyr::select(newsshow_prior_indicator) %>% unlist() %>% as.numeric(),
            y=awareness_how_indicators %>% filter(platform_label=="Prolific Academic") %>% 
              dplyr::select(newsshow_during_indicator) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative = "two.sided")

wilcox.test(x=awareness_how_indicators %>% filter(platform_label=="Prolific Academic") %>% 
              dplyr::select(social_media_prior_indicator) %>% unlist() %>% as.numeric(),
            y=awareness_how_indicators %>% filter(platform_label=="Prolific Academic") %>% 
              dplyr::select(social_media_during_indicator) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative = "two.sided")

wilcox.test(x=awareness_how_indicators %>% filter(platform_label=="Prolific Academic") %>% 
              dplyr::select(internet_prior_indicator) %>% unlist() %>% as.numeric(),
            y=awareness_how_indicators %>% filter(platform_label=="Prolific Academic") %>% 
              dplyr::select(internet_during_indicator) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative = "two.sided")


wilcox.test(x=awareness_how_indicators %>% filter(platform_label=="Prolific Academic") %>% 
              dplyr::select(academic_prior_indicator) %>% unlist() %>% as.numeric(),
            y=awareness_how_indicators %>% filter(platform_label=="Prolific Academic") %>% 
              dplyr::select(academic_during_indicator) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative = "two.sided")

wilcox.test(x=awareness_how_indicators %>% filter(platform_label=="Prolific Academic") %>% 
              dplyr::select(other_prior_indicator) %>% unlist() %>% as.numeric(),
            y=awareness_how_indicators %>% filter(platform_label=="Prolific Academic") %>% 
              dplyr::select(other_during_indicator) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative = "two.sided")


### Twitter

wilcox.test(x=awareness_how_indicators %>% filter(platform_label=="Twitter") %>% 
              dplyr::select(not_aware_prior_indicator) %>% unlist() %>% as.numeric(),
            y=awareness_how_indicators %>% filter(platform_label=="Twitter") %>% 
              dplyr::select(not_aware_during_indicator) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative = "two.sided")

wilcox.test(x=awareness_how_indicators %>% filter(platform_label=="Twitter") %>% 
              dplyr::select(newspaper_prior_indicator) %>% unlist() %>% as.numeric(),
            y=awareness_how_indicators %>% filter(platform_label=="Twitter") %>% 
              dplyr::select(newspaper_during_indicator) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative = "two.sided")

wilcox.test(x=awareness_how_indicators %>% filter(platform_label=="Twitter") %>% 
              dplyr::select(newsshow_prior_indicator) %>% unlist() %>% as.numeric(),
            y=awareness_how_indicators %>% filter(platform_label=="Twitter") %>% 
              dplyr::select(newsshow_during_indicator) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative = "two.sided")

wilcox.test(x=awareness_how_indicators %>% filter(platform_label=="Twitter") %>% 
              dplyr::select(social_media_prior_indicator) %>% unlist() %>% as.numeric(),
            y=awareness_how_indicators %>% filter(platform_label=="Twitter") %>% 
              dplyr::select(social_media_during_indicator) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative = "two.sided")

wilcox.test(x=awareness_how_indicators %>% filter(platform_label=="Twitter") %>% 
              dplyr::select(internet_prior_indicator) %>% unlist() %>% as.numeric(),
            y=awareness_how_indicators %>% filter(platform_label=="Twitter") %>% 
              dplyr::select(internet_during_indicator) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative = "two.sided")


wilcox.test(x=awareness_how_indicators %>% filter(platform_label=="Twitter") %>% 
              dplyr::select(academic_prior_indicator) %>% unlist() %>% as.numeric(),
            y=awareness_how_indicators %>% filter(platform_label=="Twitter") %>% 
              dplyr::select(academic_during_indicator) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative = "two.sided")

wilcox.test(x=awareness_how_indicators %>% filter(platform_label=="Twitter") %>% 
              dplyr::select(other_prior_indicator) %>% unlist() %>% as.numeric(),
            y=awareness_how_indicators %>% filter(platform_label=="Twitter") %>% 
              dplyr::select(other_during_indicator) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative = "two.sided")





## Supplementary Table 5

## Prior to the COVID-19 pandemic
prop.test(x=c(awareness_how %>% filter(platform_label=="Prolific Academic",time=="Prior to the COVID-19 pandemic",
                                       response=="I was not aware") %>% select(count) %>% unlist(),
              awareness_how %>% filter(platform_label=="Twitter",time=="Prior to the COVID-19 pandemic",
                                       response=="I was not aware") %>% select(count) %>% unlist()),
          n=c(awareness_how %>% filter(platform_label=="Prolific Academic",time=="Prior to the COVID-19 pandemic",
                                       response=="I was not aware") %>% select(platform_sample_size) %>% unlist(),
              awareness_how %>% filter(platform_label=="Twitter",time=="Prior to the COVID-19 pandemic",
                                       response=="I was not aware") %>% select(platform_sample_size) %>% unlist()))

prop.test(x=c(awareness_how %>% filter(platform_label=="Prolific Academic",time=="Prior to the COVID-19 pandemic",
                                       response=="Newspaper (online or print)") %>% select(count) %>% unlist(),
              awareness_how %>% filter(platform_label=="Twitter",time=="Prior to the COVID-19 pandemic",
                                       response=="Newspaper (online or print)") %>% select(count) %>% unlist()),
          n=c(awareness_how %>% filter(platform_label=="Prolific Academic",time=="Prior to the COVID-19 pandemic",
                                       response=="Newspaper (online or print)") %>% 
                select(platform_sample_size) %>% unlist(),
              awareness_how %>% filter(platform_label=="Twitter",time=="Prior to the COVID-19 pandemic",
                                       response=="Newspaper (online or print)") %>% 
                select(platform_sample_size) %>% unlist()))

prop.test(x=c(awareness_how %>% filter(platform_label=="Prolific Academic",time=="Prior to the COVID-19 pandemic",
                                       response=="News show (TV or online)") %>% select(count) %>% unlist(),
              awareness_how %>% filter(platform_label=="Twitter",time=="Prior to the COVID-19 pandemic",
                                       response=="News show (TV or online)") %>% select(count) %>% unlist()),
          n=c(awareness_how %>% filter(platform_label=="Prolific Academic",time=="Prior to the COVID-19 pandemic",
                                       response=="News show (TV or online)") %>% 
                select(platform_sample_size) %>% unlist(),
              awareness_how %>% filter(platform_label=="Twitter",time=="Prior to the COVID-19 pandemic",
                                       response=="News show (TV or online)") %>% 
                select(platform_sample_size) %>% unlist()))

prop.test(x=c(awareness_how %>% filter(platform_label=="Prolific Academic",time=="Prior to the COVID-19 pandemic",
                                       response=="Social media") %>% select(count) %>% unlist(),
              awareness_how %>% filter(platform_label=="Twitter",time=="Prior to the COVID-19 pandemic",
                                       response=="Social media") %>% select(count) %>% unlist()),
          n=c(awareness_how %>% filter(platform_label=="Prolific Academic",time=="Prior to the COVID-19 pandemic",
                                       response=="Social media") %>% select(platform_sample_size) %>% unlist(),
              awareness_how %>% filter(platform_label=="Twitter",time=="Prior to the COVID-19 pandemic",
                                       response=="Social media") %>% select(platform_sample_size) %>% unlist()))

prop.test(x=c(awareness_how %>% filter(platform_label=="Prolific Academic",time=="Prior to the COVID-19 pandemic",
                                       response=="Internet search") %>% select(count) %>% unlist(),
              awareness_how %>% filter(platform_label=="Twitter",time=="Prior to the COVID-19 pandemic",
                                       response=="Internet search") %>% select(count) %>% unlist()),
          n=c(awareness_how %>% filter(platform_label=="Prolific Academic",time=="Prior to the COVID-19 pandemic",
                                       response=="Internet search") %>% select(platform_sample_size) %>% unlist(),
              awareness_how %>% filter(platform_label=="Twitter",time=="Prior to the COVID-19 pandemic",
                                       response=="Internet search") %>% select(platform_sample_size) %>% unlist()))

prop.test(x=c(awareness_how %>% filter(platform_label=="Prolific Academic",time=="Prior to the COVID-19 pandemic",
                                       response=="Academic reports and papers") %>% select(count) %>% unlist(),
              awareness_how %>% filter(platform_label=="Twitter",time=="Prior to the COVID-19 pandemic",
                                       response=="Academic reports and papers") %>% select(count) %>% unlist()),
          n=c(awareness_how %>% filter(platform_label=="Prolific Academic",time=="Prior to the COVID-19 pandemic",
                                       response=="Academic reports and papers") %>% 
                select(platform_sample_size) %>% unlist(),
              awareness_how %>% filter(platform_label=="Twitter",time=="Prior to the COVID-19 pandemic",
                                       response=="Academic reports and papers") %>% 
                select(platform_sample_size) %>% unlist()))

prop.test(x=c(awareness_how %>% filter(platform_label=="Prolific Academic",time=="Prior to the COVID-19 pandemic",
                                       response=="Other") %>% select(count) %>% unlist(),
              awareness_how %>% filter(platform_label=="Twitter",time=="Prior to the COVID-19 pandemic",
                                       response=="Other") %>% select(count) %>% unlist()),
          n=c(awareness_how %>% filter(platform_label=="Prolific Academic",time=="Prior to the COVID-19 pandemic",
                                       response=="Other") %>% select(platform_sample_size) %>% unlist(),
              awareness_how %>% filter(platform_label=="Twitter",time=="Prior to the COVID-19 pandemic",
                                       response=="Other") %>% select(platform_sample_size) %>% unlist()))


## During the COVID-19 pandemic
prop.test(x=c(awareness_how %>% filter(platform_label=="Prolific Academic",time=="During the COVID-19 pandemic",
                                       response=="I was not aware") %>% select(count) %>% unlist(),
              awareness_how %>% filter(platform_label=="Twitter",time=="During the COVID-19 pandemic",
                                       response=="I was not aware") %>% select(count) %>% unlist()),
          n=c(awareness_how %>% filter(platform_label=="Prolific Academic",time=="During the COVID-19 pandemic",
                                       response=="I was not aware") %>% select(platform_sample_size) %>% unlist(),
              awareness_how %>% filter(platform_label=="Twitter",time=="During the COVID-19 pandemic",
                                       response=="I was not aware") %>% select(platform_sample_size) %>% unlist()))

prop.test(x=c(awareness_how %>% filter(platform_label=="Prolific Academic",time=="During the COVID-19 pandemic",
                                       response=="Newspaper (online or print)") %>% select(count) %>% unlist(),
              awareness_how %>% filter(platform_label=="Twitter",time=="During the COVID-19 pandemic",
                                       response=="Newspaper (online or print)") %>% select(count) %>% unlist()),
          n=c(awareness_how %>% filter(platform_label=="Prolific Academic",time=="During the COVID-19 pandemic",
                                       response=="Newspaper (online or print)") %>% 
                select(platform_sample_size) %>% unlist(),
              awareness_how %>% filter(platform_label=="Twitter",time=="During the COVID-19 pandemic",
                                       response=="Newspaper (online or print)") %>% 
                select(platform_sample_size) %>% unlist()))

prop.test(x=c(awareness_how %>% filter(platform_label=="Prolific Academic",time=="During the COVID-19 pandemic",
                                       response=="News show (TV or online)") %>% select(count) %>% unlist(),
              awareness_how %>% filter(platform_label=="Twitter",time=="During the COVID-19 pandemic",
                                       response=="News show (TV or online)") %>% select(count) %>% unlist()),
          n=c(awareness_how %>% filter(platform_label=="Prolific Academic",time=="During the COVID-19 pandemic",
                                       response=="News show (TV or online)") %>% 
                select(platform_sample_size) %>% unlist(),
              awareness_how %>% filter(platform_label=="Twitter",time=="During the COVID-19 pandemic",
                                       response=="News show (TV or online)") %>% 
                select(platform_sample_size) %>% unlist()))

prop.test(x=c(awareness_how %>% filter(platform_label=="Prolific Academic",time=="During the COVID-19 pandemic",
                                       response=="Social media") %>% select(count) %>% unlist(),
              awareness_how %>% filter(platform_label=="Twitter",time=="During the COVID-19 pandemic",
                                       response=="Social media") %>% select(count) %>% unlist()),
          n=c(awareness_how %>% filter(platform_label=="Prolific Academic",time=="During the COVID-19 pandemic",
                                       response=="Social media") %>% select(platform_sample_size) %>% unlist(),
              awareness_how %>% filter(platform_label=="Twitter",time=="During the COVID-19 pandemic",
                                       response=="Social media") %>% select(platform_sample_size) %>% unlist()))

prop.test(x=c(awareness_how %>% filter(platform_label=="Prolific Academic",time=="During the COVID-19 pandemic",
                                       response=="Internet search") %>% select(count) %>% unlist(),
              awareness_how %>% filter(platform_label=="Twitter",time=="During the COVID-19 pandemic",
                                       response=="Internet search") %>% select(count) %>% unlist()),
          n=c(awareness_how %>% filter(platform_label=="Prolific Academic",time=="During the COVID-19 pandemic",
                                       response=="Internet search") %>% select(platform_sample_size) %>% unlist(),
              awareness_how %>% filter(platform_label=="Twitter",time=="During the COVID-19 pandemic",
                                       response=="Internet search") %>% select(platform_sample_size) %>% unlist()))

prop.test(x=c(awareness_how %>% filter(platform_label=="Prolific Academic",time=="During the COVID-19 pandemic",
                                       response=="Academic reports and papers") %>% select(count) %>% unlist(),
              awareness_how %>% filter(platform_label=="Twitter",time=="During the COVID-19 pandemic",
                                       response=="Academic reports and papers") %>% select(count) %>% unlist()),
          n=c(awareness_how %>% filter(platform_label=="Prolific Academic",time=="During the COVID-19 pandemic",
                                       response=="Academic reports and papers") %>% 
                select(platform_sample_size) %>% unlist(),
              awareness_how %>% filter(platform_label=="Twitter",time=="During the COVID-19 pandemic",
                                       response=="Academic reports and papers") %>% 
                select(platform_sample_size) %>% unlist()))

prop.test(x=c(awareness_how %>% filter(platform_label=="Prolific Academic",time=="During the COVID-19 pandemic",
                                       response=="Other") %>% select(count) %>% unlist(),
              awareness_how %>% filter(platform_label=="Twitter",time=="During the COVID-19 pandemic",
                                       response=="Other") %>% select(count) %>% unlist()),
          n=c(awareness_how %>% filter(platform_label=="Prolific Academic",time=="During the COVID-19 pandemic",
                                       response=="Other") %>% select(platform_sample_size) %>% unlist(),
              awareness_how %>% filter(platform_label=="Twitter",time=="During the COVID-19 pandemic",
                                       response=="Other") %>% select(platform_sample_size) %>% unlist()))

## Supplementary Figure 2
during_level_of_awareness <- all_responses %>% group_by(platform_label_generic,during_level_of_awareness) %>%
  summarise(responses = length(during_level_of_awareness)) %>%
  mutate(during_level_of_awareness = ifelse(is.na(during_level_of_awareness),"Did not answer",
                                            during_level_of_awareness)) %>% ungroup() %>%
  group_by(platform_label_generic) %>%
  mutate(total = sum(responses),
         percentage = 100*responses/total,
         during_level_of_awareness = factor(during_level_of_awareness,
                                            levels=c("Too much","About right","Too little","Did not answer")))

ggplot(during_level_of_awareness,aes(x=platform_label_generic,y=percentage,fill=during_level_of_awareness))+
  geom_col()+
  theme_bw()+
  #scale_fill_viridis_d(end=0.7)+
  scale_fill_manual(values=c("#440154","#3d4d8a","#23888e","grey"))+
  labs(x="Sample",y="Percentage of respondents (%)",
       fill="How much did you know \nabout how transmission \nmodelling has been used \nthroughout the COVID-19 \npandemic?")
ggsave("outputs/SupFig2.png",width=5,height=4)

## Supplementary Table 6

awareness_level_lm <- all_responses %>% filter(!is.na(during_level_of_awareness)) %>%
  select(during_level_of_awareness,age_group,gender,platform) %>%
  mutate(awareness_number = ifelse(during_level_of_awareness=="Too much",1,
                                   ifelse(during_level_of_awareness=="About right",0,
                                          ifelse(during_level_of_awareness=="Too little",-1,NA))))


m1 <- lm(awareness_number~age_group+gender,data=awareness_level_lm  %>% filter(platform=="prolific"))
summary(m1)
anova(m1,test="Chisq")

m2 <- lm(awareness_number~age_group+gender,data=awareness_level_lm  %>% filter(platform=="twitter"))
summary(m2)
anova(m2,test="Chisq")


## Supplementary Table 7
during_awareness_how_awareness <- rbind(
  data.frame(cbind(
    "response"=rownames(as.matrix(unlist(strsplit(as.matrix(all_responses %>%
                                                              filter(platform=="prolific",
                                                                     during_level_of_awareness=="Too little") %>%
                                                              select(during_awareness_how)),";")) %>%
                                    table(),ncol=1)),
    "count"=as.matrix(unlist(strsplit(as.matrix(all_responses %>%
                                                  filter(platform=="prolific",
                                                         during_level_of_awareness=="Too little") %>%
                                                  select(during_awareness_how)),";")) %>%
                        table(),ncol=1))) %>%
    rename("count"="V2") %>%
    mutate("platform"="prolific",
           "platform_label"="Prolific Academic",
           "during_level_of_awareness"="Too little",
           "platform_sample_size"=all_responses %>% filter(platform=="prolific",
                                                           during_level_of_awareness=="Too little") %>% nrow()),
  data.frame(cbind(
    "response"=rownames(as.matrix(unlist(strsplit(as.matrix(all_responses %>%
                                                              filter(platform=="prolific",
                                                                     during_level_of_awareness=="About right") %>%
                                                              select(during_awareness_how)),";")) %>%
                                    table(),ncol=1)),
    "count"=as.matrix(unlist(strsplit(as.matrix(all_responses %>%
                                                  filter(platform=="prolific",
                                                         during_level_of_awareness=="About right") %>%
                                                  select(during_awareness_how)),";")) %>%
                        table(),ncol=1))) %>%
    rename("count"="V2") %>%
    mutate("platform"="prolific",
           "platform_label"="Prolific Academic",
           "during_level_of_awareness"="About right",
           "platform_sample_size"=all_responses %>% filter(platform=="prolific",
                                                           during_level_of_awareness=="About right") %>% nrow()),
  data.frame(cbind(
    "response"=rownames(as.matrix(unlist(strsplit(as.matrix(all_responses %>%
                                                              filter(platform=="prolific",
                                                                     during_level_of_awareness=="Too much") %>%
                                                              select(during_awareness_how)),";")) %>%
                                    table(),ncol=1)),
    "count"=as.matrix(unlist(strsplit(as.matrix(all_responses %>%
                                                  filter(platform=="prolific",
                                                         during_level_of_awareness=="Too much") %>%
                                                  select(during_awareness_how)),";")) %>%
                        table(),ncol=1))) %>%
    rename("count"="V2") %>%
    mutate("platform"="prolific",
           "platform_label"="Prolific Academic",
           "during_level_of_awareness"="Too much",
           "platform_sample_size"=all_responses %>% filter(platform=="prolific",
                                                           during_level_of_awareness=="Too much") %>% nrow()),
  
  data.frame(cbind(
    "response"=rownames(as.matrix(unlist(strsplit(as.matrix(all_responses %>%
                                                              filter(platform=="twitter",
                                                                     during_level_of_awareness=="Too little") %>%
                                                              select(during_awareness_how)),";")) %>%
                                    table(),ncol=1)),
    "count"=as.matrix(unlist(strsplit(as.matrix(all_responses %>%
                                                  filter(platform=="twitter",
                                                         during_level_of_awareness=="Too little") %>%
                                                  select(during_awareness_how)),";")) %>%
                        table(),ncol=1))) %>%
    rename("count"="V2") %>%
    mutate("platform"="twitter",
           "platform_label"="Twitter",
           "during_level_of_awareness"="Too little",
           "platform_sample_size"=all_responses %>% filter(platform=="twitter",during_level_of_awareness=="Too little") %>% nrow()),
  data.frame(cbind(
    "response"=rownames(as.matrix(unlist(strsplit(as.matrix(all_responses %>%
                                                              filter(platform=="twitter",
                                                                     during_level_of_awareness=="About right") %>%
                                                              select(during_awareness_how)),";")) %>%
                                    table(),ncol=1)),
    "count"=as.matrix(unlist(strsplit(as.matrix(all_responses %>%
                                                  filter(platform=="twitter",
                                                         during_level_of_awareness=="About right") %>%
                                                  select(during_awareness_how)),";")) %>%
                        table(),ncol=1))) %>%
    rename("count"="V2") %>%
    mutate("platform"="twitter",
           "platform_label"="Twitter",
           "during_level_of_awareness"="About right",
           "platform_sample_size"=all_responses %>% filter(platform=="twitter",
                                                           during_level_of_awareness=="About right") %>% nrow()),
  data.frame(cbind(
    "response"=rownames(as.matrix(unlist(strsplit(as.matrix(all_responses %>%
                                                              filter(platform=="twitter",
                                                                     during_level_of_awareness=="Too much") %>%
                                                              select(during_awareness_how)),";")) %>%
                                    table(),ncol=1)),
    "count"=as.matrix(unlist(strsplit(as.matrix(all_responses %>%
                                                  filter(platform=="twitter",
                                                         during_level_of_awareness=="Too much") %>%
                                                  select(during_awareness_how)),";")) %>%
                        table(),ncol=1))) %>%
    rename("count"="V2") %>%
    mutate("platform"="twitter",
           "platform_label"="Twitter",
           "during_level_of_awareness"="Too much",
           "platform_sample_size"=all_responses %>% filter(platform=="twitter",
                                                           during_level_of_awareness=="Too much") %>% nrow())
)
rownames(during_awareness_how_awareness) <- c()

during_awareness_how_awareness <- during_awareness_how_awareness %>%
  mutate(count = as.numeric(count),
         response = ifelse(response=="Other ","Other",response),
         response = factor(response,
                           levels=c("I was not aware","Newspaper (online or print)","News show (TV or online)",
                                    "Social media","Internet search","Academic reports and papers","Other")),
         during_level_of_awareness_label = factor(ifelse(during_level_of_awareness=="Too little",
                                                         "Too little awareness",
                                                         ifelse(during_level_of_awareness=="Too much",
                                                                "Too much awareness",
                                                                ifelse(during_level_of_awareness=="About right",
                                                                       "About right awareness",NA))),
                                                  levels=c("Too little awareness","About right awareness",
                                                           "Too much awareness")))

during_awareness_how_awareness <- rbind(during_awareness_how_awareness,
                                        setNames(data.frame("response"="I was not aware","count"=0,
                                                            "platform"="twitter",
                                                            "platform_label"="Twitter",
                                                            "during_level_of_awareness"="Too much",
                                                            "platform_sample_size"=17,
                                                            "during_level_of_awareness_label"="Too much awareness"),
                                                 names(during_awareness_how_awareness)),
                                        setNames(data.frame("response"="Other","count"=0,
                                                            "platform"="prolific",
                                                            "platform_label"="Prolific Academic",
                                                            "during_level_of_awareness"="Too much",
                                                            "platform_sample_size"=10,
                                                            "during_level_of_awareness_label"="Too much awareness"),
                                                 names(during_awareness_how_awareness))
)


during_awareness_how_awareness %>% mutate(percentage = count/platform_sample_size *100) %>%
  dplyr::select(response,platform_label,during_level_of_awareness,count,platform_sample_size,percentage) %>%
  arrange(platform_label,response) %>% 
  mutate(percentage = round(percentage))

during_awareness_how_awareness <- during_awareness_how_awareness %>% 
  mutate(platform_label_generic = ifelse(platform=="prolific","Online panel",
                                         ifelse(platform=="twitter","Social media",NA)))

## Supplementary Figure 3
ggplot(during_awareness_how_awareness,aes(x=response,y=count/platform_sample_size,fill=platform_label_generic))+
  geom_col(position="dodge")+
  facet_wrap(~during_level_of_awareness_label,nrow=3)+
  theme_bw()+
  scale_fill_manual(values=c("#1e2761","#408ec6"))+
  scale_y_continuous(labels=scales::label_percent(accuracy=1),n.breaks=6) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(legend.position="bottom",strip.background = element_rect(fill="white"))+
  labs(x="During the COVID-19 pandemic, how were you aware of transmission modelling?",
       y="Percentage of responsdents (%)",
       fill=" ")
ggsave("outputs/SupFig3.png",width=7,height=6)



### number who have no awareness and say is about right

prolific %>% filter(during_level_of_awareness=="About right") %>% group_by(during_awareness_use_in_policy) %>%
  summarise("count"=length(during_awareness_use_in_policy))

twitter %>% filter(during_level_of_awareness=="About right") %>% group_by(during_awareness_use_in_policy) %>%
  summarise("count"=length(during_awareness_use_in_policy))


