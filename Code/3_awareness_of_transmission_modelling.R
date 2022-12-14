### Awareness of transmission modelling

### covering the following Qs:

### How were you aware of transmission modelling? (prior and during)
### How much do you know about how transmission modelling has been used throughout the COVID-19 pandemic?

prolific <- read_excel("Data/responses_prolific.xlsx") %>% data.frame() %>%
  select(-c(Start.time,Completion.time,Email,consent_1,consent_2))

twitter <- read_excel("Data/responses_twitter.xlsx") %>% data.frame() %>%
  select(-c(Start.time,Completion.time,Email,consent_1,consent_2))

all_responses <- rbind(prolific,twitter) %>% group_by(platform) %>%
  mutate("platform_label"=ifelse(platform=="twitter","Twitter","Prolific Academic"),
         "platform_sample_size"=length(platform),
         "unique_ID"=paste0(platform,"_",ID)) %>% data.frame() %>%
  filter(!is.na(gender),!is.na(age_group),!is.na(sector),!is.na(vaccinated))


### How were you aware of transmission modelling? 

### format data

### prior to the pandemic

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
         "platform_sample_size"=nrow(all_responses %>% filter(platform=="twitter")))
rownames(prior_awareness_how_twitter) <- c()

prior_awareness_how <- rbind(prior_awareness_how_prolific,prior_awareness_how_twitter) %>%
  mutate(count=as.numeric(count),
         time="Prior to the COVID-19 pandemic") %>% data.frame()
prior_awareness_how$response <- factor(prior_awareness_how$response,
                                       levels=c("I was not aware","Newspaper (online or print)","News show (TV or online)",
                                                "Social media","Internet search","Academic reports and papers","Other"))


### during the pandemic

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
         "platform_sample_size"=nrow(all_responses %>% filter(platform=="twitter")))
rownames(during_awareness_how_twitter) <- c()

during_awareness_how <- rbind(during_awareness_how_prolific,during_awareness_how_twitter) %>%
  mutate(count=as.numeric(count),
         time="During the COVID-19 pandemic",
         response = ifelse(response=="Other ","Other",response)) %>% data.frame()


during_awareness_how$response <- factor(during_awareness_how$response,
                                        levels=c("I was not aware","Newspaper (online or print)","News show (TV or online)",
                                                 "Social media","Internet search","Academic reports and papers","Other"))


## Table 1
awareness_how <- rbind(prior_awareness_how,
                       during_awareness_how) %>%
  group_by(platform,time) %>%
  mutate(perc = count/platform_sample_size *100) %>% data.frame()
awareness_how$time <- factor(awareness_how$time,
                             levels=c("Prior to the COVID-19 pandemic","During the COVID-19 pandemic"))

## Supplementary Figure 3
ggplot(awareness_how,aes(x=response,y=count/platform_sample_size,group=time,fill=time))+
  geom_col(position="dodge")+
  facet_wrap(~platform_label)+
  theme_bw()+
  scale_fill_manual(values=c("#a8c66c","#1b6535"))+
  scale_y_continuous(labels=scales::label_percent(accuracy=1),n.breaks=6) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(legend.position="bottom",strip.background.x = element_rect(fill="white"),
        axis.text.x = element_text(size=8))+
  labs(x="How were you aware of transmission modelling?",y="Percentage of responsdents (%)",
       fill=" ")
ggsave("Plots/SupFig3.png",width=7,height=4)



### How much do you know about how transmission modelling has been used throughout the COVID-19 pandemic?

## Table 1
table(all_responses$platform,all_responses$during_level_of_awareness)

length(which(is.na(all_responses$during_level_of_awareness)==TRUE))

during_level_of_awareness <- all_responses %>% group_by(platform_label,during_level_of_awareness) %>%
  summarise(responses = length(during_level_of_awareness)) %>%
  mutate(during_level_of_awareness = ifelse(is.na(during_level_of_awareness),"Did not answer",
                                            during_level_of_awareness)) %>% ungroup() %>%
  group_by(platform_label) %>%
  mutate(total = sum(responses),
         percentage = 100*responses/total,
         during_level_of_awareness = factor(during_level_of_awareness,
                                            levels=c("Too much","About right","Too little","Did not answer")))

## Supplementary Figure 4
ggplot(during_level_of_awareness,aes(x=platform_label,y=percentage,fill=during_level_of_awareness))+
  geom_col()+
  theme_bw()+
  scale_fill_viridis_d(end=0.7)+
  labs(x="Sample",y="Percentage of respondents (%)",
       fill="How much did you know \nabout how transmission \nmodelling has been used \nthroughout the COVID-19 \npandemic?")
ggsave("Plots/SupFig4.png",width=5,height=4)


## Supplementary Table 3

all_responses_lm <- all_responses %>% filter(!is.na(during_level_of_awareness)) %>%
  select(during_level_of_awareness,age_group,gender,platform) %>%
  mutate(awareness_number = ifelse(during_level_of_awareness=="Too much",3,
                                   ifelse(during_level_of_awareness=="About right",2,
                                          ifelse(during_level_of_awareness=="Too little",1,NA))))


m1 <- glm(awareness_number~age_group+gender,data=all_responses_lm %>% filter(platform=="prolific"),
            family="poisson")
summary(m1)

m2 <- glm(awareness_number~age_group+gender,data=all_responses_lm %>% filter(platform=="twitter"),
            family="poisson")
summary(m2)



### Interaction between two questions 

during_awareness_how_awareness <- rbind(
  data.frame(cbind(
    "response"=rownames(as.matrix(unlist(strsplit(as.matrix(all_responses %>%
                                                              filter(platform=="prolific",
                                                                     during_level_of_awareness=="Too little") %>%
                                                              select(during_awareness_how)),";")) %>%
                                    table(),ncol=1)),
    "count"=as.matrix(unlist(strsplit(as.matrix(all_responses %>%filter(platform=="prolific",
                                                                        during_level_of_awareness=="Too little") %>%
                                                  select(during_awareness_how)),";")) %>%
                        table(),ncol=1))) %>%
    rename("count"="V2") %>%
    mutate("platform"="prolific",
           "platform_label"="Prolific Academic",
           "during_level_of_awareness"="Too little",
           "platform_sample_size"=all_responses %>% filter(platform=="prolific",during_level_of_awareness=="Too little") %>% nrow()),
  data.frame(cbind(
    "response"=rownames(as.matrix(unlist(strsplit(as.matrix(all_responses %>%
                                                              filter(platform=="prolific",
                                                                     during_level_of_awareness=="About right") %>%
                                                              select(during_awareness_how)),";")) %>%
                                    table(),ncol=1)),
    "count"=as.matrix(unlist(strsplit(as.matrix(all_responses %>%filter(platform=="prolific",
                                                                        during_level_of_awareness=="About right") %>%
                                                  select(during_awareness_how)),";")) %>%
                        table(),ncol=1))) %>%
    rename("count"="V2") %>%
    mutate("platform"="prolific",
           "platform_label"="Prolific Academic",
           "during_level_of_awareness"="About right",
           "platform_sample_size"=all_responses %>% filter(platform=="prolific",during_level_of_awareness=="About right") %>% nrow()),
  data.frame(cbind(
    "response"=rownames(as.matrix(unlist(strsplit(as.matrix(all_responses %>%
                                                              filter(platform=="prolific",
                                                                     during_level_of_awareness=="Too much") %>%
                                                              select(during_awareness_how)),";")) %>%
                                    table(),ncol=1)),
    "count"=as.matrix(unlist(strsplit(as.matrix(all_responses %>%filter(platform=="prolific",
                                                                        during_level_of_awareness=="Too much") %>%
                                                  select(during_awareness_how)),";")) %>%
                        table(),ncol=1))) %>%
    rename("count"="V2") %>%
    mutate("platform"="prolific",
           "platform_label"="Prolific Academic",
           "during_level_of_awareness"="Too much",
           "platform_sample_size"=all_responses %>% filter(platform=="prolific",during_level_of_awareness=="Too much") %>% nrow()),
  
  data.frame(cbind(
    "response"=rownames(as.matrix(unlist(strsplit(as.matrix(all_responses %>%
                                                              filter(platform=="twitter",
                                                                     during_level_of_awareness=="Too little") %>%
                                                              select(during_awareness_how)),";")) %>%
                                    table(),ncol=1)),
    "count"=as.matrix(unlist(strsplit(as.matrix(all_responses %>%filter(platform=="twitter",
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
    "count"=as.matrix(unlist(strsplit(as.matrix(all_responses %>%filter(platform=="twitter",
                                                                        during_level_of_awareness=="About right") %>%
                                                  select(during_awareness_how)),";")) %>%
                        table(),ncol=1))) %>%
    rename("count"="V2") %>%
    mutate("platform"="twitter",
           "platform_label"="Twitter",
           "during_level_of_awareness"="About right",
           "platform_sample_size"=all_responses %>% filter(platform=="twitter",during_level_of_awareness=="About right") %>% nrow()),
  data.frame(cbind(
    "response"=rownames(as.matrix(unlist(strsplit(as.matrix(all_responses %>%
                                                              filter(platform=="twitter",
                                                                     during_level_of_awareness=="Too much") %>%
                                                              select(during_awareness_how)),";")) %>%
                                    table(),ncol=1)),
    "count"=as.matrix(unlist(strsplit(as.matrix(all_responses %>%filter(platform=="twitter",
                                                                        during_level_of_awareness=="Too much") %>%
                                                  select(during_awareness_how)),";")) %>%
                        table(),ncol=1))) %>%
    rename("count"="V2") %>%
    mutate("platform"="twitter",
           "platform_label"="Twitter",
           "during_level_of_awareness"="Too much",
           "platform_sample_size"=all_responses %>% filter(platform=="twitter",during_level_of_awareness=="Too much") %>% nrow())
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
                                                            "platform_label"="Twitter","during_level_of_awareness"="Too much",
                                                            "platform_sample_size"=17,"during_level_of_awareness_label"="Too much awareness"),
                                                 names(during_awareness_how_awareness)),
                                        setNames(data.frame("response"="Other","count"=0,
                                                            "platform"="prolific",
                                                            "platform_label"="Prolific Academic","during_level_of_awareness"="Too much",
                                                            "platform_sample_size"=10,"during_level_of_awareness_label"="Too much awareness"),
                                                 names(during_awareness_how_awareness))
)


## Supplementary Table 4
during_awareness_how_awareness %>% mutate(percentage = count/platform_sample_size *100) %>%
  dplyr::select(response,platform_label,during_level_of_awareness,count,platform_sample_size,percentage) %>%
  arrange(platform_label,response)


## Supplementary Figure 5
ggplot(during_awareness_how_awareness,aes(x=response,y=count/platform_sample_size,fill=platform_label))+
  geom_col(position="dodge")+
  facet_wrap(~during_level_of_awareness_label,nrow=3)+
  theme_bw()+
  scale_fill_manual(values=c("#1e2761","#408ec6"))+
  scale_y_continuous(labels=scales::label_percent(accuracy=1),n.breaks=6) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme(legend.position="bottom",strip.background = element_rect(fill="white"))+
  labs(x="During the COVID-19 pandemic, how were you aware of transmission modelling?",y="Percentage of responsdents (%)",
       fill=" ")
ggsave("Plots/SupFig5.png",width=7,height=6)



