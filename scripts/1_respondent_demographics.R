### Respondent demographics

## Questions covered:
## Please select your age.
## Please select your gender.
## Please select the option below which most closely matches the sector you work in.
## Have you been vaccinated for COVID-19?

library(tidyverse)
library(readxl)
library(cowplot)

prolific <- read_excel("Data/responses_prolific.xlsx") %>% data.frame() %>%
  select(-c(Start.time,Completion.time,Email,consent_1,consent_2))

twitter <- read_excel("Data/responses_twitter.xlsx") %>% data.frame() %>%
  select(-c(Start.time,Completion.time,Email,consent_1,consent_2))

all_responses <- rbind(prolific,twitter) %>% group_by(platform) %>%
  mutate("platform_label"=ifelse(platform=="twitter","Twitter","Prolific Academic"),
         "platform_label_generic" = ifelse(platform=="twitter","Social media","Online panel"),
         "platform_sample_size"=length(platform),
         "unique_ID"=paste0(platform,"_",ID)) %>% data.frame() %>%
  filter(!is.na(gender),!is.na(age_group),!is.na(sector),!is.na(vaccinated)) %>%
  mutate(sector = factor(sector,
                         levels=c("Agriculture","Business, finance and technology","Creative","Education",
                                  "Health and social work","Production (energy, manufacturing, mining)",
                                  "Public","Research","Retail, hospitality and tourism","Other")),
         vaccinated = factor(vaccinated,
                             levels=c("Yes","No, but I plan to in the future",
                                      "No, and I do not plan to")))

## number of responses from each platform
table(all_responses$platform)

## Table 1 
table(all_responses$age_group,all_responses$platform,useNA = "ifany")
round(prop.table(table(all_responses$age_group,all_responses$platform_label,useNA = "ifany"),2)*100)

table(all_responses$gender,all_responses$platform,useNA = "ifany")
round(prop.table(table(all_responses$gender,all_responses$platform_label,useNA = "ifany"),2)*100)

table(all_responses$sector,all_responses$platform,useNA = "ifany")
round(prop.table(table(all_responses$sector,all_responses$platform_label,useNA = "ifany"),2)*100)

table(all_responses$vaccinated,all_responses$platform,useNA = "ifany")
round(prop.table(table(all_responses$vaccinated,all_responses$platform_label,useNA = "ifany"),2)*100)


## Figure 1
plot_grid(plot_grid(ggplot(all_responses,aes(x=gender,fill=platform_label_generic,group=platform_label_generic))+
                      geom_bar(aes(y=..prop..),position="dodge")+
                      theme_bw()+
                      scale_y_continuous(labels=scales::label_percent(accuracy=1),n.breaks=6) +
                      scale_fill_manual(values=c("#1e2761","#408ec6"))+
                      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
                      theme(legend.position="none")+
                      labs(x="Please select your gender.",y="Percentage of \nrespondents (%)",fill="",tag="A"),
                    ggplot(all_responses,aes(x=vaccinated,fill=platform_label_generic,
                                             group=platform_label_generic))+
                      geom_bar(aes(y=..prop..),position="dodge")+
                      theme_bw()+
                      scale_y_continuous(labels=scales::label_percent(accuracy=1),n.breaks=6) +
                      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
                      scale_fill_manual(values=c("#1e2761","#408ec6"))+
                      theme(legend.position="none")+
                      labs(x="Have you been vaccinated for COVID-19?",
                           y="Percentage of \nrespondents (%)",fill="",tag="B"),
                    rel_widths = c(1,1),align="hv"),
          ggplot(all_responses,aes(x=age_group,fill=platform_label_generic,group=platform_label_generic))+
            geom_bar(aes(y=..prop..),position="dodge")+
            theme_bw()+
            scale_y_continuous(labels=scales::label_percent(accuracy=1),n.breaks=6) +
            scale_fill_manual(values=c("#1e2761","#408ec6"))+
            theme(legend.position="none")+
            labs(x="Please select your age group.",y="Percentage of \nrespondents (%)",fill="",tag="C"),
          ggplot(all_responses,aes(x=sector,fill=platform_label_generic,group=platform_label_generic))+
            geom_bar(aes(y=..prop..),position="dodge")+
            theme_bw()+
            scale_y_continuous(labels=scales::label_percent(accuracy=1),n.breaks=6) +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
            scale_fill_manual(values=c("#1e2761","#408ec6"))+
            theme(legend.position="bottom")+
            labs(x="Please select the option which most closely matches the sector you work in.",
                 y="Percentage of \nrespondents (%)",fill="",tag="D"),
          nrow=3,rel_heights=c(1,1,1.5),align="h")
ggsave("outputs/Fig1.png",width=8,height=6)


# statistically significant differences among those who are in research and in education?
prop.test(x=c(all_responses %>% filter(platform_label=="Prolific Academic",sector=="Education") %>% nrow(),
              all_responses %>% filter(platform_label=="Twitter",sector=="Education") %>% nrow()),
          n=c(all_responses %>% filter(platform_label=="Prolific Academic") %>% nrow(),
              all_responses %>% filter(platform_label=="Twitter") %>% nrow()))

prop.test(x=c(all_responses %>% filter(platform_label=="Prolific Academic",sector=="Research") %>% nrow(),
              all_responses %>% filter(platform_label=="Twitter",sector=="Research") %>% nrow()),
          n=c(all_responses %>% filter(platform_label=="Prolific Academic") %>% nrow(),
              all_responses %>% filter(platform_label=="Twitter") %>% nrow()))

# statistically significant differences among vaccination status?
prop.test(x=c(all_responses %>% filter(platform_label=="Prolific Academic",vaccinated=="Yes") %>% nrow(),
              all_responses %>% filter(platform_label=="Twitter",vaccinated=="Yes") %>% nrow()),
          n=c(all_responses %>% filter(platform_label=="Prolific Academic") %>% nrow(),
              all_responses %>% filter(platform_label=="Twitter") %>% nrow()))

prop.test(x=c(all_responses %>% filter(platform_label=="Prolific Academic",
                                       vaccinated=="No, and I do not plan to") %>% nrow(),
              all_responses %>% filter(platform_label=="Twitter",
                                       vaccinated=="No, and I do not plan to") %>% nrow()),
          n=c(all_responses %>% filter(platform_label=="Prolific Academic") %>% nrow(),
              all_responses %>% filter(platform_label=="Twitter") %>% nrow()))

