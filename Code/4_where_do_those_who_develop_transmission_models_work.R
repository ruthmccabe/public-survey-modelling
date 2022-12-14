### Where do those who develop transmission models work?

## Questions covered:
## Where do you think those who developed and used transmission models work? (prior and during)

library(readxl)
library(tidyverse)

workplace <- read_excel("Data/modeller_workplace.xlsx") %>% select(-comments)

workplace_long <- gather(workplace,
                         key="category",value="text",
                         w1,w2,w3,w4) %>% filter(!is.na(text)) %>%
  mutate(platform_label = factor(platform_label),
         time = factor(time,
                       levels=c("Prior to the COVID-19 pandemic","During the COVID-19 pandemic")),
         text = factor(text))

## Supplementary Figure 6
workplace_long_summary <- workplace_long %>% group_by(platform_label,time,text,.drop=FALSE) %>%
  summarise("total" = length(text)) %>%
  mutate("sample_size"=ifelse(platform_label=="Prolific Academic",504,
                              ifelse(platform_label=="Twitter",202,NA)))

workplace_long_summary_tidy <- workplace_long_summary %>%
  mutate("text_summarised" = ifelse(text=="Advisory"|text=="SAGE","Advisory roles",
                                    ifelse(text=="Health service"|text=="Hospitals"|text=="NHS","Healthcare services",
                                           ifelse(text=="Unknown","Unsure",
                                                  ifelse(text=="Academia","Research (Academia)",
                                                         ifelse(text=="Research","Research (Outside of academia)",
                                                                ifelse(text=="Pharma","Pharmaceutical industry",
                                                                       ifelse(text=="Public health",
                                                                              "Public health bodies",
                                                                              ifelse(text=="NGOs"|text=="Civil service",
                                                                                     "Other",
                                                                                     as.character(text)))))))))) %>%
  group_by(platform_label,sample_size,time,text_summarised) %>%
  summarise(total = sum(total)) %>%
  mutate(text_summarised = factor(text_summarised,
                                  levels=c("Advisory roles","Government","Healthcare services","Media",
                                           "Pharmaceutical industry","Public health bodies","Research (Academia)",
                                           "Research (Outside of academia)","Unsure","Other")))

ggplot(workplace_long_summary_tidy,aes(x=text_summarised,y=100*total/sample_size,group=time,fill=time))+
  geom_col(position="dodge")+
  facet_wrap(~platform_label,nrow=2)+
  theme_bw()+
  scale_fill_manual(values=c("#a8c66c","#1b6535"))+
  labs(x="Where do you think those who developed and used transmission models work?",y="Percentage of repondents (%)",
       fill="")+
  theme(strip.background = element_rect(fill="white"),
        legend.position = "bottom")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))
ggsave("Plots/SupFig6.png",width=8.5,height=5)


## Supplementary Table 5
workplace_long_summary_tidy %>% data.frame() %>% arrange(text_summarised) %>% mutate(prop = total/sample_size *100)

