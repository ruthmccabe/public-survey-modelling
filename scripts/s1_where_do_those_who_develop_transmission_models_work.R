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
         platform_label_generic = ifelse(platform_label=="Prolific Academic","Online panel","Social media"),
         time = factor(time,
                       levels=c("Prior to the COVID-19 pandemic","During the COVID-19 pandemic")),
         text = factor(text))

## Supplementary Figure 16
workplace_long_summary <- workplace_long %>% group_by(platform_label,platform_label_generic,
                                                      time,text,.drop=FALSE) %>%
  summarise("total" = length(text)) %>%
  mutate("sample_size"=ifelse(platform_label=="Prolific Academic",504,
                              ifelse(platform_label=="Twitter",202,NA)))

workplace_long_summary_tidy <- workplace_long_summary %>%
  mutate("text_summarised" = ifelse(text=="Advisory"|text=="SAGE","Advisory roles",
                                    ifelse(text=="Health service"|text=="Hospitals"|text=="NHS",
                                           "Healthcare services",
                                           ifelse(text=="Unknown","Unsure",
                                                  ifelse(text=="Civil service","Government",
                                                  ifelse(text=="Academia","Research (Academia)",
                                                         ifelse(text=="Research",
                                                                "Research (Unspecified affiliation)",
                                                                ifelse(text=="Pharma","Pharmaceutical industry",
                                                                       ifelse(text=="Public health",
                                                                              "Public health bodies",
                                                                              ifelse(text=="NGOs","Other",
                                                                                     as.character(text))))))))))) %>%
  group_by(platform_label_generic,sample_size,time,text_summarised) %>%
  summarise(total = sum(total)) %>%
  mutate(text_summarised = factor(text_summarised,
                                  levels=c("Advisory roles","Government","Healthcare services","Media",
                                           "Pharmaceutical industry","Public health bodies","Research (Academia)",
                                           "Research (Unspecified affiliation)","Unsure","Other")))

ggplot(workplace_long_summary_tidy,aes(x=text_summarised,y=100*total/sample_size,group=time,fill=time))+
  geom_col(position="dodge")+
  facet_wrap(~platform_label_generic,nrow=2)+
  theme_bw()+
  scale_fill_manual(values=c("#a8c66c","#1b6535"))+
  labs(x="Where do you think those who developed and used transmission models work?",y="Percentage of repondents (%)",
       fill="")+
  theme(strip.background = element_rect(fill="white"),
        legend.position = "bottom")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))
ggsave("outputs/SupFig16.png",width=8.5,height=5)


## Supplementary Table 25
workplace_long_summary_tidy %>% data.frame() %>% arrange(text_summarised) %>% mutate(perc = total/sample_size *100)

## SAGE specifically
workplace_long_summary %>% filter(text=="SAGE")

## NHS specifically 
workplace_long_summary %>% filter(text=="NHS")



# need to create binary indicators for all categories 
## ifelse statements on w1-w4 using grepl


workplace_numeric <- workplace %>% 
  mutate(advisory_ind = ifelse(grepl("Advisory",w1,fixed=TRUE)|grepl("Advisory",w2,fixed=TRUE)|grepl("Advisory",w3,fixed=TRUE)|grepl("Advisory",w4,fixed=TRUE)|grepl("SAGE",w1,fixed=TRUE)|grepl("SAGE",w2,fixed=TRUE)|grepl("SAGE",w3,fixed=TRUE)|grepl("SAGE",w4,fixed=TRUE),1,0),
         sage_ind = ifelse(grepl("SAGE",w1,fixed=TRUE)|grepl("SAGE",w2,fixed=TRUE)|grepl("SAGE",w3,fixed=TRUE)|grepl("SAGE",w4,fixed=TRUE),1,0),
         government_ind = ifelse(grepl("Government",w1,fixed=TRUE)|grepl("Government",w2,fixed=TRUE)|grepl("Government",w3,fixed=TRUE)|grepl("Government",w4,fixed=TRUE)|grepl("Civil service",w1,fixed=TRUE)|grepl("Civil service",w2,fixed=TRUE)|grepl("Civil service",w3,fixed=TRUE)|grepl("Civil service",w4,fixed=TRUE),1,0),
         healthcare_ind = ifelse(grepl("Health service",w1,fixed=TRUE)|grepl("Health service",w2,fixed=TRUE)|grepl("Health service",w3,fixed=TRUE)|grepl("Health service",w4,fixed=TRUE)|grepl("Hospitals",w1,fixed=TRUE)|grepl("Hospitals",w2,fixed=TRUE)|grepl("Hospitals",w3,fixed=TRUE)|grepl("Hospitals",w4,fixed=TRUE)|grepl("NHS",w1,fixed=TRUE)|grepl("NHS",w2,fixed=TRUE)|grepl("NHS",w3,fixed=TRUE)|grepl("NHS",w4,fixed=TRUE),1,0),
         nhs_ind = ifelse(grepl("NHS",w1,fixed=TRUE)|grepl("NHS",w2,fixed=TRUE)|grepl("NHS",w3,fixed=TRUE)|grepl("NHS",w4,fixed=TRUE),1,0),
         media_ind = ifelse(grepl("Media",w1,fixed=TRUE)|grepl("Media",w2,fixed=TRUE)|grepl("Media",w3,fixed=TRUE)|grepl("Media",w4,fixed=TRUE),1,0),
         pharma_ind = ifelse(grepl("Pharma",w1,fixed=TRUE)|grepl("Pharma",w2,fixed=TRUE)|grepl("Pharma",w3,fixed=TRUE)|grepl("Pharma",w4,fixed=TRUE),1,0),
         pubhealth_ind = ifelse(grepl("Public health",w1,fixed=TRUE)|grepl("Public health",w2,fixed=TRUE)|grepl("Public health",w3,fixed=TRUE)|grepl("Public health",w4,fixed=TRUE),1,0),
         academia_ind = ifelse(grepl("Academia",w1,fixed=TRUE)|grepl("Academia",w2,fixed=TRUE)|grepl("Academia",w3,fixed=TRUE)|grepl("Academia",w4,fixed=TRUE),1,0),
         research_ind = ifelse(grepl("Research",w1,fixed=TRUE)|grepl("Research",w2,fixed=TRUE)|grepl("Research",w3,fixed=TRUE)|grepl("Research",w4,fixed=TRUE),1,0),
         unsure_ind = ifelse(grepl("Unknown",w1,fixed=TRUE)|grepl("Unknown",w2,fixed=TRUE)|grepl("Unknown",w3,fixed=TRUE)|grepl("Unknown",w4,fixed=TRUE),1,0),
         other_ind = ifelse(grepl("Other",w1,fixed=TRUE)|grepl("Other",w2,fixed=TRUE)|grepl("Other",w3,fixed=TRUE)|grepl("Other",w4,fixed=TRUE)|grepl("NGOs",w1,fixed=TRUE)|grepl("NGOs",w2,fixed=TRUE)|grepl("NGOs",w3,fixed=TRUE)|grepl("NGOs",w4,fixed=TRUE),1,0))

colSums(workplace_numeric[,9:20])
#write.csv(workplace_numeric,"Data/workplace_numeric_debug.csv",row.names = FALSE)
workplace_long_summary %>% group_by(text) %>% summarise(total = sum(total))

## Prolific Academic

wilcox.test(x=workplace_numeric %>% filter(platform_label=="Prolific Academic",
                                           time=="Prior to the COVID-19 pandemic") %>% 
              dplyr::select(advisory_ind) %>% unlist() %>% as.numeric(),
            y=workplace_numeric %>% filter(platform_label=="Prolific Academic",
                                         time=="During the COVID-19 pandemic") %>% 
              dplyr::select(advisory_ind) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")

wilcox.test(x=workplace_numeric %>% filter(platform_label=="Prolific Academic",
                                           time=="Prior to the COVID-19 pandemic") %>% 
              dplyr::select(sage_ind) %>% unlist() %>% as.numeric(),
            y=workplace_numeric %>% filter(platform_label=="Prolific Academic",
                                           time=="During the COVID-19 pandemic") %>% 
              dplyr::select(sage_ind) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")

wilcox.test(x=workplace_numeric %>% filter(platform_label=="Prolific Academic",
                                           time=="Prior to the COVID-19 pandemic") %>% 
              dplyr::select(government_ind) %>% unlist() %>% as.numeric(),
            y=workplace_numeric %>% filter(platform_label=="Prolific Academic",
                                           time=="During the COVID-19 pandemic") %>% 
              dplyr::select(government_ind) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")

wilcox.test(x=workplace_numeric %>% filter(platform_label=="Prolific Academic",
                                           time=="Prior to the COVID-19 pandemic") %>% 
              dplyr::select(healthcare_ind) %>% unlist() %>% as.numeric(),
            y=workplace_numeric %>% filter(platform_label=="Prolific Academic",
                                           time=="During the COVID-19 pandemic") %>% 
              dplyr::select(healthcare_ind) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")

wilcox.test(x=workplace_numeric %>% filter(platform_label=="Prolific Academic",
                                           time=="Prior to the COVID-19 pandemic") %>% 
              dplyr::select(nhs_ind) %>% unlist() %>% as.numeric(),
            y=workplace_numeric %>% filter(platform_label=="Prolific Academic",
                                           time=="During the COVID-19 pandemic") %>% 
              dplyr::select(nhs_ind) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")

wilcox.test(x=workplace_numeric %>% filter(platform_label=="Prolific Academic",
                                           time=="Prior to the COVID-19 pandemic") %>% 
              dplyr::select(media_ind) %>% unlist() %>% as.numeric(),
            y=workplace_numeric %>% filter(platform_label=="Prolific Academic",
                                           time=="During the COVID-19 pandemic") %>% 
              dplyr::select(media_ind) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")

wilcox.test(x=workplace_numeric %>% filter(platform_label=="Prolific Academic",
                                           time=="Prior to the COVID-19 pandemic") %>% 
              dplyr::select(pharma_ind) %>% unlist() %>% as.numeric(),
            y=workplace_numeric %>% filter(platform_label=="Prolific Academic",
                                           time=="During the COVID-19 pandemic") %>% 
              dplyr::select(pharma_ind) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")

wilcox.test(x=workplace_numeric %>% filter(platform_label=="Prolific Academic",
                                           time=="Prior to the COVID-19 pandemic") %>% 
              dplyr::select(pubhealth_ind) %>% unlist() %>% as.numeric(),
            y=workplace_numeric %>% filter(platform_label=="Prolific Academic",
                                           time=="During the COVID-19 pandemic") %>% 
              dplyr::select(pubhealth_ind) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")

wilcox.test(x=workplace_numeric %>% filter(platform_label=="Prolific Academic",
                                           time=="Prior to the COVID-19 pandemic") %>% 
              dplyr::select(academia_ind) %>% unlist() %>% as.numeric(),
            y=workplace_numeric %>% filter(platform_label=="Prolific Academic",
                                           time=="During the COVID-19 pandemic") %>% 
              dplyr::select(academia_ind) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")

wilcox.test(x=workplace_numeric %>% filter(platform_label=="Prolific Academic",
                                           time=="Prior to the COVID-19 pandemic") %>% 
              dplyr::select(research_ind) %>% unlist() %>% as.numeric(),
            y=workplace_numeric %>% filter(platform_label=="Prolific Academic",
                                           time=="During the COVID-19 pandemic") %>% 
              dplyr::select(research_ind) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")

wilcox.test(x=workplace_numeric %>% filter(platform_label=="Prolific Academic",
                                           time=="Prior to the COVID-19 pandemic") %>% 
              dplyr::select(unsure_ind) %>% unlist() %>% as.numeric(),
            y=workplace_numeric %>% filter(platform_label=="Prolific Academic",
                                           time=="During the COVID-19 pandemic") %>% 
              dplyr::select(unsure_ind) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")

wilcox.test(x=workplace_numeric %>% filter(platform_label=="Prolific Academic",
                                           time=="Prior to the COVID-19 pandemic") %>% 
              dplyr::select(other_ind) %>% unlist() %>% as.numeric(),
            y=workplace_numeric %>% filter(platform_label=="Prolific Academic",
                                           time=="During the COVID-19 pandemic") %>% 
              dplyr::select(other_ind) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")

## Twitter

wilcox.test(x=workplace_numeric %>% filter(platform_label=="Twitter",
                                           time=="Prior to the COVID-19 pandemic") %>% 
              dplyr::select(advisory_ind) %>% unlist() %>% as.numeric(),
            y=workplace_numeric %>% filter(platform_label=="Twitter",
                                           time=="During the COVID-19 pandemic") %>% 
              dplyr::select(advisory_ind) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")

wilcox.test(x=workplace_numeric %>% filter(platform_label=="Twitter",
                                           time=="Prior to the COVID-19 pandemic") %>% 
              dplyr::select(sage_ind) %>% unlist() %>% as.numeric(),
            y=workplace_numeric %>% filter(platform_label=="Twitter",
                                           time=="During the COVID-19 pandemic") %>% 
              dplyr::select(sage_ind) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")

wilcox.test(x=workplace_numeric %>% filter(platform_label=="Twitter",
                                           time=="Prior to the COVID-19 pandemic") %>% 
              dplyr::select(government_ind) %>% unlist() %>% as.numeric(),
            y=workplace_numeric %>% filter(platform_label=="Twitter",
                                           time=="During the COVID-19 pandemic") %>% 
              dplyr::select(government_ind) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")

wilcox.test(x=workplace_numeric %>% filter(platform_label=="Twitter",
                                           time=="Prior to the COVID-19 pandemic") %>% 
              dplyr::select(healthcare_ind) %>% unlist() %>% as.numeric(),
            y=workplace_numeric %>% filter(platform_label=="Twitter",
                                           time=="During the COVID-19 pandemic") %>% 
              dplyr::select(healthcare_ind) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")

wilcox.test(x=workplace_numeric %>% filter(platform_label=="Twitter",
                                           time=="Prior to the COVID-19 pandemic") %>% 
              dplyr::select(nhs_ind) %>% unlist() %>% as.numeric(),
            y=workplace_numeric %>% filter(platform_label=="Twitter",
                                           time=="During the COVID-19 pandemic") %>% 
              dplyr::select(nhs_ind) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")

wilcox.test(x=workplace_numeric %>% filter(platform_label=="Twitter",
                                           time=="Prior to the COVID-19 pandemic") %>% 
              dplyr::select(media_ind) %>% unlist() %>% as.numeric(),
            y=workplace_numeric %>% filter(platform_label=="Twitter",
                                           time=="During the COVID-19 pandemic") %>% 
              dplyr::select(media_ind) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")

wilcox.test(x=workplace_numeric %>% filter(platform_label=="Twitter",
                                           time=="Prior to the COVID-19 pandemic") %>% 
              dplyr::select(pharma_ind) %>% unlist() %>% as.numeric(),
            y=workplace_numeric %>% filter(platform_label=="Twitter",
                                           time=="During the COVID-19 pandemic") %>% 
              dplyr::select(pharma_ind) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")

wilcox.test(x=workplace_numeric %>% filter(platform_label=="Twitter",
                                           time=="Prior to the COVID-19 pandemic") %>% 
              dplyr::select(pubhealth_ind) %>% unlist() %>% as.numeric(),
            y=workplace_numeric %>% filter(platform_label=="Twitter",
                                           time=="During the COVID-19 pandemic") %>% 
              dplyr::select(pubhealth_ind) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")

wilcox.test(x=workplace_numeric %>% filter(platform_label=="Twitter",
                                           time=="Prior to the COVID-19 pandemic") %>% 
              dplyr::select(academia_ind) %>% unlist() %>% as.numeric(),
            y=workplace_numeric %>% filter(platform_label=="Twitter",
                                           time=="During the COVID-19 pandemic") %>% 
              dplyr::select(academia_ind) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")

wilcox.test(x=workplace_numeric %>% filter(platform_label=="Twitter",
                                           time=="Prior to the COVID-19 pandemic") %>% 
              dplyr::select(research_ind) %>% unlist() %>% as.numeric(),
            y=workplace_numeric %>% filter(platform_label=="Twitter",
                                           time=="During the COVID-19 pandemic") %>% 
              dplyr::select(research_ind) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")

wilcox.test(x=workplace_numeric %>% filter(platform_label=="Twitter",
                                           time=="Prior to the COVID-19 pandemic") %>% 
              dplyr::select(unsure_ind) %>% unlist() %>% as.numeric(),
            y=workplace_numeric %>% filter(platform_label=="Twitter",
                                           time=="During the COVID-19 pandemic") %>% 
              dplyr::select(unsure_ind) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")

wilcox.test(x=workplace_numeric %>% filter(platform_label=="Twitter",
                                           time=="Prior to the COVID-19 pandemic") %>% 
              dplyr::select(other_ind) %>% unlist() %>% as.numeric(),
            y=workplace_numeric %>% filter(platform_label=="Twitter",
                                           time=="During the COVID-19 pandemic") %>% 
              dplyr::select(other_ind) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")



## Supplementary Table 25

## Prior to the COVID-19 pandemic 

prop.test(x=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Advisory roles",
                                                     time=="Prior to the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(total) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",text_summarised=="Advisory roles",
                                                     time=="Prior to the COVID-19 pandemic") %>%
                ungroup() %>%dplyr::select(total) %>% unlist()),
          n=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Advisory roles",
                                                     time=="Prior to the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(sample_size) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",text_summarised=="Advisory roles",
                                                     time=="Prior to the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(sample_size) %>% unlist()))

prop.test(x=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Government",
                                                     time=="Prior to the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(total) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",text_summarised=="Government",
                                                     time=="Prior to the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(total) %>% unlist()),
          n=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Government",
                                                     time=="Prior to the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(sample_size) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",text_summarised=="Government",
                                                     time=="Prior to the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(sample_size) %>% unlist()))

prop.test(x=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Healthcare services",
                                                     time=="Prior to the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(total) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",
                                                     text_summarised=="Healthcare services",
                                                     time=="Prior to the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(total) %>% unlist()),
          n=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Healthcare services",
                                                     time=="Prior to the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(sample_size) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",
                                                     text_summarised=="Healthcare services",
                                                     time=="Prior to the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(sample_size) %>% unlist()))

prop.test(x=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",text_summarised=="Media",
                                                     time=="Prior to the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(total) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",text_summarised=="Media",
                                                     time=="Prior to the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(total) %>% unlist()),
          n=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",text_summarised=="Media",
                                                     time=="Prior to the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(sample_size) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",text_summarised=="Media",
                                                     time=="Prior to the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(sample_size) %>% unlist()))

prop.test(x=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Pharmaceutical industry",
                                                     time=="Prior to the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(total) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",
                                                     text_summarised=="Pharmaceutical industry",
                                                     time=="Prior to the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(total) %>% unlist()),
          n=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Pharmaceutical industry",
                                                     time=="Prior to the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(sample_size) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",
                                                     text_summarised=="Pharmaceutical industry",
                                                     time=="Prior to the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(sample_size) %>% unlist()))

prop.test(x=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Public health bodies",
                                                     time=="Prior to the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(total) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",
                                                     text_summarised=="Public health bodies",
                                                     time=="Prior to the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(total) %>% unlist()),
          n=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Public health bodies",
                                                     time=="Prior to the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(sample_size) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",
                                                     text_summarised=="Public health bodies",
                                                     time=="Prior to the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(sample_size) %>% unlist()))

prop.test(x=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Research (Academia)",
                                                     time=="Prior to the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(total) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",
                                                     text_summarised=="Research (Academia)",
                                                     time=="Prior to the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(total) %>% unlist()),
          n=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Research (Academia)",
                                                     time=="Prior to the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(sample_size) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",
                                                     text_summarised=="Research (Academia)",
                                                     time=="Prior to the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(sample_size) %>% unlist()))

prop.test(x=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Research (Unspecified affiliation)",
                                                     time=="Prior to the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(total) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",
                                                     text_summarised=="Research (Unspecified affiliation)",
                                                     time=="Prior to the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(total) %>% unlist()),
          n=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Research (Unspecified affiliation)",
                                                     time=="Prior to the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(sample_size) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",
                                                     text_summarised=="Research (Unspecified affiliation)",
                                                     time=="Prior to the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(sample_size) %>% unlist()))

prop.test(x=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Unsure",
                                                     time=="Prior to the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(total) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",text_summarised=="Unsure",
                                                     time=="Prior to the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(total) %>% unlist()),
          n=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Unsure",
                                                     time=="Prior to the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(sample_size) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",text_summarised=="Unsure",
                                                     time=="Prior to the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(sample_size) %>% unlist()))

prop.test(x=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",text_summarised=="Other",
                                                     time=="Prior to the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(total) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",text_summarised=="Other",
                                                     time=="Prior to the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(total) %>% unlist()),
          n=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",text_summarised=="Other",
                                                     time=="Prior to the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(sample_size) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",text_summarised=="Other",
                                                     time=="Prior to the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(sample_size) %>% unlist()))


### SAGE
prop.test(x=c(workplace_long_summary %>% filter(platform_label=="Prolific Academic",text=="SAGE",
                                                time=="Prior to the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(total) %>% unlist(),
              workplace_long_summary %>% filter(platform_label=="Twitter",text=="SAGE",
                                                time=="Prior to the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(total) %>% unlist()),
          n=c(workplace_long_summary %>% filter(platform_label=="Prolific Academic",text=="SAGE",
                                                time=="Prior to the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(sample_size) %>% unlist(),
              workplace_long_summary %>% filter(platform_label=="Twitter",text=="SAGE",
                                                time=="Prior to the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(sample_size) %>% unlist()))

### NHS 
prop.test(x=c(workplace_long_summary %>% filter(platform_label=="Prolific Academic",text=="NHS",
                                                time=="Prior to the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(total) %>% unlist(),
              workplace_long_summary %>% filter(platform_label=="Twitter",text=="NHS",
                                                time=="Prior to the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(total) %>% unlist()),
          n=c(workplace_long_summary %>% filter(platform_label=="Prolific Academic",text=="NHS",
                                                time=="Prior to the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(sample_size) %>% unlist(),
              workplace_long_summary %>% filter(platform_label=="Twitter",text=="NHS",
                                                time=="Prior to the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(sample_size) %>% unlist()))


## During the COVID-19 pandemic 

prop.test(x=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Advisory roles",
                                                     time=="During the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(total) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",
                                                     text_summarised=="Advisory roles",
                                                     time=="During the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(total) %>% unlist()),
          n=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Advisory roles",
                                                     time=="During the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(sample_size) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",text_summarised=="Advisory roles",
                                                     time=="During the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(sample_size) %>% unlist()))

prop.test(x=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Government",
                                                     time=="During the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(total) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",text_summarised=="Government",
                                                     time=="During the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(total) %>% unlist()),
          n=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Government",
                                                     time=="During the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(sample_size) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",text_summarised=="Government",
                                                     time=="During the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(sample_size) %>% unlist()))

prop.test(x=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Healthcare services",
                                                     time=="During the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(total) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",
                                                     text_summarised=="Healthcare services",
                                                     time=="During the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(total) %>% unlist()),
          n=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Healthcare services",
                                                     time=="During the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(sample_size) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",
                                                     text_summarised=="Healthcare services",
                                                     time=="During the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(sample_size) %>% unlist()))

prop.test(x=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Media",
                                                     time=="During the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(total) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",text_summarised=="Media",
                                                     time=="During the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(total) %>% unlist()),
          n=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",text_summarised=="Media",
                                                     time=="During the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(sample_size) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",text_summarised=="Media",
                                                     time=="During the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(sample_size) %>% unlist()))

prop.test(x=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Pharmaceutical industry",
                                                     time=="During the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(total) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",
                                                     text_summarised=="Pharmaceutical industry",
                                                     time=="During the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(total) %>% unlist()),
          n=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Pharmaceutical industry",
                                                     time=="During the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(sample_size) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",
                                                     text_summarised=="Pharmaceutical industry",
                                                     time=="During the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(sample_size) %>% unlist()))

prop.test(x=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Public health bodies",
                                                     time=="During the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(total) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",
                                                     text_summarised=="Public health bodies",
                                                     time=="During the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(total) %>% unlist()),
          n=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Public health bodies",
                                                     time=="During the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(sample_size) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",
                                                     text_summarised=="Public health bodies",
                                                     time=="During the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(sample_size) %>% unlist()))

prop.test(x=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Research (Academia)",
                                                     time=="During the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(total) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",
                                                     text_summarised=="Research (Academia)",
                                                     time=="During the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(total) %>% unlist()),
          n=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Research (Academia)",
                                                     time=="During the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(sample_size) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",
                                                     text_summarised=="Research (Academia)",
                                                     time=="During the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(sample_size) %>% unlist()))

prop.test(x=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Research (Unspecified affiliation)",
                                                     time=="During the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(total) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",
                                                     text_summarised=="Research (Unspecified affiliation)",
                                                     time=="During the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(total) %>% unlist()),
          n=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Research (Unspecified affiliation)",
                                                     time=="During the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(sample_size) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",
                                                     text_summarised=="Research (Unspecified affiliation)",
                                                     time=="During the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(sample_size) %>% unlist()))

prop.test(x=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Unsure",
                                                     time=="During the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(total) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",text_summarised=="Unsure",
                                                     time=="During the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(total) %>% unlist()),
          n=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",
                                                     text_summarised=="Unsure",
                                                     time=="During the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(sample_size) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",text_summarised=="Unsure",
                                                     time=="During the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(sample_size) %>% unlist()))


prop.test(x=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",text_summarised=="Other",
                                                     time=="During the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(total) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",text_summarised=="Other",
                                                     time=="During the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(total) %>% unlist()),
          n=c(workplace_long_summary_tidy %>% filter(platform_label=="Prolific Academic",text_summarised=="Other",
                                                     time=="During the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(sample_size) %>% unlist(),
              workplace_long_summary_tidy %>% filter(platform_label=="Twitter",text_summarised=="Other",
                                                     time=="During the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(sample_size) %>% unlist()))


### SAGE
prop.test(x=c(workplace_long_summary %>% filter(platform_label=="Prolific Academic",text=="SAGE",
                                                time=="During the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(total) %>% unlist(),
              workplace_long_summary %>% filter(platform_label=="Twitter",text=="SAGE",
                                                time=="During the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(total) %>% unlist()),
          n=c(workplace_long_summary %>% filter(platform_label=="Prolific Academic",text=="SAGE",
                                                time=="During the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(sample_size) %>% unlist(),
              workplace_long_summary %>% filter(platform_label=="Twitter",text=="SAGE",
                                                time=="During the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(sample_size) %>% unlist()))

### NHS 
prop.test(x=c(workplace_long_summary %>% filter(platform_label=="Prolific Academic",text=="NHS",
                                                time=="During the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(total) %>% unlist(),
              workplace_long_summary %>% filter(platform_label=="Twitter",text=="NHS",
                                                time=="During the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(total) %>% unlist()),
          n=c(workplace_long_summary %>% filter(platform_label=="Prolific Academic",text=="NHS",
                                                time=="During the COVID-19 pandemic") %>% 
                ungroup() %>% dplyr::select(sample_size) %>% unlist(),
              workplace_long_summary %>% filter(platform_label=="Twitter",text=="NHS",
                                                time=="During the COVID-19 pandemic") %>%
                ungroup() %>% dplyr::select(sample_size) %>% unlist()))



### government vs research

## prolific 

wilcox.test(x=workplace_numeric %>% filter(platform_label=="Prolific Academic",
                                           time=="During the COVID-19 pandemic") %>% 
              dplyr::select(government_ind) %>% unlist() %>% as.numeric(),
            y=workplace_numeric %>% filter(platform_label=="Prolific Academic",
                                           time=="During the COVID-19 pandemic") %>% 
              dplyr::select(academia_ind) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")

## twitter
wilcox.test(x=workplace_numeric %>% filter(platform_label=="Twitter",
                                           time=="During the COVID-19 pandemic") %>% 
              dplyr::select(government_ind) %>% unlist() %>% as.numeric(),
            y=workplace_numeric %>% filter(platform_label=="Twitter",
                                           time=="During the COVID-19 pandemic") %>% 
              dplyr::select(academia_ind) %>% unlist() %>% as.numeric(),
            paired=TRUE,
            alternative="two.sided")




