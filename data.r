#'---
#' title: "TSCI 5050: Introduction to Data Science"
#' author: 'Author One ^1^, Author Two ^1^'
#' abstract: |
#'  | Provide a summary of objectives, study design, setting, participants,
#'  | sample size, predictors, outcome, statistical analysis, results,
#'  | and conclusions.
#' documentclass: article
#' description: 'Manuscript'
#' clean: false
#' self_contained: true
#' number_sections: false
#' keep_md: true
#' fig_caption: true
#' output:
#'  html_document:
#'    toc: true
#'    toc_float: true
#'    code_folding: show
#' ---
#'
#+ init, echo=FALSE, message=FALSE, warning=FALSE
# init ----
# This part does not show up in your rendered report, only in the script,
# because we are using regular comments instead of #' comments
debug <- 0;
knitr::opts_chunk$set(echo=debug>-1, warning=debug>0, message=debug>0, class.output="scroll-20", attr.output='style="max-height: 150px; overflow-y: auto;"');

library(rio);# simple command for importing and exporting
library(pander); # format tables
#library(printr); # set limit on number of lines printed
library(dplyr); #add dplyr library
library(lubridate)#date manipulation
library(stringr) #string manipulation

options(max.print=500);
panderOptions('table.split.table',Inf); panderOptions('table.split.cells',Inf);
datalocation <- "~/Downloads/archive/"
list.files(datalocation,full.names = T)
#TEST <- import("C:/Users/nahid/OneDrive/Desktop/class/careplans.csv")
dat <- sapply(list.files(datalocation, full.names = TRUE), import,simplify = FALSE) %>% 
  setNames(.,basename(names(.)))
# data ingestion ----

# Your two data frames




# First, subset to acute/viral conditions

acute_viral <- dat$conditions.csv %>%
  
  filter(grepl("acute|viral", DESCRIPTION, ignore.case = TRUE))



# data ingestion ----
# Match patients and calculate age at encounter

acute_viral_age <- acute_viral %>%
  
  left_join(
    dat$patients.csv %>% select(Id, BIRTHDATE),
    by = c("PATIENT" = "Id")
  ) %>%
  
  mutate(
    
    START = as.Date(START),
    
    BIRTHDATE = as.Date(BIRTHDATE),
    
    age_at_encounter = time_length(
      
      interval(BIRTHDATE, START),unit = "years"))

  
      # Acute Viral Pharyngitis ----
temp <- filter(dat$conditions.csv,DESCRIPTION=="Acute viral pharyngitis (disorder)") %>% 
  mutate(month=floor_date(START, unit = "month")) %>% 
  group_by(month) %>%  summarise(count=n())   

lm(count~month,temp)


conditionslope <-  mutate(dat$conditions.csv,month=floor_date(START, unit = "month")) %>% 
  group_by(month,CODE,DESCRIPTION) %>%  summarise(count=n())   %>% 
  group_by(CODE,DESCRIPTION) %>% filter(year(month)>=2023 & length(unique(month))>10) %>% 
  summarise(events=lm(count~month)$coefficients[2]) %>% arrange(desc(events)) 
 
plot(conditionslope$events,type="l")                                                                 
abline(v=25,col="green")
#25 seems like a reasonable cutoff for the conditions
topconditionslope<- head(conditionslope,25)
  
c()



