## Load packages

library(ggplot2)
library(reshape2)
library(readxl)
library(dplyr)
library(gridExtra)
library(tidyverse)
library(data.table)

SFS4 <- read_excel("data/sfs4 bible 2024.xlsx")
BTN4 <- read_excel("data/btn4 all data 2024.xlsx")
#BTN4dbh <- read_excel("data/btn4 access for C.xlsx")
SFF1 <- read_excel("data/sff1 establishment 2024.xlsx")
SFF2 <- read_excel("data/sff2 establishment 2024.xlsx")
SFF3 <- read_excel("data/sff3 establishment 2024.xlsx")
SFF4 <- read_excel("data/sff4 establishment 2024.xlsx")
SFF5 <- read_excel("data/sff5 establishment 2024.xlsx")
SFF6 <- read_excel("data/sff6 establishment 2024.xlsx")
SFF7 <- read_excel("data/sff7 establishment 2024.xlsx")
SFF8 <- read_excel("data/sff8 establishment 2024.xlsx")
SFF9 <- read_excel("data/sff9 establishment 2024.xlsx")
SFF10 <- read_excel("data/sff10 establishment 2024.xlsx")

# Add plot numbers to treeIDs
# BTN4 already has plot numbers 

SFS4$Tree_num = paste0('SFS4-', SFS4$Tree_num)
SFF1$Tree_num = paste0('SFF1-', SFF1$Tree_num)
SFF2$Tree_num = paste0('SFF2-', SFF2$Tree_num)
SFF3$Tree_num = paste0('SFF3-', SFF3$Tree_num)
SFF4$Tree_num = paste0('SFF4-', SFF4$Tree_num)
SFF5$Tree_num = paste0('SFF5-', SFF5$Tree_num)
SFF6$Tree_num = paste0('SFF6-', SFF6$Tree_num)
SFF7$Tree_num = paste0('SFF7-', SFF7$Tree_num)
SFF8$Tree_num = paste0('SFF8-', SFF8$Tree_num)
SFF9$Tree_num = paste0('SFF9-', SFF9$Tree_num)
SFF10$Tree_num = paste0('SFF10-', SFF10$Tree_num)

#Add plot number to data
SFS4$PlotName <- "SFS4"
BTN4$PlotName <- "BTN4"
SFF1$PlotName <- "SFF1"
SFF2$PlotName <- "SFF2"
SFF3$PlotName <- "SFF3"
SFF4$PlotName <- "SFF4"
SFF5$PlotName <- "SFF5"
SFF6$PlotName <- "SFF6"
SFF7$PlotName <- "SFF7"
SFF8$PlotName <- "SFF8"
SFF9$PlotName <- "SFF9"
SFF10$PlotName <- "SFF10"

#Add treatment status

SFS4$TreatmentStatus <- "Treated"
BTN4$TreatmentStatus <- "Untreated"
SFF1$TreatmentStatus <- "Treated"
SFF2$TreatmentStatus <- "Untreated"
SFF3$TreatmentStatus <- "Untreated"
SFF4$TreatmentStatus <- "Untreated"
SFF5$TreatmentStatus <- "Treated"
SFF6$TreatmentStatus <- "Untreated"
SFF7$TreatmentStatus <- "Treated"
SFF8$TreatmentStatus <- "Treated"
SFF9$TreatmentStatus <- "Untreated"
SFF10$TreatmentStatus <- "Treated"

# Add plot size
SFS4$PlotSize <- 1
BTN4$PlotSize <- 1
SFF1$PlotSize <- 0.25
SFF2$PlotSize <- 1
SFF3$PlotSize <- 0.25
SFF4$PlotSize <- 0.25
SFF5$PlotSize <- 0.25
SFF6$PlotSize <- 0.25
SFF7$PlotSize <- 0.25
SFF8$PlotSize <- 1
SFF9$PlotSize <- 0.25
SFF10$PlotSize <- 0.25

#Select columns of interest (I'm begging someone to write a loop, this is so embarrasing)

SFS4 <- SFS4 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG, Notes)
BTN4 <- BTN4 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG, Notes)
SFF1 <- SFF1 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG, Notes)
SFF2 <- SFF2 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG, Notes)
SFF3 <- SFF3 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG, Notes)
SFF4 <- SFF4 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG, Notes)
SFF5 <- SFF5 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG,  Notes)
SFF6 <- SFF6 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG, Notes)
SFF7 <- SFF7 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG, Notes)
SFF8 <- SFF8 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG, Notes)
SFF9 <- SFF9 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG,  Notes)
SFF10 <- SFF10 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG, Notes)


#Okay well, now we merge all the plots into one dataframe
SFS4$Condition <- as.numeric(SFS4$Condition)
BTN4$Condition <- as.numeric(BTN4$Condition)
merged_plots <- bind_rows(SFS4, BTN4, SFF1, SFF2, SFF3, SFF4, SFF5, SFF6, SFF7, SFF8, SFF9, SFF10)

#Now we add a column to define MOG as Y or NA
merged_plots <- merged_plots %>%
  mutate(MOG = case_when(OG == "Y" | DBH > 30 ~ "Y"))

#calculate tree counts
merged_summary <- merged_plots %>%
  group_by(PlotName, PlotSize, TreatmentStatus) %>%
  summarize(Tree_count = n(), meanDBH = mean(DBH, na.rm = TRUE))

#calculate MOG stats
merged_MOG <- merged_plots %>%
  filter(MOG == "Y")%>%
  group_by(PlotName, PlotSize, TreatmentStatus)%>%
  summarize(MOG_count = n())

#calculate live MOG stats
merged_MOG_live <- merged_plots %>%
  filter(MOG == "Y" & Condition != 5 & Condition != 2) %>%
  group_by(PlotName, PlotSize, TreatmentStatus) %>%
  summarize(MOG_live_count = n())

#calculate OG stats
merged_OG <- merged_plots %>%
  filter(OG == "Y") %>%
  group_by(PlotName, PlotSize, TreatmentStatus) %>%
  summarize(OG_count = n())

#merge em all together.  probably a better way to do this, but here we are
merged_summary$MOG_count <- merged_MOG$MOG_count
merged_summary$MOG_live_count <- merged_MOG_live$MOG_live_count
merged_summary$OG_count <- merged_OG$OG_count

#Calculate TPH
merged_summary$TPH <- merged_summary$Tree_count/merged_summary$PlotSize
est_data<-merged_summary

# Add tree colors 

add_tree_colors <- function(df){
  df <- df%>%
    mutate(color=
             case_when(Species == "ABCO" ~ "#4EDFC7",
                       Species == "ACGL" ~ "#C21E56",
                       Species == "JUMO" ~ "#FFC0CB",
                       Species == "JUSC" ~ "#95658B",
                       Species == "PIED" ~ "#FFD700",
                       Species == "PIPO" ~ "#2E8B57",
                       Species == "PIST" ~ "#89CFF0",
                       Species == "PRVI" ~ "#9F2B68",
                       Species == "PSME" ~ "#808080",
                       Species == "QUGA" ~ "#5D3FD3",
                       Species == "QUUN" ~ "#CC5500",
                       Species == "SASC" ~ "#E3963E",
                       Species == "unknown" ~ "#AFE1AF",
                       Species == "NA" ~ "#808080"
                       
             ))
  
}

merged_plots <- add_tree_colors(merged_plots)

###Okay, now for some treatment level stats
treatment_summary <- merged_summary %>%
  group_by(TreatmentStatus)%>%
  summarize(Tree_count = sum(Tree_count), MOG_count = sum(MOG_count), MOG_live_count = sum(MOG_live_count), Surveyed_ha = sum(PlotSize))

--------------------------------------------------------------------------------

## filter data to make dbh cutoff 2.5 

dbh_2.5 <- filter(merged_plots, DBH < 2.5 & Species != "QUGA" & Species != "QUUN" & Species !="JUSC" & Species !="PRVI" & Species !="ACGL" & Species != "JUMO" & Species != "PIED" & Species != "POTR" & Species != "SASC")

dbh_2.5_table <- dbh_2.5 %>%
  group_by(TreatmentStatus, Species) %>%
  summarise(count = n()) %>%
  spread(key = TreatmentStatus, value = count, fill = 0)

## make rate of species 

rate <- dbh_2.5_table$Untreated / dbh_2.5_table$Treated

dbh_2.5_table$rate <-rate

## hist of data

ggplot(dbh_2.5, aes(x=DBH)) +
  geom_histogram() +
  facet_wrap(~ TreatmentStatus)

## species comp all data

ggplot(dbh_2.5, aes(x = TreatmentStatus, y = count, fill = Species)) +
  geom_bar(position = "fill", stat = "identity") +
  xlab("") +
  ylab("Number of species") +
  scale_fill_manual(values = c("ABCO" = "#4EDFC7",
                               "PIPO" = "#2E8B57",
                               "PIST" = "#89CFF0",
                               "PSME" = "#5D3FD3")) +
  theme_minimal()+
  theme(legend.position = "none")

## species composition by treatment

num_trees_by_treatment <- dbh_2.5 %>% 
  group_by(Species, TreatmentStatus) %>% 
  summarize(count=n())

ggplot(num_trees_by_treatment, aes(x = TreatmentStatus, y = count, fill = Species)) +
  geom_bar(position = "fill", stat = "identity") +
  xlab("") +
  ylab("Number of species") +
  scale_fill_manual(values = c("ABCO" = "#4EDFC7",
                               "PIPO" = "#2E8B57",
                               "PIST" = "#89CFF0",
                               "PSME" = "#5D3FD3")) +
  theme_minimal()+
  theme(legend.position = "none") 
  facet_wrap(~ TreatmentStatus)

## species composition by plot

num_trees_by_plot <- dbh_2.5 %>% 
  group_by(PlotName, PlotSize, Species, TreatmentStatus) %>% 
  summarize(count=n())
  
TreatedPlotTrees <- filter(num_trees_by_plot, TreatmentStatus == "Treated")
UntreatedPlotTrees <- filter(num_trees_by_plot, TreatmentStatus == "Untreated")

TreatedComp <- ggplot(TreatedPlotTrees, aes(x = PlotName, y = count, fill = Species)) +
                geom_bar(position = "fill", stat = "identity") +
                xlab("") +
                ylab("Number of species") +
                scale_fill_manual(values = c("ABCO" = "#4EDFC7",
                                             "PIPO" = "#2E8B57",
                                             "PIST" = "#89CFF0",
                                             "PSME" = "#5D3FD3")) +
                theme_minimal()+
                theme(legend.position = "none")

UntreatedComp <- ggplot(UntreatedPlotTrees, aes(x = PlotName, y = count, fill = Species)) +
  geom_bar(position = "fill", stat = "identity") +
  xlab("") +
  ylab("Number of species") +
  scale_fill_manual(values = c("ABCO" = "#4EDFC7",
                               "PIPO" = "#2E8B57",
                               "PIST" = "#89CFF0",
                               "PSME" = "#5D3FD3")) +
  theme_minimal()+
  theme(legend.position = "none")

## Need to stack graphs on top of each other 

## tree density 

num_trees_by_sp <- dbh_2.5 %>% 
group_by(Species) %>%
summarize(count=n())

density <- num_trees_by_sp$count / sum(merged_summary$PlotSize)

num_trees_by_sp$density <- density

ggplot(num_trees_by_sp, aes(x = Species, y = density, fill = Species)) +
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Trees per hectare") +
  ylim(0, 100) +
  scale_fill_manual(values = c("ABCO" = "#4EDFC7",
                               "PIPO" = "#2E8B57",
                               "PIST" = "#89CFF0",
                               "PSME" = "#5D3FD3")) +
  theme_minimal()+
  theme(legend.position = "none")

## tree density by treatment

num_trees_by_treatment <- dbh_2.5 %>% 
  group_by(Species, TreatmentStatus) %>% 
  summarize(count=n())

density <- num_trees_by_treatment$count / (sum(merged_summary$PlotSize) / 2)

num_trees_by_treatment$density <- density

ggplot(num_trees_by_treatment, aes(x = Species, y = density, fill = Species)) +
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Trees per hectare") +
  ylim(0, 200) +
  scale_fill_manual(values = c("ABCO" = "#4EDFC7",
                               "PIPO" = "#2E8B57",
                               "PIST" = "#89CFF0",
                               "PSME" = "#5D3FD3")) +
  theme_minimal()+
  theme(legend.position = "none") + 
  facet_wrap(~ TreatmentStatus)

## tree density by plot

num_trees_by_plot <- dbh_2.5 %>% 
  group_by(PlotName, PlotSize, Species, TreatmentStatus) %>% 
  summarize(count=n())

density <- num_trees_by_plot$count / num_trees_by_plot$PlotSize

num_trees_by_plot$density <- density

ggplot(num_trees_by_plot, aes(x= Species, y = density, fill = Species)) +
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Number of species") +
  ylim(0, 700) +
  scale_fill_manual(values = c("ABCO" = "#4EDFC7",
                               "PIPO" = "#2E8B57",
                               "PIST" = "#89CFF0",
                               "PSME" = "#5D3FD3")) +
  theme_minimal()+
  theme(legend.position = "none") + 
  facet_wrap(~ TreatmentStatus + PlotName, ncol = 6)

## hist of species by treatment

ggplot(dbh_2.5, aes(x=DBH, fill = TreatmentStatus)) +
  geom_histogram(stat = "count", position = "dodge") +
  xlab("DBH") +
  ylab("Number of trees") +
  ylim(0, 50) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  facet_wrap(~ Species)

## filter out conditions and make table

dbh_2.5_cond <- filter(dbh_2.5, Condition != 2  & Condition != 7  & Condition != 11 & Condition != 9 & Condition != "NA")

dbh_2.5_cond$Condition <- as.factor(dbh_2.5_cond$Condition)

dbh_2.5_cond_table <- dbh_2.5_cond %>%
  group_by(Condition, Species) %>%
  summarise(count = n()) %>%
  spread(key = Condition, value = count, fill = 0)

setnames(dbh_2.5_cond_table, old=c ("1","3", "5"), new=c ("live", "dalb", "dead"))

propDALB <- dbh_2.5_cond_table$dalb / dbh_2.5_cond_table$live

dbh_2.5_cond_table$propDALB <- propDALB

## hist of species by condition

dbh_2.5_cond$Condition <- as.factor(dbh_2.5_cond$Condition)

ggplot(dbh_2.5_cond, aes(x=DBH, fill = Condition)) +
  geom_histogram(stat = "count", position = "dodge") +
  facet_wrap(~ Species) +
  scale_color_manual(labels = c("live", "dead", "dalb"))

--------------------------------------------------------------------------------
  
## filter data to make dbh cutoff 5

dbh_5 <- filter(merged_plots, DBH < 5 & Species != "QUGA" & Species != "QUUN" & Species !="JUSC" & Species !="PRVI" & Species !="ACGL" & Species != "JUMO" & Species != "PIED" & Species != "POTR")

dbh_5_table <- dbh_5 %>%
  group_by(TreatmentStatus, Species) %>%
  summarise(count = n()) %>%
  spread(key = TreatmentStatus, value = count, fill = 0)

rate <- dbh_5_table$Untreated / dbh_5_table$Treated

dbh_5_table$rate <-rate

dbh_5_cond_table <- dbh_5 %>%
  group_by(Condition, Species) %>%
  summarise(count = n()) %>%
  spread(key = Condition, value = count, fill = 0)

setnames(dbh_5_cond_table, old=c ("1","3"), new=c ("live", "dalb"))

propDALB <- dbh_5_cond_table$dalb / dbh_5_cond_table$live

dbh_5_cond_table$propDALB <- propDALB

## hist of data

ggplot(dbh_5, aes(x=DBH)) +
  geom_histogram() +
  facet_wrap(~ TreatmentStatus)

## hist facet wrap by species

ggplot(dbh_5, aes(x=DBH, fill = TreatmentStatus)) +
  geom_histogram(stat = "count", position = "stack") +
  facet_wrap(~ Species)

dbh_5_cond <- filter(dbh_5, Condition != 2 & Condition != 6 & Condition != 7 & Condition != 9 & Condition != 11)

dbh_5_cond$Condition <- as.factor(dbh_5_cond$Condition)

ggplot(dbh_5_cond, aes(x=DBH, fill = Condition)) +
  geom_histogram(stat = "count", position = "stack") +
  facet_wrap(~ Species)

--------------------------------------------------------------------------------

## filter data to make dbh cutoff 10

dbh_10 <- filter(merged_plots, DBH < 10 & Species != "QUGA" & Species != "QUUN" & Species !="JUSC" & Species !="PRVI" & Species !="ACGL" & Species != "SASC" & Species != "unknown" & Species != "JUMO" & Species != "PIED" & Species != "POTR")

dbh_10_table <- dbh_10 %>%
  group_by(TreatmentStatus, Species) %>%
  summarise(count = n()) %>%
  spread(key = TreatmentStatus, value = count, fill = 0)

dbh_10 %>%
  group_by(Condition, Species) %>%
  summarise(count = n()) %>%
  spread(key = Condition, value = count, fill = 0)

rate <- dbh_10_table$Untreated / dbh_10_table$Treated

dbh_10_table$rate <-rate

dbh_10_cond_table <- dbh_10 %>%
  group_by(Condition, Species) %>%
  summarise(count = n()) %>%
  spread(key = Condition, value = count, fill = 0)

setnames(dbh_10_cond_table, old=c ("1","3"), new=c ("live", "dalb"))

propDALB <- dbh_10_cond_table$dalb / dbh_10_cond_table$live

dbh_10_cond_table$propDALB <- propDALB

## hist of data

ggplot(dbh_10, aes(x=DBH)) +
  geom_histogram() +
  facet_wrap(~ TreatmentStatus)

## hist facet wrap by species

ggplot(dbh_10, aes(x=DBH, fill = TreatmentStatus)) +
  geom_histogram(stat = "count", position = "stack") +
  facet_wrap(~ Species)

## hist of species by condition

dbh_10_cond <- filter(dbh_10, Condition != 2 & Condition != 6 & Condition != 7 & Condition != 9 & Condition != 11)

dbh_10_cond$Condition <- as.factor(dbh_10_cond$Condition)

ggplot(dbh_10_cond, aes(x=DBH, fill = Condition)) +
  geom_histogram(stat = "count", position = "stack") +
  facet_wrap(~ Species)

--------------------------------------------------------------------------------

## hist of all data

dbh_all <- filter(merged_plots, Species != "QUGA" & Species != "QUUN" & Species !="JUSC" & Species !="PRVI" & Species !="ACGL" & Species != "SASC" & Species != "unknown" & Species != "JUMO" & Species != "PIED")
  
ggplot(dbh_all, aes(x=DBH)) +
  geom_histogram() +
  facet_wrap(~ TreatmentStatus)

dbh_all_cond <- filter(dbh_all, Condition != 2 & Condition != 6 & Condition != 7 & Condition != 9 & Condition != 11)

dbh_all_cond$Condition <- as.factor(dbh_all_cond$Condition)

ggplot(dbh_all_cond, aes(x=DBH, fill = Condition)) +
  geom_histogram(stat = "density", position = "stack") +
  facet_wrap(~ Species) +
  theme_bw()

dbh_all_cond_table <- dbh_all_cond %>%
  group_by(Condition, Species) %>%
  summarise(count = n()) %>%
  spread(key = Condition, value = count, fill = 0)

setnames(dbh_all_cond_table, old=c ("1","3"), new=c ("live", "dalb"))

propDALB <- dbh_all_cond_table$dalb / dbh_all_cond_table$live

dbh_all_cond_table$propDALB <- propDALB

--------------------------------------------------------------------------------
  
## Veg
  
veg_data <- read.csv("data/all veg 2024.csv")

BTN4 <- veg_data %>% filter(grepl("BTN4V", PlotID, ignore.case = TRUE))
SFS4 <- veg_data %>% filter(grepl("SFS4V", PlotID, ignore.case = TRUE))
SFF1 <- veg_data %>% filter(grepl("SFF1V", PlotID, ignore.case = TRUE))
SFF2 <- veg_data %>% filter(grepl("SFF2V", PlotID, ignore.case = TRUE))
SFF3 <- veg_data %>% filter(grepl("SFF3V", PlotID, ignore.case = TRUE))
SFF4 <- veg_data %>% filter(grepl("SFF4V", PlotID, ignore.case = TRUE))
SFF5 <- veg_data %>% filter(grepl("SFF5V", PlotID, ignore.case = TRUE))
SFF6 <- veg_data %>% filter(grepl("SFF6V", PlotID, ignore.case = TRUE))
SFF7 <- veg_data %>% filter(grepl("SFF7V", PlotID, ignore.case = TRUE))
SFF8 <- veg_data %>% filter(grepl("SFF8V", PlotID, ignore.case = TRUE))
SFF9 <- veg_data %>% filter(grepl("SFF9V", PlotID, ignore.case = TRUE))
SFF10 <- veg_data %>% filter(grepl("SFF10V", PlotID, ignore.case = TRUE))

#Add treatment status

SFS4$TreatmentStatus <- "Treated"
BTN4$TreatmentStatus <- "Untreated"
SFF1$TreatmentStatus <- "Treated"
SFF2$TreatmentStatus <- "Untreated"
SFF3$TreatmentStatus <- "Untreated"
SFF4$TreatmentStatus <- "Untreated"
SFF5$TreatmentStatus <- "Treated"
SFF6$TreatmentStatus <- "Untreated"
SFF7$TreatmentStatus <- "Treated"
SFF8$TreatmentStatus <- "Treated"
SFF9$TreatmentStatus <- "Untreated"
SFF10$TreatmentStatus <- "Treated"

aerial_data <- filter(veg_data, CoverType == "Aerial")

ggplot(aerial_data, aes(x=Species, fill = Species)) +
  geom_bar(stat = "count")
  

