## Load packages

library(ggplot2)
library(reshape2)
library(readxl)
library(dplyr)
library(gridExtra)

SFS4 <- read_excel("data/sfs4 bible 2024.xlsx")
BTN4 <- read_excel("data/btn4 revisit data 2024.xlsx")
BTN4dbh <- read_excel ("data/btn4 access for C.xlsx")
SFF1 <- read_excel("data/sff1 establishment data.xlsx")
SFF2 <- read_excel("data/sff2 establishment 2024 data.xlsx")
SFF3 <- read_excel("data/sff3 establishment data.xlsx")
SFF4 <- read_excel("data/sff4 establishment data.xlsx")
SFF5 <- read_excel("data/sff5 establishment data 2024.xlsx")
SFF6 <- read_excel("data/sff6 establishment data.xlsx")
SFF8 <- read_excel("data/sff8 establishment data.xlsx")
SFF9 <- read_excel("data/sff9 establishment data.xlsx")
SFF10 <- read_excel("data/sff10 initial 2024 data.xlsx")

#Fix janky column name in SFF2
names(SFF2)[5] <- paste("DBH")

#Add plot numbers to treeIDs

SFS4$Tree_num = paste0('SFS4-', SFS4$Tree_num)
BTN4$Tree_num = paste0('BTN4-', BTN4$Tree_num)
SFF1$Tree_num = paste0('SFF1-', SFF1$Tree_num)
SFF2$Tree_num = paste0('SFF2-', SFF2$Tree_num)
SFF3$Tree_num = paste0('SFF3-', SFF3$Tree_num)
SFF4$Tree_num = paste0('SFF4-', SFF4$Tree_num)
SFF5$Tree_num = paste0('SFF5-', SFF5$Tree_num)
SFF6$Tree_num = paste0('SFF6-', SFF6$Tree_num)
SFF8$Tree_num = paste0('SFF8-', SFF8$Tree_num)
SFF9$Tree_num = paste0('SFF9-', SFF9$Tree_num)
SFF10$Tree_num = paste0('SFF10-', SFF10$Tree_num)

#do a quick n dirty join of BTN4 DBH & Species files
BTN4dbh$Tree_num <- BTN4dbh$TreeID
BTN4 <- left_join(BTN4, BTN4dbh, by = "Tree_num")
BTN4$Species <- BTN4$SpeciesID
BTN4$Notes<-BTN4$Notes.x


#Add plot number to data
SFS4$PlotName <- "SFS4"
BTN4$PlotName <- "BTN4"
SFF1$PlotName <- "SFF1"
SFF2$PlotName <- "SFF2"
SFF3$PlotName <- "SFF3"
SFF4$PlotName <- "SFF4"
SFF5$PlotName <- "SFF5"
SFF6$PlotName <- "SFF6"
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
SFF8$TreatmentStatus <- "Treated"
SFF9$TreatmentStatus <- "Untreated"
SFF10$TreatmentStatus <- "Treated"

#Add plot size
SFS4$PlotSize <- 1
BTN4$PlotSize <- 1
SFF1$PlotSize <- 0.25
SFF2$PlotSize <- 1
SFF3$PlotSize <- 0.25
SFF4$PlotSize <- 0.25
SFF5$PlotSize <- 0.25
SFF6$PlotSize <- 0.25
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
SFF8 <- SFF8 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG, Notes)
SFF9 <- SFF9 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG,  Notes)
SFF10 <- SFF10 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG, Notes)


#Okay well, now we merge all the plots into one dataframe
SFS4$Condition <- as.numeric(SFS4$Condition)
merged_plots <- bind_rows(SFS4, BTN4, SFF1, SFF2, SFF3, SFF4, SFF5, SFF6, SFF8, SFF9, SFF10)

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


