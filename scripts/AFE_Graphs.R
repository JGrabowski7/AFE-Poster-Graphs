## Load packages

library(ggplot2)
library(reshape2)
library(readxl)
library(dplyr)
library(gridExtra)
library(tidyverse)
library(data.table)
library(car)


tree_data <- read.csv("data/all_trees_2024.csv")

tree_data$Plot <- str_extract(tree_data$TreeID, "[^-]+")

tree_data <- tree_data %>%
  mutate(TreatmentStatus = (case_when(Plot == "SFS4" ~ "Treated",
                                      Plot == "BTN4" ~ "Untreated",
                                      Plot == "SFF1" ~ "Treated",
                                      Plot == "SFF2" ~ "Untreated",
                                      Plot == "SFF3" ~ "Untreated",
                                      Plot == "SFF4" ~ "Untreated",
                                      Plot == "SFF5" ~ "Treated",
                                      Plot == "SFF6" ~ "Untreated",
                                      Plot == "SFF7" ~ "Treated",
                                      Plot == "SFF8" ~ "Treated",
                                      Plot == "SFF9" ~ "Untreated",
                                      Plot == "SFF10" ~ "Treated",
  )))

tree_data <- tree_data %>%
  mutate(PlotSize = (case_when(Plot == "SFS4" ~ 1,
                               Plot == "BTN4" ~ 1,
                               Plot == "SFF1" ~ 0.25,
                               Plot == "SFF2" ~ 1,
                               Plot == "SFF3" ~ 0.25,
                               Plot == "SFF4" ~ 0.25,
                               Plot == "SFF5" ~ 0.25,
                               Plot == "SFF6" ~ 0.25,
                               Plot == "SFF7" ~ 0.25,
                               Plot == "SFF8" ~ 1,
                               Plot == "SFF9" ~ 0.25,
                               Plot == "SFF10" ~ 0.25,
  )))
--------------------------------------------------------------------------------
## filter data to make dbh cutoff 2.5 

dbh_2.5 <- filter(tree_data, Diameter < 2.5 & SpeciesID %in% c("PIPO", "PIST", "PSME", "ABCO"))

dbh_2.5_table <- dbh_2.5 %>%
  group_by(TreatmentStatus, SpeciesID) %>%
  summarise(count = n()) %>%
  spread(key = TreatmentStatus, value = count, fill = 0)

## make rate of species 

rate <- dbh_2.5_table$Untreated / dbh_2.5_table$Treated

dbh_2.5_table$rate <-rate

## HISTOGRAM OF TREES BY TREATMENT

ggplot(dbh_2.5, aes(x=Diameter)) +
  geom_histogram() +
  facet_wrap(~ TreatmentStatus)

## BOXPLOT OF TREE DENSITY BY TREATMENT

t_num_trees <- filter(dbh_2.5, TreatmentStatus == "Treated")

t_num_trees <- t_num_trees %>% 
  group_by(Plot, PlotSize, TreatmentStatus, SpeciesID) %>%
  summarize(count=n())

t_num_trees <- t_num_trees %>%
  mutate(count = ifelse(PlotSize == 0.25, count * 4, count))

t_table <- data.frame(matrix(nrow = 0, ncol = 3))

for (site in unique(t_num_trees$Plot)) {
  row_temp = data.frame(matrix(nrow = 1, ncol = 3))
  row_temp[1,1] = site
  row_temp[1,2] = "Treated"
  row_temp[1,3] = sum(t_num_trees$count[t_num_trees$Plot == site])
  t_table = rbind(t_table, row_temp)
}

u_num_trees <- filter(dbh_2.5, TreatmentStatus == "Untreated")

u_num_trees <- u_num_trees %>% 
  group_by(Plot, PlotSize, TreatmentStatus, SpeciesID) %>%
  summarize(count=n())

u_num_trees <- u_num_trees %>%
  mutate(count = ifelse(PlotSize == 0.25, count * 4, count))

u_table <- data.frame(matrix(nrow = 0, ncol = 3))

for (site in unique(u_num_trees$Plot)) {
  row_temp = data.frame(matrix(nrow = 1, ncol = 3))
  row_temp[1,1] = site
  row_temp[1,2] = "Untreated"
  row_temp[1,3] = sum(u_num_trees$count[u_num_trees$Plot == site])
  u_table = rbind(u_table, row_temp)
}

list <- list(t_table, u_table)

avg_tree_num <- list %>% reduce(full_join, by = c("X1", "X2", "X3"))

ggplot(avg_tree_num, aes(x = X2, y = X3, fill = X2), alpha = 0.5) +
  geom_boxplot() +
  xlab("") +
  ylab("Density (trees per hectare)") +
  ylim(0, 1000) +
  theme_minimal() +
  theme(legend.position = "none") 
  
## LIVE MOG BY SPECIES

tree_data <- tree_data %>%
  mutate(MOG = case_when(Old_growth == "Y" | Diameter > 30 ~ "Y"))

#MOG <- tree_data %>%
  #filter(MOG == "Y") %>%
  #group_by(TreatmentStatus) %>%
  #summarize(MOG_count = n())

all_live_MOG <- tree_data %>%
  filter(MOG == "Y" & Tree_condition %in% c(0,1,3,4,7,8,9,10,11)) %>%
  group_by(TreatmentStatus) %>%
  summarize(MOG_live_count = n())

live_MOG <- tree_data %>%
  filter(MOG == "Y" & Tree_condition %in% c(1,3,4,7) & SpeciesID %in% c("PIPO", "ABCO", "PIST", "PSME")) %>%
  group_by(TreatmentStatus, SpeciesID) %>%
  summarize(MOG_live_count = n())

ggplot(live_MOG, aes(x = SpeciesID, y = MOG_live_count, fill = SpeciesID)) +
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Number of live MOG") +
  ylim(0, 1000) +
  scale_fill_manual(values = c("ABCO" = "#4EDFC7",
                               "PIPO" = "#2E8B57",
                               "PIST" = "#89CFF0",
                               "PSME" = "#5D3FD3")) +
  theme_minimal() +
  facet_wrap(~ TreatmentStatus) +
  theme(legend.position = "none")

## SPECIES COMPOSITION BY TREATMENT

num_trees_by_treatment <- dbh_2.5 %>% 
  group_by(SpeciesID, TreatmentStatus) %>% 
  summarize(count=n())

ggplot(num_trees_by_treatment, aes(x = TreatmentStatus, y = count, fill = SpeciesID)) +
  geom_bar(position = "fill", stat = "identity") +
  xlab("") +
  ylab("Species composition") +
  scale_fill_manual(values = c("ABCO" = "#4EDFC7",
                               "PIPO" = "#2E8B57",
                               "PIST" = "#89CFF0",
                               "PSME" = "#5D3FD3")) +
  theme_minimal()

## species composition by plot

num_trees_by_plot <- dbh_2.5 %>% 
  group_by(Plot, PlotSize, SpeciesID, TreatmentStatus) %>% 
  summarize(count=n())
  
TreatedPlotTrees <- filter(num_trees_by_plot, TreatmentStatus == "Treated")
UntreatedPlotTrees <- filter(num_trees_by_plot, TreatmentStatus == "Untreated")

TreatedComp <- ggplot(TreatedPlotTrees, aes(x = Plot, y = count, fill = SpeciesID)) +
                geom_bar(position = "fill", stat = "identity") +
                xlab("") +
                ylab("Species composition") +
                ggtitle("Treated") +
                scale_fill_manual(values = c("ABCO" = "#4EDFC7",
                                             "PIPO" = "#2E8B57",
                                             "PIST" = "#89CFF0",
                                             "PSME" = "#5D3FD3",
                                             "QUGA" = "#CC5500")) +
                theme_minimal()

UntreatedComp <- ggplot(UntreatedPlotTrees, aes(x = Plot, y = count, fill = SpeciesID)) +
  geom_bar(position = "fill", stat = "identity") +
  xlab("") +
  ylab("Species composition") +
  ggtitle("Untreated") +
  scale_fill_manual(values = c("ABCO" = "#4EDFC7",
                               "PIPO" = "#2E8B57",
                               "PIST" = "#89CFF0",
                               "PSME" = "#5D3FD3",
                               "QUGA" = "#CC5500")) +
  theme_minimal()

grid.arrange(TreatedComp, UntreatedComp, ncol=1)

## tree density 

num_trees_by_sp <- dbh_2.5 %>% 
group_by(Plot, PlotSize, SpeciesID) %>%
summarize(count=n())

density <- num_trees_by_sp$count / num_trees_by_sp$PlotSize

num_trees_by_sp$density <- density

ggplot(num_trees_by_sp, aes(x = SpeciesID, y = density, fill = SpeciesID)) +
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Density (trees per hectare)") +
  ylim(0, 1500) +
  scale_fill_manual(values = c("ABCO" = "#4EDFC7",
                               "PIPO" = "#2E8B57",
                               "PIST" = "#89CFF0",
                               "PSME" = "#5D3FD3",
                               "QUGA" = "#CC5500")) +
  theme_minimal()+
  theme(legend.position = "none")

## tree density by treatment

num_trees_by_treatment <- dbh_2.5 %>% 
  group_by(SpeciesID, TreatmentStatus) %>% 
  summarize(count=n())

density <- num_trees_by_treatment$count / 3

num_trees_by_treatment$density <- density

ggplot(num_trees_by_treatment, aes(x = SpeciesID, y = density, fill = SpeciesID)) +
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Density (trees per hectare)") +
  ylim(0, 200) +
  scale_fill_manual(values = c("ABCO" = "#4EDFC7",
                               "PIPO" = "#2E8B57",
                               "PIST" = "#89CFF0",
                               "PSME" = "#5D3FD3",
                               "QUGA" = "#CC5500")) +
  theme_minimal() +
  theme(legend.position = "none") + 
  facet_wrap(~ TreatmentStatus)

## tree density by plot

num_trees_by_plot <- dbh_2.5 %>% 
  group_by(Plot, PlotSize, SpeciesID, TreatmentStatus) %>% 
  summarize(count=n())

density <- num_trees_by_plot$count / num_trees_by_plot$PlotSize

num_trees_by_plot$density <- density

ggplot(num_trees_by_plot, aes(x= SpeciesID, y = density, fill = SpeciesID)) +
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Density (trees per hectare)") +
  ylim(0, 800) +
  scale_fill_manual(values = c("ABCO" = "#4EDFC7",
                               "PIPO" = "#2E8B57",
                               "PIST" = "#89CFF0",
                               "PSME" = "#5D3FD3",
                               "QUGA" = "#CC5500")) +
  theme_minimal()+
  theme(legend.position = "none") + 
  facet_wrap(~ TreatmentStatus + Plot, ncol = 6)

# MOG vs juvenile

small_trees_by_plot <- dbh_2.5 %>% 
  group_by(Plot, SpeciesID) %>% 
  summarize(count=n())

tree_data <- tree_data %>%
  mutate(MOG = case_when(Old_growth == "Y" | Diameter > 30 ~ "Y"))

live_MOG <- tree_data %>%
  filter(MOG == "Y" & Tree_condition %in% c(1,3,4,7) & SpeciesID %in% c("PIPO", "ABCO", "PIST", "PSME")) %>%
  group_by(Plot, SpeciesID, TreatmentStatus) %>%
  summarize(MOG_live_count = n())

new_data <- full_join(live_MOG, small_trees_by_plot, by = c("Plot", "SpeciesID"))

new_data$MOG_live_count <- ifelse(is.na(new_data$MOG_live_count), 0, new_data$MOG_live_count)

new_data$count <- ifelse(is.na(new_data$count), 0, new_data$count)

ggplot() +
geom_point(new_data, mapping=aes(x=MOG_live_count,y=count, pch = TreatmentStatus)) +
  facet_wrap(~ SpeciesID, scales = "free") +
geom_smooth(new_data, mapping=aes(x=MOG_live_count,y=count), method = "lm")

## hist of species by treatment

ggplot(dbh_2.5, aes(x= Diameter, fill = TreatmentStatus)) +
  geom_histogram(stat = "count", position = "dodge") +
  xlab("DBH") +
  ylab("Number of trees") +
  ylim(0, 50) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  facet_wrap(~ SpeciesID)

## filter out conditions and make table

dbh_2.5_cond <- filter(dbh_2.5, Condition != 2  & Condition != 7  & Condition != 11 & Condition != 9 & Condition != "NA")

dbh_2.5_cond$Conditon[dbh_2.5_cond$Condition == '3'] <- '1'

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
  
## VEGETATION DATA
  
veg_data <- read.csv("data/veg features.csv")

veg_data$Plot<-str_extract(veg_data$PlotID, "[^-]+")

veg_data <- veg_data %>%
  mutate(TreatmentStatus = (case_when(Plot == "SFS4V" ~ "Treated",
                                      Plot == "BTN4V" ~ "Untreated",
                                      Plot == "SFF1V" ~ "Treated",
                                      Plot == "SFF2V" ~ "Untreated",
                                      Plot == "SFF3V" ~ "Untreated",
                                      Plot == "SFF4V" ~ "Untreated",
                                      Plot == "SFF5V" ~ "Treated",
                                      Plot == "SFF6V" ~ "Untreated",
                                      Plot == "SFF7V" ~ "Treated",
                                      Plot == "SFF8V" ~ "Treated",
                                      Plot == "SFF9V" ~ "Untreated",
                                      Plot == "SFF10V" ~ "Treated",
  )))

# basal ground cover by treatment

t_basal_data <- filter(veg_data, TreatmentStatus == "Treated",CoverType == "Basal" & Percent != "T")
t_basal_data$Percent <- as.numeric(t_basal_data$Percent)

u_basal_data <- filter(veg_data, TreatmentStatus == "Untreated", CoverType == "Basal" & Percent != "T")
u_basal_data$Percent <- as.numeric(u_basal_data$Percent)

t_mean_basal_cover<- t_basal_data %>%
  group_by(CoverClass)%>%
  summarise(PercentSum = sum(Percent))
t_mean_basal_cover <- t_mean_basal_cover %>%
  mutate(avg = t_mean_basal_cover$PercentSum / 72,
         TreatmentStatus = "Treated")

u_mean_basal_cover <- u_basal_data %>%
  group_by(CoverClass)%>%
  summarise(PercentSum = sum(Percent))
u_mean_basal_cover <- u_mean_basal_cover %>%
  mutate(avg = u_mean_basal_cover$PercentSum / 72,
         TreatmentStatus = "Untreated")

list <- list(t_mean_basal_cover, u_mean_basal_cover)

all_basal_cover <- list %>% reduce(full_join, by= c("CoverClass", "PercentSum", "avg", "TreatmentStatus"))

ggplot(all_basal_cover, aes(x = TreatmentStatus, y = avg, fill = CoverClass)) +
  geom_col(position = "fill") +
  xlab("") +
  ylab("Basal cover (%)") +
  scale_fill_manual(values = c("B" = "#3366CC",
                                 "D" = "#DC3912",
                                 "G" = "#FF9900",
                                 "L" = "#109618",
                                 "Lg" = "#990099",
                                 "M" = "#0099C6",
                                 "R" = "#DD4477",
                                 "Rt" = "#96cea4",
                                 "St" = "#B82E2E",
                                 "V" = "#316395"),
                      labels = c("B" = "bare",
                                 "D" = "duff",
                                 "G" = "gravel",
                                 "L" = "litter",
                                 "Lg" = "log",
                                 "M" = "moss",
                                 "R" = "rock",
                                 "Rt" = "root",
                                 "St" = "stump",
                                 "V" = "vegetation")) +
  theme(legend.title = "none") +
  theme_minimal()

# basal ground cover by plot

t_basal_data <- filter(veg_data, TreatmentStatus == "Treated",CoverType == "Basal" & Percent != "T")
t_basal_data$Percent <- as.numeric(t_basal_data$Percent)

u_basal_data <- filter(veg_data, TreatmentStatus == "Untreated", CoverType == "Basal" & Percent != "T")
u_basal_data$Percent <- as.numeric(u_basal_data$Percent)

t_mean_basal_cover_plot <- t_basal_data %>%
  group_by(Plot, CoverClass)%>%
  summarise(PercentSum = sum(Percent))
t_mean_basal_cover_plot <- t_mean_basal_cover_plot %>%
  mutate(avg = t_mean_basal_cover_plot$PercentSum / 72,
         TreatmentStatus = "Treated")

u_mean_basal_cover_plot <- u_basal_data %>%
  group_by(Plot, CoverClass)%>%
  summarise(PercentSum = sum(Percent))
u_mean_basal_cover_plot <- u_mean_basal_cover_plot %>%
  mutate(avg = u_mean_basal_cover_plot$PercentSum / 72,
         TreatmentStatus = "Untreated")

list <- list(t_mean_basal_cover, u_mean_basal_cover)

all_basal_cover <- list %>% reduce(full_join, by= c("CoverClass", "PercentSum", "avg", "TreatmentStatus"))

ggplot(all_basal_cover, aes(x = TreatmentStatus, y = avg, fill = CoverClass)) +
  geom_col(position = "fill") +
  xlab("") +
  ylab("Basal cover (%)") +
  scale_fill_manual(values = c("B" = "#3366CC",
                               "D" = "#DC3912",
                               "G" = "#FF9900",
                               "L" = "#109618",
                               "Lg" = "#990099",
                               "M" = "#0099C6",
                               "R" = "#DD4477",
                               "Rt" = "#96cea4",
                               "St" = "#B82E2E",
                               "V" = "#316395"),
                    labels = c("B" = "bare",
                               "D" = "duff",
                               "G" = "gravel",
                               "L" = "litter",
                               "Lg" = "log",
                               "M" = "moss",
                               "R" = "rock",
                               "Rt" = "root",
                               "St" = "stump",
                               "V" = "vegetation")) +
  theme(legend.title = "none") +
  theme_minimal()

# The vegetation percent in the graph above but split into species

trt_basal_species <- filter(veg_data, CoverType == "Basal" & TreatmentStatus == "Treated" & CoverClass == "V" & Percent != "T")
trt_basal_species$Percent <- as.numeric(trt_basal_species$Percent)

trt_mean_species_basal_cover<- trt_basal_species %>%
  group_by(Species) %>%
  summarise(PercentSum = sum(Percent))

trt_mean_species_basal_cover_lifeform <- trt_basal_species %>%
  group_by(Species, LifeForm,TreatmentStatus) %>%
  summarise(PercentSum = sum(Percent))

trt_top10_cover <- top_n(trt_mean_species_basal_cover, 10, PercentSum)
trt_top10_LifeForm <- left_join(trt_top10_cover, trt_mean_species_basal_cover_lifeform, by = c("Species", "PercentSum"), relationship = "many-to-many")
trt_top10_LifeForm$avg <- trt_top10_LifeForm$PercentSum/72

TreatmentStatus <- "Treated"
trt_top10_LifeForm$TreatmentStatus <- TreatmentStatus

untrt_basal_species <- filter(veg_data, CoverType == "Basal" & TreatmentStatus == "Untreated" & CoverClass == "V" & Percent != "T")
untrt_basal_species$Percent <- as.numeric(untrt_basal_species$Percent)

untrt_mean_species_basal_cover<- untrt_basal_species %>%
  group_by(Species) %>%
  summarise(PercentSum = sum(Percent))

untrt_mean_species_basal_cover_lifeform <- untrt_basal_species %>%
  group_by(Species, LifeForm) %>%
  summarise(PercentSum = sum(Percent))

untrt_top10_cover <- top_n(untrt_mean_species_basal_cover, 10, PercentSum)

untrt_top10_LifeForm <- left_join(untrt_top10_cover, untrt_mean_species_basal_cover_lifeform, by = c("Species", "PercentSum"), relationship = "many-to-many")

untrt_top10_LifeForm$avg <- untrt_top10_LifeForm$PercentSum/72

TreatmentStatus <- "Untreated"
untrt_top10_LifeForm$TreatmentStatus <- TreatmentStatus

top10_basal_cover <- left_join(trt_top10_LifeForm, untrt_top10_LifeForm, by = c("Species", "PercentSum", "avg", "LifeForm", "TreatmentStatus"), relationship = "many-to-many")

trt_basal_species <- ggplot(trt_top10_LifeForm, aes(x = reorder(Species, -avg), y = avg, fill = LifeForm)) +
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Average species basal cover (%)") +
  ylim(0, 1.5) +
  ggtitle("Treated") +
  scale_fill_manual(values = c(Graminoid = "#008744",
                               Forb = "#0057e7")) +
  theme_minimal()

untrt_basal_species <- ggplot(untrt_top10_LifeForm, aes(x = reorder(Species, -avg), y = avg, fill = LifeForm)) +
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Average species basal cover (%)") +
  ylim(0, 1.5) +
  ggtitle("Untreated") +
  scale_fill_manual(values = c(Graminoid = "#008744",
                               Forb = "#0057e7")) +
  theme_minimal()

grid.arrange(trt_basal_species, untrt_basal_species)

## grass cover graph and stats

basal_lifeform <- filter(veg_data, CoverType == "Basal" & CoverClass == "V" & Percent != "T")
basal_lifeform$Percent <- as.numeric(basal_lifeform$Percent)

mean_lifeform_basal_cover<- basal_lifeform %>%
  group_by(LifeForm, Plot, TreatmentStatus) %>%
  summarise(PercentSum = sum(Percent))

t_grass_by_plot <- filter(mean_lifeform_basal_cover, LifeForm == "Graminoid", TreatmentStatus == "Treated")

u_grass_by_plot <- filter(mean_lifeform_basal_cover, LifeForm == "Graminoid", TreatmentStatus == "Untreated")

t_grass_graph <- ggplot(t_grass_by_plot, aes(x = Plot, y = PercentSum)) +
                  geom_bar(stat = "identity") +
                  xlab("") +
                  ylab("Sum of grass cover") +
                  ggtitle("Treated") +
                  theme_minimal()

u_grass_graph <- ggplot(u_grass_by_plot, aes(x = Plot, y = PercentSum)) +
                  geom_bar(stat = "identity") +
                  xlab("") +
                  ylab("Sum of grass cover") +
                  ggtitle("Untreated") +
                  theme_minimal()

grid.arrange(t_grass_graph, u_grass_graph)

grass_by_plot <- filter(mean_lifeform_basal_cover, LifeForm == "Graminoid")

new_row <- data.frame(
  LifeForm = "Graminoid",
  Plot = "SFF3",
  TreatmentStatus = "Untreated",
  PercentSum = 0)

grass_by_plot <- rbind(grass_by_plot, new_row)

grass_by_plot$TreatmentStatus <- as.factor(grass_by_plot$TreatmentStatus)

shapiro.test(grass_by_plot$PercentSum)

leveneTest(PercentSum ~ TreatmentStatus, data = grass_by_plot)

wilcox.test(PercentSum ~ TreatmentStatus, grass_by_plot , exact = FALSE)

t.test(t_grass_by_plot$PercentSum, u_grass_by_plot$PercentSum)

--------
veg_data$quadrat <- gsub(".*-", "", veg_data$PlotID)

basal_lifeform <- filter(veg_data, CoverType == "Basal" & CoverClass == "V" & Percent != "T")
basal_lifeform$Percent <- as.numeric(basal_lifeform$Percent)

sum_lifeform_basal_cover<- basal_lifeform %>%
  group_by(LifeForm, quadrat, TreatmentStatus) %>%
  summarise(PercentSum = sum(Percent))

grass_by_quad <- filter(sum_lifeform_basal_cover, LifeForm == "Graminoid")




## aerial species

trt_aerial_data <- filter(veg_data, TreatmentStatus == "Treated", CoverType == "Aerial" & Percent != "T" & Percent != "" & Percent != "0" & Species != "N/A")
trt_aerial_data$Percent <- as.numeric(trt_aerial_data$Percent)

trt_mean_aerial_veg <- t_aerial_data %>%
  group_by(Species) %>%
  summarise(PercentSum = sum(Percent))

trt_mean_aerial_veg_lifeform <- trt_aerial_data %>%
  group_by(Species, LifeForm, TreatmentStatus) %>%
  summarise(PercentSum = sum(Percent))

trt_top10_aerial_cover <- top_n(, 10, PercentSum)
trt_top10_LifeForm <- left_join(trt_top10_cover, trt_mean_species_basal_cover_lifeform, by = c("Species", "PercentSum"), relationship = "many-to-many")
trt_top10_LifeForm$avg <- trt_top10_LifeForm$PercentSum/72

u_aerial_data <- filter(veg_data, TreatmentStatus == "Untreated", CoverType == "Aerial" & Percent != "T" & Percent != "" & Percent != "0" & Species != "N/A")
u_aerial_data$Percent <- as.numeric(u_aerial_data$Percent)

u_mean_aerial_veg <- u_aerial_data %>%
  group_by(Species)%>%
  summarise(PercentSum = sum(Percent))
u_mean_aerial_veg <- u_mean_aerial_veg %>%
  mutate(avg = u_mean_aerial_veg$PercentSum / 72)

t_species_aerial_graph <- ggplot(t_mean_aerial_veg, aes(x = Species, y = avg)) +
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Aerial species cover") +
  ylim(0, 4) +
  ggtitle("Treated") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=.5, hjust=1))

u_species_aerial_graph <- ggplot(u_mean_aerial_veg, aes(x = Species, y = avg)) +
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Aerial species cover") +
  ylim(0, 4) +
  ggtitle("Untreated") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=.5, hjust=1))

grid.arrange(t_species_aerial_graph, u_species_aerial_graph)

## growth form by treatment

t_aerial_data <- filter(veg_data, TreatmentStatus == "Treated", CoverType == "Aerial" & Percent != "T" & Percent != "" & Percent != "0" & Species != "N/A")
t_aerial_data$Percent <- as.numeric(t_aerial_data$Percent)

t_mean_lifeform <- t_aerial_data %>%
  group_by(LifeForm)%>%
  summarise(PercentSum = sum(Percent))
t_mean_lifeform <- t_mean_lifeform %>%
  mutate(avg = t_mean_lifeform$PercentSum / 72)

u_aerial_data <- filter(veg_data, TreatmentStatus == "Untreated", CoverType == "Aerial" & Percent != "T" & Percent != "" & Percent != "0" & Species != "N/A")
u_aerial_data$Percent <- as.numeric(u_aerial_data$Percent)

u_mean_lifeform <- u_aerial_data %>%
  group_by(LifeForm)%>%
  summarise(PercentSum = sum(Percent))
u_mean_lifeform <- u_mean_lifeform %>%
  mutate(avg = u_mean_lifeform$PercentSum / 72)

t_lifeform_graph <- ggplot(t_mean_lifeform, aes(x = LifeForm, y = avg)) +
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Average aerial lifeform cover (%)") +
  ylim(0, 8) +
  ggtitle("Treated") +
  theme_minimal()

u_lifeform_graph <- ggplot(u_mean_lifeform, aes(x = LifeForm, y = avg)) +
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Average aerial lifeform cover (%)") +
  ylim(0, 8) +
  ggtitle("Untreated") +
  theme_minimal()

grid.arrange(t_lifeform_graph, u_lifeform_graph)
