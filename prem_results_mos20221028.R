library(tidyverse)
library(readr)
library(stringr)
library(lubridate)
library(patchwork)
library(utils)
library(openxlsx)
library(hrbrthemes)
library(viridis)


df1 <- readxl::read_excel("/Users/blrice/Dropbox/Lab Projects/1 Post Doc/6 CRS2020/8 CRS2022 CONCEPT NOTE/20220701 Concept Note Draft/Data files/Mosquito Data/Data Mananjary_complete.xlsx")

str(df1)
df1$night
df1$village <- str_replace_all(df1$village, "Ampandrimana", "N1")
df1$village <- str_replace_all(df1$village, "Masindranokely", "S6")
df1$village <- str_replace_all(df1$village, "Ambalavontaka", "S2")
df1$village <- str_replace_all(df1$village, "Ambinany Tanambao", "S5")
df1$village <- str_replace_all(df1$village, "Ambodimanga", "N3")
df1$village <- str_replace_all(df1$village, "Ampandimana", "N1")
df1$village <- str_replace_all(df1$village, "Ampangalana Sud", "S1")
df1$village <- str_replace_all(df1$village, "Ampasimanjeva", "N4")
df1$village <- str_replace_all(df1$village, "Anosibe", "N2")
df1$village <- str_replace_all(df1$village, "Mahavelona", "N5")
df1$village <- str_replace_all(df1$village, "Manampana", "S3")
df1$village <- str_replace_all(df1$village, "Manampano", "S3")
df1$village

                               					
#view and detect NA
str(df1)
is.na(df1)
anyNA(df1)
df2 <- as_tibble(df1)
anyNA(df2)

df2 <- df2 %>% drop_na(quantity)

#use uncount for making row
df3 <- df2 %>% uncount(quantity)
  
####
df3 <- df3 %>% filter(trap == "PL") %>%
  filter(genus != "Non") %>%
  mutate(species_name = paste0(genus, " ", species))

p1 <- df3 %>% group_by(genus) %>% summarize(genus_count = length(genus)) %>%
  ggplot(aes(reorder(genus, -genus_count), y=genus_count)) + 
  geom_bar(stat = "identity", fill = "#06A778") +
  xlab(NULL) + ylab("Count") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p1

p2 <- df3 %>% filter(genus == "Anopheles") %>%
  group_by(species_name) %>% summarize(species_count = length(species_name)) %>%
  ggplot(aes(reorder(species_name, -species_count), y=species_count)) + 
  geom_bar(stat = "identity", fill = "#F57A2A") +
  xlab(NULL) + ylab("Count") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p2

p3 <- df3 %>% filter(genus == "Anopheles") %>%
  group_by(village, species_name) %>% summarize(species_count = length(species_name)) %>%
  ggplot(aes(reorder(species_name, -species_count), y=species_count)) + 
  geom_bar(stat = "identity", fill = "#F57A2A") +
  facet_wrap(vars(village), scales = "free_y", nrow = 4) +
  xlab(NULL) + ylab("Count") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.background = element_rect(fill = "white", colour = "slategray1"))
p3



table(df3$species)
# transform to date Night
as.Date(df3$night)

# attribute month to all date
df3<- df3%>%mutate(daty = case_when(Night < "2022-01-07" ~ "January",
                                    Night < "2022-01-08" ~ "January",
                                    Night < "2022-01-09" ~ "January",
                                    Night < "2022-01-11" ~ "January",
                                    Night < "2022-01-12" ~ "January",
                                    Night < "2022-01-13" ~ "January",
                                    Night < "2022-02-09" ~ "February",
                                    Night < "2022-05-18" ~ "May",
                                    Night < "2022-05-19" ~ "May",
                                    Night < "2022-05-20" ~ "May",
                                    Night < "2022-05-21" ~ "May",
                                    Night < "2022-05-22" ~ "May",
                                    Night < "2022-05-23" ~ "May",
                                    Night < "2022-05-24" ~ "May",
                                    Night < "2022-05-25" ~ "May",
                                    Night < "2022-07-22" ~ "July", 
                                    Night < "2022-07-23" ~ "July",
                                    Night < "2022-07-24" ~ "July",
                                    Night < "2022-07-25" ~ "July",
                                    Night < "2022-07-26" ~ "July",
                                    Night < "2022-07-27" ~ "July",
                                    Night < "2022-08-11" ~ "August",
                                    Night < "2022-08-12" ~ "August",
                                    Night < "2022-08-13" ~ "August",
                                    Night < "2022-08-14" ~ "August",
                                    Night < "2022-08-28" ~ "August",
                                    Night < "2022-08-29" ~ "August",
                                    Night < "2022-08-30" ~ "August",
                                    Night < "2022-08-31" ~ "August",
                                    Night < "2022-09-01" ~ "September",
                                    Night < "2022-09-02" ~ "September",
                                    Night < "2022-09-12" ~ "September",
                                    Night < "2022-09-13" ~ "September",
                                    Night < "2022-09-14" ~ "September",
                                    Night < "2022-09-15" ~ "September",
                                    Night < "2022-09-16" ~ "September"))
#finding NA
anyNA(df3$daty)
str(df3$daty)
df3$daty[1000:1200]
is.na(df3)
#Clean
df3 <- df3 %>% mutate(Species = recode(Species, sp. = "sp", Sp. = "sp",
                                       sp1. = "sp", Sp = "sp" ))


# To make the sampling underneath reproduisable
# group_by species
df4<- df3 %>% 
  group_by(Gender, village) %>%
  summarize(
    count= length(Species)) %>% ungroup()

str(df3)


#group_by gender and village
df4 <- df3 %>% 
  group_by(village, Gender, Species) %>%
  summarize(count = length(Species)) %>% ungroup()

#filter Anopheles species
df4<- filter(df4, Gender == 'Anopheles')

#filter for Mahavelona
df4 <- filter(df4, village == 'S3')
#sum count
df5<-df4 %>%
  mutate(
    sum = sum(count))
# calculate percentage
df5<-df4 %>%
  mutate(
    percentage = round(count/sum(count), 4)*100)

#combine to one column
df7<-pivot_longer(df5, cols = Gender:Species, values_to = 'Species') %>%
  select(-name)
##antother way to combine values to one column
df7<-df5 %>%
  mutate(species_name = paste0(Gender, "_", Species))



# Reorder following the value of another column:
df6<-df7 %>%
  mutate(name = fct_reorder(species_name, percentage)) 

#order by alphabet
df8<-df6[order(df6$species_name), ]

#plot 
df9<- df8 %>%
  ggplot(aes(x=species_name, y=percentage)) +
  geom_bar(stat="identity",fill="Black", alpha=.6, width=2/3) +
  labs(x="Species", y = "Percentage") +
  ylim(0,45)+
  coord_flip() + 
  theme_classic()
df9


# extract dataframe
df8 <- write.csv(df5, file="C:\\Users\\mihar\\Documents\\R_work\\S6.xlsx", row.names = FALSE)
df9<- write.xlsx(df8, file = "My_openxlsx_File.xlsx")
activeSheet(df9)
write.xlsx(df8, 'C:\\Users\\mihar\\Documents\\R_work\\df8.xlsx')
df7<- as.data.frame(df7)

#dendogramn essay
p<- ggplot(df8, aes(daty, species_name, fill= count)) + 
  geom_tile()  +
  scale_fill_gradient(name = "percentage",
                      low = "Yellow",high = "Lightgreen")+
  geom_text(aes(label = round(count, 1)))+
  ggtitle("Anopheles diversity")+
  theme_bw()
p
#diversity of mosquitoes
#the first column is ignored [,-1] as it is a site name, not a species count.
X <- cbind(data = df1, "Aedeomyia_furfurea","Aedes_albopictus","Aedes_aegenteopunctatus",
           "Aedes_circumulutheolis","Aedes_sp","Anopheles_coustani","Anopheles_funestus",
           "Anopheles_gambiae","Anopheles_maculipalpis","Anopheles_mascarensis", 
           "Anopheles_pharoensis", "Anopheles_rufipes", "Anopheles_sp", "Anopheles_squamosus", 
           "Coquilletidia_grandidieri", "Culex_antennatus", "Culex_bitaeniorhynchus",
           "Culex_giganteus","Culex_pipiens","Culex_poicilipes", "Culex_quinquefasciatus", 
           "Culex_sp", "Culex_tritaeniorhynchus", "Culex_univittatus","Eretmapodites_sp",
           "Mansonia_uniformis","Mimomyia_sp","Non_identifié","Orthopodomyia_sp",
           "Orthopodomyia_vernoni","Uranotaenia_sp")

           
  
