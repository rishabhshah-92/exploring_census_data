library(sf)
library(ggplot2)
library(ggmap)
library(dplyr)
library(readxl)
library(stringr)
library(geojsonsf)

source('load_state_boundaries.R')

df <- read_excel("data_dump/india/DDW-0000C-09.xlsx",sheet=2) %>%
  filter(urbanity=='Total' & area_name!='INDIA' & age_group=='Total') %>%
  filter(religion == 'All religious communities') %>%
  mutate(area_name=str_remove(area_name,"State - "),
         area_name=str_remove(area_name," ISLANDS"),
         area_name=str_remove(area_name,"NCT OF ")) %>%
  mutate(female_literacy = literate_females/tot_females
         , male_literacy = literate_males/tot_males
         , gender_gap=(male_literacy-female_literacy)*1e2)

df_to_map <- states %>% 
  merge(df, on="area_name") %>%
  mutate(border_show=area_name %in% c('ANDAMAN & NICOBAR','LAKSHADWEEP'))

NATIONWIDE_AVG <- mean(df_to_map$gender_gap)

map <- df_to_map %>%
  ggplot(aes(fill=gender_gap,linewidth=border_show,color=gender_gap))+
  geom_sf()+
  scale_fill_gradient2(low="#1B9E77",mid="white",high="#d7191c",midpoint=NATIONWIDE_AVG)+
  scale_linewidth_manual(values=c(0,0.5))+
  scale_color_gradient2(low="#1B9E77",mid="white",high="#d7191c",midpoint=NATIONWIDE_AVG)+
  theme_void()+
  labs(title="India's Gender Gap in Literacy"
       , subtitle="calculated as: % male literacy minus (-) % female literacy"
       , caption="Prepared by Dr. Rishabh Shah (linkedin.com/in/rishabh-shah-phd-36196246/)\nData Source: Indian Census Data 2011, accessed at censusindia.gov.in"
       , fill="Gender gap (% points)")+
  theme(legend.position = "inside"
        , legend.direction = "horizontal"
        , legend.position.inside=c(0.83,0.38)
        , plot.title = element_text(face = "bold", hjust=0.5, color="white",size=15)
        , plot.subtitle = element_text(hjust=0.5, color="white",size=13)
        , plot.caption = element_text(hjust=0.95, color="white")
        , legend.title = element_text(color="lightgray", size=13, hjust=0.5)
        , legend.title.position = "bottom"
        , legend.text = element_text(color="lightgray", size=11)
        , legend.key.size = unit(1, 'cm')
        , legend.key.height = unit(0.7, "cm"))+
  annotate("segment", x = 93, y = 20, xend = 93, yend = 19,
           arrow = arrow(type = "closed"),col="white",linewidth=0.0001)+ 
  annotate("text",label=sprintf("Nationwide average: %0.1f%%",NATIONWIDE_AVG)
          , x = 93, y=20.3,
          col="white", size=5, vjust=0)+
  guides(linewidth="none",color="none")+
  geom_sf(data = . %>% filter(!border_show), fill=NA,col='black',lwd=0.5)

ggsave(map, filename="gender_literacy_map.png", height=10, width=7, units="in", bg="black")
