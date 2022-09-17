rm(list = ls())
setwd("~/Box Sync/Non_school_projects/Ole Miss Football 2022")
library(gtExtras)
library(tidyverse)
library(cfbfastR)


CFBD_API_KEY = "6EurqDrc/VrqE63FxvrqvMHR8sFIUBixpN8+kyGiTD1j+nmdhFVp9MQRzCncl0r"

olemiss <- espn_cfb_pbp(game_id = 401403874, epa_wpa = TRUE) %>% 
  mutate(short_playtype = case_when(pass == 1 ~ 1,
                                    rush == 1 ~ 0,
                                    punt == 1 ~ -1,
                                    fg_inds == 1 ~ -1 ,
                                    TRUE ~ -99)) %>% 
  filter(short_playtype > -1) %>% 
  mutate(short_playtype = case_when(short_playtype == 1 ~ "Pass",
                                    short_playtype == 0 ~ "Rush" ,
                                    short_playtype == -1 ~ "Kick"))


###
downspalette <- c("green" ,"blue" , "red" , "black" )
playpalette <- c("blue" ,"red"  )






### create custom theme for plots 
theme_football <- function (base_size = 11, base_family = "") {
  theme_classic() %+replace% 
    theme(
      plot.title = element_text(hjust = 0.5, size = 24 , face = "bold.italic" ) ,
      plot.subtitle = element_text(hjust = 0.5, size = 20 ,  face = "bold.italic" ), 
      panel.grid.major.x = element_line(color = "white"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text = element_text(size=15 , vjust = 1),
      axis.title = element_text(size = 20),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 15))}
###

###check epa averages by down 
olemiss %>% 
  filter(turnover == 0 & down == 1) %>% 
  group_by(pos_team) %>% 
  summarise(mean_var = mean(def_EPA,  na.rm = TRUE))

###offense 
olemiss %>% 
  filter(pos_team=="Ole Miss" & turnover == 0 , down != 4) %>% 
  ggplot(aes(x=yards_to_goal , y = EPA))+
  geom_point(aes(color = as.factor(down) , shape = as.factor(short_playtype) , size = 2))+
  geom_smooth(aes(color = as.factor(down) ), se =FALSE )+
  annotate("text", x = 7.5 , y = -2.75 ,
           label = paste( "First: .192" , "Second: .343" , "Third: -1.76" , sep = "\n"),hjust = 0 ,size =5 , fontface = "italic")+
  scale_color_manual(name = "Down" , values =downspalette) +
  scale_shape_discrete(name = "Play Type")+
  guides( size = FALSE , color = guide_legend(override.aes = list(size = 5)), 
          shape = guide_legend(override.aes = list(size = 5)))+
  scale_x_reverse()+
  geom_hline(linetype = "dashed", size = 1,   yintercept = 0)+
  labs(x = 'Yards to Goal', y = 'Expexted Points Added', 
       title = 'Ole Miss EPA', subtitle = 'Week 2 VS Central Arkansas')+
  theme_football()

###defense
olemiss %>% 
  filter(pos_team=="Central Arkansas" & turnover == 0 & down != 4) %>% 
  ggplot(aes(x=yards_to_goal , y = def_EPA))+  
  geom_smooth(aes(color = as.factor(down)), se = FALSE )+
  geom_point(aes(color = as.factor(down) , shape = as.factor(short_playtype) , size = 2))+
  annotate("text", x = 20 , y = -1.5 ,
           label = paste( "First: -.035" , "Second: .223" , "Third: -1.05" , sep = "\n"),hjust = 0 ,size =5 , fontface = "italic")+
  scale_color_manual(name = "Down" , values =downspalette) +
  scale_shape_discrete(name = "Play Type")+
  guides( size = FALSE , color = guide_legend(override.aes = list(size = 5)), 
          shape = guide_legend(override.aes = list(size = 5)))+
  scale_x_reverse()+
  geom_hline(linetype = "dashed", size = 1,   yintercept = 0)+
  labs(x = 'Yards to Goal', y = 'Expexted Points Added', 
       title = 'Ole Miss Defense EPA', subtitle = 'Week 2 VS Central Arkansas')+
  theme_football()

### checking averages 
olemiss %>% 
  filter(turnover == 0 & pass== 1) %>% 
  group_by(pos_team) %>% 
  summarise(mean_var = mean(def_EPA,  na.rm = TRUE))

###offense play types 
olemiss %>% 
  filter(pos_team=="Ole Miss" & turnover == 0 , down != 4) %>% 
  ggplot(aes(x=yards_to_goal , y = EPA))+
  geom_point(aes(color = as.factor(short_playtype) , size = 2))+
  geom_smooth(aes(color = as.factor(short_playtype) ), se =FALSE)+
  annotate("text", x = 7.5 , y = -3.5 ,
           label = paste( "Pass: -.437" , "Rush: .245"  , sep = "\n"),hjust = 0 ,size =5 , fontface = "italic")+
  scale_color_manual(name = "Play Type" , values =playpalette) +
  guides( size = FALSE , color = guide_legend(override.aes = list(size = 5)))+
  scale_x_reverse()+
  geom_hline(linetype = "dashed", size = 1,   yintercept = 0)+
  labs(x = 'Yards to Goal', y = 'Expexted Points Added', 
       title = 'Ole Miss EPA', subtitle = 'Week 2 VS Central Arkansas')+
  theme_football()


### defense
olemiss %>% 
  filter(pos_team=="Central Arkansas" & turnover == 0 , down != 4) %>% 
  ggplot(aes(x=yards_to_goal , y = def_EPA))+
  geom_point(aes(color = as.factor(short_playtype) , size = 2))+
  geom_smooth(aes(color = as.factor(short_playtype) ), se =FALSE)+
  annotate("text", x = 20 , y = -1.5 ,
           label = paste( "Pass: -.274" , "Rush: -.353"  , sep = "\n"),hjust = 0 ,size =5 , fontface = "italic")+
  scale_color_manual(name = "Play Type" , values =playpalette) +
  guides( size = FALSE , color = guide_legend(override.aes = list(size = 5)))+
  scale_x_reverse()+
  geom_hline(linetype = "dashed", size = 1,   yintercept = 0)+
  labs(x = 'Yards to Goal', y = 'Expexted Points Added', 
       title = 'Ole Miss Defense EPA', subtitle = 'Week 2 VS Central Arkansas')+
  theme_football()

### pass means a pass play was called 
testing<- olemiss %>% 
  filter(pass != pass_attempt)
unique(testing$passer_player_name)

###down distance plot 

olemiss %>% 
  filter(pos_team=="Ole Miss" & turnover == 0 , down != 4) %>% 
  ggplot(aes(x=distance , y = yards_gained))+
  geom_point(aes(color = as.factor(short_playtype) , size = 2))+
  geom_abline(slope = 1 , intercept = 0)+
  facet_wrap(~down, labeller = labeller(down = 
                                          c("1" = "First Down",
                                            "2" = "Second Down",
                                            "3" = "Third Down"))) +
  #geom_smooth(aes(color = as.factor(short_playtype) ), se =FALSE)+
  scale_color_manual(name = "Play Type" , values =playpalette) +
  guides( size = FALSE , color = guide_legend(override.aes = list(size = 5)))+
  labs(x = 'Distance to 1st Down', y = 'Distance Gained', 
       title = 'Ole Miss Down & Distance Performance', subtitle = 'Week 1 VS Troy')+
  theme_football()

unique(olemiss$player)



