rm(list = ls())
setwd("~/Box Sync/Non_school_projects/Ole Miss Football 2022")
library(gt)
library(gtExtras)
library(tidyverse)
library(cfbfastR)
library(paletteer)

CFBD_API_KEY = "6EurqDrc/VrqE63FxvrqvMHR8sFIUBixpN8+kyGiTD1j+nmdhFVp9MQRzCncl0r"


game1 <- espn_cfb_pbp(game_id = 401403860, epa_wpa = TRUE)
game2 <- espn_cfb_pbp(game_id = 401403874, epa_wpa = TRUE)
  

all <- rbind(game1,game2) %>% 
  mutate(short_playtype = case_when(pass == 1 ~ 1,
                                    rush == 1 ~ 0,
                                    punt == 1 ~ -1,
                                    fg_inds == 1 ~ -1 ,
                                    TRUE ~ -99)) %>% 
  filter(short_playtype > -1) %>% 
  mutate(short_playtype = case_when(short_playtype == 1 ~ "Pass",
                                    short_playtype == 0 ~ "Rush" ,
                                    short_playtype == -1 ~ "Kick"))



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
all %>% 
  filter(turnover == 0 & down == 3) %>% 
  group_by(def_pos_team) %>% 
  summarise(mean_var = mean(def_EPA,  na.rm = TRUE))

###offense 
all %>% 
  filter(pos_team=="Ole Miss" & turnover == 0 ,  down != 4 ) %>% 
  ggplot(aes(x=yards_to_goal , y = EPA))+
  geom_point(aes(color = as.factor(down) , shape = as.factor(short_playtype) , size = 2))+
  geom_smooth(aes(color = as.factor(down) ), se =FALSE )+
  annotate("text", x = 7.5 , y = -2.75 ,
           label = paste( "First: .178" , "Second: .127" , "Third: .044" , sep = "\n"),hjust = 0 ,size =5 , fontface = "italic")+
  scale_color_manual(name = "Down" , values =downspalette) +
  scale_shape_discrete(name = "Play Type")+
  guides( size = FALSE , color = guide_legend(override.aes = list(size = 5)), 
          shape = guide_legend(override.aes = list(size = 5)))+
  scale_x_reverse()+
  geom_hline(linetype = "dashed", size = 1,   yintercept = 0)+
  labs(x = 'Yards to Goal', y = 'Expexted Points Added', 
       title = 'Ole Miss EPA', subtitle = '2022 Season')+
  theme_football()

###defense
all %>% 
  filter(pos_team!="Ole Miss" & turnover == 0 & down != 4) %>% 
  ggplot(aes(x=yards_to_goal , y = def_EPA))+  
  geom_smooth(aes(color = as.factor(down)), se = FALSE )+
  geom_point(aes(color = as.factor(down) , shape = as.factor(short_playtype) , size = 2))+
  annotate("text", x = 20 , y = -1.5 ,
           label = paste( "First: -.006" , "Second: .128" , "Third: -.637" , sep = "\n"),hjust = 0 ,size =5 , fontface = "italic")+
  scale_color_manual(name = "Down" , values =downspalette) +
  scale_shape_discrete(name = "Play Type")+
  guides( size = FALSE , color = guide_legend(override.aes = list(size = 5)), 
          shape = guide_legend(override.aes = list(size = 5)))+
  scale_x_reverse()+
  geom_hline(linetype = "dashed", size = 1,   yintercept = 0)+
  labs(x = 'Yards to Goal', y = 'Expexted Points Added', 
       title = 'Ole Miss Defense EPA', subtitle = '2022 Season')+
  theme_football()

###play type averages
all %>% 
  filter(turnover == 0 & pass == 1) %>% 
  group_by(def_pos_team) %>% 
  summarise(mean_var = mean(def_EPA,  na.rm = TRUE))

### offense play types
all %>% 
  filter(pos_team=="Ole Miss" & turnover == 0 , down != 4) %>% 
  ggplot(aes(x=yards_to_goal , y = EPA))+
  geom_point(aes(color = as.factor(short_playtype) , size = 2))+
  geom_smooth(aes(color = as.factor(short_playtype) ), se =FALSE)+
  annotate("text", x = 7.5 , y = -3.5 ,
           label = paste( "Pass: -.076" , "Rush: .234"  , sep = "\n"),hjust = 0 ,size =5 , fontface = "italic")+
  scale_color_manual(name = "Play Type" , values =playpalette) +
  guides( size = FALSE , color = guide_legend(override.aes = list(size = 5)))+
  scale_x_reverse()+
  geom_hline(linetype = "dashed", size = 1,   yintercept = 0)+
  labs(x = 'Yards to Goal', y = 'Expexted Points Added', 
       title = 'Ole Miss EPA', subtitle = '2022 Season')+
  theme_football()

### defense
all %>% 
  filter(pos_team!="Ole Miss" & turnover == 0 , down != 4) %>% 
  ggplot(aes(x=yards_to_goal , y = def_EPA))+
  geom_point(aes(color = as.factor(short_playtype) , size = 2))+
  geom_smooth(aes(color = as.factor(short_playtype) ), se =FALSE)+
  annotate("text", x = 20 , y = -1.5 ,
           label = paste( "Pass: -.242" , "Rush: -.151"  , sep = "\n"),hjust = 0 ,size =5 , fontface = "italic")+
  scale_color_manual(name = "Play Type" , values =playpalette) +
  guides( size = FALSE , color = guide_legend(override.aes = list(size = 5)))+
  scale_x_reverse()+
  geom_hline(linetype = "dashed", size = 1,   yintercept = 0)+
  labs(x = 'Yards to Goal', y = 'Expexted Points Added', 
       title = 'Ole Miss Defense EPA', subtitle = '2022 Season')+
  theme_football()


###calculating average epa for the player getting the ball. RB or QB
players <- all %>% 
  filter(pos_team == "Ole Miss" & turnover == 0) %>% 
  mutate(player_with_ball = case_when(pass == 1 ~ passer_player_name,
                                      rush== 1 ~ rusher_player_name, 
                                      TRUE ~ as.character(NA))) %>% 
  group_by(player_with_ball) %>%
  filter(n() >= 5) %>% 
  summarise(
    pass = mean(yards_gained [pass == 1 &  turnover  == "0"]),
    rush = mean(yards_gained[rush == 1  &  turnover  == "0"]),
    success = mean (success , na.rm = TRUE), 
    epa = mean (EPA , na.rm = TRUE),
    ypp = mean(yards_gained),
    number_of_plays = n()) %>% 
  mutate(player_pic = case_when(player_with_ball == "Jaxson Dart" ~ "https://d2mvzuao0kv8nb.cloudfront.net/images/2022/8/15/Dart_Jaxson_2022.JPG?width=300",
                                player_with_ball == "Luke Altmyer" ~ "https://d2mvzuao0kv8nb.cloudfront.net/images/2022/8/15/Altmyer_Luke_2022.JPG?width=80",
                                player_with_ball == "Quinshon Judkins" ~ "https://d2mvzuao0kv8nb.cloudfront.net/images/2022/8/15/Judkins_Quinshon_2022.JPG?width=80",
                                player_with_ball == "Ulysses Bentley IV" ~ "https://d2mvzuao0kv8nb.cloudfront.net/images/2022/8/15/Bentley_Ulysses_2022.JPG?width=80",
                                player_with_ball == "Zach Evans"~ "https://d2mvzuao0kv8nb.cloudfront.net/images/2022/8/15/Evans_Zach_2022.JPG?width=80",
                                TRUE ~ as.character(NA))) 

players %>% 
  select(player_with_ball , player_pic,  epa , ypp , number_of_plays) %>% 
  gt() %>% 
  cols_label(
    player_with_ball = "Name",
    epa = "Average EPA",
    ypp = "Yards Per Play",
    number_of_plays = "Plays",
    player_pic = "") %>% 
  gt_theme_espn() %>% 
  gt_img_rows(columns = player_pic, img_source = "web", height = 60) %>%
  gt_color_rows(epa, palette = "ggsci::blue_material") %>% 
  tab_header(title = "2022 Ole Miss Offensive Player Performance") %>% 
  tab_footnote(footnote = "Note: Turnover plays not included") %>% 
  tab_footnote(footnote = "Source: ESPN play by play data") %>% 
  tab_options(footnotes.multiline = TRUE)



