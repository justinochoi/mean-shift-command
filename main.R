library(arrow)
library(tidyverse)
library(meanShiftR)
library(baseballr) 
library(janitor) 
library(gt) 

# load in data 
statcast_data <- read_parquet(
  "https://huggingface.co/datasets/Jensen-holm/statcast-era-pitches/resolve/main/data/statcast_era_pitches.parquet"
) %>% 
  filter(game_year == '2024') 

# create new variables, modify others
statcast_data <- statcast_data %>% 
  mutate(
    plat_adv = if_else(p_throws == stand, 1, 0), 
    count = paste(balls, strikes, sep = "-"), 
    count_type = case_when(
      count %in% c('0-1','0-2','1-2') ~ "behind", 
      count %in% c('0-0','1-1','2-2','3-2') ~ "even", 
      .default = "ahead"
    ), 
    runners_on = case_when(
      is.na(on_1b) & is.na(on_2b) & is.na(on_3b) ~ 0, 
      .default = 1 
    ), 
    pfx_x = if_else(p_throws == 'L', -pfx_x, pfx_x), 
    plate_x = if_else(p_throws == 'L', -plate_x, plate_x)
  ) 

# arm angle data from baseball savant 
arm_angles <- read_csv(file.choose()) 

# incorporate arm angle information into statcast data
main_data <- statcast_data %>% 
  filter(pitch_type == 'FF') %>% 
  select(player_name, pitcher, release_speed, plate_x, plate_z, 
         plat_adv, count_type, runners_on) %>% 
  inner_join(select(arm_angles, pitcher, ball_angle), 
             by = c('pitcher')) %>% 
  mutate(
    id = row_number(), 
    velo_type = case_when(
        release_speed < quantile(release_speed, 0.25, na.rm = T) ~ 'low', 
        release_speed < quantile(release_speed, 0.75, na.rm = T) ~ 'mid', 
        .default = 'high'
        ), 
    angle_type = case_when(
      ball_angle < quantile(arm_angles$ball_angle, 0.25) ~ 'low', 
      ball_angle < quantile(arm_angles$ball_angle, 0.75) ~ 'mid', 
      .default = 'high'
      )
  ) %>% 
  drop_na() 

# function that implements mean shift algorithm on grouped data 
# returns original data with estimated modes added
mean_shift <- function(data) {
  mat <- as.matrix(select(data, plate_x, plate_z)) 
  bandwidth <- nrow(data)^(-1/9)
  ms_result <- meanShift(mat, algorithm = 'LINEAR', kernelType = 'NORMAL', 
                         bandwidth = rep(bandwidth, ncol(mat))) 
  data$mode_x <- ms_result$value[,1]
  data$mode_z <- ms_result$value[,2]
  return(data) 
} 

# store function results in separate data frame 
# also calculate euclidean distances 
ms_results <- main_data %>% 
  group_by(velo_type, angle_type, plat_adv, count_type, runners_on) %>% 
  group_map(
    ~ mean_shift(.x)
  ) %>% 
  bind_rows() %>% 
  mutate(
    euclid_dist = sqrt((plate_x - mode_x)^2 + (plate_z - mode_z)^2)
  ) 

# join results with original data 
main_data <- main_data %>% 
  left_join(select(ms_results, id, euclid_dist), 
            by = 'id') 

# results for each individual pitcher with min. 100 fastballs
player_data <- main_data %>% 
  group_by(player_name, pitcher) %>% 
  summarize(
    sample = n(), 
    miss_distance = round(mean(euclid_dist), 3) 
  ) %>% 
  arrange(miss_distance) %>% 
  filter(sample >= 100)
 
# any bias by velo and arm angle type? 
bias_check <- main_data %>% 
  group_by(angle_type, velo_type) %>% 
  summarize(
    miss_distance = round(mean(euclid_dist), 3) 
  ) 
# doesn't seem like it 
# what if we add more grouping variables? 
bias_check2 <- main_data %>% 
  group_by(angle_type, velo_type, plat_adv, count_type, runners_on) %>% 
  summarize(
    sample = n(), 
    miss_distance = round(mean(euclid_dist), 3) 
  ) 
# now we see some differences hmmmm

main_data %>% 
  group_by(runners_on) %>% 
  summarize(
    miss_distance = round(mean(euclid_dist), 3)
  )
# miss more when runners are on 

main_data %>% 
  group_by(count_type) %>% 
  summarize(
    miss_distance = round(mean(euclid_dist), 3)
  )
# miss more when 'behind' from batter POV 
# even and ahead counts are more or less the same 

# comparing correlations between miss distances and common metrics 
# data sourced from fangraphs leaderboards 
fg_data <- read.csv(file.choose())
fg_data <- fg_data %>% 
  clean_names() %>% 
  rename(pitcher = mlbamid) %>% 
  select(pitcher, era, fip, bb, loc_fa) 

# add fangraphs data 
player_data <- player_data %>% 
  left_join(fg_data, by = 'pitcher') %>% 
  drop_na(loc_fa) 

# miss distance has a much weaker correlation with walk rate 
# womp womp 
cor(player_data$bb, player_data$loc_fa) 
cor(player_data$bb, player_data$miss_distance)
  
# bottom ten in miss distance 
player_data %>% 
  arrange(miss_distance) %>% 
  filter(sample > 500) %>%
  select(player_name, sample, miss_distance) %>% 
  head(n = 10) %>% 
  gt(row_group_as_column = T) 

# top ten in miss distance 
player_data %>% 
  arrange(desc(miss_distance)) %>% 
  filter(sample > 500) %>%
  select(player_name, sample, miss_distance) %>% 
  head(n = 10) %>% 
  gt(row_group_as_column = T) 

label1 <- data.frame(metric = c("R: 0.46"))
label2 <- data.frame(metric = c("R: -0.62")) 

# miss distance vs. walk rate 
player_data %>% 
  ggplot(aes(x=miss_distance, y=bb*100)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  geom_label(data = label1, aes(x=0.75, y=20, label = metric)) + 
  theme_bw() + 
  labs(
    x = 'Average Miss Distance (ft.)', 
    y = 'Walk Rate (%)', 
    subtitle = '2024, min. 100 fastballs'
  ) + 
  ggtitle("Miss Distance vs. Walk Rate by Pitcher") + 
  theme(plot.title = element_text(face = 'bold')) 

# Location+ vs. walk rate  
player_data %>% 
  ggplot(aes(x=loc_fa, y=bb*100)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  geom_label(data = label2, aes(x=115, y=20, label = metric)) + 
  theme_bw() + 
  labs(
    x = 'Location+', 
    y = 'Walk Rate (%)', 
    subtitle = '2024, min. 100 fastballs'
  ) + 
  ggtitle("Location+ vs. Walk Rate by Pitcher") + 
  theme(plot.title = element_text(face = 'bold'))




  