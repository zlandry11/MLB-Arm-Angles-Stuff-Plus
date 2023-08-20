setwd("/Users/zach_landry/Library/Mobile Documents/com~apple~CloudDocs/Data")
library(tidyverse)
library(RMySQL)
library(DBI)
library(janitor)
library(Lahman)

# Connecting to local database and importing data
db <- dbConnect(RMySQL::MySQL(), 
                dbname = "statcast", 
                host = "localhost", 
                port = 3306, 
                user = "root",
                password = "*****")
dbListTables(db)
data <- dbReadTable(db,"pbp_2022")
summary(data)

# Relevant pitching data
data <- data %>% 
  select(pitcher, pitcher_name, p_throws, batter_name, stand, balls, strikes, pitch_name, release_speed, effective_speed, release_spin_rate, spin_axis, release_pos_x, release_pos_y, release_pos_z, release_extension, pfx_x, pfx_z, plate_x, plate_z, zone, description, launch_speed, launch_angle, estimated_ba_using_speedangle, estimated_woba_using_speedangle, launch_speed_angle, delta_run_exp)

# Gathering pitch-by-pitch data and deriving metrics
pitch_data <- data %>% 
  select(pitcher, pitcher_name, p_throws, pitch_name, release_speed, release_spin_rate, spin_axis, pfx_x, pfx_z, plate_x, plate_z, zone, release_pos_x, release_pos_z, release_extension) %>% 
  drop_na() %>% 
  mutate(BU = release_spin_rate / release_speed,
         total_movement = sqrt((pfx_x)^2 + (pfx_z)^2)*12,
         tilt = paste(as.character(floor((spin_axis*2)/60)), sprintf("%02d", (spin_axis*2) %% 60), sep = ":"))

# Creating pitcher dataset         
pitcher_data <- pitch_data %>% 
  group_by(pitcher_name, pitch_name) %>%  # Grouping data by pitcher and pitch type
  # Summarizing pitch metrics
  summarise(n_pitches = n(),
            avg_velo = round(mean(release_speed), 1),
            avg_spin_rate = round(mean(release_spin_rate), 0),
            avg_BU = round(mean(BU), 1),
            avg_spin_axis = round(mean(spin_axis), 0),
            avg_tilt = paste(as.character(floor((avg_spin_axis*2)/60)), sprintf("%02d", (avg_spin_axis*2) %% 60), sep = ":"),
            avg_horiz_movement = round(mean(pfx_x*12), 1),
            avg_vert_movement = round(mean(pfx_z*12), 1),
            avg_total_movement = round(mean(total_movement*12), 1)
  ) %>% 
  # Filtering out pitches with less than 10% usage
  ungroup() %>%  
  group_by(pitcher_name) %>%
  mutate(total_pitches_by_pitcher = sum(n_pitches)) %>% 
  filter(n_pitches >= total_pitches_by_pitcher * 0.1) %>% 
  select(-total_pitches_by_pitcher) # removing redundant column 

# Filtering out position players 
pitcher_data <- pitcher_data %>% 
  group_by(pitcher_name) %>% 
  filter(sum(n_pitches) >= 200)

# Using Lahman database to gather heights for each player
Height <- People %>% select(playerID, height) %>% mutate(height = height/12)

# Using dataset containing multiple ID formats
id_map <- read.csv("playerIdMap.csv")

Height <- Height %>% 
  inner_join(id_map, by = c("playerID" = "IDPLAYER")) %>% 
  select(playerID,MLBID, PLAYERNAME, height) 

summary(Height) # 2,918 playerID's

# Creating a release point dataset 
release_point_data <- pitch_data %>% 
  group_by(pitcher) %>% 
  summarise(n_pitches = n(),
            avg_release_height = mean(release_pos_z),
            avg_release_side = mean(release_pos_x)) %>%
  # Estimating arm angle using Palensky's method: https://medium.com/iowabaseballmanagers/similarity-scoring-college-pitchers-8332fc5860b6
  mutate(est_arm_angle_palensky = (180*atan((avg_release_height-min(avg_release_height)) / abs(avg_release_side))) / pi) %>% 
  filter(n_pitches > 200)

# Adding height to release point dataframs
release_point_data <- release_point_data %>% 
  left_join(Height, by = c("pitcher" = "MLBID")) %>% 
  select(pitcher, PLAYERNAME, n_pitches, avg_release_height, avg_release_side, height, est_arm_angle_palensky) %>% 
  clean_names()
  
summary(release_point_data) # 90 missing heights

# Removing missing values
release_point_data <- drop_na(release_point_data, c("playername", "height"))
summary(release_point_data)

# Adding Mottley's method for calculating arm angle: https://web.archive.org/web/20230123183755/https://www.rundownbaseball.com/project/calculating-arm-angles-using-statcast-data/
release_point_data <- release_point_data %>% 
  mutate(est_arm_angle_mottley = (180*atan((avg_release_height - height*.7) / abs(avg_release_side))) / pi)

ggplot(release_point_data, aes(avg_release_side, avg_release_height, color = est_arm_angle_palensky))+
  geom_point()+
  theme_minimal()

ggplot(release_point_data, aes(avg_release_side, avg_release_height, color = est_arm_angle_mottley))+
  geom_point()+
  theme_minimal()

# Comparing the arm angles of sub-zero angle guys according to Mottley 
release_point_data %>% filter(est_arm_angle_mottley < 0) %>% select(playername, est_arm_angle_mottley, est_arm_angle_palensky)

# Defining the column to group by
release_cols <- release_point_data[,"est_arm_angle_palensky"]

# Decide how many clusters to look at
n_clusters <- 10

# Initialize total within sum of squares error: wss
wss <- numeric(n_clusters)

set.seed(123)

# Look over 1 to n possible clusters
for (i in 1:n_clusters) {
  # Fit the model: km.out
  km.out <- kmeans(release_cols, centers = i, nstart = 20)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
wss_df <- tibble(clusters = 1:n_clusters, wss = wss)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters')+
  geom_hline(
    yintercept = wss, 
    linetype = 'dashed', 
    col = c(rep('#000000',4),'#FF0000', rep('#000000', 5))
  ); scree_plot


km.out <- kmeans(release_cols, centers = 5, nstart = 20); km.out


clusters <- as.factor(km.out$cluster)
# Color-blind palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(release_point_data, aes(avg_release_side, avg_release_height, color = clusters))+
  geom_point()+
  labs(x = "Release Side", y = "Release Height", color = "Arm Angle Cluster")+
  scale_color_manual(values = cbPalette)

# Defining the column to group by
release_cols2 <- release_point_data[,"est_arm_angle_mottley"]

# Initialize total within sum of squares error: wss
wss <- numeric(n_clusters)

# Look over 1 to n possible clusters
for (i in 1:n_clusters) {
  # Fit the model: km.out
  km.out <- kmeans(release_cols2, centers = i, nstart = 20)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
wss_df <- tibble(clusters = 1:n_clusters, wss = wss)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters')+
  geom_hline(
    yintercept = wss, 
    linetype = 'dashed', 
    col = c(rep('#000000',4),'#FF0000', rep('#000000', 5))
  ); scree_plot


km.out2 <- kmeans(release_cols2, centers = 5, nstart = 20); km.out2
km.out
km.out$cluster <- c("Overtop", "High 3/4", "Low 3/4", "Sidearm", "Submarine")[km.out$cluster]
km.out2$cluster <- c("Submarine", "High 3/4", "Overtop", "Low 3/4", "Sidearm")[km.out2$cluster]
km.out2

# Adding cluster label to release point data
release_point_data <- release_point_data %>% 
  mutate(cluster_palensky = km.out$cluster,
         cluster_mottley = km.out2$cluster)

ggplot(release_point_data, aes(abs(avg_release_side), avg_release_height, color = cluster_mottley))+
  geom_point(alpha = .5)+
  labs(x = "Release Side", y = "Release Height", title = "K-Means Classification of Arm Slots", color = "Arm Slot")+
  theme_minimal()+
  scale_color_manual(values = cbPalette, breaks = c("Overtop", "High 3/4", "Low 3/4", "Sidearm", "Submarine"))

df <- release_point_data %>% 
  select(avg_release_height, avg_release_side, est_arm_angle_palensky, est_arm_angle_mottley) %>% 
  mutate(avg_release_side = abs(avg_release_side)) %>% 
  scale()

# create a plot of the number of clusters vs. the total within sum of squares
fviz_nbclust(df, kmeans, method = "wss")

#calculate gap statistic based on number of clusters
gap_stat <- clusGap(df,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)

#plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat)

set.seed(10)
km <- kmeans(df, centers = 7, nstart = 25); 
km$cluster

fviz_cluster(km, data = df)

km$cluster <- as.factor(km$cluster)

ggplot(release_point_data, aes(abs(avg_release_side), avg_release_height, color = km$cluster))+
  geom_point(alpha = .5)+
  labs(x = "Release Side", y = "Release Height", title = "K-Means Classification of Arm Slots", color = "Arm Slot")+
  theme_minimal()+
  scale_color_manual(values = cbPalette)

km2 <- kmeans(df, centers = 5, nstart = 20)
km2$cluster <- as.factor(km2$cluster)

ggplot(release_point_data, aes(abs(avg_release_side), avg_release_height, color = km2$cluster))+
  geom_point(alpha = .5)+
  labs(x = "Release Side", y = "Release Height", title = "K-Means Classification of Arm Slots", color = "Arm Slot")+
  theme_minimal()+
  scale_color_manual(values = cbPalette)

# Calculating explained variance (ev) for each k-means clustering model
km.out.ev = round((km.out$betweenss / km.out$totss)*100, 2)
km.out2.ev = round((km.out2$betweenss / km.out2$totss)*100, 2) # highest
km.ev = round((km$betweenss / km$totss)*100, 2)
km2.ev = round((km2$betweenss / km2$totss)*100, 2)

mismatch <- release_point_data %>% 
  select(playername, cluster_palensky, cluster_mottley) %>% 
  filter(cluster_palensky != cluster_mottley)

install.packages("factoextra")
library(factoextra)
library(cluster)


