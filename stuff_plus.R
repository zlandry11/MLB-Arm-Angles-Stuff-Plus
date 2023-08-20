library(tidyverse)
library(RMySQL)
library(DBI)

# Connecting to local database and importing data
db <- dbConnect(RMySQL::MySQL(), 
                dbname = "statcast", 
                host = "localhost", 
                port = 3306, 
                user = "root",
                password = "Haverford2023!")

df <- dbSendQuery(db, "select 
game_year as year,
pitcher_name as pitcher, 
pitch_name as pitch_type, 
count(*) as n_pitches,
avg(release_speed) as avg_velo, 
avg(release_spin_rate) as avg_spin_rate,
avg(spin_axis) as avg_spin_axis,
avg(pfx_x)*12 as avg_hb,
avg(pfx_z)*12 as avg_vb,
avg(delta_run_exp)*100 as 'rv100' from master
where pitch_name in ('Cutter', '4-Seam Fastball', 'Changeup', '2-Seam Fastball', 'Curveball', 'Sinker', 'Slider', 'Knuckle Curve', 'Split-Finger', 'Forkball', 'Knuckleball', 'Screwball', 'Slurve', 'Sweeper', 'Slow Curve')
group by pitch_name, pitcher_name, game_year
having count(*) > 50;")

df <- fetch(df, n=-1)

df %>% summarise(n_pitchers = n_distinct(pitcher))

df <- df %>% 
  group_by(pitcher, year) %>% 
  filter(sum(n_pitches) >= 100) %>% 
  ungroup()

df %>% 
  filter(pitch_type=="4-Seam Fastball") %>% 
  ggplot(aes(avg_vb, rv100))+
  geom_point()

mlr.model <- df %>% 
  filter(pitch_type=="4-Seam Fastball") %>% 
  lm(rv100 ~ avg_velo + avg_vb, data = .)

summary(mlr.model)
