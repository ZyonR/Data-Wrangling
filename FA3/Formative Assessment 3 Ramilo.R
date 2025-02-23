library(tidyverse)
library(ggrepel)
library(kableExtra)
library(cowplot)

# Loading the data set
load("~/RStudioFiles/Data-Wrangling/FA3/ml_pay.rdata")
is_tibble(ml_pay) # FALSE

# From a data frame into a tibble
mlb_raw <- as_tibble(ml_pay)
is_tibble(mlb_raw)
print(mlb_raw)

# Print dimensions of the tibble
cat("`mlb_raw` dimensions: ",dim(mlb_raw)[1],"X",dim(mlb_raw)[2])

# It matches the descriptions of the data set

# Split the data set into 2
# Aggregated data
mlb_aggregate <- mlb_raw %>% 
  select(Team.name.2014,payroll,avgwin) %>% 
  rename(
    team = Team.name.2014,
    payroll_aggregate = payroll,
    pct_wins_aggregate = avgwin
  )
# year-by-year data
mlb_yearly <- mlb_raw %>% 
  select(-c(payroll,avgwin)) %>% 
  rename(
    team = Team.name.2014
  ) %>% 
  pivot_longer(
    cols = p1998:p2014,
    names_to = "p_year",
    values_to = "payroll"
  ) %>% 
  pivot_longer(
    cols = X2014:X1998,
    names_to = "Xyear",
    values_to = "num_wins"
  ) %>% 
  pivot_longer(
    cols = X2014.pct:X1998.pct,
    names_to = "Xyear.pct",
    values_to = "pct_wins"
  ) %>% 
  mutate(
    p_year = as.numeric(str_remove(p_year,"p")),
    Xyear = as.numeric(str_remove(Xyear,"X")),
    Xyear.pct = as.numeric(str_remove(str_remove(Xyear.pct,"X"),".pct"))
  )

payroll_mlb <- mlb_yearly %>% 
  select(team,p_year,payroll)%>%
  distinct(team, p_year, payroll, .keep_all = TRUE) %>% 
  rename(
    year = p_year
  )

num_wins_mlb <- mlb_yearly %>% 
  select(team,Xyear,num_wins)%>%
  distinct(team, Xyear, num_wins, .keep_all = TRUE) %>% 
  rename(
    year = Xyear
  )

win_perc_mlb <- mlb_yearly %>% 
  select(team,Xyear.pct,pct_wins)%>%
  distinct(team, Xyear.pct, pct_wins, .keep_all = TRUE) %>% 
  rename(
    year = Xyear.pct
  )

mlb_yearly <- payroll_mlb %>% 
  left_join(num_wins_mlb,by=c("team","year")) %>% 
  left_join(win_perc_mlb,by=c("team","year")) %>%
  select(team, year, payroll, pct_wins, num_wins)
view(mlb_yearly)

# Finding the dimensions of aggregate and year-by-year tables
# Year-by-year
cat("`mlb_yearly` dimensions: ",dim(mlb_yearly)[1],"X",dim(mlb_yearly)[2])
# Aggregate
cat("`mlb_aggregate` dimensions: ",dim(mlb_aggregate)[1],"X",dim(mlb_aggregate)[2])

# Double checking the aggregated table
mlb_aggregate_computed <- mlb_yearly %>% 
  group_by(team) %>% 
  summarize(
    payroll_aggregate_computed = sum(payroll)/1000,
    pct_wins_aggregate_computed = sum(num_wins)/(n()*162)
  )

# Joining 2 tables together
mlb_aggregate_joined <- mlb_aggregate %>% 
  left_join(mlb_aggregate_computed,by="team")

# Creating 2 plots for aggregated vs computed
aggregated_v_computed_plt1 <- ggplot(mlb_aggregate_joined,aes(x=payroll_aggregate_computed,y=payroll_aggregate))+
  geom_point(
    size = 1.5,
    alpha = 0.8
  )+
  theme_minimal()+
  labs(
    title = " ",
    caption = "Major League Baseball",
    subtitle = "Represented by Billions",
  )+
  xlab("Payroll Aggregate Computed Value")+
  ylab("Payroll Aggregate Value")+
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "solid", size = 0.5)

aggregated_v_computed_plt2 <- ggplot(mlb_aggregate_joined,aes(x=pct_wins_aggregate_computed,y=pct_wins_aggregate))+
  geom_point(
    size = 1.5,
    alpha = 0.8
  )+
  theme_minimal()+
  labs(
    title = " ",
    caption = "Major League Baseball",
    subtitle = "Represented as Decimals",
  )+
  xlab("Win Percentage Computed Value")+
  ylab("Win Percentage Aggregated Value")+
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "solid", size = 0.5)

# Joining 2 plots together
aggregated_v_computed_plt <- plot_grid(
  aggregated_v_computed_plt1, 
  aggregated_v_computed_plt2, 
  labels = c("Payroll", "Win Percentage"), 
  ncol = 2
)

# Plot payroll interms of year
mlb_means <- mlb_yearly %>%
  group_by(team) %>%
  summarize(mean_payroll = mean(payroll, na.rm = TRUE))

payroll_per_year_plt <- ggplot(mlb_yearly,aes(x=year,y=payroll))+
  geom_point(
    size = 1.5,
    alpha = 0.8
  )+
  theme_minimal()+
  xlab("Year")+
  ylab("Payroll Value")+
  facet_wrap(~team,ncol=5)+
  geom_hline(data = mlb_means, aes(yintercept = mean_payroll), color = "red", linetype = "solid", size = 0.7)

# Find highest payroll within the computed data
sorted_payroll_mlb <- mlb_aggregate_computed %>% 
  select(team,payroll_aggregate_computed) %>% 
  arrange(desc(payroll_aggregate_computed))

kable(head(sorted_payroll_mlb,3), caption = "Top 3 Highest Payroll")

# Find winrate percentage increase
sorted_pct_win_increase <- mlb_yearly %>% 
  select(team,year,pct_wins) %>% 
  pivot_wider(names_from = "year",values_from = pct_wins) %>% 
  mutate(
    pct_increase = `2014` - `1998`
  ) %>% 
  left_join(mlb_aggregate_computed,by="team") %>% 
  select(team,pct_increase,payroll_aggregate_computed) %>% 
  arrange(desc(pct_increase))
kable(head(sorted_pct_win_increase,3), caption = "Top 3 Highest Win Percentage Increase")

# Win percentage
mlb_pct_win_means <- mlb_yearly %>%
  group_by(team) %>%
  summarize(mean_pct_win = mean(pct_wins, na.rm = TRUE))

pct_win_per_year_plt <- ggplot(mlb_yearly,aes(x=year,y=pct_wins))+
  geom_point(
    size = 1.5,
    alpha = 0.8
  )+
  theme_minimal()+
  xlab("Year")+
  ylab("Win Percentage")+
  facet_wrap(~team,ncol=5)+
  geom_hline(data = mlb_pct_win_means, aes(yintercept = mean_pct_win), color = "red", linetype = "solid", size = 0.7)

# top 3 teams with high pct
sorted_pct_win_mlb <- mlb_aggregate_computed %>% 
  select(team,pct_wins_aggregate_computed) %>% 
  arrange(desc(pct_wins_aggregate_computed))

kable(head(sorted_pct_win_mlb,3), caption = "Top 3 Highest Win Percentage")

# top 3 most erratic teams
sorted_erratic_pct_win_increase <- mlb_yearly %>% 
  select(team,pct_wins) %>% 
  group_by(team)%>%
  summarise(
    pct_wins_sd = sd(pct_wins)
  ) %>% 
  arrange(desc(pct_wins_sd))
kable(head(sorted_erratic_pct_win_increase,3), caption = "Top 3 Most Erratic Teams")

# Plot win percentage and payroll
win_perc_v_payroll <- ggplot(mlb_aggregate, aes(x = pct_wins_aggregate, y = payroll_aggregate)) +
  geom_point(
    size = 1.5,
    alpha = 0.8
  )+
  geom_text_repel(
    aes(label = team),
    family = "Poppins",
    size = 2.5,
    min.segment.length = 0, 
    seed = 42, 
    box.padding = 0.5,
    max.overlaps = Inf,
    arrow = arrow(length = unit(0.010, "npc")),
    nudge_x = .001,
    nudge_y = .001,
    color = "grey50"
  )+
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 0.5)

# Identify the most efficient teams
mlb_aggregate_computed <- mlb_aggregate_computed %>% 
  mutate(
    team_efficiency = pct_wins_aggregate_computed / payroll_aggregate_computed
  ) %>% 
  arrange(desc(team_efficiency))

kable(head(mlb_aggregate_computed,3), caption = "Top 3 Most Efficient Teams")
