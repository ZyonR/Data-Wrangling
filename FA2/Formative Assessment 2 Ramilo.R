library(tidyverse)
library(wbstats)

who <- who2
population <- wb_data(indicator = "SP.POP.TOTL")
population$country <- replace(population$country, population$country == "United States", "United States of America")

# Determine Shape of Dataset
dim(who)
dim(population)

# Identify Datatypes
sapply(population, class)
colSums(is.na(population))

# Fix any anomalies
population2 <- subset(population, select = -c(unit,obs_status,footnote,iso2c,iso3c,last_updated)) %>% 
  na.omit()
glimpse(population2)
colSums(is.na(population2))

# Identify variables (WHO dataset)
colnames(who)
who2 <- pivot_longer(who,cols=sp_m_014:rel_f_65,names_to = "new_ep_f014", values_to = "cases") %>% 
  mutate(cases = replace_na(cases, 0))
glimpse(who2)

# Seperate values
who_tidy <- who2 %>% 
  separate(new_ep_f014,into=c("tb_type","sex","age_range"), sep="_")
glimpse(who_tidy)

# Identify variables (Population dataset)
colnames(population2)

# Perform pivot operation on population data
population3 <- population2 %>% 
  pivot_wider(names_from = date,values_from = SP.POP.TOTL)
glimpse(population3)

# Population Tidy Ver.
population_tidy <- population2 %>% 
  rename(
    population=SP.POP.TOTL,
    year = date
  )
glimpse(population_tidy)

# Joining Datasets
tuberculosis <- who_tidy %>%
  group_by(country, year, sex, tb_type, age_range) %>%
  summarise(cases = sum(cases, na.rm = TRUE), .groups = "drop")

tuberculosis <- merge(tuberculosis, population_tidy, by = c("country", "year"), all.x = TRUE)
glimpse(tuberculosis)


# United States TB Cases
us_tb_cases<- tuberculosis %>% 
  filter(country == "United States of America" & year >= 2001) %>% 
  group_by(sex) %>% 
  summarise(sum_cases = sum(cases))
glimpse(us_tb_cases)


# agregate cases by 100K
tuberculosis2 <- tuberculosis %>% 
  group_by(year, sex, tb_type, age_range) %>% 
  mutate(cases_per_100k = (cases / population) * 100000)
glimpse(tuberculosis2)

# Highest cases per 100K
highest_cases_per_100k <- tuberculosis2 %>% 
  group_by(country,year) %>% 
  summarise(sum_cases_per_100k = sum(cases_per_100k)) %>% 
  arrange(desc(sum_cases_per_100k))
head(highest_cases_per_100k,10)
tail(highest_cases_per_100k%>%na.omit(),10)  

cases_per_100k_CIUS <- highest_cases_per_100k %>% 
  filter(country == "China" | country == "India" | country == "United States of America") %>% 
  arrange(year)
view(cases_per_100k_CIUS)

# PLotting cases
ggplot(cases_per_100k_CIUS,aes(x=year,y=sum_cases_per_100k,color=country))+
  geom_line()+
  scale_y_log10()

# Distributions
tuberculosis_ageRange <- tuberculosis2 %>%
  group_by(age_range) %>%
  summarise(total_cases_per_100k = sum(cases_per_100k, na.rm = TRUE), .groups = "drop")
ggplot(tuberculosis_ageRange, aes(x = total_cases_per_100k)) +
  geom_density(alpha = 0.5) +
  scale_y_log10() + 
  labs(
    title = "Distribution of TB Cases per 100k by Age Group",
    x = "Total Cases per 100k (log scale)",
    y = "Density"
  ) +
  theme_minimal()

# cases per 100k x population in the 2000
tb_population <- tuberculosis2 %>% 
  filter(year==2000) %>% 
  group_by(population) %>% 
  summarise(total_cases_per_100k = sum(cases_per_100k, na.rm = TRUE), .groups = "drop")

ggplot(tb_population,aes(x=population,y=total_cases_per_100k))+
  geom_line()+ 
  scale_x_log10()
