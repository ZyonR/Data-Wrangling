library(tidyverse)
library(wbstats)

who <- who2
population <- wb_data(indicator = "SP.POP.TOTL")

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
tuberculosis <- merge(who_tidy,population_tidy,by=c("country","year")) %>% 
  group_by(country,year,sex) %>% 
  summarise(cases = sum(cases))
tuberculosis <- merge(tuberculosis,population_tidy,by=c("country","year"))
glimpse(tuberculosis)

# United States TB Cases
us_tb_cases<- tuberculosis %>% 
  filter(country == "United States" & year >= 2001)
glimpse(us_tb_cases)

