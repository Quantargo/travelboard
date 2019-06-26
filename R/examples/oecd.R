# See also https://github.com/expersso/OECD
# - e.g. Price levels,
# - Unemployment, Youth Unemployment  
# - Corruption
# - Better Life Index (BLI)
#
# Better Life index: http://www.oecdbetterlifeindex.org/
# https://stats.oecd.org/

library(OECD)
library(tidyverse)
# https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes
countries <- c(Austria = "AUT", Greece = "GRC", Italy = "ITA", Spain = "ESP", Portugal = "PRT")
               #Switzerland = "CHE")

# Search at https://stats.oecd.org using box on the left 
# Alternatively, use search_dataset() function from the OECD package
dataset_list <- get_datasets()
search_dataset("Better Life", data = dataset_list) # BLI
search_dataset("Economic Outlook No 105", data = dataset_list) # EO (Forecasts for Unemployment, Growth, etc.)
search_dataset("Population", data = dataset_list) # RPOP (Population Statistics by Sex, Age)
search_dataset("Purchasing Power Parity", data = dataset_list) # PPPGDP (PPP, Exchange Rates)

#bli <- readRDS(file = "data/oecd/bli.rds")
bli <- readRDS(file = "~/workshop/data/oecd/bli.rds")
df <- bli %>% 
  filter(INEQUALITY == "TOT", LOCATION %in% countries)
ggplot(df) + geom_bar(aes(x = LOCATION, y = obsValue, fill = LOCATION), stat = "identity") + 
  facet_wrap(~INDICATOR, scales = "free_y")

# Plot Life Satisfaction only
# See https://stats.oecd.org for more descriptions
bli %>% 
  filter(INEQUALITY == "TOT", LOCATION %in% countries, INDICATOR == "SW_LIFS") %>%
ggplot() + geom_bar(aes(x = LOCATION, y = obsValue, fill = LOCATION), stat = "identity")

# Get PPP values, exchange rates, etc.
# Database inventory: http://www.oecd.org/sdd/na/44221974.pdf
# See also https://www.oecd.org/sdd/prices-ppp/purchasingpowerparities-frequentlyaskedquestionsfaqs.htm
pppgdp <- readRDS(file = "~/workshop/data/oecd/pppgdp.rds")
dat_filter <- pppgdp %>% 
  filter(LOCATION %in% countries, UNIT == "NATUSD")
ggplot(dat_filter) + 
  geom_line(aes(obsTime, obsValue, group = LOCATION, color = LOCATION))

# https://www.oecd.org/eco/outlook/EO104_Database_Inventory.pdf
eo <- readRDS(file = "~/workshop/data/oecd/eo.rds")
# Plot (projected) unemployment rate (UNR), 
eo %>%
  mutate(obsTime = as.integer(obsTime), obsValue = obsValue / 100) %>%
  filter(LOCATION %in% countries, VARIABLE == "UNR") %>%
  ggplot() + 
  geom_line(aes(obsTime, obsValue, group = LOCATION, color = LOCATION)) + 
  scale_y_continuous(labels = scales::percent)
  
# Plot (projected) GDP growth
eo %>%
  mutate(obsTime = as.integer(obsTime)) %>%
  filter(LOCATION %in% countries, VARIABLE == "GDPV_ANNPCT") %>%
  ggplot() + 
  geom_line(aes(obsTime, obsValue, group = LOCATION, color = LOCATION))

# Plot (projected) Population growth
eo %>%
  mutate(obsTime = as.integer(obsTime)) %>%
  filter(LOCATION %in% countries, VARIABLE == "POP") %>%
  group_by(LOCATION) %>%
  arrange(obsTime) %>%
  mutate(diff = obsValue - dplyr::lag(obsValue), 
         percent_diff = diff / dplyr::lag(obsValue)) %>%
  ggplot() + 
  geom_line(aes(obsTime, percent_diff, group = LOCATION, color = LOCATION))
