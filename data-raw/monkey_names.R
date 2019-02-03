library(tidyverse)
library(rvest)

wiki <- read_html("https://en.wikipedia.org/wiki/List_of_New_World_monkey_species")

monkey_names <- 
  wiki %>%
  html_nodes("table") %>%
  map(html_table) %>%
  map_dfr(as_tibble) %>%
  select(contains("name")) %>%
  filter(complete.cases(.)) %>% 
  pull(`Scientific name`) %>%
  tolower() %>%
  str_remove("\\(.+\\) ") %>%
  str_replace_all(" ", "_") %>%
  unique()

usethis::use_data(monkey_names, internal = TRUE)

