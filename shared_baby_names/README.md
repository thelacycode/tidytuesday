Stacked bar plot showing some of the most shared names by boys and girls over the last few decades.


# Filtering out anything earlier than 1960 and grouping by decade

unisex_names <- babynames %>%
  filter(year >= 1960) %>%
  mutate(decade = floor(year/10)*10) %>%
  group_by(decade, name, sex) %>%
  mutate(n2 = sum(n)) %>% 
  arrange(desc(n2)) %>% 
  slice(1) %>%
  ungroup() %>% 
  group_by(decade, name) %>%
  summarize(name, sex, n2, n3 = sum(n2), decade) %>%
  filter(n2 != n3, n2/n3 > .39, n2/n3 < .61, n3 > 1500) %>%
  ungroup() %>% 
  group_by(decade) %>% 
  arrange(desc(n3)) %>% 
  slice(1:10) %>% 
  ungroup() %>% 
  mutate(decade = recode(decade,"1960" = "60s", "1970" = "70s", "1980" = "80s", "1990" = "90s",
                         "2000" = "2000s", "2010" = "2010s"))
