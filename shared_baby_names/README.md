Stacked bar plot showing some of the most shared names by boys and girls over the last few decades.


## Filtering out anything earlier than 1960 and grouping by decade

````
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
````

## Setting up stacked bar plot

````
p <- unisex_names %>%
  ggplot(aes(x = name, y = n2)) +
  theme_stata() +
  scale_fill_stata()
  
shared_name_plot <-    p +
  geom_bar(position = 'fill', stat = 'identity', aes(fill = sex)) +
  geom_text(aes(label = n2), position = 'fill', vjust=.5,hjust=4.25, size=3, 
      color = "#F5F5F5") +
  coord_flip()  +
  facet_wrap(~decade, scales = "free_y", ncol = 2) +
  labs(y = "Percentage of Total", x = "Baby Names",
       title = "Most Common Names Used For Boys and Girls Per Decade",
       fill = "Sex") +
  theme(plot.title = element_text(color = "#28282B", size = 20, face='bold'),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "#F5F5F5" ),
        legend.title = element_text(color = "#28282B", face='bold' )) + 
  scale_y_continuous(labels = scales::percent)
````  
  
  <img src="/shared_name_plot.png" width="625" height="550" />
