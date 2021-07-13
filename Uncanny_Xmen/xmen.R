install.packages("tidytuesdayR")
library(grid)
library(gridExtra)
library(ggplot2)
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, ggvis, httr, lubridate,
               plotly, rio, rmarkdown, shiny, stringr, tidyr)
tuesdata <- tidytuesdayR::tt_load('2020-06-30')

#data from 2018 more metadata about specific comic book characters
olddata <- tidytuesdayR::tt_load(2018, week = 9)


comic_bechdel <- tuesdata$comic_bechdel
characters <- tuesdata$characters

character_visualization <- tuesdata$character_visualization
xmen_bechdel <- tuesdata$xmen_bechdel
covers <-  tuesdata$covers
issue_collaborators <- tuesdata$issue_collaborators
locations <- tuesdata$locations

week9_comic_charachters <-  olddata$week9_comic_characters


characters$character <- factor(characters$character)

##character_sum <- colSums(Filter(is.numeric, characters))

## Identify correlate EIC to bechdel
collaborator_bechdel_join <- xmen_bechdel %>% 
  inner_join(issue_collaborators, by = c("issue" = "issue")) %>% 
  group_by(editor_in_chief, pass_bechdel) %>%
  select(-notes, -editor, -reprint, -penciller)  %>%  
  count(issue) 


(bechdel_plot <- collaborator_bechdel_join %>% 
    ggplot(aes(x=editor_in_chief, y=n, fill=pass_bechdel)) + 
    geom_col() +
    labs(title = "How often comic editors pass the Bechdel test", 
       x = "\n Editor-in-Chief", y = "Number of Issues \n") +
    theme_bw() +
    theme(panel.grid = element_blank(), 
  axis.text = element_text(size = 12), 
  axis.title = element_text(size = 12), 
  plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
  plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
  legend.title = element_text(face = "bold"),
  legend.position = "bottom", 
  legend.box.background = element_rect(color = "grey", size = 0.3)))

ggsave(filename = "images/bechdel_plot.png", plot = bechdel_plot, width = 10,
       height =10)

## summaries of character &  actions
character_sum2 <- characters %>% 
  group_by(character) %>% 
  summarise(across(where(is.numeric), sum)) %>%
  select(-issue, -number_of_kills_non_humans, -expresses_reluctance_to_fight) %>% 
  ungroup()

(character_plot <- character_sum2 %>% 
    tidyr::gather("id", "value", 2:16) %>% 
    ggplot(., aes(character, value, fill = id)) + 
    geom_col() +
    coord_flip())

(character_plot2 <- character_sum2 %>% 
    tidyr::gather("id", "value", 2:16) %>% 
    ggplot(., aes(id, value, fill = character)) + 
    geom_col() +
  coord_flip())

ggsave(filename = "images/actions_by_character.png", plot = character_plot, 
       width = 10,height =10)

ggsave(filename = "images/character_by_action.png", plot = character_plot2, 
       width = 10, height =10)
