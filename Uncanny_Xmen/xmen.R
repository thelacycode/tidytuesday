install.packages("tidytuesdayR")

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

character_sum <- colSums(Filter(is.numeric, characters))

character_sum2 <- characters %>% 
 group_by(character) %>% 
  select(-issue) 
  
  
## Identify correlate EIC to bechdel
collaborator_bechdel_join <- xmen_bechdel %>% 
  inner_join(issue_collaborators, by = c("issue" = "issue")) %>% 
  group_by(editor_in_chief, pass_bechdel) %>%
  select(-notes, -editor, -reprint, -penciller) %>% 
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


