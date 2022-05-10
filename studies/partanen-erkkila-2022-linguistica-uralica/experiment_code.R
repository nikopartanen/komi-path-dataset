# This is the data analysis code for study 
#
# Partanen, Niko and Erkkilä, Riku 2022 Cases 
# denoting path in Komi: semantic, dialectological
# and historical perspectives. Linguistica Uralica LVIII, 2. 
# DOI: https://dx.doi.org/10.3176/lu.2022.2.01
#
# This code is distributed within:
# 
# Partanen, Niko and Erkkilä, Riku 2021 Komi Path Dataset (v1.1). Zenodo. 
# Zenodo. https://doi.org/10.5281/zenodo.5159286
#
# Please not that the DOI-identifier above refers to the newest version.
# The version used in this study was updated 10.5.2022 corresponding
# to the version 1.1. 

library(tidyverse)
# library(googlesheets4)

# The data was stored in a Google Sheet, now
# converted to tsv for archiving. 

#komi <- read_sheet("https://docs.google.com/spreadsheets/d/1xONRBpCqLzHfgMEweNcUhR6xvoWR5n1gw9fd_xuFULI/edit#gid=0")
#write_delim(komi, "data/observations.tsv", delim = "\t")
komi <- read_delim("data/observations.tsv", delim = "\t")

table_number_test <- table(komi %>%
                             filter(pos %in% c("NOUN")) %>%
                             pull(number), komi %>%
                             filter(pos %in% c("NOUN")) %>%
                             pull(case))

# Chi-squared test

chisq.test(table_number_test, correct = FALSE)$p.value

komi %>%
  filter(pos %in% c("ADP", "NOUN")) %>%
  mutate(pos = factor(pos, levels = c("ADV", "ADP", "NOUN"))) %>%
  ggplot(data = ., aes(x = pos, fill = case)) +
  geom_bar() +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)) + 
  theme(legend.text = element_text(size=9),
        legend.title = element_text(size=9)) +
  ylab("Occurrences") +
  xlab("Part of Speech") +
  labs(fill = "Case") +
  theme(axis.title.x = element_text(vjust=-0.5))

adp_noun <- komi %>%
  filter(pos %in% c("ADP", "NOUN"))

pos_case <- table(adp_noun$pos, adp_noun$case)

komi %>%
  filter(pos %in% c("ADP", "NOUN")) %>%
  mutate(pos = factor(pos, levels = c("ADV", "NOUN", "ADP"))) %>%
  mutate(number = factor(number, levels = c("sg", "pl"))) %>%
  ggplot(data = ., aes(x = number, fill = case)) +
  geom_bar(position = "fill") +
  facet_wrap(~ pos) +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)) + 
  theme(legend.text = element_text(size=9),
        legend.title = element_text(size=9)) +
  theme(axis.title.x = element_text(vjust=-0.5))

pos_num <- table(adp_noun$pos, adp_noun$number)

noun <- komi %>%
  filter(pos %in% c("NOUN"))

noun_case  <- table(noun$case, noun$number)

# Categories are separated here

cats <- komi %>% filter(! is.na(category_based_on_finnish)) %>%
  filter(pos %in% c("ADP", "NOUN")) %>%
  mutate(category = 
case_when(str_detect(tolower(category_based_on_finnish), "väylä") ~ "Oblong",
                              
str_detect(tolower(category_based_on_finnish), "tila") ~ "Two or three dimensional",
                              
str_detect(tolower(category_based_on_finnish), "aukko") ~ "Hole",
                              TRUE ~ "other")) %>%
  filter(! category == "other") %>%
  mutate(category = case_when(category == "Oblong" ~ "Elongated",
                              category == "Hole" ~ "Passage",
                              TRUE ~ category)) %>%
  mutate(category = fct_relevel(category, c("Passage", "Elongated", "Two or three dimensional")))

table(cats$category, cats$case)

# Published figure is generated next

cats %>%
  mutate(case = case_when(case == "prl" ~ "PRL",
                          case == "tra" ~ "TRA",
                          TRUE ~ "other")) %>%
  ggplot(data = ., aes(x = category, fill = case)) +
  geom_bar(position="dodge") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)) + 
  theme(legend.text = element_text(size=9),
        legend.title = element_text(size=9)) +
  ylab("Occurrences") +
  xlab("Landmark feature") +
  labs(fill = "Case") +
  theme(axis.title.x = element_text(vjust=-0.5))

ggsave("plots/landmark_plot.png", width = 6, height = 4)

# Chi-squared test

cats_table <- table(cats$case, cats$category)
chisq.test(cats_table)

# Next we create a table that contain the motion type classifications
# for different verbs

#verb_classes <- read_sheet("https://docs.google.com/spreadsheets/d/1xONRBpCqLzHfgMEweNcUhR6xvoWR5n1gw9fd_xuFULI/edit#gid=0", "motion_classes")
#write_delim(verb_classes, "data/verb_classes.tsv", delim = "\t")
verb_classes <- read_delim("data/verb_classes.tsv", delim = "\t")


left_join(komi %>% 
            rename(lemma = motion_predicate_lemma), 
          verb_classes %>%
            mutate(class = str_extract(Alternative_class, "(suunnaton 
toiminta|suunnaton liike|suuntainen liike|suuntainen toiminta)"))) %>%
  filter(! is.na(class)) %>%
  mutate(class = case_when(class == "suuntainen liike" ~ "Non-directed motion",
                           class == "suunnaton toiminta" ~ "Non-directed action",
                           class == "suunnaton liike" ~ "Directed motion",
                           class == "suuntainen toiminta" ~ "Directed action",
                           TRUE ~ "unknown")) %>%
  mutate(case = case_when(case == "prl" ~ "PRL",
                          case == "tra" ~ "TRA",
                          TRUE ~ "other")) %>%
  ggplot(data = ., aes(x = class, fill = case)) +
  geom_bar(position="fill") +
  facet_grid(pos ~ .) + 
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)) + 
  theme(legend.text = element_text(size=9),
        legend.title = element_text(size=9)) +
  ylab("Percentage") +
  xlab("") +
  labs(fill = "Case") +
  theme(axis.title.x = element_text(vjust=-0.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("plots/motion_plot.png", width = 8, height = 4)

# Next we process the data for statistical tests

motion <- left_join(komi %>% 
                      rename(lemma = motion_predicate_lemma), 
                    verb_classes %>%
                      mutate(class = str_extract(Alternative_class, "(suunnaton toiminta|suunnaton liike|suuntainen liike|suuntainen toiminta)"))) %>%
  filter(! is.na(class)) %>%
  mutate(class = case_when(class == "suuntainen liike" ~ "Directless motion",
                           class == "suunnaton toiminta" ~ "Directless action",
                           class == "suunnaton liike" ~ "Directed motion",
                           class == "suuntainen toiminta" ~ "Directed action",
                           TRUE ~ "unknown"))

# Both the distribution within nouns and adpositions is 
# tested separately

motion_noun <- motion %>% filter(pos == "NOUN")
motion_noun_table <- table(motion_noun$case, motion_noun$class)
chisq.test(motion_noun_table, correct = FALSE)

# The exact effects here need more investigation. There is some
# statistical significance, but the pattern is not easy to understand.
# The adpositions certainly show some effects when compared to
# the nouns. 

motion_adp <- motion %>% filter(pos == "ADP")
motion_adp_table <- table(motion_adp$case, motion_adp$class)
chisq.test(motion_adp_table, correct = FALSE)


