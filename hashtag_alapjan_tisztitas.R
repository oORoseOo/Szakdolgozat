library(dplyr)
library(ggplot2)
library(tidytext)
library(stringr)
library(tidyr)



#autonomous----

#stop szavak és tokenizálás
tidy_autonomtweets <- autonom_tweets %>%
  select(created_at, is_retweet, text) %>%
  unnest_tokens(word, text, token = "tweets", strip_url = TRUE) %>%
  mutate(is_word = "Word")%>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"),
         !str_detect(word, "^@|^#autonomous$|amp"),
         #str_detect(is_retweet, "FALSE"),
         !str_detect(word, "^#"))

#szógyakoriságok
tidy_autonomtweets_freq <- tidy_autonomtweets %>% 
  group_by(is_word) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_autonomtweets %>% 
              group_by(is_word) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

#szógyakoriságok bar charton
tidy_autonomtweets_freq %>%
  top_n(20) %>%
  ggplot(aes(x=reorder(word, freq), y=freq, fill=is_word))+
  theme_light()+
  geom_col(show.legend = FALSE)+
  facet_wrap(~is_word, scales = "free")+
  labs(x="Kifejezések", y="Szavak gyakorisága",
       title = "Autonomous hashtag # használata")+
  coord_flip() 

ggsave(
  filename = "bi_autonomous_barchart.png",
  plot = last_plot(),
  device = "png",
  path = "D:/Egyetem/Szakdolgozat/kepek/koddal_mentett",
  scale = 1,
  width = 15,
  height = 17.43,
  units = c("cm"),
  dpi = 300,
  limitsize = TRUE)

library(ggwordcloud)
tidy_autonomtweets_freq %>% 
  top_n(40) %>%
  ggplot(rank, mapping = aes(label=word, size=freq, color=freq)) +
  geom_text_wordcloud(eccentricity = 1) +
  scale_size_area(max_size = 10) +
  labs(title="Szógyakoriság felhõje")
  theme_minimal() +
  scale_color_gradient(low="#fb6a4a", high="#67000d") 
  
  
#bigrammmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
  
  autonom_bigrams <- autonom_tweets %>%
    select(created_at, is_retweet, text) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2)
  
  
  autonom_bigrams_separated <- autonom_bigrams %>%
    separate(bigram, c("word1", "word2"), sep = " ")
  
  autonom_bigrams_filtered <- autonom_bigrams_separated %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)%>%
    mutate(is_word = "Word")%>%
    filter(!str_detect(word1, "http|t.co"), !str_detect(word2, "http|t.co"))
  
  autonom_bigram_counts <- autonom_bigrams_filtered %>% 
    group_by(is_word)%>%
    count(word1, word2, sort = TRUE)
  
  autonom_bigram_united <- autonom_bigram_counts%>%
    unite(bigram, word1, word2, sep = " ")
  
  autonom_bigram_freq <- autonom_bigram_united%>%
    left_join(autonom_bigram_united %>% 
                group_by(is_word) %>% 
                summarise(total = n())) %>%
    mutate(bi_freq = n/total)
  
  
  autonom_bigram_freq %>%
    top_n(10) %>%
    ggplot(aes(x=reorder(bigram, bi_freq), y=bi_freq, fill=is_word))+
    theme_light()+
    geom_col(show.legend = FALSE)+
    facet_wrap(~is_word, scales = "free")+
    labs(x=NULL, y="gyakoriság")+
    coord_flip()
  
  
  
#battery----
#stop szavak és tokenizálás
tidy_batterytweets <- battery_tweets %>%
  select(created_at, is_retweet, text) %>%
  unnest_tokens(word, text, token = "tweets", strip_url = TRUE) %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"),
         str_detect(is_retweet, "FALSE"),
         !str_detect(word, "^@|^$|^amp$|battery"))
  
#szógyakoriságok
tidy_batterytweets_freq <- tidy_batterytweets %>% 
  group_by(is_retweet) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_batterytweets %>% 
              group_by(is_retweet) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

#szógyakoriságok bar charton
tidy_batterytweets_freq %>%
  top_n(20) %>%
  ggplot(aes(x=reorder(word, freq), y=freq, fill=is_retweet))+
  theme_light()+
  geom_col(show.legend = FALSE)+
  facet_wrap(~is_retweet, scales = "free")+
  labs(x="Kifejezések", y="Szavak gyakorisága", title = "#battery tweetek leggyakoribb szavai")+
  coord_flip()

ggsave(
  filename = "battery_barchart2.png",
  plot = last_plot(),
  device = "png",
  path = "D:/Egyetem/Szakdolgozat/kepek/koddal_mentett",
  scale = 1,
  width = 15,
  height = 17.43,
  units = c("cm"),
  dpi = 300,
  limitsize = TRUE)

#charging----
#stop szavak és tokenizálás
tidy_chargingtweets <- charging_tweets %>%
  select(created_at, is_retweet, text) %>%
  unnest_tokens(word, text, token = "tweets", strip_url = TRUE) %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"),
         str_detect(word, "^#"),
         str_detect(is_retweet, "FALSE"),
         !str_detect(word, "^@|^$|^amp$|^#charging$"))

#szógyakoriságok
tidy_chargingtweets_freq <- tidy_chargingtweets %>% 
  group_by(is_retweet) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_chargingtweets %>% 
              group_by(is_retweet) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

#szógyakoriságok bar charton
tidy_chargingtweets_freq %>%
  top_n(20) %>%
  ggplot(aes(x=reorder(word, freq), y=freq, fill=is_retweet))+
  theme_light()+
  geom_col(show.legend = FALSE)+
  facet_wrap(~is_retweet, scales = "free")+
  labs(x="Kifejezések", y="Szavak gyakorisága", title = "#charging top 15 hashtagje")+
  coord_flip()

ggsave(
  filename = "charging_barchart2.png",
  plot = last_plot(),
  device = "png",
  path = "D:/Egyetem/Szakdolgozat/kepek/koddal_mentett",
  scale = 1,
  width = 15,
  height = 10.43,
  units = c("cm"),
  dpi = 300,
  limitsize = TRUE)

#climatechange----
#stop szavak és tokenizálás
tidy_climatechangetweets <- climatechange_tweets %>%
  select(created_at, is_retweet, text) %>%
  unnest_tokens(word, text, token = "tweets", strip_url = TRUE) %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"))

#szógyakoriságok
tidy_climatechangetweets_freq <- tidy_climatechangetweets %>% 
  group_by(is_retweet) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_climatechangetweets %>% 
              group_by(is_retweet) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

#szógyakoriságok bar charton
tidy_climatechangetweets_freq %>%
  top_n(20) %>%
  ggplot(aes(x=reorder(word, freq), y=freq, fill=is_retweet))+
  theme_light()+
  geom_col(show.legend = FALSE)+
  facet_wrap(~is_retweet, scales = "free")+
  labs(x="Kifejezések", y="Szavak gyakorisága")+
  coord_flip()

ggsave(
  filename = "climatechange_barchart.png",
  plot = last_plot(),
  device = "png",
  path = "D:/Egyetem/Szakdolgozat/kepek/koddal_mentett",
  scale = 1,
  width = 30.99,
  height = 17.43,
  units = c("cm"),
  dpi = 300,
  limitsize = TRUE)



#ev_tweets----
#stop szavak és tokenizálás
tidy_evtweets <- ev_tweets %>%
  select(created_at, is_retweet, text) %>%
  unnest_tokens(word, text, token = "tweets", strip_url = TRUE) %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"),
         str_detect(is_retweet, "FALSE"),
         !str_detect(word, "^@|^#|^$|^amp$"))

#szógyakoriságok
tidy_evtweets_freq <- tidy_evtweets %>% 
  group_by(is_retweet) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_evtweets %>% 
              group_by(is_retweet) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

#szógyakoriságok bar charton
tidy_evtweets_freq %>%
  top_n(20) %>%
  ggplot(aes(x=reorder(word, freq), y=freq, fill=is_retweet))+
  theme_light()+
  geom_col(show.legend = FALSE)+
  facet_wrap(~is_retweet, scales = "free")+
  labs(x="Kifejezések", y="Szavak gyakorisága", title="'#EV' tweetek leggyakoribb szavai ")+
  coord_flip()


ggsave(
  filename = "ev_barchart2.png",
  plot = last_plot(),
  device = "png",
  path = "D:/Egyetem/Szakdolgozat/kepek/koddal_mentett",
  scale = 1,
  width = 14.99,
  height = 17.43,
  units = c("cm"),
  dpi = 300,
  limitsize = TRUE)


#5g----
#stop szavak és tokenizálás
tidy_fivetweets <- fiveg_tweets %>%
  select(created_at, is_retweet, text, lang) %>%
  mutate(is_word = "Word")%>%
  unnest_tokens(word, text, token = "tweets", strip_url = TRUE) %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"),
         !str_detect(word, "^@|5g|amp"),
         #str_detect(word, "^#"),
         str_detect(lang, "en"))

#szógyakoriságok
tidy_fivetweets_freq <- tidy_fivetweets %>% 
  group_by(is_word) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_fivetweets %>% 
              group_by(is_word) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

#szógyakoriságok bar charton
tidy_fivetweets_freq %>%
  top_n(20) %>%
  ggplot(aes(x=reorder(word, freq), y=freq, fill=is_word))+
  theme_light()+
  geom_col(show.legend = FALSE)+
  facet_wrap(~is_word, scales = "free")+
  labs(x="Kifejezések", y="Szavak gyakorisága", title="Az 5g # használat")+
  coord_flip()

ggsave(
  filename = "5g_barchart.png",
  plot = last_plot(),
  device = "png",
  path = "D:/Egyetem/Szakdolgozat/kepek/koddal_mentett",
  scale = 1,
  width = 15,
  height = 17.43,
  units = c("cm"),
  dpi = 300,
  limitsize = TRUE)


#lithium----
#stop szavak és tokenizálás
tidy_lithiumtweets <- lithium_tweets %>%
  select(created_at, is_retweet, text) %>%
  unnest_tokens(word, text, token = "tweets", strip_url = TRUE) %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"),
         str_detect(is_retweet, "FALSE"),
         !str_detect(word, "^@|^#|^$|^amp$|lithium"))

#szógyakoriságok
tidy_lithiumtweets_freq <- tidy_lithiumtweets %>% 
  group_by(is_retweet) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_lithiumtweets %>% 
              group_by(is_retweet) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

#szógyakoriságok bar charton
tidy_lithiumtweets_freq %>%
  top_n(20) %>%
  ggplot(aes(x=reorder(word, freq), y=freq, fill=is_retweet))+
  theme_light()+
  geom_col(show.legend = FALSE)+
  facet_wrap(~is_retweet, scales = "free")+
  labs(x="Kifejezések", y="Szavak gyakorisága", title = "#lithium tweetek leggyakoribb szavai")+
  coord_flip()

ggsave(
  filename = "lithium_barchart2.png",
  plot = last_plot(),
  device = "png",
  path = "D:/Egyetem/Szakdolgozat/kepek/koddal_mentett",
  scale = 1,
  width = 14.99,
  height = 17.43,
  units = c("cm"),
  dpi = 300,
  limitsize = TRUE)



#machinelearning----
#stop szavak és tokenizálás
tidy_machinelearningtweets <- machinelearning_tweets %>%
  select(created_at, is_retweet, text) %>%
  unnest_tokens(word, text, token = "tweets", strip_url = TRUE) %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"))

#szógyakoriságok
tidy_machinelearningtweets_freq <- tidy_machinelearningtweets %>% 
  group_by(is_retweet) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_machinelearningtweets %>% 
              group_by(is_retweet) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

#szógyakoriságok bar charton
tidy_machinelearningtweets_freq %>%
  top_n(20) %>%
  ggplot(aes(x=reorder(word, freq), y=freq, fill=is_retweet))+
  theme_light()+
  geom_col(show.legend = FALSE)+
  facet_wrap(~is_retweet, scales = "free")+
  labs(x="Kifejezések", y="Szavak gyakorisága")+
  coord_flip()


ggsave(
  filename = "machinelearning_barchart.png",
  plot = last_plot(),
  device = "png",
  path = "D:/Egyetem/Szakdolgozat/kepek/koddal_mentett",
  scale = 1,
  width = 30.99,
  height = 17.43,
  units = c("cm"),
  dpi = 300,
  limitsize = TRUE)


#zeroemission----
#stop szavak és tokenizálás
tidy_zeroemissiontweets <- zeroemissions_tweets %>%
  select(created_at, is_retweet, text) %>%
  unnest_tokens(word, text, token = "tweets", strip_url = TRUE) %>%
  mutate(is_word = "Word")%>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"))

#szógyakoriságok
tidy_zeroemissiontweets_freq <- tidy_zeroemissiontweets %>% 
  group_by(is_word) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_zeroemissiontweets %>% 
              group_by(is_word) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

#szógyakoriságok bar charton
tidy_zeroemissiontweets_freq %>%
  top_n(20) %>%
  ggplot(aes(x=reorder(word, freq), y=freq, fill=is_word))+
  theme_light()+
  geom_col(show.legend = FALSE)+
  facet_wrap(~is_word, scales = "free")+
  labs(x="Kifejezések", y="Szavak gyakorisága")+
  coord_flip()


ggsave(
  filename = "zeroemissions_barchart.png",
  plot = last_plot(),
  device = "png",
  path = "D:/Egyetem/Szakdolgozat/kepek/koddal_mentett",
  scale = 1,
  width = 15,
  height = 17.43,
  units = c("cm"),
  dpi = 300,
  limitsize = TRUE)


#electricvehicles----
#stop szavak és tokenizálás
tidy_electricvehiclestweets <- electricvehicles_tweets %>%
  select(created_at, is_retweet, text) %>%
  unnest_tokens(word, text, token = "tweets", strip_url = TRUE) %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"),
         str_detect(is_retweet, "FALSE"),
         !str_detect(word, "^@|^#|^$|^amp$"))

#szógyakoriságok
tidy_electricvehiclestweets_freq <- tidy_electricvehiclestweets %>% 
  group_by(is_retweet) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_electricvehiclestweets %>% 
              group_by(is_retweet) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

#szógyakoriságok bar charton
tidy_electricvehiclestweets_freq %>%
  top_n(20) %>%
  ggplot(aes(x=reorder(word, freq), y=freq, fill=is_retweet))+
  theme_light()+
  geom_col(show.legend = FALSE)+
  facet_wrap(~is_retweet, scales = "free")+
  labs(x="Kifejezések", y="Szavak gyakorisága", title="'#electricvehicle' tweetek leggyakoribb szavai ")+
  coord_flip()

ggsave(
  filename = "electricvehicle_barchart2.png",
  plot = last_plot(),
  device = "png",
  path = "D:/Egyetem/Szakdolgozat/kepek/koddal_mentett",
  scale = 1,
  width = 14.99,
  height = 17.43,
  units = c("cm"),
  dpi = 300,
  limitsize = TRUE)
#autonomousvehicles----

#stop szavak és tokenizálás
tidy_autonomousvehiclestweets <- autonomousvehicles_tweets %>%
  select(created_at, is_retweet, text) %>%
  unnest_tokens(word, text, token = "tweets", strip_url = TRUE) %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"),
         !str_detect(word, "^@|^#"))

#szógyakoriságok
tidy_autonomousvehiclestweets_freq <- tidy_autonomousvehiclestweets %>% 
  group_by(is_retweet) %>% 
  count(word, sort = freq) %>% 
  left_join(tidy_autonomousvehiclestweets %>% 
              group_by(is_retweet) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

#szógyakoriságok bar charton
tidy_autonomousvehiclestweets_freq %>%
  top_n(15) %>%
  ggplot(aes(x=reorder(word, freq), y=freq, fill=is_retweet))+
  theme_light()+
  geom_col(show.legend = FALSE)+
  facet_wrap(~is_retweet, scales = "free")+
  labs(x="Kifejezések", y="Szavak gyakorisága")+
  coord_flip()

ggsave(
  filename = "autonomousvehicle_barchart.png",
  plot = last_plot(),
  device = "png",
  path = "D:/Egyetem/Szakdolgozat/kepek/koddal_mentett",
  scale = 1,
  width = 30.99,
  height = 17.43,
  units = c("cm"),
  dpi = 300,
  limitsize = TRUE)


#hundreddaysofcode----
#stop szavak és tokenizálás
tidy_aitweets <- ai_tweets %>%
  select(created_at, is_retweet, text) %>%
  unnest_tokens(word, text, token = "tweets", strip_url = TRUE) %>%
  mutate(is_word = "word")%>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         !str_detect(word, "amp"),
         str_detect(word, "[a-z]"))

#szógyakoriságok
tidy_aitweets_freq <- tidy_aitweets %>% 
  group_by(is_word) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_aitweets %>% 
              group_by(is_word) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

#szógyakoriságok bar charton
tidy_aitweets_freq %>%
  top_n(20) %>%
  ggplot(aes(x=reorder(word, freq), y=freq, fill=is_word))+
  theme_light()+
  geom_col(show.legend = FALSE)+
  facet_wrap(~is_word, scales = "free")+
  labs(x="Kifejezések", y="Szavak gyakorisága", title = "Az ai # használat")+
  coord_flip()

ggsave(
  filename = "ai_barchart.png",
  plot = last_plot(),
  device = "png",
  path = "D:/Egyetem/Szakdolgozat/kepek/koddal_mentett",
  scale = 1,
  width = 15,
  height = 17.43,
  units = c("cm"),
  dpi = 300,
  limitsize = TRUE)

