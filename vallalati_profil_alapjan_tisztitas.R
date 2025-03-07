library(dplyr)
library(ggplot2)
library(tidytext)
library(stringr)
library(tidyr)
#tweets els�dleges tulajdons�gok----
glimpse(tweets)

ptweets <- tweets %>%
  select(user_id, created_at, screen_name, text, is_retweet, favorite_count, retweet_count) %>%
  arrange(desc(created_at))

head(ptweets)


#ptweets sima eloszl�sok id�ben----

ptweets %>%
  filter(screen_name=="nikolamotor") %>%
  ggplot(aes(x = created_at, fill = screen_name)) +
  theme_light()+
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  facet_wrap(~screen_name)+
  labs(y = "Tweetek sz�ma",
       x = "Tweetek l�trehoz�s�nak d�tuma")

#tokeniz�l�s �s stop szavak sz�r�se ID n�lk�l----
  regex <- "&amp;|&lt;|&gt;"
tidy_ptweets <- ptweets %>%
  filter(is_retweet==FALSE) %>%
  select(created_at, screen_name, text) %>%
  mutate(text = str_remove_all(text, regex)) %>%
  unnest_tokens(word, text, token = "tweets", strip_url = TRUE) %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
        str_detect(word, "[a-z]"),
        !str_detect(word, "^@|^#"))
        #str_detect(screen_name, "BMWi|elonmusk|nikolamotor|Tesla|VolvoGroup"))


#sz�gyakoris�gok
frequency <- tidy_ptweets %>% 
  group_by(screen_name) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_ptweets %>% 
              group_by(screen_name) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

#sz�gyakoris�gok bar charton----
frequency %>%
  top_n(15) %>%
  ggplot(aes(x=reorder(word, freq), y=freq, fill=screen_name))+
  theme_light()+
  geom_col(show.legend = FALSE)+
  facet_wrap(~screen_name, scales = "free")+
  labs(x=NULL, y="gyakoris�g", title = "Top 15 leggyakrabban haszn�lt kifejez�s")+
  coord_flip()


#sz�tdarabol�s oszlopkra screen_name szerint
frequency_jitter <- frequency %>% 
  select(screen_name, word, freq) %>% 
  spread(screen_name, freq) %>% 
  arrange(Tesla, VolvoTrucksUK, elonmusk, DaimlerTruckBus, BMWi,BMW,
          nikolamotor, AudiOfficial, NissanUSA, NissanElectric, VolvoGroup, MobilityDaimler,
          Toyota, ToyotaMotorCorp, Porsche)

#sz�gyakoris�gok �sszehasonl�t�sa pontokkal (modul�ris)
library(scales)

ggplot(frequency_jitter, aes(BMW, BMWi)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")


#Tesla-Audi sz�haszn�lat �sszehasonl�t�s 2019----
Tesla_AudiOfficial <- tidy_ptweets %>%
  filter(screen_name=="Tesla" | screen_name=="AudiOfficial") %>%
  filter(created_at >= as.POSIXct("2020-01-01 00:00:00"),
         created_at < as.POSIXct("2021-01-01 00:00:00"))

#adattiszt�t�s �s logr�ta k�sz�t�s
wr_Tesla_AudiOfficial <- Tesla_AudiOfficial %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, screen_name) %>%
  group_by(word) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  spread(screen_name, n, fill = 0) %>%
  mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
  mutate(logratio = log(Tesla / AudiOfficial)) %>%
  arrange(desc(logratio))

#egyenl�en haszn�lt szavak
wr_Tesla_AudiOfficial %>% 
  arrange(abs(logratio))

#top15 jellegzetes szo
wr_Tesla_AudiOfficial %>%
  group_by(logratio < 0) %>%
  top_n(8, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  ylab("es�lyh�nyados") +
  scale_fill_discrete(name = "", labels = c("Tesla", "AudiOfficial"))



#Tesla-NissanElectric sz�haszn�lat �sszehasonl�t�s 2019----
Tesla_NissanElectric <- tidy_ptweets %>%
  filter(screen_name=="Tesla" | screen_name=="NissanElectric") %>%
  filter(created_at >= as.POSIXct("2020-01-01 00:00:00"),
         created_at < as.POSIXct("2021-01-01 00:00:00"))

#adattiszt�t�s �s logr�ta k�sz�t�s
wr_Tesla_NissanElectric <- Tesla_NissanElectric %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, screen_name) %>%
  group_by(word) %>%
  filter(sum(n) >= 6) %>%
  ungroup() %>%
  spread(screen_name, n, fill = 0) %>%
  mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
  mutate(logratio = log(Tesla / NissanElectric)) %>%
  arrange(desc(logratio))

#egyenl�en haszn�lt szavak
wr_Tesla_NissanElectric %>% 
  arrange(abs(logratio))

#top15 jellegzetes szo
wr_Tesla_NissanElectric %>%
  group_by(logratio < 0) %>%
  top_n(8, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  ylab("es�lyh�nyados") +
  scale_fill_discrete(name = "", labels = c("Tesla", "NissanElectric"))













#sz�haszn�lat v�ltoz�sa 2016-2021----
library(lubridate)
library(purrr)

#szavak el�fordul�sa havonta, s_n szerint csoportos�tva (25-n�l t�bb)
words_by_time <- tidy_ptweets %>%
  filter(created_at >= as.POSIXct("2016-01-01 00:00:00"),
         created_at < as.POSIXct("2021-02-28 00:00:00")) %>%
  filter(!str_detect(word, "^@")) %>%
  mutate(time_floor = floor_date(created_at, unit = "1 month")) %>%
  count(time_floor, screen_name, word) %>%
  group_by(screen_name, time_floor) %>%
  mutate(time_total = sum(n)) %>%
  group_by(screen_name, word) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) 
#  filter(word_total > 20)

#dataframe-ben dataframe szavak �s s_n szerint
nested_data <- words_by_time %>%
  nest(-word, -screen_name)


#modelkapcsol�s glm
nested_models <- nested_data %>%
  mutate(models = map(data, ~ glm(cbind(count, time_total)
                                  ~ time_floor, .,
                                  family = "binomial")))

#id�beli v�ltoz�sok kiszed�se, �s �sszehasonlit�s
library(broom)

slopes <- nested_models %>%
  mutate(models = map(models, tidy)) %>%
  unnest(cols = c(models)) %>%
  filter(term == "time_floor") %>%
  mutate(adjusted.p.value = p.adjust(p.value))

#legink�bb megv�ltozott gyakoris�g� szavak (jelenleg nem sz�ks�ges)
top_slopes <- slopes %>% 
  filter(adjusted.p.value < 0.05)

#vonaldiagram (modul�ris)
library(viridis)

words_by_time %>%
  inner_join(slopes, by = c("word", "screen_name")) %>%
  filter(str_detect(word, "^battery$|^co2$"), str_detect(screen_name, "Tesla|nikolamotor|BMWi")) %>%
  ggplot(aes(time_floor, count/time_total, color = screen_name)) +
  geom_line(size = 1.3) +
  geom_smooth(method = "loess", se = FALSE, color="darkgreen")+
  facet_wrap(~word)+
  labs(x = "Id�vonal", y = "sz�gyakoris�g")+
  scale_color_viridis(discrete = TRUE, option = "C")+
  theme(legend.title = element_blank())


ggsave(
  filename = "kwh100_co2_BMWi_tweetek_intime.png",
  plot = last_plot(),
  device = "png",
  path = "D:/Egyetem/Szakdolgozat/kepek/koddal_mentett",
  scale = 1,
  width = 30.99,
  height = 17.43,
  units = c("cm"),
  dpi = 300,
  limitsize = TRUE)

#n-gram----

#bigram2----

tweet_bigrams <- ptweets %>%
  select(created_at, screen_name, text) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
  

bigrams_separated <- tweet_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
  
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)%>%
  filter(!str_detect(word1, "http|t.co"), !str_detect(word2, "http|t.co"),
         str_detect(screen_name, "Tesla"))

bigram_counts <- bigrams_filtered %>% 
  group_by(screen_name)%>%
  count(word1, word2, sort = TRUE)

bigram_united <- bigram_counts%>%
  unite(bigram, word1, word2, sep = " ")

bigram_freq <- bigram_united%>%
  left_join(bigram_united %>% 
              group_by(screen_name) %>% 
              summarise(total = n())) %>%
  mutate(bi_freq = n/total)
  
  
bigram_freq %>%
  top_n(10) %>%
  ggplot(aes(x=reorder(bigram, bi_freq), y=bi_freq, fill=screen_name))+
  theme_light()+
  geom_col(show.legend = FALSE)+
  facet_wrap(~screen_name, scales = "free")+
  labs(x=NULL, y="gyakoris�g")+
  coord_flip()
  
  

  
  
#trigram----
tweet_trigrams <- ptweets %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3)

trigrams_separated <- tweet_trigrams %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")



trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)%>%
  filter(!word3 %in% stop_words$word)%>%
  filter(str_detect(word1, "[a-z]"))%>%
  filter(!str_detect(word1, "http"), !str_detect(word1, "t.co"), 
         !str_detect(word2, "http"), !str_detect(word2, "t.co"),
         !str_detect(word3, "http"), !str_detect(word3, "t.co"))

trigram_counts <- trigrams_filtered %>% 
  group_by(screen_name)%>%
  count(word1, word2, word3, sort = TRUE)

trigram_united <- trigram_counts%>%
  unite(trigram, word1, word2, word3, sep = " ")

trigram_freq <- trigram_united%>%
  left_join(trigram_united %>% 
              group_by(screen_name) %>% 
              summarise(total = n())) %>%
  mutate(tri_freq = n/total)


trigram_freq %>%
  top_n(10) %>%
  ggplot(aes(x=reorder(trigram, tri_freq), y=tri_freq, fill=screen_name, sort(tri_freq)))+
  theme_light()+
  geom_col(show.legend = FALSE)+
  facet_wrap(~screen_name, scales = "free")+
  labs(x=NULL, y="gyakoris�g")+
  coord_flip()





