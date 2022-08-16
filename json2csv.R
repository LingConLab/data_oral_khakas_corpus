library(tidyverse)
files <- list.files("data", pattern = "json")

map_dfr(files, function(json){
js <- jsonlite::read_json(str_c("data/", json))

map_dfr(seq_along(js$sentences), function(i){
  js$sentences[[i]]$words %>% 
    map("wf") %>% 
    modify_if(is.null, ~ NA) %>% 
    unlist() ->
    word_forms
  
  js$sentences[[i]]$words %>% 
    map("ana") %>% 
    map(1) %>% 
    map("gloss") %>% 
    modify_if(is.null, ~ NA) %>% 
    unlist() ->
    gloss

  js$sentences[[i]]$words %>% 
    map("ana") %>% 
    map(1) %>% 
    map("parts") %>% 
    modify_if(is.null, ~ NA) %>% 
    unlist() ->
    morphonology
  
  js$sentences[[i]]$words %>% 
    map("ana") %>% 
    map(1) %>% 
    map("parts") %>% 
    modify_if(is.null, ~ NA) %>% 
    unlist() ->
    morphonology
  
  tibble(filename = js$meta$filename,
         speaker = js$sentences[[i]]$meta$speaker,
         recorded = js$meta$year,
         lang = js$sentences[[i]]$lang,
         text = js$sentences[[i]]$text,
         word_forms,
         morphonology,
         gloss,
         language = "khak1248",
         dataset_creator  = "Vera Maltseva",
         dataset_provider = "George Moroz") 
})
}) ->
  result

result %>% 
  distinct(filename, recorded, lang, text) %>% 
  group_by(filename, recorded, lang) %>%
  mutate(sentence_id = 1:n()) %>% 
  pivot_wider(names_from = lang, values_from = text) %>% 
  rename(text = `0`,
         translation = `1`) ->
  translation_pairs

result %>% 
  filter(lang == 0) %>% 
  left_join(translation_pairs) %>%
  select(filename, speaker, recorded, sentence_id, text, translation, word_forms, morphonology, gloss, language, dataset_creator, dataset_provider, sentence_id, translation)  %>% 
  write_csv("data_oral_khakas_corpus.csv")
