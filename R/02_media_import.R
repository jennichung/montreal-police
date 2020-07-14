#### MEDIA IMPORT ##############################################################

source("R/01_helper_functions.R")


# Get file paths ----------------------------------------------------------

files <- list.files(path = "data/media", full.names = TRUE)


# Import files ------------------------------------------------------------

media <- lnt_read(files)

media <- 
  left_join(media@meta, media@articles) %>% 
  mutate(word_count = as.numeric(str_remove(Length, " words"))) %>% 
  dplyr::select(ID, newspaper = Newspaper, date = Date, word_count,
                section = Section, author = Author, edition = Edition,
                headline = Headline, text = Article) %>% 
  filter(word_count > 100) %>% 
  group_by(author) %>% 
  distinct(headline, .keep_all = TRUE) %>% 
  ungroup() %>% 
  # Assign doc_id for processing
  mutate(doc_id = 1:n())


# Lemmatize ---------------------------------------------------------------

# Initialize spaCy
spacy_initialize()

# Prepare articles for word search by removing stop words and lemmatizing
iterations <- ceiling(nrow(media) / 500)
spacy_articles <- vector("list", iterations)

for (i in seq_len(iterations)) {
  spacy_articles[[i]] <- 
    spacy_parse(
      media[((i - 1) * 500 + 1):min((i * 500), nrow(media)),]$text, 
      pos = FALSE, entity = FALSE, tag = FALSE)
  
  spacy_articles[[i]] <-
    spacy_articles[[i]] %>%
    mutate(doc_id = paste0("text", 
                           ((i - 1) * 500) + as.numeric(substr(doc_id, 5, 7)))
           ) %>%
    as_tibble()
}

spacy_articles <- bind_rows(spacy_articles) %>% as_tibble()

save(spacy_articles, file = "spacy_temp.Rdata")

lemmatized_articles <- 
  spacy_articles %>%
  filter(lemma != "-PRON-") %>%
  mutate(lemma = str_replace_all(lemma, "[^a-zA-Z0-9 ]", " ")) %>%
  filter(!lemma %in% filter(stop_words, lexicon == "snowball")$word) %>%
  mutate(lemma = strsplit(as.character(lemma), " ")) %>%
  unnest(lemma) %>%
  filter(!lemma %in% filter(stop_words, lexicon == "snowball")$word) %>%
  group_by(doc_id) %>%
  dplyr::summarise(lemmas = paste(as.character(lemma), collapse = " ")) %>%
  mutate(doc_id = as.numeric(paste(flatten(str_extract_all(
    doc_id,"[[:digit:]]+"))))) %>%
  arrange(doc_id) %>%
  mutate_each(list(tolower)) %>% 
  # Warning message: `mutate_each_()` is deprecated as of dplyr 0.7.0. Please use `across()` instead.
  mutate(lemmas = str_squish(str_replace_all(lemmas, "[^a-zA-Z0-9 ]", " "))
  ) %>%
  mutate(lemmas = gsub('\\b\\w{1,2}\\b','', lemmas))

# Search for police mentions in the cleaned text
lemmatized_articles <-
  lemmatized_articles %>% 
  filter(str_count(lemmas, "police") > 2, str_count(lemmas, "montreal") > 2)

# Trim original media files to only ones that mention police > 2 & montreal > 2
media <- media %>% filter(doc_id %in% lemmatized_articles$doc_id)

# Reassign an ID to allow for future text processing
media <- media %>% mutate(ID = 1:n())

lemmatized_articles <- 
  lemmatized_articles %>% 
  mutate(doc_id = 1:n())


# Clean up ----------------------------------------------------------------

save(media, lemmatized_articles, file = "data/media.Rdata")
rm(files, i, iterations, spacy_articles)
