#### HELPER FUNCTIONS ##########################################################

library(ggmap)
library(sf)
library(spacyr) # Need to download miniconda and spacy_install()
# Run spacy_download_langmodel("fr") before lemmatizing French articles
library(LexisNexisTools)
library(SentimentAnalysis)
library(tidytext)
library(tidyverse)
library(upgo)