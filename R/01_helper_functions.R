#### HELPER FUNCTIONS ##########################################################

library(ggmap)
library(sf)
library(spacyr) # Need to download miniconda and spacy_install()
# Run spacy_download_langmodel("fr") before lemmatizing French articles
library(tidytext)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(LexisNexisTools)
library(SentimentAnalysis)
library(upgo)
library(cancensus)

options(cancensus.api_key = "CensusMapper_1e60750cf5d513988f754396257111f4")