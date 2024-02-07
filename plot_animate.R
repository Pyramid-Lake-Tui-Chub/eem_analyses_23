#### INSTALL PACKAGES ----
library(tidyverse)
library(dplyr)
library(gganimate)
library(gifski)
library(av)

#### PERCENT OF HIGH STAND: ANIMATION ----
ofHist_anim <- animate(from_historic_perc_100, duration = 132, fps = 132/132, renderer = gifski_renderer())
