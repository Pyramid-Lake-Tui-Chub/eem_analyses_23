#### INSTALL PACKAGES ----
library(tidyverse)
library(tidyr)
library(plotrix)
library(cowplot)
library(reshape2)
library(RColorBrewer)
library(gridExtra)
library(grid)
library(knitr)
library(dplyr)
library(viridis)
library(hrbrthemes)
theme_set(theme_cowplot())

#### DATA EXPLORATION ----
# set working directory/save data
setwd("C:/Documents/Pyramid_Lake/RCreations/csv_files")
fish <-read.csv(file="SAB_TuiChubFall_2092023.csv")
elev <-read.csv(file="elev_tds_avg.csv")

# data manipulation
fish$Month<-as.factor(fish$Month)

# change names
names(fish)[1] <- c("year")

# match on year, inner join based on fish
fish_elev <- inner_join(fish,
                        elev,
                        by="year")

# initial plot of year and cpue
coef <- 1156

fish_year_elev <- ggplot(fish_elev, aes(x=year))+
  geom_point(aes(y=cpe))+
  geom_point(aes(y=elev_masl-1155), color = "grey23", size = 1)+
  #geom_line((y=cpe), color = "black", size = 1) +
  #geom_line(y=(elev_masl-1156), color = "black", size = 1)+
  geom_smooth(aes(y=cpe), method = loess, color = "grey10", alpha = 0.8)+
  geom_smooth(aes(y=elev_masl-1156), method = loess, color = "grey40", alpha = 0.7) +
  scale_y_continuous(name = "Tui Chub CPUE (fish/hour)", sec.axis = sec_axis(~. +1155, name = "Elevation (masl)")) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs(x="Year")
fish_year_elev


