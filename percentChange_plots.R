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

# data upload
data <- read.csv("C:/Documents/Pyramid_Lake/RCreations/RProjects/eem_analyses/eem_analyses_23.csv")

#### PLOTS ----
## No color in these plots for thesis
# subset to greater than 1,095 masl-point at which lake turns into totally littoral
data <- subset(data, high_elev >= 1085)

# core spawning habitat area vs. lake elevation (no percent!)
spawnHab_elev <- ggplot(data, aes(x= high_elev, y= area_lit)) + 
  geom_point(shape=21, color="gray27", fill="gray27", size=2)+
  geom_line(color="gray34", linewidth = 0.9) +
  scale_x_reverse(breaks = seq(1205, 1085, by = -5)) +
  labs(x= "Lake Elevation (masl)", y= bquote("Core Spawning Habitat "(m^3))) +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12, angle = 45, vjust = 0.5 ),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
spawnHab_elev

# export
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "spawnHab_elev.png", units = "in", width = 8, height = 6, res=600)
spawnHab_elev
dev.off()

# Change in the percent of littoral area compared to area of the total lake with decline
# not sure yet if I want to turn this into volume or something
perc_of_full_noColor <- ggplot(data, aes(x=high_elev, y= specLit_curTotLake_perc)) + 
  geom_point(shape=21, color="gray27", fill="gray27", size=2)+
  geom_line(color="gray34", linewidth = 0.9) +
  scale_x_reverse(breaks = seq(1205, 1060, by = -5)) +
  scale_y_continuous(breaks = seq(100, 0, by = -10)) +
  labs(x= "Lake Elevation (masl)", y= "% Core Habitat of Total Lake Area") +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=14, angle = 45, vjust = 0.5 ),
        axis.text.y=element_text(size=14),
        axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
perc_of_full_noColor

# export
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "per_of_full_noColor.png", units = "in", width = 8, height = 6, res=600)
perc_of_full_noColor
dev.off()

################################################################################
##### STACK #####
# make core spawning habitat area vs. lake elevation with no color
# or x axis for the stack

# make text smaller
spawnHab_elev <- ggplot(data, aes(x= high_elev, y= area_lit)) + 
  geom_point(shape=21, color="gray27", fill="gray27", size=1.5)+
  geom_line(color="gray34", linewidth = 0.5) +
  scale_x_reverse(breaks = seq(1205, 1085, by = -5)) +
  labs(x= "Lake Elevation (masl)", y= bquote("Core Spawning Habitat "(m^3))) +
  theme(axis.text.x = element_blank(),
        axis.text.y=element_text(size=9, angle = 90, hjust = 0.5 ),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
spawnHab_elev

perc_of_full_noColor <- ggplot(data, aes(x=high_elev, y= specLit_curTotLake_perc)) + 
  geom_point(shape=21, color="gray27", fill="gray27", size=1.5)+
  geom_line(color="gray34", linewidth = 0.5) +
  scale_x_reverse(breaks = seq(1205, 1060, by = -5)) +
  scale_y_continuous(breaks = seq(100, 0, by = -10)) +
  labs(x= "Lake Elevation (masl)", y= "Core Spawning Habitat/Total Lake Area (%)") +
  theme(axis.text.x = element_text(size=9, angle = 45, vjust = 0.5 ),
        axis.text.y=element_text(size=9),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
perc_of_full_noColor

setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "eem_stacked_noColor.png", units = "in", width = 7.5, height = 8, res=600)
grid.newpage()
grid.draw(rbind(ggplotGrob(spawnHab_elev), ggplotGrob(perc_of_full_noColor), size = "last"))
dev.off()



# Change in percent littoral area from historic, 100 y axis --SHOWS THE SAME TREND AS 
# CORE HABITAT vs. ELEVATION
from_historic_noColor <- ggplot(data, aes(x=high_elev, y= specLit_histLit_perc)) + 
  geom_point(shape=21, color="gray27", fill="gray27", size=2)+
  geom_line(color="gray34", linewidth = 0.9) +
  scale_x_reverse(breaks = seq(1205, 1085, by = -5)) +
  scale_y_continuous(breaks = seq(200, 0, by = -10)) +
  labs(x= "Lake Elevation (masl)", y= "% of Historic Core Spawning Habitat") +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12, angle = 45, vjust = 0.5 ),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
from_historic_noColor


#### PLOTS WITH COLOR FOR PRESENTATIONS

# Change in percent littoral area from historic, 100 y axis
from_historic_perc_100 <- ggplot(data, aes(x=high_elev, y= specLit_histLit_perc)) + 
  geom_point(shape=21, color="black", fill="black", size=2)+
  geom_line(color="black", size = 0.9) +
  geom_hline (yintercept = 100, 
              colour="slateblue4", 
              size=1.2,
              alpha = 0.8) +
  geom_vline (xintercept = 1160, 
              color = "grey", 
              alpha = 0.5,
              linetype = "solid",
              size = 1.0) +
  geom_vline (xintercept = 1157, 
              color = "seagreen", 
              alpha = 0.5,
              linetype = "solid",
              size = 1.0) +
  scale_x_reverse(breaks = seq(1205, 1060, by = -5)) +
  scale_y_continuous(breaks = seq(200, 0, by = -10)) +
  labs(x= "Lake Elevation (masl)", y= "% of Historic Core Habitat Area") +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12, angle = 45, vjust = 0.5 ),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
from_historic_perc_100

# Change in the percent of littoral area compared to area of the total lake with decline
# not sure yet if I want to turn this into volume or something
perc_of_full <- ggplot(data, aes(x=high_elev, y= specLit_curTotLake_perc)) + 
  geom_point(shape=21, color="black", fill="black", size=2)+
  geom_line(color="black", size = 0.9) +
  geom_vline (xintercept = 1160, 
              color = "grey", 
              alpha = 0.5,
              linetype = "solid",
              size = 1.0) +
  geom_vline (xintercept = 1157, 
              color = "seagreen", 
              alpha = 0.5,
              linetype = "solid",
              size = 1.0) +
  scale_x_reverse(breaks = seq(1205, 1060, by = -5)) +
  scale_y_continuous(breaks = seq(100, 0, by = -10)) +
  labs(x= "Lake Elevation (masl)", y= "% Core Habitat of Total Lake Area") +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=14, angle = 45, vjust = 0.5 ),
        axis.text.y=element_text(size=14),
        axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
perc_of_full
