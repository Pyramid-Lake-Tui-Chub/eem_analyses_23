## SCRIPT TO EXPORT PLOTS

# Graph 1
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "eem_litfromhistoric23.png", units = "in", width = 8, height = 6, res=600)
from_historic_perc_100
dev.off()

# Graph 2
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "eem_perc_of_full23.png", units = "in", width = 8, height = 6, res=600)
perc_of_full
dev.off()


