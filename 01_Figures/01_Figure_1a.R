#####################################
### FIGURE 1A MAP OF SAMPLES ########
#####################################
library(ggplot2)
library(maps)
library(mapdata)
library(rworldmap)

dat <- read.csv ("Wild_wolf_parturition_May15_final_2026.csv", header=TRUE)

world <- map_data('world')
p <- ggplot(world, aes(lat, long)) +
    geom_map(map=world, aes(map_id=region), fill="gray95", color="darkgray") +
    coord_quickmap() + theme_classic() 

q <- p +  geom_point(data = dat, 
             aes(x = Long, y = Lat, fill=DOY), 
             alpha = 1, 
             size = 2.5, shape = 21) +
   scale_fill_gradient2(low = "white", mid="yellow", high = "blue", midpoint = 125, na.value = NA) 
q + scale_y_continuous(breaks = round(seq(min(10), max(90), by = 10),0))

ggsave("May15_2206_Figure1.pdf", width=20,height=11)
