library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)


states <- map_data("state")

surv_states <- subset(states, region %in% c("illinois", "iowa", "missouri"))

R1 <- data.frame(region = c (1,1,1,1),
                 lat = c(41.272832, 41.272832, 40.655011,40.655011),
                 lon = c (-89.684037, -89.208337,-89.208337,-89.684037))
R2 <- data.frame(region = c(2,2,2,2),
                 lat = c(40.574579, 40.574579, 39.837738, 39.837738),
                 lon = c(-90.657318, -89.682281, -89.682281, -90.657318))
R3 <- data.frame(region = c(3,3,3,3),
                 lat = c(41.242343, 41.242343, 40.401114, 40.401114),
                 lon = c(-91.536937, -90.916078, -90.916078,-91.536937))
R4 <- data.frame(region = c(4,4,4,4,4,4),
                 lat = c(40.400100, 40.400100, 39.808032, 39.808032, 38.819802, 38.819802),
                 lon = c(-91.649563, -91.007922, -91.007922, -90.086517, -90.086517, -91.649563))




Regions <- rbind(R1, R2, R3, R4)


ll_means <- sapply(Regions[3:2], mean)



zoom_map <- get_map(location = ll_means,  maptype = "hybrid", source = "google", zoom = 7)

ggmap(zoom_map) + 
  geom_polygon(data = R1, aes(x = lon, y = lat), alpha = 0.5, color = "yellow") +
  geom_polygon(data = R2, aes(x = lon, y = lat), alpha = 0.5, color = "white") +
  geom_polygon(data = R3, aes(x = lon, y = lat), alpha = 0.5, color = "red") +
  geom_polygon(data = R4, aes(x = lon, y = lat), alpha = 0.5, color = "green") +
  annotate("text", x=mean(R1$lon),y=mean(R1$lat),label="R1",color="yellow",size=5) +
  annotate("text", x=mean(R2$lon),y=mean(R2$lat),label="R2",color="white",size=5) +
  annotate("text", x=mean(R3$lon),y=mean(R3$lat),label="R3",color="red",size=5) +
  annotate("text", x=mean(R4$lon),y=mean(R4$lat),label="R4",color="green",size=5)


bb_R1 <- sapply(R1[3:2], mean)
bb_R2 <- sapply(R2[3:2], mean)
bb_R3 <- sapply(R3[3:2], mean)
bb_R4 <- sapply(R4[3:2], mean)

zoom_R1 <- get_map(location = bb_R1,  maptype = "hybrid", source = "google", zoom = 10)

ggmap(zoom_R1)+ 
  geom_polygon(data = R1, aes(x = lon, y = lat), alpha = 0.2, color = "yellow")+
  annotate("text", x=mean(R1$lon),y=mean(R1$lat),label="R1",color="yellow",size=10)




zoom_R2 <- get_map(location = bb_R2,  maptype = "hybrid", source = "google", zoom = 9)

ggmap(zoom_R2)+
  geom_polygon(data = R2, aes(x = lon, y = lat), alpha = 0.2, color = "white") +
  annotate("text", x=mean(R2$lon),y=mean(R2$lat),label="R2",color="white",size=10) 

zoom_R3 <- get_map(location = bb_R3,  maptype = "hybrid", source = "google", zoom = 9)

ggmap(zoom_R3)+
  geom_polygon(data = R3, aes(x = lon, y = lat), alpha = 0.2, color = "red") +
  annotate("text", x=mean(R3$lon),y=mean(R3$lat),label="R3",color="red",size=10) 


zoom_R4 <- get_map(location = bb_R4,  maptype = "hybrid", source = "google", zoom = 8)

ggmap(zoom_R4)+
  geom_polygon(data = R4, aes(x = lon, y = lat), alpha = 0.2, color = "green") +
  annotate("text", x=mean(R4$lon),y=mean(R4$lat),label="R4",color="green",size=10) 
