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

reg_bb <- c(left = min(Regions$lon - 0.2),
            bottom = min(Regions$lat - 0.2),
            right = max(Regions$lon + 0.2),
            top = max(Regions$lat + 0.2))

reg_stamen <- get_stamenmap(bbox = reg_bb,
                                zoom = 12)


ggmap(reg_stamen) + 
  geom_polygon(data = R1, aes(x = lon, y = lat), alpha = 0.5, color = "yellow") +
  geom_polygon(data = R2, aes(x = lon, y = lat), alpha = 0.5, color = "white") +
  geom_polygon(data = R3, aes(x = lon, y = lat), alpha = 0.5, color = "red") +
  geom_polygon(data = R4, aes(x = lon, y = lat), alpha = 0.5, color = "green") +
  annotate("text", x=mean(R1$lon),y=mean(R1$lat),label="R1",color="yellow",size=5) +
  annotate("text", x=mean(R2$lon),y=mean(R2$lat),label="R2",color="white",size=5) +
  annotate("text", x=mean(R3$lon),y=mean(R3$lat),label="R3",color="red",size=5) +
  annotate("text", x=mean(R4$lon),y=mean(R4$lat),label="R4",color="green",size=5)


R1_bb <- c(left = min(R1$lon - 0.002),
           bottom = min(R1$lat - 0.002),
           right = max(R1$lon + 0.002),
           top = max(R1$lat + 0.002))
R1_stamen <- get_stamenmap(bbox = R1_bb,
                            zoom = 12)

ggmap(R1_stamen)+
  annotate("text", x=-89.55,y= 41.2,label="R1",color="yellow",size=10)



R2_bb <- c(left = min(R2$lon - 0.002),
           bottom = min(R2$lat - 0.002),
           right = max(R2$lon + 0.002),
           top = max(R2$lat + 0.002))
R2_stamen <- get_stamenmap(bbox = R2_bb,
                           zoom = 12)

ggmap(R2_stamen) +
  annotate("text", x=-90.45,y= 40.4,label="R2",color="white",size=10)



R3_bb <- c(left = min(R3$lon - 0.002),
           bottom = min(R3$lat - 0.002),
           right = max(R3$lon + 0.002),
           top = max(R3$lat + 0.002))
R3_stamen <- get_stamenmap(bbox = R3_bb,
                           zoom = 12)

ggmap(R3_stamen) +
  annotate("text", x=-91.45,y= 41.1,label="R3",color="red",size=10)



R4_bb <- c(left = min(R4$lon - 0.002),
           bottom = min(R4$lat - 0.002),
           right = max(R4$lon + 0.002),
           top = max(R4$lat + 0.002))
R4_stamen <- get_stamenmap(bbox = R4_bb,
                           zoom = 12)

ggmap(R4_stamen)+
  geom_polygon(data = R4, aes(x = lon, y = lat), alpha = 0.2, color = "green") +
  annotate("text", x=-91.3,y= 39.17,label="R4",color="green",size=10)



