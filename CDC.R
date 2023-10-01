###############################################################################
# Carolina Data Challenge
###############################################################################

# installing libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(maptools)
library(sp)
library(naniar)

# setting wd
setwd("/Users/emilianoplanchon/Documents/Code/MSA/R")

# reading data in
wildfire <- read_excel("NautralSciences_Dataset.xlsx", sheet = "Fires")
wildfire <- as.data.frame(wildfire)

# variables with no missing variables
vars_not_miss <- miss_var_summary(wildfire)[miss_var_summary(wildfire)$pct_miss == 0,]$variable

# dataset with no missing variables
no_missing <- wildfire[,vars_not_miss]

# formatting date columns
no_missing$DISCOVERY_DATE <- as.Date(no_missing$DISCOVERY_DATE, origin=-2440587)

# creating a fire month column
no_missing$FIRE_MONTH <- str_sub(as.character(no_missing$DISCOVERY_DATE), 6, 7)

library(sp)
library(maps)
library(maptools)
# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees
latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- map('county', fill = TRUE, col = "transparent", plot = FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs = IDs,
                                     proj4string = CRS("+proj=longlat +datum=WGS84"))
  # Convert pointsDF to a SpatialPoints object
  pointsSP <- SpatialPoints(pointsDF,
                            proj4string = CRS("+proj=longlat +datum=WGS84"))
  # Use ‘over’ to get _indices_ of the Polygons object containing each point
  indices <- over(pointsSP, counties_sp)
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}
# Test the function using points in Wisconsin and Oregon.
con <- latlong2county(wildfire[,c('LONGITUDE', 'LATITUDE')])
counties <- c()
for (i in 1:length(con)) {
  counties[length(counties) + 1] <- str_split(con[i], ',')[[1]][2]
}

no_missing$COUNTY <- counties

write.csv(no_missing, "/Users/emilianoplanchon/Documents/Code/MSA/R/cdc.csv", row.names=FALSE)

###
###
###

no_missing3 <- no_missing[no_missing$STATE == "ID",]

ga_avg <- as.data.frame(no_missing3 %>% 
                      group_by(FIRE_MONTH) %>% 
                      summarise(AVG_SIZE = mean(FIRE_SIZE))
)

ga <- as.data.frame(no_missing3 %>% 
                group_by(FIRE_YEAR,FIRE_MONTH) %>% 
                summarise(AVG_SIZE = n()) %>%
                mutate(date = as.Date(paste0(FIRE_MONTH, "/01/", FIRE_YEAR), format = '%m/%d/%Y'))
)

ga_plot <- ggplot(ga, aes(x = date)) +
  geom_line(aes(y = AVG_SIZE)) +
  labs(x = "\nDate", y = "Average Size of Wildfires\n", title = "Average Monthly Size of Wildfires Over Time in GA") +
  theme_classic() +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_color_brewer(palette="Set1") + 
  theme(panel.background = element_blank(), 
        panel.grid.major = element_line(colour = "lightgray")) +
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=24)) + 
  theme(axis.text.y = element_text(size = 24)) +
  theme(axis.text.x = element_text(size = 24)) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) + 
  theme(legend.text=element_text(size=20), legend.title=element_text(size=20)) + 
  labs(colour = 'Legend') + 
  scale_x_date(date_breaks = "24 month", date_labels = "%b '%y")

ggsave(filename = "CDC_ga.png", dpi = 'retina',
       plot = ga_plot,
       width = 12, height = 8)

ga_edit <- ga
ga_edit[ga_edit$AVG_SIZE > 30,]$AVG_SIZE <- 4

ggplot(ga_edit, aes(x = date)) +
  geom_line(aes(y = AVG_SIZE)) +
  labs(x = "\nDate", y = "Average Size of Wildfires\n", title = "Average Monthly Size of Wildfires Over Time in GA") +
  theme_classic() +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_color_brewer(palette="Set1") + 
  theme(panel.background = element_blank(), 
        panel.grid.major = element_line(colour = "lightgray")) +
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=24)) + 
  theme(axis.text.y = element_text(size = 24)) +
  theme(axis.text.x = element_text(size = 24)) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) + 
  theme(legend.text=element_text(size=20), legend.title=element_text(size=20)) + 
  labs(colour = 'Legend') + 
  scale_x_date(date_breaks = "24 month", date_labels = "%b '%y")

no_missing4 <- no_missing[no_missing$STATE == "CA",]
no_missing4 <- no_missing4[!(no_missing4$STAT_CAUSE_CODE %in% c(1, 5)), ]

#no_missing_time_human <- no_missing[!(no_missing$STAT_CAUSE_CODE %in% c(1, 5)),]
as.data.frame(no_missing4 %>% 
                group_by(FIRE_MONTH) %>% 
                summarise(AVG_SIZE = n())
)

no_missing5 <- no_missing[no_missing$STATE == "NY",]

#no_missing_time_human <- no_missing[!(no_missing$STAT_CAUSE_CODE %in% c(1, 5)),]
as.data.frame(no_missing5 %>% 
                group_by(FIRE_MONTH) %>% 
                summarise(AVG_SIZE = n())
)

#no_missing_time_human <- no_missing[!(no_missing$STAT_CAUSE_CODE %in% c(1, 5)),]
as.data.frame(no_missing %>% 
                group_by(FIRE_MONTH) %>% 
                summarise(AVG_SIZE = mean(FIRE_SIZE))
)

as.data.frame(no_missing %>% 
                group_by(STATE) %>% 
                summarise(total_size = sum(FIRE_SIZE))
)



