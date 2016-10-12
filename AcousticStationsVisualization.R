## Date: 9 October 2016
## Creator: Christina Smith
## Purpose: 
##
## NOTES: 

library(dplyr)
library(ggplot2)
library(rgdal)
library(png)
library(grid)

## Read in reduced Acoustic Station Data.
daily_count <- read.csv("AcousticStations_maptime.csv")
## Convert date into R date
daily_count$Date <- as.Date(daily_count$Date)

## Set variables for plotting from user input
start <- as.Date("2011-11-16")
end <- as.Date("2014-12-20")
timestep <- "month"   # use string: "week", "month" or "quarter"
type <- "adult"     # use sting: "both", "adult" or "pinkie"
region <- "all"     # use string "all", "east", "west", "north" or "south"

## Select only the data needed for visualisations based on user input
plotdata <- filter(daily_count, Date >= start & Date<= end)

if (region != "all"){
    plotdata<- filter(plotdata, Station_Region == region)}

if (type != "both"){
    plotdata<- filter(plotdata, Life_stage == type)}

## Divide data into bins according to user input
timecuts <- seq(start, end, timestep)
dates <- timecuts[-length(timecuts)] # remove last element
timebins <- cut(plotdata$Date, breaks = timecuts)
detections <- data.frame(dates, tapply(plotdata$Number_fish, timebins, sum))

names(detections) = c("Date", "Number_fish")
detections[is.na(detections)] <- 0

## Make the time series plot
maintitle <- paste0("Total ",timestep,"ly detections of ", type,
                    " fish in ", region, " region(s).")

timeplot <- ggplot(detections, aes(Date, Number_fish)) + geom_point() + geom_line()
timeplot <- timeplot + labs(x = "", y = "Total Detections", title = maintitle) 
timeplot <- timeplot + theme_bw()

print(timeplot)
filename=paste0(timestep,"lydetections_", region, "region_", type, ".png")
png(filename,  width = 650, height = 600)
print(mapplot)
dev.off()

###############################################################################

## Load Google Earth image fiel for base image
img <- readPNG("GoogleEarth.png")
g <- rasterGrob(img, interpolate=TRUE)

## Load shapefiles for coast, ramps, stations, bathymatry
#coast <- readOGR(dsn="Map layers/vic25_poly_dd94", layer="vic25_poly_dd94")
station <- data.frame(readOGR(dsn="Map layers/Snapper_recievers_pp", layer="Snapper_recievers_pp"))
#bathy <- readOGR(dsn="Map layers/pp_bath6_25g", layer="pp_bath6_25g")
ramp <- read.csv("Map layers/PPB_WP_Ramps_coordinates_dec_deg_GDA94.csv")
ramp <- select(ramp,Latitude,Longitude)

## Station positions by region
statreg <- unique(select(daily_count, Station_Region, Latitude, Longitude))
statreg <- group_by(statreg, Station_Region)
centre <- c(144.8,-38.1)
c1 <- centre[2]-0.6*centre[1]
c2 <- centre[2]+0.6*centre[1]

## Prepare the data for mapping
mapdata <- mutate(plotdata, Bins = timebins) %>%
    group_by(Bins, StationID, Latitude, Longitude) %>%
    summarise(Detections = sum(Number_fish))

splitmapdata <- split(mapdata, mapdata$Bins)

n <- 2
currentdata <- data.frame(splitmapdata[n])
names(currentdata) <- gsub("X|[0-9]*\\.", "", names(currentdata))

## Make the map plot by:

maptitle <- paste0("Distribution of ", type, " fish in ", region, 
                   " region(s) for the ",timestep," starting on ", 
                   currentdata[[1]][1], ".")

## setting limits, labels, ratio
df <- data.frame(x = 144.34:145.13, y = -38.38:-37.8)
mapplot <- ggplot(df, aes(x, y)) + geom_blank() + theme_bw()
mapplot <- mapplot + coord_fixed(ratio = 1.269, xlim = c(144.34,145.13), 
                                 ylim = c(-38.38,-37.8), expand = FALSE)
mapplot <- mapplot + labs(x = "", y = "", title=maptitle)

## showing the base image
mapplot <- mapplot + annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf,
                                       ymax = Inf)

## building up the bathymatry, coast, station, ramp & region layers
#mapplot <- mapplot + geom_polygon(data=bathy, aes(x=long, y=lat, group=group), 
#                                  fill=NA, color="CornflowerBlue")
#mapplot <- mapplot + geom_polygon(data=coast, aes(x=long, y=lat, group=group), 
#                                  fill=NA, color="black", size=0.75)
mapplot <- mapplot + geom_point(data=station, aes(x=Long, y=Lat),
                                pch="*", size=8, color='White')
#mapplot <- mapplot + geom_point(data=statreg, aes(x=Longitude, y=Latitude, 
#                                                  color=Station_Region), 
#                                pch=1, size=4)
#mapplot <- mapplot + geom_point(data=statreg, aes(x=Longitude, y=Latitude),
#                                pch="*", size=8, color='White')
mapplot <- mapplot + geom_point(data=ramp, aes(x=Longitude, y=Latitude),
                                pch=24, size=4, color='black',fill="white")
mapplot <- mapplot + geom_abline(slope=0.6, intercept=c1, color="grey") 
mapplot <- mapplot + geom_abline(slope=-0.6, intercept=c2, color="grey")

## ... adding the data for each time bin where bin is denoted by n
mapplot <- mapplot + geom_point(data=currentdata, size=5,
                                aes(x=Longitude, y=Latitude, color=Detections))
mapplot <- mapplot + geom_point(data=currentdata, size=6, pch=21, color="black",
                                aes(x=Longitude, y=Latitude))
mapplot <- mapplot +scale_color_gradientn(colors = heat.colors(10))
#mapplot <- mapplot + geom_point(data=currentdata, color=magenta, 
#                                aes(x=Longitude, y=Latitude, size=Number))
print(mapplot)

filename=paste0("map",region,"_",type,timestep,"ly.png")
png(filename,  width = 650, height = 600)
print(mapplot)
dev.off()
