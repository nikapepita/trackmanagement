#R Package gdistance: Distances and Routes on
# Geographical Grids

##Easy low coast calculation
#Tutorial http://www.projectpanormos.com/sacredway/routeanalysis/
#https://dyerlab.github.io/applied_population_genetics/ecological-distance.html
##load packages

#load packages

loadpackages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("osmdata", "tidyverse", "sf", "gmap", "gdistance", "raster","rasterVis","rgdal","ggmap")
loadpackages(packages)

##Input Data

setwd("E:/Annika/R_Project/Data/")
Startpoint <- readOGR(dsn = ".", layer = "StartJossgrund")
Startpoint.df <- as(Startpoint, "data.frame")

MKK_shape <- readOGR(dsn = ".", layer="MKK_Kreis")
Clip_Region <- MKK_shape[2, ]
ext <- extent(MKK_shape[2, ])

SRTM = raster::raster("E:/Annika/R_Project/Data/SRTM.tif")
LULC = raster::raster("E:/Annika/R_Project/Data/Corine2018.tif")


#Set Variable - Study Adrea

AOI <- "Jossgrund"


##Download DEM

###add!!! see 3D Mapping

#Create Raster

SRTM_resam <- resample(SRTM, LULC, method = 'bilinear')

# r12 has the minimal common extent to crop
r12 = SRTM_resam + LULC

SRTM_Masked <- mask(SRTM_resam,  r12)
SRTM_Cropped <- crop(SRTM_Masked,  Clip_Region)


slope <- terrain(SRTM_Cropped, opt = 'slope', unit = 'degrees')  #calculate slope

plot(slope)


aspect <- terrain(SRTM_Cropped, opt = 'aspect', unit = 'degrees') #calculate aspect

#Load LULC
LULC_Masked <- mask(LULC,  r12)
LULC_Cropped <- crop(LULC_Masked,  Clip_Region)

plot(LULC_Cropped2)

###Reclassify LULC

#Urban Fabric
values(LULC_Cropped)[values(LULC_Cropped) == 111] = 1
values(LULC_Cropped)[values(LULC_Cropped) == 112] = 1

#Industrial, commercial and transport units
values(LULC_Cropped)[values(LULC_Cropped) == 121] = 0
values(LULC_Cropped)[values(LULC_Cropped) == 122] = 0
values(LULC_Cropped)[values(LULC_Cropped) == 123] = 0
values(LULC_Cropped)[values(LULC_Cropped) == 124] = 0

#Mine,dump and construction sites
values(LULC_Cropped)[values(LULC_Cropped) == 131] = 0
values(LULC_Cropped)[values(LULC_Cropped) == 132] = 0
values(LULC_Cropped)[values(LULC_Cropped) == 133] = 0

#Artificial, non agricultural vegetated areas

values(LULC_Cropped)[values(LULC_Cropped) == 141] = 2
values(LULC_Cropped)[values(LULC_Cropped) == 142] = 2

#Agricultural areas
#Arable land
values(LULC_Cropped)[values(LULC_Cropped) == 211] = 0
values(LULC_Cropped)[values(LULC_Cropped) == 212] = 0
values(LULC_Cropped)[values(LULC_Cropped) == 213] = 0

#Permanet crops
values(LULC_Cropped)[values(LULC_Cropped) == 221] = 2
values(LULC_Cropped)[values(LULC_Cropped) == 222] = 2
values(LULC_Cropped)[values(LULC_Cropped) == 223] = 2

#Pastures
values(LULC_Cropped)[values(LULC_Cropped) == 231] = 5

#Heterogeneous agricultural areas
values(LULC_Cropped)[values(LULC_Cropped) == 241] = 0
values(LULC_Cropped)[values(LULC_Cropped) == 242] = 2
values(LULC_Cropped)[values(LULC_Cropped) == 243] = 2
values(LULC_Cropped)[values(LULC_Cropped) == 244] = 2

#Forest and seminatural areas
#Forest
values(LULC_Cropped)[values(LULC_Cropped) == 311] = 5
values(LULC_Cropped)[values(LULC_Cropped) == 312] = 5
values(LULC_Cropped)[values(LULC_Cropped) == 313] = 5

#Shrub and/or herbaceous vegetations associations

values(LULC_Cropped)[values(LULC_Cropped) == 321] = 10
values(LULC_Cropped)[values(LULC_Cropped) == 322] = 10
values(LULC_Cropped)[values(LULC_Cropped) == 323] = 7
values(LULC_Cropped)[values(LULC_Cropped) == 324] = 8

#Open spaces with little or no vegetation

values(LULC_Cropped)[values(LULC_Cropped) == 331] = 10
values(LULC_Cropped)[values(LULC_Cropped) == 332] = 8
values(LULC_Cropped)[values(LULC_Cropped) == 333] = 7
values(LULC_Cropped)[values(LULC_Cropped) == 334] = 0
values(LULC_Cropped)[values(LULC_Cropped) == 335] = 0

#Wetlands
#Inland wetlands
values(LULC_Cropped)[values(LULC_Cropped) == 411] = 7
values(LULC_Cropped)[values(LULC_Cropped) == 412] = 7

#Coastal wetlands

values(LULC_Cropped)[values(LULC_Cropped) == 421] = 0
values(LULC_Cropped)[values(LULC_Cropped) == 422] = 0
values(LULC_Cropped)[values(LULC_Cropped) == 423] = 0

#Waterbodies
#Inland waters
values(LULC_Cropped)[values(LULC_Cropped) == 511] = -1
values(LULC_Cropped)[values(LULC_Cropped) == 512] = -1

#Marine waters
values(LULC_Cropped)[values(LULC_Cropped) == 521] = -1
values(LULC_Cropped)[values(LULC_Cropped) == 522] = -1
values(LULC_Cropped)[values(LULC_Cropped) == 523] = -1

plot(LULC_Cropped)

rm(SRTM,SRTM_resam,r12,LULC_Masked,SRTM_Masked,LULC)

#coords <- c(xyFromCell(SRTM_Cropped, LULC_Cropped[i], spatial = FALSE))

#view <- viewshed(SRTM_Cropped, ext, coords, h1 = 0, h2 = 0)

#Download POIs, RoadNetwork

###DEM
###View
###Roads with different value

#availabel features in OSM
#the first five features
head(available_features())
available_features()

#amenities
available_tags("route")
available_tags("highway")
available_tags("amenity")
available_tags("water")

#shops
head(available_tags("shop"))

#########Get OSM Data

library(osmdata)

##Create a Loop

#OSM_Data (One Feature)


q1 <- opq(AOI) %>%
  add_osm_feature(key = 'waterway')
waterway <- osmdata_sp(q1)
waterway <- waterway$osm_lines

q1 <- opq(AOI) %>%
  add_osm_feature(key = 'natural', 'spring')
spring <- osmdata_sp(q1)
spring <- spring$osm_points

q1 <- opq(AOI) %>%
  add_osm_feature(key = 'highway')
highway <- osmdata_sp(q1)
highway <- highway$osm_lines

####Transfer to Raster
waterway <-
  spTransform(
    waterway,
    "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  )

waterway <- mask(LULC_Cropped, waterway)
waterway[is.na(waterway[])] <- 0
values(waterway)[values(waterway) > 0] = -1
plot(waterway)



spring <-
  spTransform(
    spring,
    "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  )

spring <- mask(LULC_Cropped, spring)
spring[is.na(spring[])] <- 0
values(spring)[values(spring) > 0] = 10
plot(spring)


highway <-
  spTransform(
    highway,
    "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  )

highway <- mask(LULC_Cropped, highway)
highway[is.na(highway[])] <- 0
values(highway)[values(highway) > 0] = 10
plot(highway)


#Transfer Raster in Dataframe

LULC_Cropped_temp <- rasterToPoints(LULC_Cropped)

#Make the points a dataframe for ggplot
LULC_df <- data.frame(LULC_Cropped_temp)
#Make appropriate column headings
colnames(LULC_df) <- c("Longitude", "Latitude", "Class")

ggplot() +
  geom_raster(data = LULC_df, aes(x = Latitude, y = Longitude, fill = Class)) +
  scale_colour_gradientn(colours = terrain.colors(10)) +
  geom_point(
    data = Startpoint.df,
    aes(x = coords.x2, y = coords.x1),
    color = "green"
  ) +
  theme_bw() +
  coord_equal() +
  scale_fill_gradient("LULC", limits = c(0, 10)) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16, angle = 90),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.key = element_blank()
  )

########### Create Cost Raster 

cost <- stack(LULC_Cropped, spring, highway)

cost1 <- calc(cost, sum)

###Plot Least Coth Path on Cost Raster
#Transfer Raster in Dataframe

cost1_temp <- rasterToPoints(cost1)

#Make the points a dataframe for ggplot
cost1_df <- data.frame(cost1_temp)
#Make appropriate column headings
colnames(cost1_df) <- c("Longitude", "Latitude", "Class")


#Random Startpoint
#set.seed(123)

#Startpoint <- sampleRandom(cost1, size=1, cells=TRUE, sp=TRUE)
#Route <- as.data.frame(Startpoint.df)

Route <- raster::extract(
  cost1, Startpoint.df[1,2:3], df = TRUE, cellnumbers=T)


Route <- cbind(Route,Startpoint.df[1,2:3])


for(i in 1:20) {
  
  
  rowcol <- rowColFromCell(cost1, Route$cell[i])
  
  cell1 <- cellFromRowCol(cost1, c(rowcol[1,1])-1, rowcol[1,2])
  cell2 <- cellFromRowCol(cost1, c(rowcol[1,1])-1, c(rowcol[1,2])-1)
  cell3 <- cellFromRowCol(cost1, c(rowcol[1,1])-1, c(rowcol[1,2])+1)
  #cell4 <- cellFromRowCol(cost1, rowcol[1,1], c(rowcol[1,2])+1)
  #cell5 <- cellFromRowCol(cost1, c(rowcol[1,1])+1, c(rowcol[1,2])+1)
  #cell6 <- cellFromRowCol(cost1, c(rowcol[1,1])+1, c(rowcol[1,2])-1)
  #cell7 <- cellFromRowCol(cost1, c(rowcol[1,1])+1, rowcol[1,2])
  #cell8 <- cellFromRowCol(cost1, rowcol[1,1], c(rowcol[1,2])-1)
  
  cells <- as.data.frame(rbind(cell1,cell2,cell3))
  #cells <- as.data.frame(rbind(cell1,cell2,cell3,cell4,cell5,cell6,cell7,cell8))
  
  xy <- as.data.frame(xyFromCell(cost, cells$V1, spatial = FALSE))
  
  cells <- cbind(cells,xy)
  
  cent_max <- raster::extract(
    cost1, cells[,2:3], df = TRUE, cellnumbers=T)
  
  cent_max = subset(cent_max, !(cent_max$cells %in% Route$cell))
  
  cent_max <- cent_max[order(cent_max$layer), ]
  
  Route[i + 1, 4:5] <-
    xyFromCell(cost1, cent_max$cells[1], spatial = FALSE)
  Route[i + 1, 2:3] <- cent_max[1, 2:3]
}

ggplot() +
  geom_raster(data = cost1_df,
              aes(
                x = Latitude,
                y = Longitude,
                fill = Class
              )) +
  scale_colour_gradientn(colours = terrain.colors(10)) +
  geom_point(
    data=Route,
    aes(x=coords.x2,y=coords.x1),
    color = "green"
  ) 


###Least Cost Path
trCost <- transition(cost1, transitionFunction = min, 8)
trCost2 <- geoCorrection(trCost, type = "c")


cost2 <- accCost(trCost2,c(543032.8,5568808))
cost3 <- costDistance(trCost2,c(543032.8,5568808), c(542352.4,5572853))

tr <- transition(1 / rs1, transitionFunction = mean, directions = 4)
tr <- geoCorrection(tr,
                    type = "c",
                    multpl = FALSE,
                    scl = FALSE)

a <- c(Startpoint.df[1, 2], Startpoint.df[1, 3])

b <- c(Startpoint.df[2, 2], Startpoint.df[2, 3])

path.1 <- shortestPath(trCost2, a, b, output = "SpatialLines")
plot(path.1)

path.1.df <-
  SpatialLinesDataFrame(path.1, data = data.frame(ID = 1))
path.1.df_fortify <- fortify(path.1.df)


###Plot Least Coth Path on Cost Raster
#Transfer Raster in Dataframe

cost1_temp <- rasterToPoints(cost1)

#Make the points a dataframe for ggplot
cost1_df <- data.frame(cost1_temp)
#Make appropriate column headings
colnames(cost1_df) <- c("Longitude", "Latitude", "Class")

ggplot() +
  geom_raster(data = cost1_df,
              aes(
                x = cost1_df$Latitude,
                y = cost1_df$Longitude,
                fill = cost1_df$Class
              )) +
  scale_colour_gradientn(colours = terrain.colors(10)) +
  geom_point(
    data = Startpoint.df,
    aes(x = Startpoint.df$coords.x2, y = Startpoint.df$coords.x1),
    color = "green"
  ) +
  geom_line(data = path.1.df_fortify,
            aes(x = path.1.df_fortify$lat, y = path.1.df_fortify$long)) +
  theme_bw() +
  coord_equal() +
  scale_fill_gradient("Cost", limits = c(0, 2000)) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16, angle = 90),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.key = element_blank()
  )





#####################

data(volcano)
library(spatstat)
LLC <- data.frame(E = 174.761345, N = -36.879784)
coordinates(LLC) <- ~ E + N
proj4string(LLC) <- CRS("+proj=longlat +datum=WGS84")
LLC.NZGD49 <- spTransform(LLC, CRS("+init=epsg:27200"))
volcano.r <- as.im(list(
  x = seq(
    from = 2667405,
    length.out = 61,
    by = 10
  ),
  y = seq(
    from = 6478705,
    length.out = 87,
    by = 10
  ),
  z = t(volcano)[61:1, ]
))
volcano.sp <- as(volcano.r, "SpatialGridDataFrame")
proj4string(volcano.sp) <- CRS("+init=epsg:27200")
str(volcano.sp)
spplot(volcano.sp,
       at = seq(min(volcano.sp$v), max(volcano.sp$v), 5),
       col.regions = topo.colors(45))

r <- raster(volcano.sp)

altDiff <- function(x)
  x[2] - x[1]
hd <- transition(r, altDiff, 8, symm = FALSE)

slope <- geoCorrection(hd)

adj <- adjacent(r,
                cells = 1:ncell(r),
                pairs = TRUE,
                directions = 8)
speed <- slope
speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))

Conductance <- geoCorrection(speed)

A <- c(2667670, 6479000)
B <- c(2667800, 6479400)
AtoB <- shortestPath(Conductance, A, B, output = "SpatialLines")
BtoA <- shortestPath(Conductance, B, A, output = "SpatialLines")


plot(r,
     xlab = "x coordinate (m)",
     ylab = "y coordinate (m)",
     legend.lab = "Altitude (masl)")
lines(AtoB, col = "red", lwd = 2)
lines(BtoA, col = "blue")
text(A[1] - 10, A[2] - 10, "A")
text(B[1] + 10, B[2] + 10, "B")
