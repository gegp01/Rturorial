# From georeferenced dataset including a group of interest and other observations to contrast the group of interest, and historical observations on land use (see data_compiler.R) 
# This R code aims at: 
# (1) Forming a subset with the class of interest and a random selection of a null model (no_class)
# (2) Performing recursive partitioning on the data, and the historical variables of Land Use

##################################################
# PART 1. READ DATA (example) AND LIST THE GOUP OF CLASSES OF INTEREST
# Example dataset of shorebirds with speciment in museum collections and reported in GBIF, including the historical data of land use change.
D = readRDS("data_gbif_luc.rds") 

# Note: there are 7259 data with NA in the variables states, hence we exlude these observations.
D = D[is.na(D$primf)==F,]

# LIST OF CLASSES OF INTEREST, BASED ON A VARIABLE IN THE DATASET D.
#taxa = unique(D$species)
taxa = readRDS("sp Mx.rds")

# SELECT A GROUP OF INTEREST (e.g. "Charadrius nivosus", which is number 31 in the list taxa).
v = 31

# FORM A BINARY CLASS BASED ON THE GROUP OF INTEREST
class = ifelse(D$species==taxa[v], 1, 0)

# SPLIT THE DATASET IN THE CLASS AND NO_CLASS
Y1 = D[class==1,]
Y0 = D[class==0,]

# MODELO NULO A NIVEL DE COLECTA
#idx = 1:nrow(Y0)
#Y0_subset = Y0[sample(idx, nrow(Y1)),]

# SELECT A SUBSET OF DATA BASED ON A GROUP OF INTEREREST WITHIN THE NO_CLASS. For instance we might want to compare the class, with the no_class members of the same family.
g = unique(D$family[D$species == taxa[v]]) # g is the family group
Y0_subset_ = Y0[Y0$family==g,]

idx = 1:nrow(Y0_subset_)
Y0_subset = Y0_subset_[sample(idx, nrow(Y1)),]

# LABEL THE TWO SUBSETS AS CLASS (1) AND NO_CLASS (0)
Y1$class = 1
Y0_subset$class = 0

# PUT ALL DATA TOGETHER IN A DATAFRAME Q, TO PERFORM RECURSIVE PARTITIONING
Q = rbind(Y1, Y0_subset)

##########################################
# PART 2. PERFORM THE GEOGRAPHIC MODEL. 
require(rpart)
require(rpart.plot)

modelo = clase ~ primn + secdn + range + pastr + c3per + c3ann + c3nfx + c4per + c4ann + urban + secma
tree = rpart(modelo, data = Q, method = "class")
b <- tree$cptable[which.min(tree$cptable[, "xerror"]), "CP"]
pruned_tree <- prune(tree, cp = b)

# par(font.main=3)
# rpart.plot(pruned_tree, tweak = 1.2, main=taxa[v])

###################################################
# Create buffers around the observations in GBIF

require(sp)
require(rgdal)

mx = readOGR(dsn="mexico.geojson")
plot(mx, add=F)

NewCRS = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
X = SpatialPoints(data.frame(lon  = Y1$decimalLongitude, lat = Y1$decimalLatitude), proj4string=NewCRS)

# Reproject
X_ = spTransform(X, NewCRS)
mx_ = spTransform(mx, NewCRS)

X_ = buffer(X, width = 150000)

# run f.PREDICT WITHIN THE BUFFERS AS POLYGON IN THE FUNCTION!
path2land = "https://gegp01.github.io/ServSoc/countries.geojson"
land<-readOGR(dsn = path2land)
# dissolve land

land_ = spTransform(land, NewCRS)
land_ = aggregate(land_) # dissolve all polygons in land

#plot(mx_)
plot(X_, add= F, lty = "dotted", border="blue") # plot distribution (buffers)
plot(land_, add= T, border = "darkslategrey", col = "beige")

path2states = "~/LUCIDA/data/LUH2 v2h Release_10_14_16/states.nc"
varX= "primn"
band = 1165

#####################################
# FUNCTION f1(extract values from raster within a buffer around the observations in GBIF)

f1 = function(path, varX, band){
  # 1. Select data from taxa (Y1) of interest, Reproject to CRS, and create buffers (X_)
  NewCRS = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  X = SpatialPoints(data.frame(lon  = Y1$decimalLongitude, lat = Y1$decimalLatitude), proj4string=NewCRS)
  X_ = spTransform(X, NewCRS)
  X_ = buffer(X, width = 150000)
  # Extract values of raster 
  rasX<-raster(path2states, band=band, varname = varX)
  mask(rasX, X_)
  }

#####################################
# 

  all.vars<-c("primf", "primn", "secdf", "secdn"
              , "range", "pastr", "c3per", "c3ann"
              , "c3nfx", "c4per", "c4ann", "urban", "secma") # List of variables to include in the analysis
  
  r1<-f1(path = path, varX = "primf", band = bandX) # choose a band (year) to predict. Note that the band depends on the raster being used.
  r2<-f1(path = path, varX = "primn", band = bandX) 
  r3<-f1(path = path, varX = "secdf", band = bandX)
  r4<-f1(path = path, varX = "secdn", band = bandX)
  r5<-f1(path = path, varX = "range", band = bandX)
  r6<-f1(path = path, varX = "pastr", band = bandX)
  r7<-f1(path = path, varX = "c3per", band = bandX)
  r8<-f1(path = path, varX = "c3ann", band = bandX)
  r9<-f1(path = path, varX = "c3nfx", band = bandX)
  r10<-f1(path = path, varX = "c4per", band = bandX)
  r11<-f1(path = path, varX = "c4ann", band = bandX)
  r12<-f1(path = path, varX = "urban", band = bandX)
  r13<-f1(path = path, varX = "secma", band = bandX)
  
  id1 <- rasterToPolygons(r1)
  id2 <- rasterToPolygons(r2)
  id3 <- rasterToPolygons(r3)
  id4 <- rasterToPolygons(r4)
  id5 <- rasterToPolygons(r5)
  id6 <- rasterToPolygons(r6)
  id7 <- rasterToPolygons(r7)
  id8 <- rasterToPolygons(r8)
  id9 <- rasterToPolygons(r9)
  id10 <- rasterToPolygons(r10)
  id11 <- rasterToPolygons(r11)
  id12 <- rasterToPolygons(r12)
  id13 <- rasterToPolygons(r13)
  
  # MERGE DATA 
  # GET NAMES OF POLYGONS
  
  f.poli<-function(q) {
    P@polygons[[q]]@ID
  }
  
  P = id1
  id1.x<-sapply(q= 1:length(P), f.poli)
  
  P = id2
  id2.x<-sapply(q= 1:length(P), f.poli)
  
  P = id3
  id3.x<-sapply(q= 1:length(P), f.poli)
  
  P = id4
  id4.x<-sapply(q= 1:length(P), f.poli)
  
  P = id5
  id5.x<-sapply(q= 1:length(P), f.poli)
  
  P = id6
  id6.x<-sapply(q= 1:length(P), f.poli)
  
  P = id7
  id7.x<-sapply(q= 1:length(P), f.poli)
  
  P = id8
  id8.x<-sapply(q= 1:length(P), f.poli)
  
  P = id9
  id9.x<-sapply(q= 1:length(P), f.poli)
  
  P = id10
  id10.x<-sapply(q= 1:length(P), f.poli)
  
  P = id11
  id11.x<-sapply(q= 1:length(P), f.poli)
  
  P = id12
  id12.x<-sapply(q= 1:length(P), f.poli)
  
  P = id13
  id13.x<-sapply(q= 1:length(P), f.poli)
  
  id1@data <- data.frame(id1@data
                         , id2@data[match(id1.x, id2.x),]
                         , id3@data[match(id1.x, id3.x),]
                         , id4@data[match(id1.x, id4.x),]
                         , id5@data[match(id1.x, id5.x),]
                         , id6@data[match(id1.x, id6.x),]
                         , id7@data[match(id1.x, id7.x),]
                         , id8@data[match(id1.x, id8.x),]
                         , id9@data[match(id1.x, id9.x),]
                         , id10@data[match(id1.x, id10.x),]
                         , id11@data[match(id1.x, id11.x),]
                         , id12@data[match(id1.x, id12.x),]
                         , id13@data[match(id1.x, id13.x),])
  
  names(id1@data)<-all.vars
  
  V<-pruned_tree # Pruned tree for further analyses
  
  
#  names(id1)<-all.vars
  guess<-id1@data
  
  y<-predict(V, guess)
  
  # ADD COLUMN OF ABSENCE PRESENCE TO THE POLYGON id1. 
  # CREATE NEW DATAFRAME P
  
  P<-id1
  P@data<-data.frame(P@data, y)
  
  # SAVE P AS geoJson
  
  require(leaflet)
  require(leafletR)
  
  source("https://gegp01.github.io/leafletR/toGeoJSON.R")
  source("https://gegp01.github.io/leafletR/dfToGeoJSON.R")
  source("https://gegp01.github.io/leafletR/spToGeoJSON.R")
  source("https://gegp01.github.io/leafletR/fileToGeoJSON.R")
  
  x = taxa[v]
  
  toGeoJSON(P, x)









