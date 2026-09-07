#Average 20th century January temperature by states
##Uncomment when use for the first time
##install.packages("maps");
##install.packages("maptools");
##install.packages("sp");
library(maps)
library(maptools)
library(sp)
##First read input data and add a column with states name
##20th centuary average
txt <- "AL  AZ AR    CA  CO  CT  DE  FL  GA  ID  IL  IN  IA KS KY LA ME MD MA MI MN MS MO MT NE NV NH NJ NM NY NC ND OH OK OR PA RI SC SD TN TX UT VT VA WA WV WI WY
45.1 40.7 39.3 42.3 23.7 25.4 33.3 57.8 46.1 22.0 25.3 26.5 17.7 28.6 33.6 49.4 13.2 32.5 24.1 18.2 6.5 45.2 29.4 16.7 22.1 30.0 17.1 29.6 33.7 19.9 40.0 6.4 26.9 36.5 30.2 25.3 27.2 44.6 15.0 37.2 45.4 25.3 15.3 34.8 28.4 30.6 12.4 18.4"
dat <- stack(read.table(text = txt,  header = TRUE))
names(dat)[2] <-'state.abb'
dat$states <- tolower(state.name[match(dat$state.abb,  state.abb)])
##Then load the map and convert to a SpatialPolygons
mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))
##And now you add the information from your data:
idx <- match(unique(nms),  dat$states)
dat2 <- data.frame(value = dat$value[idx], state = unique(nms))
row.names(dat2) <- unique(nms)
USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
##Finally you plot it:
spplot(USAsp['value']);
