install.packages("rgdal")
library(rgdal)

#download the file
temp <- tempfile(fileext = ".zip")
download.file("http://ec.europa.eu/eurostat/cache/GISCO/geodatafiles/NUTS_2013_01M_SH.zip", temp)
unzip(temp)

#load the data and filter it to Hungary and NUTS2 level
EU_NUTS = readOGR(dsn = "./NUTS_2013_01M_SH/data", layer = "NUTS_RG_01M_2013")
map_nuts2 <- subset(EU_NUTS, STAT_LEVL_ == 2) # set NUTS level
country <- substring(as.character(map_nuts2$NUTS_ID), 1, 2)
map <- c("AT","BE","BG","CY","CZ","DE","DK","EL","ES","FI","FR","HR","HU","IE","IT","LT","LU","LV","MT","NL","NO","PL","PT","RO","SE","SI","SK","UK") # limit it to countries in study
map_nuts2a <- map_nuts2[country %in% map,]

#plot it
plot(map_nuts2a, col = colorRampPalette(c("white", "red"))(nrow(map_nuts2a@data)))