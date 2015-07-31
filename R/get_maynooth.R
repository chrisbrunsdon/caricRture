library(osmar)
corner_bbox(-6.614012,53.367606,-6.577706,53.394843) %>% get_osm -> maynooth
subset(maynooth,ids=find_down(maynooth,way(find(maynooth,way(tags(k=="highway")))))) %>% as_sp('lines') %>% to_osm -> maynooth_hway
kill_factors <- function(df) {
  dfc <- df
  for (i in 1:ncol(df)) {
    if (is.factor(dfc[,i])) dfc[,i] <- as.character(dfc[,i])
  }
  dfc
}
data.frame(maynooth$ways$tags) %>% kill_factors -> tagset
tagset %>% subset(k == 'highway') -> hset

hset[match(maynooth_hway$id,hset$id),c(1,3)] -> hset
rownames(hset) <- as.character(hset$id)
colnames(hset) <- c("ID","class")
maynooth_hway <- SpatialLinesDataFrame(maynooth_hway,hset)

foot <- c("path","pedestrian","footway","track","steps","residential")

subset(maynooth,ids=find_down(maynooth,way(find(maynooth,way(tags(k=="building")))))) %>% as_sp('polygons') %>% to_osm -> maynooth_bldg
tagset %>% subset(k == 'building') -> bset
bset[match(maynooth_bldg$id,bset$id),c(1,3)] -> bset
rownames(bset) <- as.character(bset$id)
colnames(bset) <- c("ID","class")
maynooth_bldg <- SpatialPolygonsDataFrame(maynooth_bldg,bset)

code <- 
  "x = x/sum(x)"
x <- as.numeric(1:10)
n <- as.integer(10)

cubefn3 <- cfunction(sig = signature(n="integer", x="numeric"), implicit = "none", dim = c("", "(n)"), code, language="F95")
