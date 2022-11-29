# starter
{
library(tidyverse)
library(sf)
library(rgdal) #readOGR
library(ggplot2)
library(osmdata)
library(raster)
library(terra)
library(dbscan)
library(ggforce)
  
getwd()
setwd("C:/Users/ewado/OneDrive/Dokumenty/Studia/R-mat")

pow<-readOGR(".", "powiaty")
pow<-spTransform(pow, CRS("+proj=longlat +datum=NAD83")) 
waw.pov<-pow[pow$jpt_nazwa_=='powiat Warszawa',]

pow.sf <- st_read("powiaty.shp")
pow.sf <- st_transform(pow.sf, crs = "+proj=longlat +datum=NAD83")

waw.pow.sf <- pow.sf[pow.sf$jpt_nazwa_=='powiat Warszawa',]
waw.pow.sf1 <- pow.sf %>% filter(jpt_nazwa_=='powiat Warszawa')
WAW_box <- getbb("Warsaw, Poland")

WAW_box[1,2]<-21.27115
}

# Fast food - restaurant - convenience store - supermarket
{
  values = c("fast_food","restaurant", "convenience", "supermarket")
  keys= c("amenity", "amenity", "shop", "shop")
  df=data.frame()
  
  for(i in 1:4){
  variable.sf = opq(WAW_box) %>% 
    add_osm_feature(key=keys[i], value = values[i]) %>% 
    osmdata_sf()
  
  variable.sf$osm_points <- variable.sf$osm_points %>% 
    filter(st_within(variable.sf$osm_points %>% 
                       st_transform(crs = "+proj=longlat +datum=NAD83"), 
                     waw.pow.sf, sparse = F))
  
  variable.sf$osm_polygons  <- variable.sf$osm_polygons  %>% 
    filter(st_within(variable.sf$osm_polygons  %>% 
                       st_transform(crs = "+proj=longlat +datum=NAD83"), 
                     waw.pow.sf, sparse = F))
 
  variable.pts<-variable.sf$osm_points
  variable.poly<-variable.sf$osm_polygons
  variable.multi<-variable.sf$osm_multipolygons
  
  variable.pts.g<-variable.pts$geometry
  variable.poly.g<-variable.poly$geometry
  variable.poly.union<-st_union(variable.poly)
  
  variable.filtered <- filter(variable.pts, st_within(variable.pts, variable.poly.union, sparse=F))
  variable.filtered
  
  pts.df<-as.data.frame(variable.pts.g)
  pts_in_poly.df<-as.data.frame(variable.filtered)
  variable.final<-anti_join(pts.df, pts_in_poly.df) #points outside polygons
  
  variable.final<-st_as_sf(variable.final)
  
  crds.pts<-as(variable.final, "Spatial") %>% coordinates()
  crds.poly<-as(variable.poly.g, "Spatial") %>% coordinates()
  
  df0<-data.frame(ID=1:(nrow(variable.final)+length(variable.poly.g)),
                                      type=keys[i], sub_type=values[i], 
                    crds_x=rbind(crds.pts, crds.poly)[,1], crds_y=rbind(crds.pts, crds.poly)[,2])
  df<-rbind(df,df0)
  Sys.sleep(1)
  }
  
  df$type = ifelse(df$type == "amenity", "Restaurant", "Store")
  
}

# Distance from the city center
{
  
  sp<-SpatialPointsDataFrame(df[,4:5], df[,1:3])
  
  center<-SpatialPoints(data.frame(x=21.01506, y=52.2309)) # center coordinates
  dist<-pointDistance(sp, center, T)
  
  sp$dist_center<-dist
  
  crs(sp)<-"+proj=longlat +datum=NAD83"
  spsf<-st_as_sf(sp)
  
  ggplot() + 
    geom_sf(waw.pow.sf, mapping = aes(geometry = geometry))+
    geom_sf(spsf, mapping = aes(geometry = geometry, col=dist_center))
}

# Population (demographics)
{
  grid.waw<-readOGR(".", "grid_waw")
  grid.waw.df<-as.data.frame(grid.waw)
  grid.waw.sf<-st_as_sf(grid.waw)
  
  variables<-c("TOT_MALE", "TOT_FEM", "TOT_0_14", "TOT_65__")
  
  for(var in variables){
  
    count500<-rep(0, nrow(sp)) # keeps count of people (500 meters)
    count1500<-rep(0, nrow(sp)) # keeps count of people (1500 meters)
    
    for(i in 1:nrow(grid.waw)){ # for each cell...
      if(i%%25==0) print(paste0(var, " ", i))
      if(grid.waw.df[i, var]>0){  #...if someone lives in this cell...
        sample<-spsample(grid.waw[i,], n=grid.waw.df[i, var], type="random") #...sample people...
        
        a<-which(spDists(sp, grid.waw[i,],T)<2) #...and for the objects, that are nearby...
        
        for(j in a){
          count500[j]<-count500[j]+length(sample[spDists(sample, sp[j,], T)<0.5]) #...adjust counters. 
          count1500[j]<-count1500[j]+length(sample[spDists(sample, sp[j,], T)<1.5])
        }
        
      }}
    
    spsf[,paste0(var, 500)]<-count500
    spsf[,paste0(var, 1500)]<-count1500
    spsf[,paste0(var, "Neigh")]<-count1500-count500
    
    # sp[,paste0(var, 500)]<-count500
    # sp[,paste0(var, 1500)]<-count1500
    
    # ggplot() + 
    #   geom_sf(grid.waw.sf, mapping = aes(geometry = geometry, fill=TOT_MALE, alpha=0.7))+
    #   geom_sf(spsf, size=0.5, mapping = aes(geometry = geometry, col=male500))+
    #   scale_fill_viridis_c()
  }
  
  spsf$F2M500<-spsf$TOT_FEM500/spsf$TOT_MALE500*100 # feminization index - 500 meters (first-order neighborhood)
  spsf$F2MNeigh<-spsf$TOT_FEMNeigh/spsf$TOT_MALENeigh*100 # feminization index - second-order neighborhood
  
  spsf$pop500<-spsf$TOT_MALE500+spsf$TOT_FEM500 # Total population - 500 meters
  spsf$pop1500<-spsf$TOT_MALE1500+spsf$TOT_FEM1500 # Total population - 1500 meters
  spsf$popNeigh<-spsf$pop1500-spsf$pop500 # Total population - second-order neighborhood
  
  spsf$TOT_0_14500_proc<-ifelse(spsf$pop500>0, spsf$TOT_0_14500/sp$pop500,0)
  spsf$TOT_0_14Neigh_proc<-ifelse(spsf$popNeigh>0, spsf$TOT_0_14Neigh/spsf$popNeigh,0)
  
  spsf$TOT_65__500_proc<-ifelse(spsf$pop500>0, spsf$TOT_65__500/spsf$pop500,0)
  spsf$TOT_65__Neigh_proc<-ifelse(spsf$popNeigh>0, spsf$TOT_65__Neigh/spsf$popNeigh,0)
  
}

# Public transport - departures
{
  setwd("C:/Users/ewado/OneDrive/Dokumenty/Studia/R-mat/dataset_pkt/nowe")
  txt<-read.csv("stops.txt")
  txt_time<-read.csv("stop_times.txt")
  
  head(txt)
  head(txt_time)
  
  st<-data.frame(x=txt$stop_lon, y=txt$stop_lat, stop_id=txt$stop_id)
  head(st)
  
  txt_time2<- txt_time %>% 
    group_by(stop_id) %>% 
    summarise(N = n())
  
  st<-merge(st, txt_time2, by="stop_id")
  st<-st[,-1]
  
  points.sp<-SpatialPoints(st[,1:2])
  points.sp$N<-st$N
  
  proj4string(points.sp)<-CRS("+proj=longlat +datum=NAD83")
  points.sp<-spTransform(points.sp, CRS("+proj=longlat +datum=NAD83"))
  
  for(j in 1:nrow(spsf)){
    spsf$transport500[j]<-sum(points.sp$N[which(spDists(points.sp, sp[j,], T)<0.5)])
    spsf$transportNeigh[j]<-sum(points.sp$N[which(spDists(points.sp, sp[j,], T)>=0.5 & spDists(points.sp, sp[j,], T)<1.5)])
  }
  
}

# Public transport - metro
{
  variable.sf = opq(WAW_box) %>% 
    add_osm_feature(key='railway', value = 'subway_entrance') %>% 
    osmdata_sf()
  
  variable.sf$osm_points <- variable.sf$osm_points %>% 
    filter(st_within(variable.sf$osm_points %>% 
                       st_transform(crs = "+proj=longlat +datum=NAD83"), 
                     waw.pow.sf, sparse = F))
  
  variable.pts<-variable.sf$osm_points
  variable.pts.sp<-as(variable.pts, "Spatial")
  crs(variable.pts.sp)<-"+proj=longlat +datum=NAD83"

  for(j in 1:nrow(sp)){
  spsf$metro500[j]<-ifelse(any(spDists(variable.pts.sp, sp[j,], T)<=0.5), 1, 0)
  spsf$metroNeigh[j]<-ifelse(any(spDists(variable.pts.sp, sp[j,], T)>0.5 & spDists(variable.pts.sp, sp[j,], T)<1.5), 1, 0)
  }
  
}

# Transport - parking lots
{
  variable.sf = opq(WAW_box) %>% 
    add_osm_feature(key = "amenity", value = "parking") %>% 
    osmdata_sf() 
  
  variable.sf$osm_polygons<-variable.sf$osm_polygons[st_is_valid(variable.sf$osm_polygons),]
  
  variable.sf$osm_polygons  <- variable.sf$osm_polygons  %>% 
    filter(st_within(variable.sf$osm_polygons  %>% 
                       st_transform(crs = "+proj=longlat +datum=NAD83"), 
                     waw.pow.sf, sparse = F))%>% 
    st_make_valid()

  variable.sf$osm_multipolygons<-variable.sf$osm_multipolygons[st_is_valid(variable.sf$osm_multipolygons),]
  
  variable.sf$osm_multipolygons  <- variable.sf$osm_multipolygons  %>% 
    filter(st_within(variable.sf$osm_multipolygons  %>% 
                       st_transform(crs = "+proj=longlat +datum=NAD83"), 
                     waw.pow.sf, sparse = F))%>% 
    st_make_valid()
  
  variable.multi <- st_geometry(variable.sf$osm_multipolygons)
  variable.multi.aspoly<-variable.multi %>% st_cast("POLYGON") #z.multi0
  variable.poly<-st_geometry(variable.sf$osm_polygons)
  variable.poly.joined<-c(variable.poly,variable.multi.aspoly) #z.poly0
  
  union<-list()
  
  for(i in (0:12)){
    union[[i+1]]<-st_union(variable.poly.joined[i*1000+1:((i+1)*1000)])
  }
  
  union[[14]]<-st_union(variable.poly.joined[13001:13480])
  union2<-list()
  
  for(i in 0:6){
    union2[[i+1]]<-st_union(union[[2*i+1]], union[[2*i+2]])
  }
  
  union3<-list()
  
  for(i in 0:2){
    union3[[i+1]]<-st_union(union2[[2*i+1]], union[[2*i+2]])
  }
  
  union4<-st_union(union3[[1]], union3[[2]])
  union5<-st_union(union3[[3]], union2[[7]])
  
  variable.poly.final<-st_union(union4, union5)
  variable.poly.final.aspoly<-variable.poly.final %>% st_cast("POLYGON")
  
  spsf.g<-st_geometry(spsf)
  sp_circles500 <- st_buffer(spsf.g, dist = 500)
  variable.poly.final<-variable.poly.final %>% st_transform(crs = "+proj=longlat +datum=NAD83")
  parking.area500<-st_intersection(variable.poly.final, sp_circles500) %>% st_area #length 5492
  intersecting_circes<-st_intersects(variable.poly.final, sp_circles500) # length 5492
  ind<-intersecting_circes[[1]]
  spsf$parking500<-0
  spsf$parking500[ind]<-as.numeric(parking.area500)
  
  sp_circles1500 <- st_buffer(sp_g, dist = 1500)
  parking.area1500<-st_intersection(variable.poly.final, sp_circles1500) %>% st_area #length 5512
  intersecting_circes<-st_intersects(variable.poly.final, sp_circles1500)
  ind<-intersecting_circes[[1]]
  spsf$parking1500<-0
  spsf$parking1500[ind]<-as.numeric(parking.area1500)
  spsf$parkingNeigh<-spsf$parking1500 - spsf$parking500
}

# Leisure facilities
{ 
  
  key<-c(rep("amenity", 4), rep("leisure", 2), rep("tourism", 5))
  value<-c("casino", "arts_centre", "cinema", "theatre", "adult_gaming_centre", "dance", "artwork",
           "attraction", "zoo", "museum", "theme_park")
  
  variable.sf = opq(WAW_box) %>% 
    add_osm_feature(key=key[1], value = value[1]) %>% 
    osmdata_sf()
  
  for(i in 1:length(key)){
    variable.sf0 = opq(WAW_box) %>% 
    add_osm_feature(key=key[i], value = value[i]) %>% 
    osmdata_sf()
  
    variable.sf<-c(variable.sf, variable.sf0)
  }
  
  variable.sf$osm_points <- variable.sf$osm_points %>% 
    filter(st_within(variable.sf$osm_points %>% 
                       st_transform(crs = "+proj=longlat +datum=NAD83"), 
                     waw.pow.sf, sparse = F))
  
  variable.sf$osm_polygons  <- variable.sf$osm_polygons  %>% 
    filter(st_within(variable.sf$osm_polygons  %>% 
                       st_transform(crs = "+proj=longlat +datum=NAD83"), 
                     waw.pow.sf, sparse = F))
  
  variable.pts<-variable.sf$osm_points
  variable.poly<-variable.sf$osm_polygons
  variable.multi<-variable.sf$osm_multipolygons
  
  variable.pts.g<-variable.pts$geometry
  variable.poly.g<-variable.poly$geometry
  variable.union<-st_union(variable.poly)
  
  points.filtered <- filter(variable.pts, st_within(variable.pts, variable.union, sparse=F))
  
  variable.pts.g.df<-as.data.frame(variable.pts.g)
  points.filtered.df<-as.data.frame(points.filtered)
  variable.final<-anti_join(variable.pts.g.df, points.filtered.df) # Points outside polygons
  
  variable.final<-st_as_sf(variable.final)
  
  # ggplot()+
  #   geom_sf(waw.pow.sf, mapping=aes(geometry=geometry))+
  #   #geom_sf(z.poin, mapping=aes(geometry=geometry))+
  #   geom_sf(variable.poly.g, mapping=aes(geometry=geometry), fill="red")+
  #   geom_sf(variable.final, mapping=aes(geometry=geometry), size=0.05)+
  #   ggtitle("Black dots, red polygons")
  
  facilities1<-as(variable.final, "Spatial")
  facilities2<-SpatialPoints(as(variable.poly.g, "Spatial") %>% coordinates())
  crs(facilities1)<-"+proj=longlat +datum=NAD83"
  crs(facilities2)<-"+proj=longlat +datum=NAD83"
  facilities<-facilities1+facilities2
  
  
  for(j in 1:nrow(sp)){
    spsf$facilities500[j]<-length(which(spDists(facilities, sp[j,], T)<0.5))
    spsf$facilitiesNeigh[j]<-length(which(spDists(facilities, sp[j,], T)>=0.5 & spDists(facilities, sp[j,], T)<1.5))
  }
  
}

# Hotels
{
  variable.sf = opq(WAW_box) %>% 
    add_osm_feature(key='tourism', value = c("hostel", "guest_house", "hotel", "motel")) %>% 
    osmdata_sf()
  
  variable.sf$osm_points <- variable.sf$osm_points %>% 
    filter(st_within(variable.sf$osm_points %>% 
                       st_transform(crs = "+proj=longlat +datum=NAD83"), 
                     waw.pow.sf, sparse = F))
  
  variable.sf$osm_polygons  <- variable.sf$osm_polygons  %>% 
    filter(st_within(variable.sf$osm_polygons  %>% 
                       st_transform(crs = "+proj=longlat +datum=NAD83"), 
                     waw.pow.sf, sparse = F))

  variable.pts<-variable.sf$osm_points
  variable.poly<-variable.sf$osm_polygons
  variable.multi<-variable.sf$osm_multipolygons
  
  variable.pts.g<-variable.pts$geometry
  variable.poly.g<-variable.poly$geometry
  variable.union<-st_union(variable.poly)
  
  points.filtered <- filter(variable.pts, st_within(variable.pts, variable.union, sparse=F))
  
  variable.pts.g.df<-as.data.frame(variable.pts.g)
  points.filtered.df<-as.data.frame(points.filtered)
  variable.final<-anti_join(variable.pts.g.df, points.filtered.df) # Points outside polygons
  
  variable.final<-st_as_sf(variable.final)
  
  hotels1<-as(variable.final, "Spatial")
  hotels2<-SpatialPoints(as(variable.poly.g, "Spatial") %>% coordinates())
  crs(hotels1)<-"+proj=longlat +datum=NAD83"
  crs(hotels2)<-"+proj=longlat +datum=NAD83"
  hotels<-hotels1+hotels2
  

  for(j in 1:nrow(sp)){
    spsf$hotels500[j]<-length(which(spDists(hotels, sp[j,], T)<0.5))
    spsf$hotelsNeigh[j]<-length(which(spDists(hotels, sp[j,], T)>=0.5 & spDists(hotels, sp[j,], T)<1.5))
  }
}

# Schools/universities
{
  variable.sf = opq(WAW_box) %>% 
    add_osm_feature(key='amenity', value = c("college", "school", "university")) %>% 
    osmdata_sf()
  
  variable.sf$osm_polygons  <- variable.sf$osm_polygons  %>% 
    filter(st_within(variable.sf$osm_polygons  %>% 
                       st_transform(crs = "+proj=longlat +datum=NAD83"), 
                     waw.pow.sf, sparse = F))
  
  variable.poly<-variable.sf$osm_polygons

  schools<-as(variable.poly$geometry, "Spatial")
  crs(schools)<-"+proj=longlat +datum=NAD83"
  crs(sp)<-"+proj=longlat +datum=NAD83"
  
  schools1<-vector()
  schools1N<-vector()
  
  for(j in 1:nrow(sp)){
    
    schoolsdist<-spDists(schools, sp[j,], T)
    schools1[j]<-length(which(schoolsdist<0.5))
    schools1N[j]<-length(which(schoolsdist>=0.5 & schoolsdist<1.5))
    }
  
  spsf$schools500<-schools1
  spsf$schoolsNeigh<-schools1N
  
}

# Nature
{
  variable.sf = opq(WAW_box) %>% 
    add_osm_feature(key = "landuse", value = "forest") %>% 
    osmdata_sf() 
  
  variable.sf2 = opq(WAW_box) %>% 
    add_osm_feature(key = "leisure", value = "park") %>% 
    osmdata_sf() 
  
  variable.sf<-c(variable.sf, variable.sf2)
  waw_buff <- st_buffer(waw.pow.sf, dist = 1500)
  
  variable.sf$osm_polygons  <- variable.sf$osm_polygons  %>% 
    filter(st_within(variable.sf$osm_polygons  %>% 
                       st_transform(crs = "+proj=longlat +datum=NAD83"), 
                     waw_buff, sparse = F))%>% 
    st_make_valid()
  
  variable.sf$osm_multipolygons  <- variable.sf$osm_multipolygons  %>% 
    filter(st_within(variable.sf$osm_multipolygons  %>% 
                       st_transform(crs = "+proj=longlat +datum=NAD83"), 
                     waw_buff, sparse = F))%>% 
    st_make_valid()
  
  variable.sf$osm_multipolygons <- st_transform(variable.sf$osm_multipolygons, crs = "+proj=longlat +datum=NAD83") %>% 
    st_make_valid()
  
  variable.sf$osm_polygons <- st_transform(variable.sf$osm_polygons, crs = "+proj=longlat +datum=NAD83") %>% 
    st_make_valid()
  
  variable.multi <- variable.sf$osm_multipolygons
  variable.multi.aspoly.g<-variable.multi %>% st_cast("POLYGON") %>% st_geometry
  variable.poly<-st_geometry(variable.sf$osm_polygons)
  variable.poly.joined<-c(variable.poly,variable.multi.aspoly.g)
  variable.poly.union<-st_union(variable.poly.joined)
  variable.poly.final<-variable.poly.union %>% st_cast("POLYGON")
  
  variable.poly.union<-variable.poly.union %>% st_transform(crs = "+proj=longlat +datum=NAD83")
  nature.area500<-st_intersection(variable.poly.union, sp_circles) %>% st_area 
  intersecting_circes<-st_intersects(variable.poly.union, sp_circles500) 
  ind<-intersecting_circes[[1]]
  spsf$nature500<-0
  spsf$nature500[ind]<-as.numeric(nature.area500)
  
  nature.area1500<-st_intersection(variable.poly.union, sp_circles1500) %>% st_area 
  intersecting_circes<-st_intersects(variable.poly.union, sp_circles1500) 
  ind<-intersecting_circes[[1]]
  spsf$nature1500<-0
  spsf$nature1500[ind]<-as.numeric(nature.area1500)
  spsf$natureNeigh<-spsf$nature1500-spsf$nature500
  
  }

# Clusters - restaurants
{
  sp_r<-sp[sp$type=="Restaurant",]
  rest<-df[df$type=="Restaurant",4:5]
  cluster_r<-dbscan(rest, eps=0.003, minPts=5)
  hullplot(rest, cluster_r, add=T, pch=16, main="Restaurants clusters", expand=-10, xlab="longitude", ylab="latitude")
  
  plot(waw.pov, add=T)

  clusterN<-as.numeric(cluster_r[[1]])
  
  sp_r$clusterR<-ifelse(clusterN==0, 0, 1)
  
  sp_cv<-sp[sp$type=="Store",]
  cv<-df[df$type=="Store",4:5]
  pred_cv<-predict(cluster_r, newdata=cv, data=rest)
  predN<-as.numeric(pred_cv)
  sp_cv$clusterR<-ifelse(predN==0, 0, 1)
  
  spsf$clusterR<--1
  spsf$clusterR[spsf$type=="Restaurant"]<-sp_r$clusterR
  spsf$clusterR[spsf$type=="Store"]<-sp_cv$clusterR
}

# Clusters - stores
{
  cluster_cv<-dbscan(cv, eps=0.003, minPts=5)
  hullplot(cv, cluster_cv, pch=16, main="Convenience Stores clusters", solid=T, xlab="longitude", ylab="latitude")
  plot(waw.pov, add=T)
  
  clusterN<-as.numeric(cluster_cv[[1]])
  
  sp_cv$clusterCV<-ifelse(clusterN==0, 0, 1)
  
  pred_r<-predict(cluster_cv, newdata=rest, data=cv)
  predN<-as.numeric(pred_r)
  sp_r$clusterCV<-ifelse(predN==0, 0, 1)
  
  spsf$clusterCV<--1
  spsf$clusterCV[sp$type=="Restaurant"]<-sp_r$clusterCV
  spsf$clusterCV[sp$type=="Store"]<-sp_cv$clusterCV
  
}

# Shopping mall
{
  variable.sf = opq(WAW_box) %>% 
    add_osm_feature(key='shop', value = "mall") %>% 
    osmdata_sf()
  
  variable.sf$osm_polygons  <- variable.sf$osm_polygons  %>% 
    filter(st_within(variable.sf$osm_polygons  %>% 
                       st_transform(crs = "+proj=longlat +datum=NAD83"), 
                     waw.pow.sf, sparse = F))
  
  variable.sf$osm_multipolygons   <- variable.sf$osm_multipolygons   %>% 
    filter(st_within(variable.sf$osm_multipolygons   %>% 
                       st_transform(crs = "+proj=longlat +datum=NAD83"), 
                     waw.pow.sf, sparse = F))
 
  variable.multi <- st_geometry(variable.sf$osm_multipolygons)
  variable.multi.aspoly<-variable.multi %>% st_cast("POLYGON")
  variable.poly<-st_geometry(variable.sf$osm_polygons)
  variable.poly.joined<-c(variable.poly,variable.multi.aspoly)
  variable.final<-st_union(variable.poly.joined)
  
  variable.final<-variable.final %>% st_transform(crs = "+proj=longlat +datum=NAD83")
  a<-st_covers(variable.final, spsf)
  spsf$mall<-0
  spsf$mall[a[[1]]]<-1 

}

# The end
{
  dataset<-spsf[,c("dist_center", "F2M500", "F2MNeigh", "pop500", "popNeigh", "TOT_0_14500_proc", "TOT_0_14Neigh_proc", "TOT_65__500_proc",
                   "TOT_65__Neigh_proc", "transport500", "transportNeigh", "metro500", "metroNeigh", "facilities500", "facilitiesNeigh",
                   "hotels500", "hotelsNeigh", "schools500", "schoolsNeigh", "parking500", "nature500", "natureNeigh","clusterR",
                   "clusterCV", "parkingNeigh")]
  
  dataset<-cbind(df[,4:5], dataset)
  rownames(dataset)<-c()
  colnames(dataset)[3:27]<-c("d_centre", "f2m_500", "f2m_N", "pop_500", "pop_N", "child_pct_500", "child_pct_N", "elderly_pct_500",
                           "elderly_pct_N", "transport_500", "transport_N", "metro_500", "metro_N", "facility_500", "facility_N",
                           "hotels_500", "hotels_N", "schools_500", "schools_N", "parking_500", "nature_500", "nature_N","cluster_R",
                           "cluster_CV", "parking_N")
  names(dataset)
  dataset$f2m_500<-ifelse(is.na(dataset$f2m_500), 0, dataset$f2m_500)
  
  write.csv2(dataset, "ae_dataset.csv")
  
}

