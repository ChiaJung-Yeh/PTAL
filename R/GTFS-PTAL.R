library(dplyr)
library(data.table)
library(sf)
library(stringr)
library(ggplot2)
library(xlsx)
library(lubridate)


osm_data=read.csv("https://raw.githubusercontent.com/ChiaJung-Yeh/GTFS-PTAL/refs/heads/main/data/osm_region_geofabrik.csv")%>%
  mutate(region=gsub("\\.", "", gsub("\\/", "-", region)))
usethis::use_data(osm_data, overwrite=T)


#### Download OSM Network ####
#' @export
OSM_Network=function(country, district=NULL){
  if(is.null(district)){
    osm_region=filter(osm_region_all, region==country)
  }else{
    osm_region=filter(osm_region_all, region==country, subregion==district)
  }

  if(nrow(osm_region)==0){
    stop("Please use the valid country and district name! Please use '' to  check out the valid country and disteict name!")
  }
  select(osm_region_all, continent, region, subregion)
  usethis::use_data(TDX_County, overwrite=T)

}


for(i in c(1:nrow(osm_region))){
  tempdir()
  if(nrow(osm_region)==1){
    download.file(osm_region$osm_pbf, paste0("Feature/OSM/", all_country[i], "/", osm_region$region, ".osm.pbf"), mode="wb")
    gdal_utils(
      util="vectortranslate",
      source=paste0("Feature/OSM/", all_country[i], "/", osm_region$region, ".osm.pbf"),
      destination=paste0("Feature/OSM/", all_country[i], "/", osm_region$region ,".gpkg"),
      options = c("-f", "GPKG")
    )
  }else{
    for(j in c(1:nrow(osm_region))){
      dir.create(paste0("Feature/OSM/", all_country[i], "/", osm_region$subregion[j]))
      if(grepl("russia", osm_region$osm_pbf[j])){
        url=gsub("/europe/", "/", osm_region$osm_pbf[j])
      }else{
        url=osm_region$osm_pbf[j]
      }
      download.file(url, paste0("Feature/OSM/", all_country[i], "/", osm_region$subregion[j], "/", osm_region$subregion[j], ".osm.pbf"), mode="wb")
      gdal_utils(
        util="vectortranslate",
        source=paste0("Feature/OSM/", all_country[i], "/", osm_region$subregion[j], "/", osm_region$subregion[j], ".osm.pbf"),
        destination=paste0("Feature/OSM/", all_country[i], "/", osm_region$subregion[j], "/", osm_region$subregion[j] ,".gpkg"),
        options = c("-f", "GPKG")
      )
    }
  }
}







