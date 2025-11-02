library(dplyr)
library(data.table)
library(sf)
library(stringr)
library(lubridate)

# usethis::use_package("dplyr")
# usethis::use_package("data.table")
# usethis::use_package("sf")
# usethis::use_package("stringr")
# usethis::use_package("lubridate")


# osm_data=read.csv("https://raw.githubusercontent.com/ChiaJung-Yeh/PTAL/refs/heads/main/data/osm_region_geofabrik.csv")%>%
#   mutate(region=gsub("\\.", "", gsub("\\/", "-", region)))%>%
#   select(continent, region, subregion)
# usethis::use_data(osm_data, overwrite=T)


#### Download OSM Network ####
#' @export
OSM_Network=function(country, district=NULL){
  osm_region=read.csv("https://raw.githubusercontent.com/ChiaJung-Yeh/PTAL/refs/heads/main/data/osm_region_geofabrik.csv")%>%
    mutate(region=gsub("\\.", "", gsub("\\/", "-", region)))
  DIRTEMP=tempdir(check=T)


  if(is.null(district)){
    osm_region=filter(osm_region, region==country)
  }else{
    osm_region=filter(osm_region, region==country, subregion==district)
  }

  if(nrow(osm_region)==0){
    stop("Please use the valid country and district name! Please use 'osm_data' to  check out the valid region (country) and subregion (district) name!")
  }

  for(i in c(1:nrow(osm_region))){
    finame=ifelse(osm_region$subregion[i]!="", paste0(osm_region$region[i], "_", osm_region$subregion[i]), osm_region$region[i])
    download.file(osm_region$osm_pbf[i], paste0(DIRTEMP, "/", finame, ".osm.pbf"), mode="wb")
    gdal_utils(
      util="vectortranslate",
      source=paste0(DIRTEMP, "/", finame, ".osm.pbf"),
      destination=paste0(DIRTEMP, "/", finame,".gpkg"),
      options = c("-f", "GPKG"),
      quiet=T
    )
    road_sf=st_read(paste0(DIRTEMP, "/", finame,".gpkg"), layer="lines")[, c("osm_id","name","highway","other_tags")]
  }

  unlink(DIRTEMP, recursive=T)
  return(road_sf)
}










