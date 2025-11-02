library(dplyr)
library(data.table)
library(sf)
library(stringr)
library(lubridate)
library(reticulate)
library(xml2)

# usethis::use_package("dplyr")
# usethis::use_package("data.table")
# usethis::use_package("sf")
# usethis::use_package("stringr")
# usethis::use_package("lubridate")
# usethis::use_package("reticulate")
# usethis::use_package("xml2")


# osm_data=read.csv("https://raw.githubusercontent.com/ChiaJung-Yeh/PTAL/refs/heads/main/other_data/osm_region_geofabrik.csv")%>%
#   mutate(region=gsub("\\.", "", gsub("\\/", "-", region)))%>%
#   select(continent, region, subregion)
# usethis::use_data(osm_data, overwrite=T)



#### Planit Setup ####
#' @export
Planit_Setup=function(env){
  if (!require(reticulate)) install.packages("reticulate")

  if(sum(env==reticulate::conda_list()$name)==0){
    reticulate::conda_create(env)
  }
  use_condaenv(env)
  if(!py_module_available("planit")){
    py_install("PLANit-Python", pip=T)
    py_install("numpy==1.26.4", pip = TRUE)
  }

  py_run_string("
from planit import *
import subprocess, sys
from subprocess import Popen as og_Popen

def safe_popen(*args, **kwargs):
    kwargs['stdin'] = subprocess.DEVNULL
    kwargs['stdout'] = subprocess.DEVNULL
    kwargs['stderr'] = subprocess.DEVNULL
    return og_Popen(*args, **kwargs)

subprocess.Popen = safe_popen
")
  py_run_string("
planit_instance = Planit()
network_converter = planit_instance.converter_factory.create(ConverterType.NETWORK)
")
  print("Setup Successfully!!")
}



#### Download OSM Network ####
#' @export
OSM_Network=function(country, district=NULL, env, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(sf)) install.packages("sf")
  if (!require(xml2)) install.packages("xml2")
  if (!require(reticulate)) install.packages("reticulate")
  options(timeout=1000)

  osm_region=read.csv("https://raw.githubusercontent.com/ChiaJung-Yeh/PTAL/refs/heads/main/other_data/osm_region_geofabrik.csv")%>%
    mutate(region=gsub("\\.", "", gsub("\\/", "-", region)))
  DIRTEMP=gsub("\\\\", "/", tempdir(check=T))

  if(is.null(district)){
    osm_region=filter(osm_region, region==country)
  }else{
    osm_region=filter(osm_region, region==country, subregion==district)
  }

  if(nrow(osm_region)==0){
    stop("Please use the valid country and district name! Check out 'osm_data' for the valid region (country) and subregion (district) name!")
  }

  cat("Download OSM Data...\n")
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
    road_sf=st_read(paste0(DIRTEMP, "/", finame,".gpkg"), layer="lines", quiet=T)[, c("osm_id","name","highway","other_tags")]
  }


  cat("Convert OSM to Network...\n")
  py_run_string(paste0("osm_reader = network_converter.create_reader(NetworkReaderType.OSM, '", country, "')"))
  py_run_string(paste0("osm_reader.settings.set_input_file('", paste0(DIRTEMP, "/", finame, ".osm.pbf"), "')"))
  py_run_string("planit_writer = network_converter.create_writer(NetworkWriterType.PLANIT)")
  py_run_string(paste0("planit_writer.settings.set_output_directory('", paste0(DIRTEMP, "/", finame), "')"))
  py_run_string("network_converter.convert(osm_reader, planit_writer)")


  cat("Extract Node & Link...\n")
  doc=xml2::read_xml(paste0(DIRTEMP, "/", finame, "/network.xml"))

  # Extract nodes
  nodes_xml=xml_find_all(doc, ".//node")
  nodes=data.frame(nodeid=xml_attr(nodes_xml, "id"),
                   externalid=xml_attr(nodes_xml, "externalid"),
                   coords=xml_text(xml_find_first(nodes_xml, ".//gml:pos", xml_ns(doc))))%>%
    tidyr::separate(coords, into=c("lat","lon"), sep=" ") %>%
    mutate(lat=as.numeric(lat), lon=as.numeric(lon))
  setDT(nodes)

  # Extract links
  links_xml=xml_find_all(doc, ".//link")
  links=data.frame(
    linkid=xml_attr(links_xml, "id"),
    name=xml_text(xml_find_first(links_xml, "name")),
    externalid=xml_attr(links_xml, "externalid"),
    fromnode=xml_attr(links_xml, "nodearef"),
    tonode=xml_attr(links_xml, "nodebref"),
    geometry=xml_text(xml_find_all(links_xml, ".//gml:coordinates"))
  )
  setDT(links)

  temp=st_drop_geometry(road_sf)[, c("osm_id","highway")]
  setDT(temp)
  links=merge.data.table(links, temp, by.x="externalid", by.y="osm_id")

  links_geo=select(links, linkid, geometry)
  links_geo$geometry=lapply(strsplit(links_geo$geometry, " "), function(x) matrix(as.numeric(unlist(strsplit(x, ","))), ncol=2, byrow=T)[,2:1])%>%
    lapply(function(x) st_linestring(x))
  links_geo=st_sf(links_geo, crs=4326)

  # Extract link segments
  link_seg_xml=xml_find_all(doc, ".//link//linksegment")
  temp=xml_attr(xml_find_first(link_seg_xml, ".."), "id")
  link_seg=data.frame(linkid=temp,
                      linksegmentid=xml_attr(link_seg_xml, "id"),
                      dir=xml_attr(link_seg_xml, "dir"),
                      typeref=xml_attr(link_seg_xml, "typeref"),
                      numberoflanes=xml_text(xml_find_first(link_seg_xml, "numberoflanes")),
                      maxspeed=xml_text(xml_find_first(link_seg_xml, "maxspeed")))
  setDT(link_seg)


  if (nchar(out)!=0 & out!=F){
    fwrite(nodes, paste0(out, "/", finame, "_node.csv"))
    fwrite(links, paste0(out, "/", finame, "_link.csv"))
    fwrite(link_seg, paste0(out, "/", finame, "_linksegment.csv"))
    write_sf(road_sf, paste0(out, "/", finame, "_osmline.gpkg"))
    write_sf(links_geo, paste0(out, "/", finame, "_linkgeo.shp"))
  }

  unlink(DIRTEMP, recursive=T)
  return(list(road_sf=road_sf, nodes=nodes, links=links, link_seg=link_seg, links_geo=links_geo))
}



