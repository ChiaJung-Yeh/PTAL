library(dplyr)
library(data.table)
library(sf)
library(stringr)
library(lubridate)
library(fasttime)
library(reticulate)
library(xml2)
library(dodgr)

#' @import data.table

# usethis::use_package("dplyr")
# usethis::use_package("data.table")
# usethis::use_package("sf")
# usethis::use_package("stringr")
# usethis::use_package("lubridate")
# usethis::use_package("fasttime")
# usethis::use_package("reticulate")
# usethis::use_package("xml2")
# usethis::use_package("dodgr")


# osm_data=read.csv("https://raw.githubusercontent.com/ChiaJung-Yeh/PTAL/refs/heads/main/other_data/osm_region_geofabrik.csv")%>%
#   mutate(region=gsub("\\.", "", gsub("\\/", "-", region)))%>%
#   select(continent, region, subregion)
# greater_sydney=read_sf("G:/AU Data/Digital Boundary/Significant Urban Areas (SUA)")%>%
#   filter(SUA_NAME21=="Sydney")%>%
#   select(SUA_CODE21, SUA_NAME21)

# usethis::use_data(osm_data, overwrite=T)
# usethis::use_data(greater_sydney, overwrite=T)



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
    py_install("numpy==1.26.4", pip=T)
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

  tryCatch({
    py_run_string(paste0("osm_reader = network_converter.create_reader(NetworkReaderType.OSM, '", "Australia", "')"))
  }, error=function(err){
    stop("Please restart R to ensure PLANit runs correctly.")
  })

  print("Setup Successfully!!")
}



#### Download OSM Network ####
#' @export
OSM_Network=function(country, district=NULL, bbox=NULL, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(data.table)) install.packages("data.table")
  if (!require(sf)) install.packages("sf")
  if (!require(xml2)) install.packages("xml2")
  if (!require(reticulate)) install.packages("reticulate")
  if (!require(dodgr)) install.packages("dodgr")
  options(timeout=1000)

  osm_region=read.csv("https://raw.githubusercontent.com/ChiaJung-Yeh/PTAL/refs/heads/main/other_data/osm_region_geofabrik.csv")%>%
    mutate(region=gsub("\\.", "", gsub("\\/", "-", region)))
  DIRTEMP=gsub("\\\\", "/", tempdir(check=T))

  if(!is.null(bbox)){
    if(st_crs(bbox)$epsg!=4326){
      stop("Please ensure that the CRS of box is EPSG:4326!")
    }
  }

  if(is.null(district)){
    osm_region=filter(osm_region, region==country)
  }else{
    osm_region=filter(osm_region, region==country, subregion==district)
  }

  if(nrow(osm_region)==0){
    stop("Please use the valid country and district name! Check out 'osm_data' for the valid region (country) and subregion (district) name!")
  }else if(nrow(osm_region)>1){
    stop("This function can only work on a single region/subregion. Please choose one of the following subregions:\n", paste(osm_region$subregion, collapse=", "))
  }

  cat("Download OSM Data...\n")
  finame=ifelse(osm_region$subregion!="", paste0(osm_region$region, "_", osm_region$subregion), osm_region$region)
  download.file(osm_region$osm_pbf, paste0(DIRTEMP, "/", finame, ".osm.pbf"), mode="wb")

  if(!is.null(bbox)){
    cat("Clip OSM Data...\n")
    if(sum(dir(getwd())=="osmconvert.exe")==0){
      download.file("https://raw.githubusercontent.com/ChiaJung-Yeh/PTAL/refs/heads/main/other_data/osmconvert64-0.8.8p.exe", paste0(getwd(), "/osmconvert.exe"), mode="wb")
    }

    system2(paste0(getwd(), "/osmconvert.exe"),
            c(paste0(DIRTEMP, "/", country, ".osm.pbf"),
              paste0("-b=", paste(bbox, collapse=",")),
              paste0("-o=", paste0(DIRTEMP, "/", country, "_final.osm.pbf"))))
  }else{
    file.rename(paste0(DIRTEMP, "/", finame, ".osm.pbf"), gsub(".osm.pbf", "_final.osm.pbf", paste0(DIRTEMP, "/", finame, ".osm.pbf")))
  }
  gc()

  gdal_utils(
    util="vectortranslate",
    source=paste0(DIRTEMP, "/", finame, "_final.osm.pbf"),
    destination=paste0(DIRTEMP, "/", finame,"_final.gpkg"),
    options=c("-f", "GPKG"),
    quiet=T
  )
  road_sf=st_read(paste0(DIRTEMP, "/", finame, "_final.gpkg"), layer="lines", quiet=T)


  cat("Convert OSM to Network...\n")
  py_run_string(paste0("osm_reader = network_converter.create_reader(NetworkReaderType.OSM, '", country, "')"))
  py_run_string(paste0("osm_reader.settings.set_input_file('", paste0(DIRTEMP, "/", finame, "_final.osm.pbf"), "')"))
  py_run_string("planit_writer = network_converter.create_writer(NetworkWriterType.PLANIT)")
  py_run_string(paste0("planit_writer.settings.set_output_directory('", DIRTEMP, "')"))
  py_run_string("network_converter.convert(osm_reader, planit_writer)")


  cat("Extract Node & Link...\n")
  doc=xml2::read_xml(paste0(DIRTEMP, "/network.xml"))

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
  links=merge.data.table(links, temp, by.x="externalid", by.y="osm_id", all.x=T)

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


  # Network data
  cat("Create Network Dataset...\n")
  road_link=rename(links, way=highway)%>%
    filter(way!="")%>%
    dplyr::select(-geometry)%>%
    left_join(links_geo)%>%
    st_sf(crs=4326)

  wts=distinct(st_drop_geometry(road_link), way)%>%
    left_join(filter(dodgr::weighting_profiles[[1]], name=="foot"), by=c("way"), by="linkid")%>%
    select(name, way, value)
  road_net=weight_streetnet(road_link, wt_profile=wts, type_col="way", id_col="linkid")


  if (nchar(out)!=0 & out!=F){
    fwrite(nodes, paste0(out, "_node.csv"))
    fwrite(links, paste0(out, "_link.csv"))
    fwrite(link_seg, paste0(out, "_linksegment.csv"))
    write_sf(road_sf, paste0(out, "_osmline.gpkg"))
    write_sf(links_geo, paste0(out, "_linkgeo.shp"))
    saveRDS(road_net, paste0(out, "_road_net.rds"))
  }

  unlink(DIRTEMP, recursive=T)
  return(list(road_sf=road_sf, nodes=nodes, links=links, link_seg=link_seg, links_geo=links_geo, road_net=road_net))
}



#### Read GTFS Data ####
#' @export
read_gtfs=function(path, crs){
  if (!require(data.table)) install.packages("data.table")
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(sf)) install.packages("sf")

  DIRTEMP=gsub("\\\\", "/", tempdir(check=T))

  if(grepl(".zip", path)){
    untar(path, exdir=paste0(DIRTEMP, "/gtfs"))
    dir_files=dir(paste0(DIRTEMP, "/gtfs"), full.names=T)
  }else{
    dir_files=dir(path, full.names=T)
  }

  fir_files=dir_files[grepl(paste(c("stop","routes","calendar","trips"), collapse="|"), dir_files)]
  all_dt=list()
  for(i in fir_files){
    all_dt[[gsub(".csv|.txt", "", tail(unlist(strsplit(i, "/")), 1))]]=fread(i)
  }

  all_dt$stops=filter(all_dt$stops, !is.na(stop_lon), !is.na(stop_lat))%>%
    st_as_sf(coords=c("stop_lon", "stop_lat"), crs=4326, remove=F)%>%
    st_transform(crs=crs)

  if(length(st_crs(all_dt$stops)$units)==0){
    warning("The specified CRS is not projected coordinate reference system. Please use an appropriate CRS for calculating distances correctly.")
  }

  unlink(DIRTEMP, recursive=T)
  return(all_dt)
}



#### GTFS Summary ####
#' @export
gtfs_summary=function(gtfs, gtfs_mode, test_date, time_period="08:15~09:15", tz){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(data.table)) install.packages("data.table")
  if (!require(sf)) install.packages("sf")
  if (!require(lubridate)) install.packages("lubridate")
  if (!require(fasttime)) install.packages("fasttime")

  requireNamespace("data.table")

  tryCatch({
    wod=tolower(lubridate::wday(test_date, label=T, abbr=F, locale="en"))
  }, error=function(err){
    stop("The format of 'test_date' must be 'YYYY-MM-DD'.")
  })
  if(!grepl("^([01]\\d|2[0-3]):[0-5]\\d~([01]\\d|2[0-3]):[0-5]\\d$", time_period)){
    stop("The format of 'time_period' must be 'hh:mm~hh:mm'.")
  }

  temp=filter(gtfs$calendar_dates, exception_type==2)%>%
    mutate(date=as.Date(as.character(date), format="%Y%m%d"))%>%
    filter(date==test_date)
  calendar=gtfs$calendar%>%
    mutate(start_date=as.Date(as.character(start_date), format="%Y%m%d"),
           end_date=as.Date(as.character(end_date), format="%Y%m%d"))%>%
    filter(end_date>=test_date, start_date<=test_date, !!sym(wod)==1, !service_id %in% temp$service_id)

  trips=gtfs$trips[service_id %in% calendar$service_id]

  temp=unlist(strsplit(time_period, "~"))
  stop_times=gtfs$stop_times[trip_id %in% trips$trip_id]%>%
    mutate(arrival_time=force_tz(fastPOSIXct(paste0(test_date, " ", arrival_time), tz="GMT"), tz),
           departure_time=force_tz(fastPOSIXct(paste0(test_date, " ", departure_time), tz="GMT"), tz))
  stop_times=stop_times[arrival_time>=as.POSIXct(paste0(test_date, " ", temp[1]), tz) & stop_times$arrival_time<=as.POSIXct(paste0(test_date, " ", temp[2]), tz)]%>%
    merge.data.table(unique(trips[, c("route_id","trip_id","direction_id")]), by="trip_id")
  stop_times_sum=stop_times[, by=.(stop_id, route_id, direction_id), .(Trips=.N)]%>%
    merge.data.table(unique(gtfs$routes[, c("route_id","route_type")]), by="route_id")%>%
    merge.data.table(gtfs_mode, by="route_type")

  stop_route=unique(merge.data.table(unique(gtfs$stop_times[, c("stop_id","trip_id")]), unique(gtfs$trips[, c("route_id","trip_id")]), by="trip_id")[, c("stop_id","route_id")])%>%
    merge.data.table(unique(gtfs$routes[, c("route_type","route_id")]), by="route_id")%>%
    merge.data.table(gtfs_mode, by="route_type")%>%
    dplyr::select(stop_id, mode_type, mode)%>%
    unique()
  stop_route=stop_route[, by=.(stop_id), .(mode_type=paste(sort(unique(mode_type)), collapse="|"))]

  return(list(stop_times_sum=stop_times_sum, stop_route=stop_route))
}



#### Calculate PTAL ####
#' @export
gtfs_ptal=function(sarea_center, gtfs, stop_times_sum, stop_route, road_net, gtfs_mode,
                   walk_kph=4.8, bus_dist=640, rail_dist=960, ferry_dist=960){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(data.table)) install.packages("data.table")
  if (!require(sf)) install.packages("sf")
  if (!require(dodgr)) install.packages("dodgr")

  if(sum(class(sarea_center)=="sf")==0){
    stop("Please provide a 'sf' data for 'sarea_center'.")
  }
  if(length(st_crs(sarea_center)$units)==0){
    stop("The specified CRS is not projected coordinate reference system. Please use an appropriate CRS for calculating distances correctly.")
  }
  if(st_crs(gtfs$stops)$epsg!=st_crs(sarea_center)$epsg){
    stop("Please make sure that the CRS of both data 'sarea_center' and 'gtfs$stops' are identical.")
  }


  cat("Match PT stops for each grid...\n")
  sarea_center$GridID=as.character(sarea_center$GridID)
  temp_id=st_intersects(st_buffer(sarea_center, 1000), gtfs$stops)

  temp=st_transform(sarea_center[,"GridID"], crs=4326)%>%
    cbind(st_coordinates(.))%>%
    st_drop_geometry()%>%
    rename(X_O=X, Y_O=Y)
  grid_stop=data.table(temp[rep(c(1:nrow(sarea_center)), times=lengths(temp_id)),],
                       st_drop_geometry(gtfs$stops)[unlist(temp_id), c("stop_id","stop_name","stop_lat","stop_lon","parent_station")] %>% rename(X_D=stop_lon, Y_D=stop_lat))

  # include all exit locations of the parent stations (i.e., find the shortest path based on the location of exit, instead of platform)
  temp=st_drop_geometry(gtfs$stops)[gtfs$stops$parent_station %in% grid_stop$parent_station & location_type==2, c("stop_id","stop_lat","stop_lon","parent_station")]%>%
    rename(stop_exit_id=stop_id, X_D=stop_lon, Y_D=stop_lat)
  temp=grid_stop[parent_station %in% temp$parent_station, c("GridID","X_O","Y_O","stop_id","parent_station")]%>%
    merge.data.table(temp, by="parent_station", allow.cartesian=T)
  grid_stop=bind_rows(grid_stop, temp)

  if(nrow(grid_stop)==0){
    grid_edf_sum=data.frame(GridID=sarea_center$GridID, EDF=0)
    return(list(grid_edf_sum=grid_edf_sum, grid_edf=NA))
  }



  cat("Routing analysis (walking distance)...\n")
  temp=range(grid_stop$X_O, grid_stop$X_D)+c(-0.001,0.001)
  road_net_temp=road_net[road_net$from_lon>=temp[1] & road_net$to_lon>=temp[1] & road_net$from_lon<=temp[2] & road_net$to_lon<=temp[2],]
  temp=range(grid_stop$Y_O, grid_stop$Y_D)+c(-0.001,0.001)
  road_net_temp=road_net_temp[road_net_temp$from_lat>=temp[1] & road_net_temp$to_lat>=temp[1] & road_net_temp$from_lat<=temp[2] & road_net_temp$to_lat<=temp[2],]

  # route_pair=dodgr_paths(road_net_temp, grid_stop[, c("X_O","Y_O")], grid_stop[, c("X_D","Y_D")], pairwise=T)
  # route_pair_od=data.table(grid_stop[rep(c(1:nrow(grid_stop)), times=unlist(lapply(route_pair, function(x) length(x[[1]])))), c("GridID","stop_id")],
  #                          from_id=unlist(route_pair))
  # route_pair_od[, to_id := shift(from_id, type="lead"), by=.(GridID, stop_id)]
  # route_pair_od=route_pair_od[!is.na(to_id)]
  # route_pair_od=left_join(route_pair_od, road_net_sf)
  # setDT(route_pair_od)

  all_od=unique(grid_stop[, c("X_O","Y_O","X_D","Y_D")])
  route_pair_dist=dodgr_dists(road_net_temp, all_od[, c("X_O","Y_O")], all_od[, c("X_D","Y_D")], pairwise=T)
  all_od$Distance=as.numeric(route_pair_dist)
  grid_stop=merge.data.table(grid_stop, all_od, by=c("X_O","Y_O","X_D","Y_D"))
  grid_stop=grid_stop[!is.na(Distance)]

  # select the distance with the closest exit
  grid_stop=grid_stop[grid_stop[, .I[which.min(Distance)], by=.(GridID, stop_id)]$V1]



  cat("PTAL calculation...\n")
  grid_stop_times=merge.data.table(grid_stop, stop_times_sum, by="stop_id")
  grid_stop_times=grid_stop_times[grid_stop_times[, .I[which.min(Distance)], by=.(GridID, route_id)]$V1]

  grid_stop_times=mutate(grid_stop_times, TEMP=case_when(
    mode=="rail" & Distance<=rail_dist ~ 1,
    mode=="bus" & Distance<=bus_dist ~ 1,
    mode=="ferry" & Distance<=ferry_dist ~ 1,
    TRUE ~ 0
  ))

  grid_stop_times=grid_stop_times[TEMP==1]%>%
    dplyr::select(-TEMP)
  grid_stop_times$Reliability=ifelse(grid_stop_times$mode=="rail", 0.75, 2)

  # # PTAL considers directions in a simplified way. If a service runs in both directions, the most frequent direction is used in the calculation
  # grid_stop_times=grid_stop_times[grid_stop_times[, .I[which.max(Trips)], by=.(GridID, stop_id, route_id)]$V1]

  grid_edf=mutate(grid_stop_times, WalkTime=Distance/1000/walk_kph*60,
                  SWT=0.5*(60/Trips), AWT=SWT+Reliability, TAT=WalkTime+AWT,
                  EDF=0.5*(60/TAT))

  suppressWarnings({grid_edf[, weight := ifelse(EDF==max(EDF), 1, 0.5), by=.(GridID, mode_type)]})

  grid_edf_sum=grid_edf[, by=.(GridID), .(EDF=sum(EDF*weight))]
  # grid_edf_sum=mutate(grid_edf_sum, PTAL=case_when(
  #   EDF==0 ~ "0",
  #   EDF<=2.5 ~ "1a",
  #   EDF<=5 ~ "1b",
  #   EDF<=10 ~ "2",
  #   EDF<=15 ~ "3",
  #   EDF<=20 ~ "4",
  #   EDF<=25 ~ "5",
  #   EDF<=40 ~ "6a",
  #   EDF>40 ~ "6b",
  #   TRUE ~ "0"
  # ), PTAL=factor(PTAL, levels=c("0","1a","1b","2","3","4","5","6a","6b")))%>%
  #   left_join(data.frame(PTAL=c("0","1a","1b","2","3","4","5","6a","6b"),
  #                        PTAL_col=c("white","#16497D","#116FB8","#27ADE3","#91C953","#FFF101","#FBC08E","#EE1D23","#841517")))

  # grid_edf_sum=merge.data.table(st_drop_geometry(sarea_center)[, "GridID"], grid_edf_sum, sort=F, all.x=T)%>%
  #   mutate(EDF=ifelse(is.na(EDF), 0, EDF))
  grid_edf_sum=left_join(st_drop_geometry(sarea_center)[, "GridID"], grid_edf_sum)%>%
    mutate(EDF=ifelse(is.na(EDF), 0, EDF))

  return(list(grid_edf=grid_edf, grid_edf_sum=grid_edf_sum))
}



