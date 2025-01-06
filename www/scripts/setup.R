#! Setting up workspace
## Modifying options
options(scipen = 999)
options(timeout = 999)
## Installing and loading required packages
### Commenting out because it is not good practice for Shiny
# usePackage <- function(p) {
#   if (!is.element(p, installed.packages()[,1]))
#     install.packages(p, dep = TRUE, repos = "http://cran.us.r-project.org")
#   require(p, character.only = TRUE)
# }
#! Cumulative package loading
## Setup and Download
library(dplyr)
library(sf)
library(tigris)
library(tidyr)
library(shiny)
library(leaflet)
library(leafem)
## Commuting Flows
library(spdep)
library(ggplot2)
## Positive Residuals
library(sp)
library(spgwr)
library(igraph)
## Density Peaks
library(purrr)
library(car)
library(descr)
library(DescTools)
library(ggpubr)
library(haven)
library(psych)
library(writexl)
library(openxlsx)
## Double Thresholds
## Spatial Autocorrelation
library(e1071)

##### Data setup (run manually prior to deployment ; too expensive to run on server)
if (!file.exists("www/base_data/chicagoBaseData.shp")) {
  
  ## Tracts shapefile
  msaTracts <- tracts(state = "IL",
                      county = c("Cook County",
                                 "DeKalb County",
                                 "DuPage County",
                                 "Grundy County",
                                 "Kane County",
                                 "Kendall County",
                                 "Lake County",
                                 "McHenry County",
                                 "Will County"),
                      year = 2021) %>%
    rbind(
      tracts(state = "IN",
             county = c("Jasper County",
                        "Lake County",
                        "Newton County",
                        "Porter County"),
             year = 2021),
      tracts(state = "WI",
             county = c("Kenosha County"),
             year = 2021)
    ) %>%
    select(GEOID,
           geometry) %>%
    st_transform(crs = 4326)
  
  ## LEHD Data
  lehd.baseurl <- "https://lehd.ces.census.gov/data/lodes/LODES8/"
  state.list <- c("il", "in", "wi")
  dir.create("www/base_data/tempdir")
  
  ### O-D Data
  for (i in 1:length(state.list)) {
    download.file(paste0(lehd.baseurl,
                         state.list[i], "/",
                         "od", "/",
                         state.list[i], "_", 
                         "od", "_", 
                         "main", "_",
                         "JT00", "_",
                         "2021", ".csv.gz"),
                  destfile = paste0("www/base_data/tempdir/",
                                    state.list[i],
                                    "_od",
                                    "_main",
                                    ".csv.gz"))
    download.file(paste0(lehd.baseurl,
                         state.list[i], "/",
                         "od", "/",
                         state.list[i], "_", 
                         "od", "_", 
                         "aux", "_",
                         "JT00", "_",
                         "2021", ".csv.gz"),
                  destfile = paste0("www/base_data/tempdir/",
                                    state.list[i],
                                    "_od",
                                    "_aux",
                                    ".csv.gz"))
  }
  
  msaOD <- read.csv(paste0("www/base_data/tempdir/", list.files("www/base_data/tempdir/")[1]))
  for (i in 2:length(list.files("www/base_data/tempdir/"))) {
    msaOD <- msaOD %>%
      rbind(read.csv(paste0("www/base_data/tempdir/", list.files("www/base_data/tempdir/")[i])))
  }
  unlink("www/base_data/tempdir/*")
  
  ### WAC Data
  for (i in 1:length(state.list)) {
    download.file(paste0(lehd.baseurl,
                         state.list[i], "/",
                         "wac", "/",
                         state.list[i], "_", 
                         "wac", "_", 
                         "S000", "_",
                         "JT00", "_",
                         "2021", ".csv.gz"),
                  destfile = paste0("www/base_data/tempdir/",
                                    state.list[i],
                                    "_wac",
                                    ".csv.gz"))
  }
  msaWAC <- read.csv(paste0("www/base_data/tempdir/", list.files("www/base_data/tempdir/")[1]))
  for (i in 2:length(list.files("www/base_data/tempdir/"))) {
    msaWAC <- msaWAC %>%
      rbind(read.csv(paste0("www/base_data/tempdir/", list.files("www/base_data/tempdir/")[i])))
  }
  unlink("www/base_data/tempdir/*")
  
  ## Clean up MSA OD and MSA WAC data and join to MSA tract shapefile
  
  ### O-D Data
  msaOD <- msaOD %>%
    mutate(w_geocode = substr(w_geocode, 1, 11),
           h_geocode = substr(h_geocode, 1, 11))
  msaOD.workEqualsHome <- msaOD %>%
    filter(w_geocode == h_geocode) %>%
    mutate(w_geocode = as.character(w_geocode),
           h_geocode = as.character(h_geocode))
  msaOD.Inflow <- msaOD %>%
    group_by(w_geocode) %>%
    summarize(Inflow = sum(S000)) %>%
    mutate(w_geocode = as.character(w_geocode))
  msaOD.Outflow <- msaOD %>%
    group_by(h_geocode) %>%
    summarize(Outflow = sum(S000)) %>%
    mutate(h_geocode = as.character(h_geocode))
  
  msaTracts <- msaTracts %>%
    left_join(msaOD.Inflow, by = c("GEOID" = "w_geocode")) %>%
    left_join(msaOD.Outflow, by = c("GEOID" = "h_geocode")) %>%
    mutate(Inflow = ifelse(is.na(Inflow), 0, Inflow),
           Outflow = ifelse(is.na(Outflow), 0, Outflow))
  
  for (i in 1:nrow(msaTracts)) {
    ua <- msaTracts$GEOID[i]
    matched.val <- msaOD.workEqualsHome$S000[match(ua, msaOD.workEqualsHome$w_geocode)]
    if (!is.na(matched.val)) {
      msaTracts$Inflow[i] <- msaTracts$Inflow[i] - matched.val
      msaTracts$Outflow[i] <- msaTracts$Outflow[i] - matched.val
    }
  }
  
  ### WAC Data
  msaWAC <- msaWAC %>%
    select(w_geocode, 9:28) %>%
    pivot_longer(
      cols = starts_with("CNS"),
      names_to = "NAICS",
      names_prefix = "CNS",
      values_to = "Jobs",
      values_drop_na = TRUE
    ) %>%
    mutate(w_geocode = substr(w_geocode, 1, 11)) %>%
    group_by(w_geocode) %>%
    filter(Jobs > 0) %>%
    summarise(NAICS = n_distinct(NAICS),
              Jobs = sum(Jobs)) %>%
    mutate(w_geocode = as.character(w_geocode)) %>%
    filter(Jobs > 0)
  
  msaTracts <- msaTracts %>%
    left_join(msaWAC, by = c("GEOID" = "w_geocode")) %>%
    mutate(NAICS = ifelse(is.na(NAICS), 0, NAICS),
           Jobs = ifelse(is.na(Jobs), 0, Jobs))
  
  ## Calculate additional columns
  
  ### Land area and job density (in hectares)
  msaTracts <- msaTracts %>%
    mutate(
      Area.HA = as.numeric(st_area(.)) * 0.0001,
      JobDensity = Jobs / Area.HA
    )
  
  ### CBD and distance from CBD (in log(km))
  msaCBD <- msaTracts %>%
    filter(Jobs == max(Jobs))
  msaTracts$DistCBD <- as.vector(st_distance(msaCBD %>% st_centroid,
                                             msaTracts %>% st_centroid) / 1000)
  msaTracts$DistCBD <- ifelse(is.infinite(msaTracts$DistCBD), 0, msaTracts$DistCBD)
  
  ## Clean up file and environment
  rm(list=setdiff(ls(), "msaTracts"))
  unlink("www/base_data/tempdir", recursive = TRUE)
  msaTracts <- msaTracts %>%
    select(
      GEOID,
      Area.HA,
      Jobs,
      JobDensity,
      Inflow,
      Outflow,
      NAICS,
      DistCBD,
      geometry
    )
  
  ## Write out
  st_write(msaTracts,
           "www/base_data/chicagoBaseData.shp")
  
}

##### Functions
## Decider/"Master" function
prepareShapefile <- function(method.name,
                             options.contiguity = NULL,
                             options.jobCutoff = NULL,
                             options.nearestNeighbors = NULL,
                             options.alphaLevel = NULL) {
  
  if (method.name == "Commuting Flows") {
    output.centers <- method.commutingflows(options.contiguity)
  } else if (method.name == "Density Peaks") {
    output.centers <- method.densitypeaks()
  } else if (method.name == "Double Thresholds") {
    output.centers <- method.doublethresholds(options.contiguity)
  } else if (method.name == "Positive Residuals") {
    output.centers <- method.positiveresiduals(options.alphaLevel,
                                               options.jobCutoff)
  } else if (method.name == "Spatial Autocorrelation") {
    output.centers <- method.spatialautocorrelation(options.alphaLevel,
                                                    options.nearestNeighbors)
  }
  
  return(output.centers)
  
}

## Commuting Flows function
method.commutingflows <- function(options.contiguity) {
  
  msaTracts <- st_read("www/base_data/chicagoBaseData.shp")
  
  ### Calculate FC, DDI, PC for each tract
  msaTracts <- msaTracts %>%
    mutate(
      ## FC: Flow centrality, = Inflow / Outflow
      FlowCentrality = Inflow / Outflow,
      ## DDI: Directional Dominance Index, = Inflow / (average Inflow for all UAs)
      DirectionalDominance = Inflow / (mean(Inflow, na.rm = T)),
      ## PC: Productive completeness, = number of unique NAICS / (average number of unique NAICS per UA)
      ProductiveCompleteness = NAICS / (mean(NAICS, na.rm = T)) 
    ) %>%
    mutate(
      ## In FlowCentrality, we can get NaN or Inf if Outflow == 0 | Outflow == Inflow == 0
      FlowCentrality = ifelse(!is.finite(FlowCentrality), 0, FlowCentrality)
    )
  
  ### Identify Subcenters and filter
  msaTracts <- msaTracts %>%
    mutate(
      Subcenter = ifelse(
        (FlowCentrality > 1 &
           DirectionalDominance > 1 &
           ProductiveCompleteness > 1),
        1,
        0)) %>%
    filter(
      Subcenter == 1
    )
  
  ### Dissolve contiguous units
  adjacency.matrix <- st_touches(msaTracts)
  g <- graph_from_adj_list(adjacency.matrix)
  clusters <- components(g)$membership
  
  msaTracts <- msaTracts %>%
    mutate(Cluster = clusters,
           Count = 1) %>%
    group_by(Cluster) %>%
    summarize(
      Area_HA = sum(Area_HA),
      Jobs = sum(Jobs),
      JobDensity = sum(JobDensity), # recalculate later
      Count = sum(Count)
    )
  
  ### If contiguity is required, filter by above one "Count"
  if (options.contiguity == TRUE) {
    msaTracts <- msaTracts %>%
      filter(Count > 1)
  }
  
  ### Recalculate Job Density
  msaTracts <- msaTracts %>%
    mutate(JobDensity = Jobs / Area_HA)
  
  ### Return sf object
  return(msaTracts)
  
}

## Density Peaks function
method.densitypeaks <- function() {
  
  msaTracts <- st_read("www/base_data/chicagoBaseData.shp")
  
  ### Compute Log of Job Density
  msaTracts <- msaTracts %>%
    mutate(lnJobDensity = log(JobDensity))
  
  ### Find the 95th percentile for job density
  msaTracts.top5percent <- msaTracts %>%
    filter(JobDensity > quantile(JobDensity, 0.95))
  
  ### Create smoothing spline, identify knots
  fit.vt <- smooth.spline(msaTracts.top5percent$DistCBD,
                          msaTracts.top5percent$lnJobDensity,
                          cv = TRUE)
  fitted.values <- predict(fit.vt, n = 1000)
  change.indices <- which(diff(sign(diff(fitted.values$y))) < 0) + 1
  knots <- fitted.values$x[change.indices]
  knot.table <- data.frame(Knot = 1:length(knots),
                           Value = round(knots, 2))
  
  ### Find closest subcenter for each knot value
  knot.table <- knot.table %>%
    mutate(
      closest_DistCBD = map_dbl(Value, 
                                ~msaTracts.top5percent$DistCBD[which.min(abs(msaTracts.top5percent$DistCBD - .x))])
    )
  
  ### Identify subcenters and filter
  msaTracts <- msaTracts %>%
    mutate(
      Subcenter = ifelse(
        DistCBD %in% knot.table$closest_DistCBD,
        1,
        0)
    ) %>%
    #### Add the CBD
    mutate(
      Subcenter = ifelse(
        DistCBD == min(DistCBD),
        1,
        Subcenter)
    ) %>%
    filter(
      Subcenter == 1
    )
  
  ### Create buffer rings
  bufferRings <- st_buffer(st_centroid(msaTracts[which(msaTracts$Jobs == max(msaTracts$Jobs)),]),
                           knot.table$Value[1] * 1000)
  for (i in 2:nrow(knot.table)) {
    temp <- st_buffer(st_centroid(msaTracts[which(msaTracts$Jobs == max(msaTracts$Jobs)),]),
                      knot.table$Value[i] * 1000)
    bufferRings <- rbind(bufferRings,
                         temp)
  }
  bufferRings <- bufferRings %>%
    mutate(ID = seq(1, nrow(bufferRings), 1)) %>%
    select(ID, geometry)
  
  ### Clean up sf object
  msaTracts <- msaTracts %>%
    mutate(Cluster = NA,
           Count = NA) %>%
    select(Cluster, 
           Area_HA, 
           Jobs, 
           JobDensity, 
           Count, 
           geometry)
  
  ### Need tracts + rings; make a list
  output.list <- list()
  output.list[[1]] <- msaTracts
  output.list[[2]] <- bufferRings
  
  ### Return list
  return(output.list)
}

## Double Thresholds function
method.doublethresholds <- function(options.contiguity) {
  
  msaTracts <- st_read("www/base_data/chicagoBaseData.shp")
  
  ### Set thresholds for total jobs and job density
  threshold.jobs <- mean(msaTracts$Jobs, na.rm = T) + sd(msaTracts$Jobs, na.rm = T)
  threshold.jobdensity <- mean(msaTracts$JobDensity, na.rm = T)
  
  ### Identify subcenters and filter
  msaTracts <- msaTracts %>%
    mutate(
      Subcenter = ifelse(
        (Jobs > threshold.jobs &
           JobDensity > threshold.jobdensity),
        1,
        0)
    ) %>%
    filter(Subcenter == 1)
  
  ### Dissolve contiguous units
  adjacency.matrix <- st_touches(msaTracts)
  g <- graph_from_adj_list(adjacency.matrix)
  clusters <- components(g)$membership
  
  msaTracts <- msaTracts %>%
    mutate(Cluster = clusters,
           Count = 1) %>%
    group_by(Cluster) %>%
    summarize(
      Area_HA = sum(Area_HA),
      Jobs = sum(Jobs),
      JobDensity = sum(JobDensity), # recalculate later
      Count = sum(Count)
    )
  
  ### If contiguity is required, filter by above one "Count"
  if (options.contiguity == TRUE) {
    msaTracts <- msaTracts %>%
      filter(Count > 1)
  }
  
  ### Recalculate Job Density
  msaTracts <- msaTracts %>%
    mutate(JobDensity = Jobs / Area_HA)
  
  ### Return sf object
  return(msaTracts)
  
}

## Positive Residuals function
method.positiveresiduals <- function(options.alphaLevel,
                                     options.jobCutoff) {
  
  msaTracts <- st_read("www/base_data/chicagoBaseData.shp")
  
  ### Compute log of DistCBD
  msaTracts$logDistCBD <- ifelse(msaTracts$DistCBD == 0, 0, log(msaTracts$DistCBD))
  
  ### Convert to sp object and run gwr
  msaTracts.sp <- as(msaTracts, "Spatial")
  
  gwr.bw <- gwr.sel(Jobs ~ logDistCBD,
                    msaTracts.sp,
                    adapt = TRUE)
  
  gwr.fit <- gwr(Jobs ~ logDistCBD,
                 data = msaTracts.sp,
                 adapt = gwr.bw,
                 se.fit = T,
                 hatmatrix = T)
  
  gwr.results <- as.data.frame(gwr.fit$SDF)
  
  ### Join results to tracts, calculate positivity of residuals
  msaTracts <- cbind(msaTracts,
                     select(gwr.results,
                            pred,
                            pred.se))
  
  msaTracts <- msaTracts %>%
    mutate(
      pred = ifelse(pred < 0, 0, pred),
      resid = (Jobs - pred) / pred.se
    )
  
  ### Measure "significant" positivity based on supplied alphaLevel
  if (options.alphaLevel == "0.01") {
    msaTracts <- msaTracts %>%
      mutate(
        Subcenter = ifelse(
          resid > 2.576, ## critical value; two-tailed 99%
          1,
          0)
      )
  } else if (options.alphaLevel == "0.05") {
    msaTracts <- msaTracts %>%
      mutate(
        Subcenter = ifelse(
          resid > 1.960, ## critical value; two-tailed 95%
          1,
          0)
      )
  } else if (optons.alphaLevel == "0.10") {
    msaTracts <- msaTracts %>%
      mutate(
        Subcenter = ifelse(
          resid > 1.645, ## critical value; two-tailed 90%
          1,
          0)
      )
  }
  
  ### Filter
  msaTracts <- msaTracts %>%
    filter(Subcenter == 1)
  
  ### Dissolve contiguous units
  adjacency.matrix <- st_touches(msaTracts)
  g <- graph_from_adj_list(adjacency.matrix)
  clusters <- components(g)$membership
  
  msaTracts <- msaTracts %>%
    mutate(Cluster = clusters,
           Count = 1) %>%
    group_by(Cluster) %>%
    summarize(
      Area_HA = sum(Area_HA),
      Jobs = sum(Jobs),
      JobDensity = sum(JobDensity), # recalculate later
      Count = sum(Count)
    )
  
  ### Filter by supplied jobCutoff value
  msaTracts <- msaTracts %>%
    filter(Jobs >= options.jobCutoff)
  
  ### Recalculate Job Density
  msaTracts <- msaTracts %>%
    mutate(JobDensity = Jobs / Area_HA)
  
  ### Return sf object
  return(msaTracts)
  
}

## Spatial Autocorrelation function
method.spatialautocorrelation <- function(options.alphaLevel,
                                          options.nearestNeighbors) {
  
  msaTracts <- st_read("www/base_data/chicagoBaseData.shp")
  
  ### Set up Moran's I dependencies; log of employment density
  msaTracts$lnJobDensity <- log(msaTracts$JobDensity)
  msaTracts$lnJobDensity <- ifelse(is.infinite(msaTracts$lnJobDensity),
                                   0,
                                   msaTracts$lnJobDensity)
  
  ### Create knn weighting matrix
  knn.weights <- knearneigh(st_coordinates(st_centroid(msaTracts$geometry)),
                            k = options.nearestNeighbors)
  knn.list <- nb2listw(knn2nb(knn.weights), style = "W")
  
  ### Run local Moran's I
  local.moran <- localmoran(msaTracts$lnJobDensity, knn.list)
  local.moran <- data.frame(local.moran, attr(local.moran, "quad"))
  names(local.moran)[5] <- "p_val"
  local.moran$row <- 1:dim(msaTracts)[1]
  
  ### Merge the results with the tracts
  msaTracts$row <- 1:dim(msaTracts)[1]
  msaTracts <- merge(msaTracts, local.moran,
                     by = "row",
                     all.x = TRUE,
                     sort = FALSE)
  
  ### Identify subcenters based on supplied alpha level
  if (options.alphaLevel == "0.01") {
    msaTracts <- msaTracts %>%
      mutate(
        Subcenter = ifelse(p_val < 0.01,
                           1,
                           0)
      )
  } else if (options.alphaLevel == "0.05") {
    msaTracts <- msaTracts %>%
      mutate(
        Subcenter = ifelse(p_val < 0.05,
                           1,
                           0)
      )
  } else if (options.alphaLevel == "0.10") {
    msaTracts <- msaTracts %>%
      mutate(
        Subcenter = ifelse(p_val < 0.1,
                           1,
                           0)
      )
  }
  
  ### Join "Category" information
  msaTracts <- msaTracts %>%
    filter(Subcenter == 1) %>%
    mutate(
      Category = as.character(mean)
    ) %>%
    filter(Jobs > 0) %>%
    filter(Category %in% c("High-High", "High-Low"))
  
  ### Separately dissolve the "High-High" and "High-Low" tracts, then combine
  msaTracts.highhigh <- msaTracts %>%
    filter(Category == "High-High")
  adjacency.matrix <- st_touches(msaTracts.highhigh)
  g <- graph_from_adj_list(adjacency.matrix)
  clusters <- components(g)$membership
  msaTracts.highhigh <- msaTracts.highhigh %>%
    mutate(Cluster = clusters,
           Count = 1) %>%
    group_by(Cluster) %>%
    summarize(
      Area_HA = sum(Area_HA),
      Jobs = sum(Jobs),
      JobDensity = sum(JobDensity), # recalculate later
      Category = max(Category),
      Count = sum(Count)
    )
  
  msaTracts.highlow <- msaTracts %>%
    filter(Category == "High-Low")
  adjacency.matrix <- st_touches(msaTracts.highlow)
  g <- graph_from_adj_list(adjacency.matrix)
  clusters <- components(g)$membership
  msaTracts.highlow <- msaTracts.highlow %>%
    mutate(Cluster = clusters,
           Count = 1) %>%
    group_by(Cluster) %>%
    summarize(
      Area_HA = sum(Area_HA),
      Jobs = sum(Jobs),
      JobDensity = sum(JobDensity), # recalculate later
      Category = max(Category),
      Count = sum(Count)
    )
  
  msaTracts <- rbind(msaTracts.highhigh, msaTracts.highlow)
  
  ### Recalculate Job Density
  msaTracts <- msaTracts %>%
    mutate(JobDensity = Jobs / Area_HA)
  
  ### Renumber clusters
  msaTracts$Cluster <- seq(1, nrow(msaTracts), 1)
  
  ### Return sf object
  return(msaTracts)
  
}

## Map-maker

mapmaker <- function(layer.toMap, 
                     rings.toMap = NULL) {
  
  layer.chicagocity <- st_read("www/shapefiles_contextlayers/Boundary_Chicago/Boundary_Chicago.shp")
  layer.msacounties <- st_read("www/shapefiles_contextlayers/Boundary_MSACounties/Boundary_MSACounties.shp")
  layer.states <- st_read("www/shapefiles_contextlayers/Boundary_States/Boundary_States.shp")
  
  ## If the method is not density peaks
  if (is.null(rings.toMap)) {
    
    ## If the method is Spatial Autocorrelation
    if ("Category" %in% colnames(layer.toMap)) {
      pal <- leaflet::colorFactor(c("darkblue", "pink"), domain = unique(layer.toMap$Category))
      output.map <- leaflet(options = leafletOptions(zoomsnap = 0.25, zoomDelta = 0.25)) %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        ## MSA States
        addPolygons(data = layer.states,
                    color = "black",
                    fillColor = NULL,
                    opacity = 0.5,
                    fillOpacity = 0,
                    stroke = TRUE,
                    weight = 0.75,
                    group = "States") %>%
        ## MSA Counties
        addPolygons(data = layer.msacounties,
                    color = "black",
                    fillColor = NULL,
                    opacity = 0.75,
                    fillOpacity = 0,
                    stroke = TRUE,
                    weight = 0.75,
                    group = "MSA Counties") %>%
        ## Chicago
        addPolygons(data = layer.chicagocity,
                    color = "black",
                    fillColor = NULL,
                    opacity = 0.75,
                    fillOpacity = 0,
                    stroke = TRUE,
                    weight = 0.75,
                    group = "City of Chicago") %>%
        ## Subcenters
        addPolygons(data = layer.toMap,
                    color = "black",
                    fillColor = pal(layer.toMap$Category),
                    opacity = 1,
                    fillOpacity = 0.75,
                    stroke = TRUE,
                    weight = 0.5,
                    popup = paste0("Cluster ID: ", layer.toMap$Cluster,
                                   "<br>",
                                   "Category: ", layer.toMap$Category,
                                   "<br>",
                                   "Area (Hectares): ", round(layer.toMap$Area_HA),
                                   "<br>",
                                   "Total Jobs: ", layer.toMap$Jobs,
                                   "<br>",
                                   "Jobs per Hectare: ", round(layer.toMap$JobDensity, 2),
                                   "<br>",
                                   "Number of Tracts: ", layer.toMap$Count),
                    highlightOptions = highlightOptions(color = "#10539A",
                                                        weight = 2,
                                                        fillColor = "lightblue"),
                    group = "Subcenters") %>%
        addLegend("bottomright",
                  pal = pal,
                  values = layer.toMap$Category)
    } 
    ## Method is Commuting Flows, Double Thresholds, or Positive Residuals
    else {
      output.map <- leaflet(options = leafletOptions(zoomControl = TRUE,
                                                     zoomSnap = 0.25,
                                                     zoomDelta = 0.25)) %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        ## MSA States
        addPolygons(data = layer.states,
                    color = "black",
                    fillColor = NULL,
                    opacity = 0.5,
                    fillOpacity = 0,
                    stroke = TRUE,
                    weight = 0.75,
                    group = "States") %>%
        ## MSA Counties
        addPolygons(data = layer.msacounties,
                    color = "black",
                    fillColor = NULL,
                    opacity = 0.75,
                    fillOpacity = 0,
                    stroke = TRUE,
                    weight = 0.75,
                    group = "MSA Counties") %>%
        ## Chicago
        addPolygons(data = layer.chicagocity,
                    color = "black",
                    fillColor = NULL,
                    opacity = 0.75,
                    fillOpacity = 0,
                    stroke = TRUE,
                    weight = 0.75,
                    group = "City of Chicago") %>%
        ## Subcenters
        addPolygons(data = layer.toMap,
                    color = "black",
                    fillColor = "darkblue",
                    opacity = 1,
                    fillOpacity = 0.75,
                    stroke = TRUE,
                    weight = 0.5,
                    popup = paste0("Cluster ID: ", layer.toMap$Cluster,
                                   "<br>",
                                   "Area (Hectares): ", round(layer.toMap$Area_HA),
                                   "<br>",
                                   "Total Jobs: ", layer.toMap$Jobs,
                                   "<br>",
                                   "Jobs per Hectare: ", round(layer.toMap$JobDensity, 2),
                                   "<br>",
                                   "Number of Tracts: ", layer.toMap$Count),
                    highlightOptions = highlightOptions(color = "#10539A",
                                                        weight = 2,
                                                        fillColor = "lightblue"),
                    group = "Subcenters")
    }
  }
  ## If the method is density peaks
  else {
    output.map <- leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      ## MSA States
      addPolygons(data = layer.states,
                  color = "black",
                  fillColor = NULL,
                  opacity = 0.5,
                  fillOpacity = 0,
                  stroke = TRUE,
                  weight = 0.75,
                  group = "States") %>%
      ## MSA Counties
      addPolygons(data = layer.msacounties,
                  color = "black",
                  fillColor = NULL,
                  opacity = 0.75,
                  fillOpacity = 0,
                  stroke = TRUE,
                  weight = 0.75,
                  group = "MSA Counties") %>%
      ## Chicago
      addPolygons(data = layer.chicagocity,
                  color = "black",
                  fillColor = NULL,
                  opacity = 0.75,
                  fillOpacity = 0,
                  stroke = TRUE,
                  weight = 0.75,
                  group = "City of Chicago") %>%
      ## Density Peaks Buffer Rings
      addPolygons(data = rings.toMap,
                  color = "darkred",
                  fillColor = NULL,
                  opacity = 0.75,
                  fillOpacity = 0,
                  stroke = TRUE,
                  weight = 0.5) %>%
      ## Subcenters
      addPolygons(data = layer.toMap,
                  color = "black",
                  fillColor = "darkblue",
                  opacity = 1,
                  fillOpacity = 0.75,
                  stroke = TRUE,
                  weight = 0.5,
                  popup = paste0("Cluster ID: ", layer.toMap$Cluster,
                                 "<br>",
                                 "Area (Hectares): ", round(layer.toMap$Area_HA),
                                 "<br>",
                                 "Total Jobs: ", layer.toMap$Jobs,
                                 "<br>",
                                 "Jobs per Hectare: ", round(layer.toMap$JobDensity, 2),
                                 "<br>",
                                 "Number of Tracts: ", layer.toMap$Count),
                  highlightOptions = highlightOptions(color = "#10539A",
                                                      weight = 2,
                                                      fillColor = "lightblue"),
                  group = "Subcenters") 
  }
  
  ## Final adjustments
  output.map <- output.map %>%
    ## Enable layer controls 
    addLayersControl(
      overlayGroups = c("States",
                        "MSA Counties",
                        "City of Chicago",
                        "Subcenters")
    ) %>%
    ## Set View
    setView(lat = 41.725, lng = -87.845, zoom = 8)
  
  ## return leaflet object
  return(output.map)
  
}

##### Testing
# method.name <- "Spatial Autocorrelation"
# options.contiguity <- TRUE
# options.jobCutoff <- 10000
# options.nearestNeighbors <- 7
# options.alphaLevel <- "0.05"
# 
# test.results <- prepareShapefile(method.name,
#                                  options.contiguity,
#                                  options.jobCutoff,
#                                  options.nearestNeighbors,
#                                  options.alphaLevel)
# 
# test.map <- mapmaker(layer.toMap = test.results,
#                      rings.toMap = NULL)
