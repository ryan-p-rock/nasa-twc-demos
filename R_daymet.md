Downloading and processing daymet data with R
================

## Setting up working directory

Let’s do our work using relative paths and make some directories to
store things. This will make a folder called `data` and two
subdirectories called `daymet` and `aoi`.

``` r
library(here)
setwd(here::here())
dir.create("data")
dir.create("data/aoi")
dir.create("data/daymet")
```

## Load in our AOI

This is a shapefile of the extent of our study region - a fire that
occurred in 2003 in the Mission Mountains in Montana. First, we’re going
to convert this to a geographic projection so we can use its bounds to
query daymet data.

``` r
library(sf)
AOI_f <- "data/aoi/mt4743511379720030810_20020710_20040925_burn_bndy.shp"

# load in data
aoi <- st_read(AOI_f)
```

    ## Reading layer `mt4743511379720030810_20020710_20040925_burn_bndy' from data source `C:\Users\ethan.shafron\Desktop\nasa_r_daymet\data\aoi\mt4743511379720030810_20020710_20040925_burn_bndy.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 1 feature and 22 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -1348006 ymin: 2838633 xmax: -1337494 ymax: 2846044
    ## Projected CRS: USA_Contiguous_Albers_Equal_Area_Conic_USGS_version

``` r
# convert to wgs84
aoi <- st_transform(aoi, 4326)

# plot
plot(aoi$geometry)
```

![](R_daymet_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

## Querying daymet data using DaymetR

The next step involves creating a query for our data request. In this
section we use a bounding box for our area of interest (AOI), and
specify a date range, region, variable of interest, and folder for the
output files. For this example, I am only looking for precipitation
data, but we could easily add more variables for download during this
step. The actual download is commented out because it takes \~15 minutes
to complete, and this is an R markdown document.

``` r
library(daymetr)
bbox <- st_bbox(aoi)
#download_daymet_ncss(location = c(bbox[["ymax"]], bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]]),
#                     start = 1986,
#                     end = 2020,
#                     param = "prcp",
#                     mosaic = "na",
#                     path = "data/daymet")
```

## Working with the data

Before creating any data output files, we are going to do some plotting
so that we loosely know what we’re working with. A little bit about this
study site - it’s in a mountainous region along a significant
elevational gradient, which translates into a significant range in
precipitation levels.

``` r
library(raster)
library(lubridate)
library(dplyr)

## years of data
years <- 1986:2020

## function to create a dataframe of average values given a variable and vector of years
create_time_series <- function(variable, years){
  outdf <- data.frame()
  for (year in years){
    daymet_ds <- brick(paste0("data/daymet/", variable, "_", "daily_", year, "_ncss.nc"), layer=variable)
    data <- raster::getValues(daymet_ds)
    daily_means <- colMeans(data)
    dates <- ymd(gsub(".", "/", gsub("X", "", colnames(data)), fixed=TRUE))
    df <- data.frame(dates, daily_means)
    names(df) <- c("date", paste0("mean_", variable))
    outdf <- rbind(outdf, df)
  }
  return(outdf)
}

# call the function
prcp_ts <- create_time_series("prcp", years)

# make weekly mean values of precipitation dataframe
weekly_means <- prcp_ts %>%
        mutate(woy = week(prcp_ts$date)) %>%
        group_by(woy) %>%
        summarise(mean(mean_prcp, na.rm = TRUE))

# rename data frame columns
names(weekly_means) <- c("week", "mean_prcp")

# view top rows
head(weekly_means)
```

    ## # A tibble: 6 x 2
    ##    week mean_prcp
    ##   <dbl>     <dbl>
    ## 1     1      5.07
    ## 2     2      4.78
    ## 3     3      4.07
    ## 4     4      3.65
    ## 5     5      4.33
    ## 6     6      3.81

``` r
# Let's plot out the weekly means using ggplot2
library(ggplot2)
ggplot(data = weekly_means) +
  geom_line(aes(x=week, y=mean_prcp))+
  xlab("Week of year")+
  ylab("Precipitation (mm/day water equivalent)")
```

![](R_daymet_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Creating an aggregated output raster

Now that we’ve inspected our data and it looks reasonable, we will go
ahead and make a raster of average summer precipitation, an important
factor for wildfire management and habitat suitability for numerous
species.

``` r
### output filename
clipped_gtiff <- paste0(getwd(), "/data/mean_summer_precipitation_26911_resampled_clipped.tif")

### list all the files in the daymet directory
files <- list.files("data/daymet", full.names = TRUE)

#initialize empty list
layerList <- list()

### For each file, get the total precip in summer, and add to list of rasters
for (i in 1:length(files)){
  ds <- brick(files[i])
  yearsum <- sum(subset(ds, 152:244))
  layerList[i] <- yearsum
}

### create raster that is the mean of all the yearly sums of precip
yearlyAvgPrcp <- mean(brick(layerList))

### correct projection info before masking
raster::projection(yearlyAvgPrcp) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=km +no_defs"
### project to NAD83 utm zone 11
yearlyAvgPrcp <- raster::projectRaster(yearlyAvgPrcp, crs = "+init=epsg:26911")
### project to NAD83 utm zone 11
aoi <- st_transform(aoi, 26911)

### resample to finer resolution using bilinear interpolation. This is a mediocre method for upscaling continuous data
disagged_raster <- raster::disaggregate(yearlyAvgPrcp, fact=34, method = "bilinear")

### clip/crop/mask the dataset with the AOI
cropped_dataset <- raster::mask(disagged_raster, aoi)

### write it to file
raster::writeRaster(cropped_dataset, clipped_gtiff)

### plot - note how the resampling method (cubic convolution vs bilinear interpolation) affects the final output in this tutorial vs the Python one
plot(cropped_dataset)
```

![](R_daymet_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
