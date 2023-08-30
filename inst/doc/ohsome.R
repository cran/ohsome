## ---- include = FALSE---------------------------------------------------------
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
httr::set_config(httr::config(http_version = 1))
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "figures/", 
  out.width = "100%",
  purl = NOT_CRAN,
  eval = NOT_CRAN
)

## ----library------------------------------------------------------------------
library(ohsome)

## ----fix, include = FALSE-----------------------------------------------------
# avoid messages when handling franconia with old-style crs object
franconia <- sf::st_set_crs(mapview::franconia, 4326)

## ----elements_count-----------------------------------------------------------
library(mapview)

q <- ohsome_elements_count(franconia, filter = "craft=brewery")

## ----post---------------------------------------------------------------------
ohsome_post(q, strict = FALSE)

## ----time, eval = FALSE-------------------------------------------------------
#  ohsome_elements_count(franconia, filter = "craft=brewery", time = "2010/2020/P1M")

## ----pipe, dev = "svg"--------------------------------------------------------
library(ggplot2)

q |> 
	set_time("2010/2020/P1M") |>
	ohsome_post() |>
	ggplot(aes(x = timestamp, y = value)) +
	geom_line()

## ----groupBy_boundary, message = FALSE----------------------------------------
library(dplyr)

franconia |> 
	mutate(id = NAME_ASCI) |>
	ohsome_elements_count(filter = "craft=brewery", time = "2021-06-01") |>
	set_grouping("boundary") |>
	ohsome_post()

## ----density------------------------------------------------------------------
franconia |> 
	mutate(id = NAME_ASCI) |>
	ohsome_elements_count(filter = "craft=brewery", return_value = "density") |>
	set_time("2021-06-01") |>
	set_grouping("boundary") |>
	ohsome_post() |>
	mapview(zcol = "value", layer.name = "Breweries per sqkm")

## ----building_levels, warning = FALSE-----------------------------------------
hd_station_500m <- ohsome_boundary("8.67542,49.40347,500")

ohsome_elements_geometry(
	boundary = hd_station_500m, 
	filter = "building=* and type:way", 
	time = "2021-12-01",
	properties = "tags", 
	clipGeometry = FALSE
) |>
	ohsome_post() |>
	transmute(level = factor(`building:levels`)) |>
	mapview(zcol = "level", lwd = 0, layer.name = "Building level")

## ----buildings----------------------------------------------------------------
hd_station_1km <- ohsome_boundary("8.67542,49.40347,1000")

ohsome_elements_geometry(
	boundary = hd_station_1km, 
	filter = "building=* and type:way", 
	time = "2021-12-01",
	properties = "tags", 
	clipGeometry = FALSE
) |>
	ohsome_post() |>
	transmute(level = factor(`building:levels`)) |>
	mapview(zcol = "level", lwd = 0, layer.name = "Building level")

## ----contribution_count-------------------------------------------------------
ohsome_contributions_count(
	boundary = "0,0,10", 
	filter = "man_made=*", 
	time = "2010/2020/P1Y",
	contributionType = "deletion"
) |>
	ohsome_post()

## ----contribution_extraction--------------------------------------------------
nominatimlite::geo_lite_sf("Berlin Neukoelln", points_only = FALSE) |>
	ohsome_contributions_centroid() |>
	set_filter("amenity=*") |>
	set_time("2020-03,2020-04") |>
	set_properties("contributionTypes") |> 
	ohsome_post() |>
	filter(`@tagChange`) |>
	mapview(layer.name = "Amenities with Tag Changes")

## ----nepal--------------------------------------------------------------------
ohsome_users_count(
	boundary = "82.3055,6.7576,87.4663,28.7025",
	filter = "building=* and geometry:polygon",
	time = "2015-03-01/2015-08-01/P1M"
) |>
	ohsome_post()

## ----bbox---------------------------------------------------------------------
q <- ohsome_query("users/count") |>
	set_boundary(sf::st_bbox(franconia))

q$body$bboxes

## ----getbb--------------------------------------------------------------------
osmdata::getbb("Kigali") |> 
	ohsome_elements_length(time = "2018/2018-12/P1M", filter = "route=bus") |>
	ohsome_post()

## ----circles------------------------------------------------------------------
c("Circle 1:8.6528,49.3683,1000", "Circle 2:8.7294,49.4376,1000") |>
	ohsome_elements_count(filter = "amenity=*", grouping = "boundary", time = 2021) |>
	ohsome_post()

## ----set_parameters-----------------------------------------------------------
q <- ohsome_elements_count("8.5992,49.3567,8.7499,49.4371")

q |>
	set_endpoint("ratio", append = TRUE) |>
	set_parameters(
		filter = "building=*", 
		filter2 = "building=* and building:levels=*",
		time = "2010/2020/P2Y"
	) |>
	ohsome_post()


## ----groupby_boundary_groupby_tag, message = FALSE----------------------------
building_levels <- franconia |>
	mutate(id  = NUTS_ID) |>
	ohsome_elements_count(grouping = c("boundary", "tag"), format = "csv") |>
	set_filter("building=* and geometry:polygon") |>
	set_time("2015/2020") |>
	set_groupByKey("building:levels") |>
	ohsome_post()

dim(building_levels)

## ----tidy---------------------------------------------------------------------
library(tidyr)

building_levels |>
	pivot_longer(-timestamp, names_to = c("id", "levels"), names_sep = "_")

