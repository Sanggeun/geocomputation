# install.packages("sf")
# install.packages("raster")
# install.packages("spData")
# remotes::install_github("Nowosad/spDataLarge")

library(sf)
library(raster)
library(spData)
library(spDataLarge)

# 2. 2. 1

vignette(package = "sf")
vignette("sf1")

names(world)

world$geom

plot(world)

summary(world)

world_mini = world[1:2, 1:3]
world_mini

# 2.2.2 

library(sp)
world_sp = as(world, Class = "Spatial")
world_sf = st_as_sf(world_sp)


# 2.2.3

plot(world[3:6])
plot(world["pop"])

world_asia = world[world$continent == "Asia",]
asia = st_union(world_asia)

plot(world["pop"], reset = FALSE)
plot(asia, add = TRUE, col = "red")


# 2.2.4

plot(world["continent"], reset = FALSE)
cex = sqrt(world$pop) /1000
world_cents = st_centroid(world, of_largest = TRUE)
plot(st_geometry(world_cents), add = TRUE, cex = cex)


india = world[world$name_long == "India",]
plot(st_geometry(india), expandBB = c(0, 0.2, 0.1, 1), col = "gray", lwd = 3)
plot(world_asia[0], add =TRUE)

# 2.2.5 



# 2.2.6

st_point(c(5,2))
st_point(c(5,2,3))
st_point(c(5,2,1), dim = "XYM")
st_point(c(5,2,3,1))

multipoint_matrix = rbind(c(5,2), c(1,3), c(3,4), c(3,2))
st_multipoint(multipoint_matrix)

linestring_matrix = rbind(c(1,5), c(4,4), c(4,1), c(2,2), c(3,2))
st_linestring(linestring_matrix)

## Polygon 
polygon_list = list(rbind(c(1,5), c(2,2), c(4,1), c(4,4), c(4,4), c(1,5)))
st_polygon(polygon_list)

## polygon with a hole
polygon_border = rbind(c(1,5), c(2,2), c(4,1), c(4,4), c(1,5))
polygon_hole = rbind(c(2,4), c(3,4), c(3,3), c(2,3), c(2,4))
polygon_with_hole_list = list(polygon_border, polygon_hole)
st_polygon(polygon_with_hole_list)


## multilinestring
multilinestring_list = list(rbind(c(1,5), c(4,4), c(4,1), c(2,2), c(3,2)),
                            rbind(c(1,2), c(2,4)))
st_multilinestring(multilinestring_list)

## multipolygon
multipolygon_list = list(list(rbind(c(1,5), c(2,2), c(4,1), c(4,4), c(1,5))),
                         list(rbind(c(0,2), c(1,2), c(1,3), c(0,3), c(0,2))))
st_multipolygon(multipolygon_list)

## geometrycollection
geometrycollection_list = list(st_multipoint(multipoint_matrix),
                               st_linestring(linestring_matrix))
st_geometrycollection(geometrycollection_list)

# 2.2.7

## sfc point
point1 = st_point(c(5,2))
point2 = st_point(c(1,3))

points_sfc = st_sfc(point1, point2)
points_sfc

## sfc polygon
polygon_list1 = list(rbind(c(1,5), c(2,2), c(4,1), c(4,4), c(1,5)))
polygon1 = st_polygon(polygon_list1)
polygon_list2 = list(rbind(c(0,2), c(1,2), c(1,3), c(0,3), c(0,2)))
polygon2 = st_polygon(polygon_list2)
polygon_sfc = st_sfc(polygon1, polygon2)
st_geometry_type(polygon_sfc)

## sfc multistring
multilinestring_list1 = list(rbind(c(1,5), c(4,4), c(4,1), c(2,2), c(3,1)),
                             rbind(c(1,2), c(2,4)))
multilinestring1 = st_multilinestring(multilinestring_list1)
multilinestring_list2 = list(rbind(c(2,9), c(7,9), c(5,6), c(4,7), c(2,2)),
                             rbind(c(1,7), c(3,8)))
multilinestring2 = st_multilinestring(multilinestring_list2)
multilinestring_sfc = st_sfc(multilinestring1, multilinestring2)
st_geometry_type(multilinestring_sfc)

## sfc geometry
point_multilinestring_sfc = st_sfc(point1, multilinestring1)
st_geometry_type(point_multilinestring_sfc)

st_crs(points_sfc)

# EPSG definition
points_sfc_wgs = st_sfc(point1, point2, crs = 4326)
st_crs(points_sfc_wgs)

# PROJ4STRING definition
st_sfc(point1, point2, crs = "+proj=longlat +datum=WGS84 +no_defs")


# 2.2.8

lnd_point = st_point(c(0.1, 51.5))
lnd_geom = st_sfc(lnd_point, crs = 4326)
lnd_attrib = data.frame(
  name  = "London",
  temperature = 25,
  data = as.Date("2017-06-21")
)

lnd_sf = st_sf(lnd_attrib, geometry = lnd_geom)

lnd_sf

class(lnd_sf)


# 2.3.1 

raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
new_raster = raster(raster_filepath)

new_raster

# 2.3.2 
plot(new_raster)

# 2.3.3
raster::writeFormats()
rgdal::gdalDrivers()

new_raster2 = raster(nrows = 6, ncols = 6, res = 0.5,
                     xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5,
                     vals = 1:36)


multi_raster_file = system.file("raster/landsat.tif", package = "spDataLarge")
r_brick = brick(multi_raster_file)
r_brick
nlayers(r_brick)

raster_on_disk = raster(r_brick, layer = 1)
raster_in_memory = raster(xmn = 301905, xmx = 335745,
                          ymn = 4111245, ymx = 4154085, res = 30)
values(raster_in_memory) = sample(seq_len(ncell(raster_in_memory)))
crs(raster_in_memory) = crs(raster_on_disk)

r_stack = stack(raster_in_memory, raster_on_disk)
r_stack

# 2.4 

# 2.4.3 
crs_data = rgdal::make_EPSG()

View(crs_data)

vector_filepath = system.file("vector/zion.gpkg", package = "spDataLarge")
new_vector = st_read(vector_filepath)

st_crs(new_vector)

new_vector = st_set_crs(new_vector, 4326)

projection(new_raster)

projection(new_raster) = "+proj=utm +zone=12 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0
+units=m +no_defs"

# 2.5

luxembourg = world[world$name_long == "Luxembourg",]
st_area(luxembourg)

st_area(luxembourg)/1000000

units::set_units(st_area(luxembourg), km^2)

res(new_raster)

repr = projectRaster(new_raster, crs = "+init=epsg:26912")
res(repr)
