library(sf)
library(raster)
library(dplyr)
library(spData)

# 4.2
canterbury = nz %>% filter(Name == "Canterbury")
canterbury_height = nz_height[canterbury, ]

nz_height[canterbury, , op = st_disjoint]

sel_sgdp = st_intersects(x = nz_height, y = canterbury)
class(sel_sgdp)

sel_logical = lengths(sel_sgdp) > 10
canterbury_height2 = nz_height[sel_logical,]

canterbury_height3 = nz_height %>%
  filter(st_intersects(x = ., y = canterbury, sparse = FALSE))

# 4.2.2
# create a polygon
a_poly = st_polygon(list(rbind(c(-1, -1), c(1, -1), c(1, 1), c(-1, -1))))
a = st_sfc(a_poly)
# create a line
l_line = st_linestring(x = matrix(c(-1, -1, -0.5, 1), ncol = 2))
l = st_sfc(l_line)
# create points
p_matrix = matrix(c(0.5, 1, -1, 0, 0, 1, 0.5, 1), ncol = 2)
p_multi = st_multipoint(x = p_matrix)
p = st_cast(st_sfc(p_multi), "POINT")

st_intersects(p, a)
st_intersects(p, a, sparse = FALSE)

st_disjoint(p, a, sparse = FALSE)[, 1]

st_within(p, a, sparse = FALSE)[, 1]

st_touches(p, a, sparse = FALSE)[, 1]

sel = st_is_within_distance(p, a, dist = 0.9)
lengths(sel) > 0

# 4.2.3
set.seed(2018)
(bb_world = st_bbox(world))

random_df = tibble(
  x = runif(n = 10, min = bb_world[1], bb_world[3]),
  y = runif(n = 10, min = bb_world[2], bb_world[4])
)

random_points = random_df %>%
  st_as_sf(coords = c("x", "y")) %>%
  st_set_crs(4326)

world_random = world[random_points, ]
nrow(world_random)

random_joined = st_join(random_points, world["name_long"])

# 4.2.4 
plot(st_geometry(cycle_hire), col = "blue")
plot(st_geometry(cycle_hire_osm), add = TRUE, pch = 3, col = "red")

any(st_touches(cycle_hire, cycle_hire_osm, sparse = FALSE))

cycle_hire_P = st_transform(cycle_hire, 27700)
cycle_hire_osm_P = st_transform(cycle_hire_osm, 27700)
sel = st_is_within_distance(cycle_hire_P, cycle_hire_osm_P, dist = 20)
summary(lengths(sel) > 1)

z = st_join(cycle_hire_P, cycle_hire_osm_P, 
            join = st_is_within_distance, dist = 20)
nrow(cycle_hire_P)
nrow(z)

z = z %>%
  group_by(id) %>%
  summarise(capacity = mean(capacity))

nrow(z) == nrow(cycle_hire)

plot(cycle_hire_osm["capacity"])
plot(z["capacity"])

# 4.2.5 
nv_avheight = aggregate(x = nz_height, by = nz, FUN = mean)

nv_avheight2 = nz %>%
  st_join(nz_height) %>%
  group_by(Name) %>%
  summarize(elevation = mean(elevation, na.rm = TRUE))


agg_aw = st_interpolate_aw(incongruent[, "value"], aggregating_zones, 
                           extensive = TRUE)

agg_aw


# 4.2.6
nz_heightest = nz_height %>% top_n(n = 1, wt = elevation)
canterbury_centroid = st_centroid(canterbury)
st_distance(nz_heightest, canterbury_centroid)

co = filter(nz, grepl("Canter|Otag", Name))
st_distance(nz_height[1:3, ], co)

plot(st_geometry(co)[2])
plot(st_geometry(nz_height[2:3]), add = TRUE)


# 4.3.1

id = cellFromXY(elev, xy = c(0.1, 0.1))







