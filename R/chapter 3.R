library(sf)
library(raster)
library(dplyr)
library(stringr)
library(tidyr)
library(spData)

# 3.2
methods(class = "sf")

dim(world)
nrow(world)
ncol(world)

world_df = st_drop_geometry(world)
class(world_df)

# 3.2.1 

world[1:6, ]
world[, 1:3]
world[, c("name_long", "lifeExp")]

sel_area = world$area_km2 < 10000
summary(sel_area)
small_countries = world[sel_area, ]
small_countries = world[world$area_km2 < 10000, ]
small_countries = subset(world, area_km2 < 10000)

world1 = dplyr::select(world, name_long, pop)
names(world1)

world2 = dplyr::select(world, name_long:pop)

world3 = dplyr::select(world, -subregion, -area_km2)

world4 = dplyr::select(world, name_long, population = pop)
names(world4)

world5 = world[, c("name_long", "pop")]
names(world5)[names(world5) == "pop"] = "population"

d = data.frame(pop = 1:10, area = 1:10)

d[, "pop", drop = FALSE]
select(d, pop)

d[,"pop"]
pull(d, "pop")

world[, "pop"]

world$pop
pull(world, pop)

slice(world, 3:5)

world6 = filter(world, lifeExp > 82)

world7 = world %>% 
  filter(continent == "Asia") %>%
  dplyr::select(name_long, continent) %>%
  slice(1:5)

world8 = slice(
  dplyr::select(
    filter(world, continent == "Asia"),
    name_long, continent),
  1:5
  )

# 3.2.2 
world_agg1 = aggregate(pop ~ continent, FUN = sum, data = world, na.rm = TRUE)
class(world_agg1)

world_agg2 = aggregate(world["pop"], by = list(world$continent), FUN = sum, na.rm = TRUE)
class(world_agg2)

world_agg3 = world %>%
  group_by(continent) %>%
  summarize(pop = sum(pop, na.rm = TRUE))

world %>% 
  summarize(pop = sum(pop, na.rm = TRUE), n = n())

world %>%
  dplyr::select(pop, continent) %>%
  group_by(continent) %>%
  summarize(pop = sum(pop, na.rm = TRUE), n_countries = n()) %>%
  top_n(n = 3, wt = pop) %>%
  arrange(desc(pop)) %>%
  st_drop_geometry()

# 3.2.3 

world_coffee = left_join(world, coffee_data)
class(world_coffee)

names(world_coffee)
plot(world_coffee["coffee_production_2017"])

coffee_renamed = rename(coffee_data, nm = name_long)
world_coffee2 = left_join(world, coffee_renamed, by = c(name_long = "nm"))

world_coffee_inner = inner_join(world, coffee_data)
nrow(world_coffee_inner)

setdiff(coffee_data$name_long, world$name_long)

str_subset(world$name_long, "Dem*.+Congo")

coffee_data$name_long[grepl("Congo,", coffee_data$name_long)] = 
  str_subset(world$name_long, "Dem*.+Congo")
world_coffee_match = inner_join(world, coffee_data)
nrow(world_coffee_match)

coffee_world = left_join(coffee_data, world)
class(coffee_world)

st_as_sf(coffee_world)

# 3.2.4 

world %>%
  mutate(pop_dens = pop / area_km2)

world %>%
  transmute(pop_dens = pop / area_km2)

world_unite = world %>%
  unite("con_reg", continent:region_un, sep = ":", remove = TRUE)

world_separate = world_unite %>%
  separate(con_reg, c("continent", "region_un"), sep = ":")

world %>%
  rename(name = name_long)

new_names = c("i", "n", "c", "r", "s", "t", "a", "p", "l", "gP", "geom")
world %>%
  setNames(new_names)

world_data = world %>% st_drop_geometry()
class(world_data)

# 3.3

elev = raster(nrows = 6, ncols = 6, res = 0.5, 
              xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5,
              vals = 1:36)

grain_order = c("clay", "silt", "sand")
grain_char = sample(grain_order, 36, replace = TRUE)
grain_fact = factor(grain_char, levels = grain_order)
grain = raster(nrows = 6, ncols = 6, res = 0.5, 
               xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5,
               vals = grain_fact)

ratify(grain)
levels(grain)[[1]] = cbind(levels(grain)[[1]], wetness = c("wet", "moist", "dry"))
levels(grain)

# 3.3.1 
elev[1, 1] # row 1, column 1
elev[1] # ID 1
values(elev)
getValues(elev)

r_stack = stack(elev, grain)
names(r_stack) = c("elev", "grain")

raster::subset(r_stack, "elev")
r_stack[["elev"]]
r_stack$elev

elev[1, 1] = 0
elev[]

elev[1, 1:2] = 0

# 3.3.2 
elev
summary(elev)
cellStats(elev, sd)

summary(brick(elev, grain))

hist(elev)
