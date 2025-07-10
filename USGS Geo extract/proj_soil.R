library(dplyr)
library(stars)
library(sf)
library(soilDB)

test_sites <- read.csv("TestSites.csv")
test_sites_sf <- st_as_sf(test_sites, coords = c("lon","lat"), crs = 4269)

phy_prov <- read_sf("MLRA_52_2022/MLRA_52.shp")
phy_prov <- st_transform(phy_prov, crs = 4269)


geo_code <- read_sf("USGS_SGMC_Shapefiles/SGMC_Geology.shp")
geo_code <- st_transform(geo_code, crs = 4269)


joined <- st_join(test_sites_sf,phy_prov)


test_sites_sf_clean <- test_sites_sf |>
  select(Study,Site.Name,geometry)

test_sites_sf_clean <- test_sites_sf_clean[-2,]
  

# Check geometry validity
#valid_geo <- st_is_valid(geo_code)

# Drop invalid features or fix them
#geo_code <- geo_code[valid_geo, ]  # drop invalid rows
# Or try fixing them:
geo_code <- st_make_valid(geo_code)

geo_st_sites <- st_join(test_sites_sf_clean,geo_code)

#geo_st_sites_select <- geo_st_sites |>
  #select(Study,Site.Name,UNIT_LINK,MAJOR1)


lith_table <- read.csv("USGS_SGMC_Tables_CSV/SGMC_Lithology.csv")
units_table <- read.csv("USGS_SGMC_Tables_CSV/SGMC_Units.csv")

units_joined <- inner_join(geo_st_sites_select,units_table, by = "UNIT_LINK")
lith_joined <- inner_join(geo_st_sites_select,lith_table, by = "UNIT_LINK") |>
  filter(LITH_RANK == "Major")












