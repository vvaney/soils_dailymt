library(dplyr)
library(stars)
library(sf)
library(soilDB)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

setwd("GitHub/soils_dailymt")

all_sites <- read.csv("clean_sites.csv")
all_sites <- st_as_sf(all_sites, coords = c("lon","lat"), crs = 4269)

phy_prov <- read_sf("MLRA_52_2022/MLRA_52.shp")
phy_prov <- st_transform(phy_prov, crs = 4269)


geo_code <- read_sf("USGS_SGMC_Shapefiles/SGMC_Geology.shp")
geo_code <- st_transform(geo_code, crs = 4269)
geo_code <- st_make_valid(geo_code)

joined_phy <- st_join(all_sites,phy_prov)
geo_st_sites <- st_join(all_sites,geo_code)


geo_st_sites_select <- geo_st_sites |>
  select(ID,Site.Name,Town..County.State,ORIG_LABEL,SGMC_LABEL,UNIT_LINK,UNIT_NAME,geometry)

#all_in_all <- st_join(joined_phy,geo_st_sites)


geo_find <- st_drop_geometry(geo_st_sites_select)

lith_table <- read.csv("USGS_SGMC_Tables_CSV/SGMC_Lithology.csv")
units_table <- read.csv("USGS_SGMC_Tables_CSV/SGMC_Units.csv")
age_table <- read.csv("USGS_SGMC_Tables_CSV/SGMC_Age.csv")


geo_find_lith <-inner_join(geo_find,lith_table, by = "UNIT_LINK")

geo_find_unit_match <- geo_find |>
  select(-ORIG_LABEL,-SGMC_LABEL,-UNIT_NAME)

geo_find_unit <- inner_join(geo_find_unit_match,units_table, by = "UNIT_LINK")

geo_find_age <- inner_join(geo_find_unit_match,age_table, by = "UNIT_LINK")




geo_find_lith <- geo_find_lith |>
  filter(LITH_RANK == "Major")


us_states <- ne_states(country = "United States of America", returnclass = "sf")


southeast_states <- c(
  "Alabama", "Arkansas", "Florida", "Georgia", "Kentucky", "Louisiana",
  "Mississippi", "North Carolina", "South Carolina", "Tennessee", "Virginia","Texas"
)


se_states <- us_states[us_states$name %in% southeast_states, ]


ggplot() +
  geom_sf(data = se_states, fill = "white", color = "black") +
  geom_sf(data = look_m |> filter(not == "Ab"), aes(color = MLRARSYM), size = 1) +
  coord_sf() +
  theme_minimal() +
  labs(title = "Southeastern USA with Point Data", color = "MLRA Symbol")


####################################--------##############-----#############
#saving them tables


write.csv(geo_find_lith,"Lith_desc.csv")
write.csv(geo_find_unit,"Unit_desc.csv")
write.csv(joined_phy,"MLRA_desc.csv")
write.csv(geo_find_age,"Age_desc.csv")






##### if interested in cook classification system revist below ######


# all_in_all <- all_in_all |>
#   mutate(GEO_CODE = case_when(
#     UNIT_NAME %in% c("Cockfield Formation", "Cook Mountain Formation") ~ "Cb",
#     MLRARSYM == "134" ~ "Lo",
#     UNIT_NAME == "Pamlico shoreline complex - marsh and lagoonal facies" ~ "Pa",
#     UNIT_NAME %in% c(
#       "Talbot shoreline complex - barrier island facies",
#       "Penholoway shoreline complex - marsh and lagoonal facies",
#       "Wicomico Shoreline Complex - barrier island facies",
#       "Wicomico Shoreline Complex - marsh and lagoonal facies"
#     ) ~ "Al",
#     
#     UNIT_NAME %in% c("Citronelle formation", "Citronelle Formation") ~ "Ct",
#     
#     MLRARSYM %in% c(
#       "150", "151", "152", "133A", "133B", "135A", "135B", "136",
#       "116", "117", "118", "119", "122", "123", "124", "125",
#       "128", "129", "130", "131A", "131B", "131C", "131D"
#     ) ~ "Av",
#     
#     
#     TRUE ~ NA
#   ))
# 
# 
# all_in_all <- all_in_all %>%
#   mutate(Phy_Prov = case_when(
#     MLRARSYM %in% c(
#       "135A", "135B",
#       "116", "117", "118", "119", "122", "123", "124", "125",
#       "128", "129", "130", "131A", "131B", "131C", "131D"
#     ) ~ "AA",
#     
#     MLRARSYM == "134" ~ "LP",
#     MLRARSYM %in% c("133A","133C") ~ "SC",
#     MLRARSYM == "133B" ~ "WG",
#     MLRARSYM == "136" ~ "PD",
#     MLRARSYM == "137" ~ "SH",
#     MLRARSYM %in% c("138","153","154","155","156","153A"
#     ) ~ "AF",
#     MLRARSYM %in% c("150", "151","152") ~ "GF",
#     MLRARSYM %in% c("116", "117","118","119","122",
#                     "123","124","125","128","129","130") ~ "MT",
#     TRUE ~ NA
#   ))


