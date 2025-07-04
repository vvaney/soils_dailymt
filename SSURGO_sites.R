
library(sf)
library(soilDB)
library(dplyr)


pts <- st_transform(all_sites, crs = 4326)

pts$pt_id <- seq_len(nrow(pts)) # This keeps your custom strings

# STEP 3: Extract mukey using custom IDs
mu_points <- SDA_spatialQuery(
  geom = pts,
  what = "mukey",
  byFeature = TRUE,
  idcol = "pt_id"  # Must match the column you just created
)

head(mu_points)


unique_mukeys <- unique(mu_points$mukey)
mu_in_clause <- format_SQL_in_statement(unique_mukeys)

component_data <- SDA_query(sprintf("
  SELECT *
  FROM component
  WHERE mukey IN %s
", mu_in_clause))


pts_with_mu <- pts %>%
  left_join(mu_points, by = "pt_id")

pts_with_components <- pts_with_mu %>%
  left_join(component_data, by = "mukey") |>
  filter(comppct_r > 50)

comp_select <- pts_with_components |>
  select(ID,Study,Test,Site.Name,Town..County.State,Planted,Harvested,Species,acres,mukey,muname,comppct_r,compname,
    drainagecl,taxclname,taxorder,taxsuborder,taxgrtgroup,taxsubgrp,taxpartsize,taxceactcl,taxtempcl,taxmoistscl,cokey,geometry)


#write.csv(comp_select,file = "component_ssurgo.csv")
#write.csv(pts_with_components, file = "comp_all_vars_ssurgo.csv")
