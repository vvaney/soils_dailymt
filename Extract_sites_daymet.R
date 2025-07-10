library(dplyr)
library(daymetr)

all_sites <- read.csv("GitHub/soils_dailymt/clean_sites.csv")

all_sites$Planted[all_sites$ID == "LCP_CCLONES1_Rey"] <- 2002
all_sites$Planted[all_sites$ID == "LCP_CCLONES1_BFG"] <- 2002

all_sites$Harvested <- ifelse(is.na(all_sites$Harvested), 2023, all_sites$Harvested)


all_sites <- all_sites |>
  select(-Series,-Soil.type.and.or.CRIFF.series)



df <- all_sites 

dir.create("DAYMET_saved", showWarnings = FALSE)

for (i in 1:nrow(df)) {
  

  site_name <- df$ID[i]
  lat <- df$lat[i]
  lon <- df$lon[i]
  start_year <- df$Planted[i]    
  end_year <- df$Harvested[i]    
  
  # Only run if both years are not missing
  if (!is.na(start_year) && !is.na(end_year)) {
    
    # Download and save the data to disk
    download_daymet(
      site = site_name,
      lat = lat,
      lon = lon,
      start = start_year,
      end = end_year,
      path = "DAYMET_saved",     # folder to save files
      internal = FALSE             # FALSE = save as .csv, not returned to R
    )
    
    print(paste("Downloaded:", site_name))
    
  } else {
    print(paste("Skipping:", site_name, "- missing year(s)"))
  }
}






read_daymet <- function(filepath) {
  df <- read.csv(filepath, skip = 6, header = TRUE)
  
  # Extract ID from filename
  site_id <- gsub("_\\d{4}_\\d{4}\\.csv", "", basename(filepath))
  df$ID <- site_id
  
  return(df)
}



files <- list.files("DAYMET_saved", full.names = TRUE, pattern = "\\.csv$")
site_ids <- gsub("_\\d{4}_\\d{4}\\.csv", "", basename(files))


daymet_list <- lapply(files, read_daymet)

# 5. Name each element of the list by site ID
names(daymet_list) <- site_ids



library(data.table)

summarize_daymet_per_year <- function(dm) {
  required_cols <- c("tmin..deg.c.", "tmax..deg.c.", "vp..Pa.", "prcp..mm.day.", "year", "ID")
  
  
  dm$svp.min <- 0.6108 * exp((17.27 * dm$tmin..deg.c.) / (273.3 + dm$tmin..deg.c.))
  dm$svp.max <- 0.6108 * exp((17.27 * dm$tmax..deg.c.) / (273.3 + dm$tmax..deg.c.))
  
  dm$vpd <- ((dm$svp.max + dm$svp.min) / 2) - (dm$vp..Pa. / 1000)
  dm$vpd <- ifelse(dm$vpd < 0, 0.000001, dm$vpd)
  
  dm$vpdmx <- dm$svp.max - (dm$vp..Pa. / 1000)
  dm$vpdmx <- ifelse(dm$vpdmx <= 0, 0.000001, dm$vpdmx)
  
  dm$tavg <- (dm$tmax..deg.c. + dm$tmin..deg.c.) / 2
  
  dmdt <- data.table(dm)
  
  summary <- dmdt[, .(
    tavg = mean(tavg, na.rm = TRUE),
    tmax = mean(tmax..deg.c., na.rm = TRUE),
    tmin = mean(tmin..deg.c., na.rm = TRUE),
    vpd = mean(vpd, na.rm = TRUE),
    vpdmx = mean(vpdmx, na.rm = TRUE),
    prcp = sum(prcp..mm.day., na.rm = TRUE)
  ), by = .(year, ID)]
  
  return(summary)
}


climate_summary_list <- lapply(daymet_list, summarize_daymet_per_year)
climate_summary_all <- data.table::rbindlist(climate_summary_list)


write.csv(climate_summary_all,file = "GitHub/soils_dailymt/climate_summary_yearly.csv")








