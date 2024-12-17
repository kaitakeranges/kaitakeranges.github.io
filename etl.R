library(httr)
library(jsonlite)
library(tidyverse)


#### API calls to retrieve data from traps.nz. One call for trap status (1. df_trap_status), the second for trap records (2. df_trap_records)
#### 1. df_trap_status. This holds data for trap installation date, last record etc. ####

base_url <- 'https://io.trap.nz/geo/trapnz-projects/wfs/'
end_url <- "?service=WFS&version=2.0.0&request=GetFeature&typeName=trapnz-projects:my-projects-traps&outputFormat=json"


df_traps <- fromJSON(paste0(base_url, Sys.getenv("API_KEY"), "/", Sys.getenv("PROJECT_KEY"), end_url)) # Full result for traps
df_trap_points_list <- df_traps[[2]][3]$geometry # Parse the trap geometry first into a dataframe

df_trap_points <- do.call(rbind, lapply(df_trap_points_list$coordinates, function(x) {
  data.frame(longitude = x[1], latitude = x[2])
}))

df_trap_properties <- df_traps$features$properties # Parse the trap properties into a second dataframe

df_trap_status <- cbind(df_trap_points, df_trap_properties) # Join coordinates to properties
saveRDS(df_trap_status, file = "df_trap_status.rds")

date_trap_status <- as.Date(today())
saveRDS(date_trap_status, file = "date_trap_status.rds")

rm("df_traps", "df_trap_points_list", "df_trap_points", "df_trap_properties") # Remove the intermediate lists and dataframes
#### 1. End of etl for df_trap_status. ####



#### 2. df_trap_records ####
# For this end point, you can specify a numner of records (max = 10,000) and a startindex.
# So start with startindex of 0, then add 10,000 to this for each loop until the response != 200.
startindex <- readRDS("startindex.rds")
base_url <- 'https://io.trap.nz/geo/trapnz-projects/wfs/'
#end_url <- '?service=WFS&version=2.0.0&request=GetFeature&typeName=trapnz-projects:my-projects-trap-records&outputFormat=json' THIS RETURNS ALL PROJECTS THAT USER HAS ACCESS TO.
end_url <- '?service=WFS&version=2.0.0&request=GetFeature&typeName=trapnz-projects:default-project-trap-records&outputFormat=json'
url_records <- paste0(base_url, Sys.getenv("API_KEY"), '/', Sys.getenv("PROJECT_KEY"), end_url)
if(is.null(startindex) || startindex == 0) {
  startindex <- 0
  rm(df_trap_records)
} 
count <- 5000
repeat {
  get_url <- paste0(url_records, "&count=", count, "&startindex=", startindex)
  df_raw <- GET(get_url)
  if(status_code(df_raw) !=200) {
    print(paste("API request failed with status code: "), status_code(df_raw))
    break
  }
  df_raw_content <- content(df_raw, "parsed", simplifyVector = TRUE)
  count_records <- nrow(df_raw_content$features$properties)
  if(is.null(count_records) || count_records == 0) {
    print("No more records to return")
    break
  }
  df_trap_properties <- df_raw_content$features$properties
  df_trap_properties <- df_trap_properties |> 
    mutate(species_level_1 = 
             case_when(
               species_caught == "Rat - Ship" ~ "Rat",
               species_caught == "Rat - Norway" ~ "Rat",
               species_caught == "Rat - Kiore" ~ "Rat",
               species_caught == "Canadian geese" ~ "Other",
               species_caught == "Rabbit" ~ "Other",
               species_caught == "Bird" ~ "Other",
               species_caught == "Magpie" ~ "Other",
               species_caught == "Hare" ~ "Other",
               species_caught == "Unspecified" ~ "Other",
               TRUE ~ species_caught
             )) |> 
    mutate(species_level_2 = 
             case_when(
               species_level_1 == "Rat" ~ "Rat",
               species_level_1 == "Stoat" ~ "Mustelid",
               species_level_1 == "Ferret" ~ "Mustelid",
               species_level_1 == "Weasel" ~ "Mustelid",
               species_level_1 == "Cat" ~ "Cat",
               species_level_1 == "None" ~ "None",
               TRUE ~ "Other"
             )
    ) |> 
    mutate(
      year = year(as.Date(record_date)),
      last_14_days = 
        case_when(
          as.Date(today()) <= as.Date(record_date) + 14 ~ 1,
          TRUE ~ 0
        ),
      last_28_days = 
        case_when(
          as.Date(today()) <= as.Date(record_date) + 28 ~ 1,
          TRUE ~ 0
        ),
      last_14_days_ly = 
        case_when(
          as.Date(today()) - 365 <= as.Date(record_date) + 14 & as.Date(today()) - 365 > as.Date(record_date) ~ 1,
          TRUE ~ 0
        ),
      last_28_days_ly = 
        case_when(
          as.Date(today()) - 365 <= as.Date(record_date) + 28 & as.Date(today()) - 365 > as.Date(record_date) ~ 1,
          TRUE ~ 0
        ),
      yyyy_ww = strftime(as.Date(record_date), "%Y-%V")
    )
  df_trap_point <- do.call(rbind, lapply(df_raw_content$features$geometry$coordinates, function(x) {
    data.frame(longitude = x[1], latitude = x[2])
  }))
  df_trap_combined <- cbind(df_trap_point, df_trap_properties)
  if(exists('df_trap_records') && is.data.frame(get('df_trap_records'))) {
    df_trap_records <- rbind(df_trap_records, df_trap_combined)
  } else {
    df_trap_records <- df_trap_combined
  }
  startindex <- startindex + count_records
  #print(paste("startindex is now: ", startindex))
}

saveRDS(startindex, file = "startindex.rds")
saveRDS(df_trap_records, file = "df_trap_records.rds")