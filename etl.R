library(httr)
library(jsonlite)
library(tidyverse)


#### API calls to retrieve data from traps.nz. One call for trap status (1. df_trap_status), the second for trap records (2. df_trap_records)
#### 1. df_trap_status. This holds data for trap installation date, last record etc. ####

current_tz <- Sys.timezone()

api_key <- Sys.getenv("API_KEY")
project_key <- Sys.getenv("PROJECT_KEY")
if (api_key == "") stop("API_KEY is not set")
if (project_key == "") stop("PROJECT_KEY is not set")

base_url <- 'https://io.trap.nz/geo/trapnz-projects/wfs/'
end_url <- "?service=WFS&version=2.0.0&request=GetFeature&typeName=trapnz-projects:my-projects-traps&outputFormat=json"

df_traps <- fromJSON(paste0(base_url, api_key, "/", project_key, end_url)) # Full result for traps
df_trap_points_list <- df_traps[[2]][3]$geometry # Parse the trap geometry first into a dataframe

df_trap_points <- do.call(rbind, lapply(df_trap_points_list$coordinates, function(x) {
  data.frame(longitude = x[1], latitude = x[2])
}))

df_trap_properties <- df_traps$features$properties # Parse the trap properties into a second dataframe

df_trap_status <- cbind(df_trap_points, df_trap_properties)

saveRDS(df_trap_status, file = "df_trap_status.rds")

if(current_tz == "Pacific/Auckland") {
  date_refreshed <- as_datetime(Sys.time())
  } else {
  date_refreshed <- as_datetime(Sys.time()) + lubridate::hours(13)
  }
saveRDS(date_refreshed, file = "date_refreshed.rds")

rm("df_traps", "df_trap_points_list", "df_trap_points", "df_trap_properties") # Remove the intermediate lists and dataframes
#### 1. End of etl for df_trap_status. ####


# 2. df_trap_records ----
# For this end point, you can specify a number of records (max = 10,000) and a startindex.
# So start with startindex of 0, then add 5,000 to this for each loop until the response != 200.

startindex <- readRDS("startindex.rds")
base_url <- 'https://io.trap.nz/geo/trapnz-projects/wfs/'
end_url <- '?service=WFS&version=2.0.0&request=GetFeature&typeName=trapnz-projects:default-project-trap-records&outputFormat=json'
url_records <- paste0(base_url, api_key, '/', project_key, end_url)

if(is.null(startindex) || startindex == 0) {
   startindex <- 0
   if(exists("df_trap_records")){rm("df_trap_records")}
}

df_trap_records <- readRDS("df_trap_records.rds")

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
   print(paste("count raw records: ", count_records))
   
   if(is.null(count_records) || count_records == 0) {
     print("No more records to return")
     break
   }

  df_trap_properties <- df_raw_content$features$properties
  df_trap_properties <- df_trap_properties |>
    select(
       record_id,
       line,
       trap_id,
       trap_code,
       trap_type,
       strikes,
       species_caught,
       record_date,
       prev_record_date,
       recorded_by,
       username
     ) %>%
    mutate(
      record_date = force_tz(as_datetime(record_date), tzone_out = "Pacific/Auckland")
    ) %>% 
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
    mutate(Trapper = str_trim(coalesce(recorded_by, username))) |>
    mutate(Trapper = case_when(
       Trapper == "Brandon Arana Kingi" ~ "Brandon Kingi",
       Trapper == "Jc" ~ "JC",
       Trapper == "Vaughan scott Turner" ~ "Vaughan Turner",
       Trapper == "tobyshanley" ~ "Toby Shanley",
       Trapper == "rachatsea" ~ "Rachel Law",
       Trapper == "Mark D." ~ "Mark Danenhauer",
       Trapper == "John and Murray" ~ "John Freeman",
       Trapper == "steve leach" ~ "Steve Leach",
       Trapper == "Adrian van't Hof" ~ "Adrian Van't Hof",
       Trapper == "Bazz" ~ "Baz",
       Trapper == "Cameron B" ~ "Cameron Blencowe",
       Trapper == "Jared Coombess" ~ "Jared Coombes",
       Trapper == "Jared Coomes" ~ "Jared Coombes",
       Trapper == "Adrian van’t Hof" ~ "Adrian Van’t Hof",
       Trapper == "Tobyshanley" ~ "Toby Shanley",
       Trapper == "Mholmes" ~ "Melissa Holmes",
       Trapper == "MHolmes" ~ "Melissa Holmes",
       Trapper == "Mellissa Holmes" ~ "Melissa Holmes",
       Trapper == "hank reinders" ~ "Hank Reinders",
       Trapper == "Hank reinders" ~ "Hank Reinders",
       Trapper == "Janko" ~ "Janko Reinders",
       Trapper == "jankoreinders" ~ "Janko Reinders",
       Trapper == "Jackie keenan" ~ "Jackie Keenan",
       Trapper == "Jackie" ~ "Jackie Keenan",
       Trapper == "jackie keenan" ~ "Jackie Keenan",
       Trapper == "Troy" ~ "Troy Bethell",
       Trapper == "Troy Bethel" ~ "Troy Bethell",
       Trapper == "plobb" ~ "Paul Lobb",
       Trapper == "G Lilburn" ~ "Grant Lilburn",
       Trapper == "grant lilburn" ~ "Grant Lilburn",
       Trapper == "Stuartjulian" ~ "Stuart Julian",
       Trapper == "StuartJulian" ~ "Stuart Julian",
       Trapper == "shaunamoff" ~ "Shaun Moffitt",
       Trapper == "mholmes" ~ "Melissa Holmes",
       Trapper == "Robbie Mcgreggor" ~ "Robbie McGregor",
       Trapper == "Robert Charles McGregor" ~ "Robbie McGregor",
       Trapper == "Robert McGregor" ~ "Robbie McGregor",
       Trapper == "Rob Mcgregor" ~ "Robbie McGregor",
       Trapper == "Robbie" ~ "Robbie McGregor",
       Trapper == "Robbie McGregor" ~ "Robbie McGregor",
       Trapper == "rob mcgregor" ~ "Robbie McGregor",
       Trapper == "Robbie" ~ "Robbie McGregor",
       Trapper == "Mark.danenhauer" ~ "Mark Danenhauer",
       Trapper == "mark d" ~ "Mark Danenhauer",
       Trapper == "mark d." ~ "Mark Danenhauer",
       Trapper == "mark D." ~ "Mark Danenhauer",
       Trapper == "Mark d." ~ "Mark Danenhauer",
       Trapper == "atcullen45" ~ "Andy Cullen",
       Trapper == "Mereana" ~ "Mereana Hanrahan",
       Trapper == "Mereana Hanraan" ~ "Mereana Hanrahan",
       Trapper == "Merean Hanrahan" ~ "Mereana Hanrahan",
       Trapper == "john freeman" ~ "John Freeman",
       Trapper == "john Freeman" ~ "John Freeman",
       Trapper == "John freeman" ~ "John Freeman",
       Trapper == "john fremman" ~ "John Freeman",
       Trapper == "bazz" ~ "Baz",
       Trapper == "johvel53@gmail.com" ~ "John Velvin",
       Trapper == "bkingi" ~ "Brandon Kingi",
       Trapper == "pete" ~ "Pete Morgan",
       Trapper == "Pete" ~ "Pete Morgan",
       Trapper == "Pete M" ~ "Pete Morgan",
       Trapper == "Pete m" ~ "Pete Morgan",
       Trapper == "Petemorgan" ~ "Pete Morgan",
       Trapper == "pmorgan" ~ "Pete Morgan",
       Trapper == "Pmorgan" ~ "Pete Morgan",
       Trapper == "Peter Morgan" ~ "Pete Morgan",
       Trapper == "Susan E" ~ "Susan Eagar",
       Trapper == "Chania" ~ "Chania Hattle",
       Trapper == "roger" ~ "Roger Jones",
       Trapper == "Roger" ~ "Roger Jones",
       Trapper == "R Jones" ~ "Roger Jones",
       Trapper == "chania hattle" ~ "Chania Hattle",
       Trapper == "nickbarm" ~ "Nick Armstrong",
       Trapper == "tdixon" ~ "Toby Dixon",
       Trapper == "Tdixon" ~ "Toby Dixon",
       Trapper == "joash" ~ "Joash",
       Trapper == "dave ferens" ~ "Dave Ferens",
       Trapper == "liampaterson" ~ "Liam Paterson",
       Trapper == "Liampaterson" ~ "Liam Paterson",
       Trapper == "Liam" ~ "Liam Paterson",
       Trapper == "martin m" ~ "Martin M",
       Trapper == "spencer" ~ "Spencer Lister",
       Trapper == "Spencer" ~ "Spencer Lister",
       Trapper == "Robert m" ~ "Robert Morgan",
       Trapper == "Robert morgan" ~ "Robert Morgan",
       Trapper == "secampbellnz@gmail.com" ~ "Sarah Campbell",
       Trapper == "Tom" ~ "Tom Ryder",
       Trapper == "daryl" ~ "Daryl",
       Trapper == "DARYL" ~ "Daryl",
       Trapper == "Drayl" ~ "Daryl",
       Trapper == "Tim" ~ "Tim Sjoberg",
       Trapper == "stevefrancis" ~ "Steve Francis",
       Trapper == "N Lightbourne" ~ "Nathan Lightbourne",
       Trapper == "Ian" ~ "Ian Swan",
       Trapper == "Samuel Mullin" ~ "Sam Mullin",
       Trapper == "stevefrancis" ~ "Steve Francis",
       Trapper == "Bryce" ~ "Bryce Vickers",
       Trapper == "T Morris" ~ "Trevor Morris",
       Trapper == "Daveclarke" ~ "Dave Clarke",
       Trapper == "jimmyc" ~ "Jimmy C",
       Trapper == "JimmyC" ~ "Jimmy C",
       Trapper == "KTurton" ~ "K Turton",
       Trapper == "Mdixon" ~ "M Dixon",
       Trapper == "taranakimounga" ~ "Taranaki Mounga Project",
       Trapper == "cblencowe" ~ "Cameron Blencowe",
       Trapper == "gabriel.lennon" ~ "Gabriel Lennon",
       TRUE ~ Trapper
     )) |>
    mutate(
       Trapper_anon = Trapper %>%
         str_split(" ") %>%                       # Split name into words
         map_chr(~ if (length(.x) == 1) {
           .x                                    # If only one word, return as is
         } else {
           paste(.x[1], paste(substr(.x[-1], 1, 1), collapse = ""), sep = " ") # Keep first name, initials for others
         })
      ) |>
    mutate(
       year = year(as.Date(record_date)),
       last_14_days = 0,
       last_28_days = 0,
       last_14_days_ly = 0,
       last_28_days_ly = 0,
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
   print(paste("startindex now: ", startindex))
}

df_trap_records <- df_trap_records %>% 
  mutate(
    last_14_days =
      case_when(
        #as.Date(with_tz(Sys.time(), tzone = "Pacific/Auckland")) <= as.Date(record_date) + 14 ~ 1,
        as.Date(readRDS("date_refreshed.rds")) <= as.Date(record_date) + 14 ~ 1,
        TRUE ~ 0
      ),
    last_28_days =
      case_when(
        #as.Date(with_tz(Sys.time(), tzone = "Pacific/Auckland")) <= as.Date(record_date) + 28 ~ 1,
        as.Date(readRDS("date_refreshed.rds")) <= as.Date(record_date) + 28 ~ 1,
        TRUE ~ 0
      ),
    last_14_days_ly =
      case_when(
        #as.Date(with_tz(Sys.time(), tzone = "Pacific/Auckland")) - 365 <= as.Date(record_date) + 14 & as.Date(with_tz(Sys.time(), tzone = "Pacific/Auckland")) - 365 > as.Date(record_date) ~ 1,
        as.Date(readRDS("date_refreshed.rds")) - 365 <= as.Date(record_date) + 14 & as.Date(readRDS("date_refreshed.rds")) - 365 > as.Date(record_date) ~ 1,
        TRUE ~ 0
      ),
    last_28_days_ly =
      case_when(
        #as.Date(with_tz(Sys.time(), tzone = "Pacific/Auckland")) - 365 <= as.Date(record_date) + 28 & as.Date(with_tz(Sys.time(), tzone = "Pacific/Auckland")) - 365 > as.Date(record_date) ~ 1,
        as.Date(readRDS("date_refreshed.rds")) - 365 <= as.Date(record_date) + 28 & as.Date(readRDS("date_refreshed.rds")) - 365 > as.Date(record_date) ~ 1,
        TRUE ~ 0
      )
  )

saveRDS(startindex, file = "startindex.rds")
saveRDS(df_trap_records, file = "df_trap_records.rds")

if(current_tz == "Pacific/Auckland") {
  date_trap_status <- as_datetime(max(df_trap_records$record_date, na.rm = TRUE))
} else {
  date_trap_status <- as_datetime(max(df_trap_records$record_date, na.rm = TRUE)) + lubridate::hours(13)
}
saveRDS(date_trap_status, file = "date_trap_status.rds")

if(exists("df_raw")){rm("df_raw")}
if(exists("df_raw_content")){rm("df_raw_content")}
if(exists("df_trap_combined")){rm("df_trap_combined")}
if(exists("df_trap_point")){rm("df_trap_point")}
if(exists("df_trap_properties")){rm("df_trap_properties")}

# Create dataframe for trap table ----

if (is.null(df_trap_records)) stop("Error: df_trap_records is NULL")
print(paste("Rows in df_trap_records:", nrow(readRDS("df_trap_records.rds"))))

df_trap_table <- readRDS("df_trap_records.rds") |>
   filter(year > 2017) |>
   select(record_date, year, record_id, species_level_1, last_14_days, last_28_days, last_14_days_ly, last_28_days_ly)

if (is.null(df_trap_table)) stop("Error: df_trap_table is NULL")
print(paste("Rows in df_trap_table:", nrow(df_trap_table)))

df_trap_table_1 <- df_trap_table |>
   group_by(year, species_level_1) |>
   summarise(count = n()) |>
   pivot_wider(names_from = species_level_1, values_from = count, values_fill = 0) |>
   mutate(period = as.character(year))

df_trap_table_2_columns <- c("last_14_days", "last_28_days", "last_14_days_ly", "last_28_days_ly")

df_trap_table_2 <- df_trap_table |>
   filter_at(vars(df_trap_table_2_columns), any_vars(. == 1)) |>
   pivot_longer(cols = df_trap_table_2_columns, names_to = "period", values_to = "flag") |>
   filter(flag == 1) |>
   group_by(period, species_level_1) |>
   summarise(count = n()) |>
   pivot_wider(names_from = species_level_1, values_from = count, values_fill = 0)

df_trap_table_data <- rbind(df_trap_table_1, df_trap_table_2) |>
   select(-year) |>
   mutate(period = case_when(
     period == "last_14_days" ~ "Last 2 Weeks",
     period == "last_28_days" ~ "Last 4 Weeks",
     period == "last_14_days_ly" ~ "Last 2 Weeks (last year)",
     period == "last_28_days_ly" ~ "Last 4 Weeks (last year)",
     TRUE ~ period
   )) |>
   arrange(desc(period)) |>
   mutate_all((~replace(., is.na(.), 0))) |>
   mutate(across(Cat:Possum, ~format(., big.mark = ","))) |>
   arrange(factor(period, c("Last 2 Weeks", "Last 2 Weeks (last year)", "Last 4 Weeks", "Last 4 Weeks (last year)")))
df_trap_table_data <- df_trap_table_data[, c("period", "None", "Rat", "Stoat", "Ferret", "Weasel", "Hedgehog", "Cat", "Other")]

saveRDS(df_trap_table_data, file = "df_trap_table_data.rds")

 if(exists("df_trap_table")){rm("df_trap_table")}
 if(exists("df_trap_table_1")){rm("df_trap_table_1")}
 if(exists("df_trap_table_2")){rm("df_trap_table_2")}

 # Create dataframe for trapline ----
trap_species_caught <- readRDS("df_trap_records.rds") |>
   select(trap_id, trap_code, species_level_2, strikes) |>
   mutate(species = case_when(
     species_level_2 == "Rat" ~ "Rat",
     species_level_2 == "Mustelid" ~ "Mustelid",
     species_level_2 == "Cat" ~ "Other",
     species_level_2 == "None" ~ "None",
     TRUE ~ "Other"
   )) |>
   filter(species_level_2 != "None") |>
   group_by(trap_id, trap_code, species) |>
   summarize(strikes = sum(strikes)) |>
   ungroup() |>
   pivot_wider(names_from = species, values_from = strikes, values_fill = 0) |>
   relocate(trap_id, trap_code, Rat, Mustelid, Other)

trap_line_summary <- df_trap_records |>
   select(line, trap_id, trap_code, record_date, prev_record_date, strikes, species_caught) |>
   drop_na(line) |>
   group_by(trap_id) |>
   filter(species_caught != "None") |>
   summarize(
     last_catch = max(record_date),
     last_species = last(species_caught[record_date == max(record_date)])) |>
   ungroup() |>
   right_join(df_trap_records, by = "trap_id") |>
   select(line, trap_id, trap_code, record_date, prev_record_date, species_caught, strikes, record_id, last_catch, last_species) |>
   mutate(days_since = ifelse(is.na(as.integer(as.Date(record_date) - as.Date(prev_record_date))), 0, as.integer(as.Date(record_date) - as.Date(prev_record_date)))) |>
   filter(species_caught != "None")

 trap_line_table <- trap_line_summary |>
   group_by(line, trap_code, last_catch, last_species) |>
   summarise(
     "strikes" = sum(strikes),
     "time checked" = n(),
     "ave_days_between" = ave(days_since)) |>
   ungroup() |>
   select(line, trap_code, strikes, ave_days_between, last_catch, last_species) |>
   mutate(
     ave_days_between = round(ave_days_between, digits = 0),
     last_catch = as.Date(last_catch, format = "%Y-%m-%d"),
     days_last_catch = format(as.integer(as.Date(today()) - as.Date(last_catch)), big.mark = ",")) |>
   unique() |>
   drop_na(line) |>
   left_join(trap_species_caught, by = join_by(trap_code == trap_code)) |>
   select(-trap_id)

saveRDS(trap_line_table, file = "trap_line_table.rds")
if(exists("trap_species_caught")){rm("trap_species_caught")}
if(exists("trap_line_summary")){rm("trap_line_summary")}

# Create dataframe for trapstatus.qmd ----
date_today <- readRDS("date_refreshed.rds")

df_trap_info <- readRDS("df_trap_records.rds") %>% 
  filter(! trap_type %in% c('Unspecified', 'A24', 'Blitz', 'SA Cat', 'Rewild F-bomb')) %>% 
  drop_na(line) %>% 
  distinct(line, trap_id, longitude, latitude) %>% 
  count(line, name = "no_traps")

df_traps_checked <- readRDS("df_trap_records.rds") %>% 
  filter(! trap_type %in% c('Unspecified', 'A24', 'Blitz', 'SA Cat', 'Rewild F-bomb')) %>% 
  drop_na(line) %>%
  mutate(record_date = as.Date(record_date)) %>% 
  distinct(line, trap_id, record_date) %>% 
  count(line, record_date, name = "checks")

df_traps_checked <- df_traps_checked %>% 
  left_join(df_trap_info, by = "line") %>% 
  mutate(pct_checked = checks / no_traps)

df_line_checked <- df_traps_checked %>% 
  filter(pct_checked > 0.5) %>% 
  group_by(line, no_traps) %>% 
  summarise(last_over_half = max(record_date), .groups = "drop") %>% 
  mutate(days_since_over_half = as.numeric(as.Date(today()) - as.Date(last_over_half)))

df_trap_status_2 <- readRDS("df_trap_records.rds") |>
  drop_na(line) |>
  filter(! trap_type %in% c('Unspecified', 'A24', 'Blitz', 'SA Cat', 'Rewild F-bomb')) |>
  select(
    line,
    trap_code,
    record_date,
    Trapper_anon,
    species_level_2
  ) |>
  mutate(record_date = as.Date(record_date)) |>
  mutate(days_since = as.numeric(as.Date(date_today) - as.Date(record_date))) |>
  select(line, trap_code, Trapper_anon, record_date, days_since, species_level_2) |>
  group_by(line, Trapper_anon, record_date, days_since) |>
  summarize(
    traps = n(),
    rats = sum(species_level_2 == "Rat"),
    mustelids = sum(species_level_2 == "Mustelid")
  ) |>
  arrange(line, desc(record_date)) %>% 
  left_join(df_line_checked, by = "line") %>% 
  select(
    line,
    no_traps,
    last_over_half,
    days_since_over_half,
    Trapper_anon,
    record_date,
    days_since,
    traps,
    rats,
    mustelids
  ) %>% 
  arrange(line, desc(last_over_half), desc(record_date))

generate_icons <- function(count, icon_path, title) {
  if (count > 0) {
    paste0(
      sapply(
        seq_len(count),
        function(x) sprintf('<img src="%s" style="height:20px;width:20px;margin-right:2px;" title="%s" />', icon_path, title)
      ),
      collapse = ""
    )
  } else {
    ""
  }
}

generate_title <- function(mustelid_count, rat_count) {
  mustelid_title = ""
  rat_title = ""
  sep <- ifelse(rat_count > 0 && mustelid_count > 0, ", ", "")
  mustelid_title <- ifelse(mustelid_count > 0, sprintf("%s mustelid(s)", mustelid_count), "")
  rat_title <- ifelse(rat_count > 0, sprintf("%s rat(s)", rat_count), "")
  return (paste(mustelid_title, rat_title, sep=sep))
}

df_trap_status_2 <- df_trap_status_2 %>%  
  mutate(
    Mustelid_Icons = sapply(mustelids, generate_icons, icon_path = "/stoat.svg", title=generate_title(mustelids,rats)),
    Rat_Icons = sapply(rats, generate_icons, icon_path = "/rat.svg", title=generate_title(mustelids,rats)),
    Catch_Icons = paste0(Mustelid_Icons, Rat_Icons)
  ) %>% 
  mutate(
    line_map = case_when(last_over_half == record_date ~ sprintf('<img src="%s" style="height:100px;width:100px;margin-right:2px;" title="%s" />', paste0("/",trimws(line), ".png"), trimws(line)),
                         TRUE ~ "")
  ) %>% 
  mutate(line = trimws(line)) %>% 
  select(
    line,
    line_map,
    no_traps,
    last_over_half,
    days_since_over_half,
    Trapper_anon,
    record_date,
    days_since,
    traps,
    rats,
    mustelids,
    Mustelid_Icons,
    Rat_Icons,
    Catch_Icons
  )

saveRDS(df_trap_status_2, file = "df_trap_status_2.rds")
