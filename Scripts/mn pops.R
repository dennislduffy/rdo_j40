library(RSQLite)
library(tidyverse)
library(janitor)
library(tidycensus)

# db <- dbConnect(RSQLite::SQLite(), "Databases/rdo_files.db") #connect to database
# 
# username <- readline("Enter User Name: ")
# pass <- readline("Enter Password: ")
# dbname = readline("Enter dbname: ")
# host = readline("Enter host: ")
# port = as.numeric(readline("Enter port: "))
# 
# warehouse <- dbConnect(RPostgreSQL::PostgreSQL(),
#                  dbname = dbname, 
#                  host = host, 
#                  port = port, 
#                  user = username, 
#                  password = pass)
# 
# 
# #pull standard county names from data warehouse
# 
# county_query <- dbSendQuery(warehouse, 
#             "select co_name, co_code_3
#             from reference.county"
#             )
# 
# county_info <- dbFetch(county_query)

census_counties <- get_acs(geography = "county", 
                           variables = "B01001A_001", 
                           state = "MN", 
                           year = 2022, 
                           survey = "acs5") |> 
  select(GEOID, NAME) |> 
  mutate(NAME = str_replace(NAME, " County, Minnesota", ""), 
         NAME = case_when(
           NAME == "St. Louis" ~ "Saint Louis", 
           TRUE ~ NAME
         )) |> 
  mutate(census_code = str_sub(GEOID, start = 3, end = 5))


#pull Census populations of towns and minor civil divisions

pops <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2020-2023/cities/totals/sub-est2023_27.csv") |> 
  tibble() 

county_place <- pops |> 
  filter(SUMLEV == "157") |> 
  select(COUNTY, PLACE)

incorp <- pops |> 
  filter(SUMLEV == 162) |> 
  select(PLACE, NAME, POPESTIMATE2023) |> 
  left_join(county_place, by = "PLACE") |> 
  mutate(NAME = str_replace(NAME, " city", "")) |> 
  left_join(census_counties, by = c("COUNTY" = "census_code")) |> 
  rename(COUNTY_NAME = NAME.y, 
         NAME = NAME.x) |> 
  select(-c(GEOID))


dbWriteTable(db, "town_populations", incorp)
  

