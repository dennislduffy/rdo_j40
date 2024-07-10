library(RSQLite)
library(tidyverse)
library(janitor)

db <- dbConnect(RSQLite::SQLite(), "Databases/rdo_files.db") #connect to database

username <- readline("Enter User Name: ")
pass <- readline("Enter Password: ")
dbname = readline("Enter dbname: ")
host = readline("Enter host: ")
port = as.numeric(readline("Enter port: "))

warehouse <- dbConnect(RPostgreSQL::PostgreSQL(),
                 dbname = dbname, 
                 host = host, 
                 port = port, 
                 user = username, 
                 password = pass)


#pull standard county names from data warehouse

county_query <- dbSendQuery(warehouse, 
            "select co_name, co_code_3
            from reference.county"
            )

county_info <- dbFetch(county_query)

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
  left_join(county_info, by = c("COUNTY" = "co_code_3")) |> 
  rename(COUNTY_NAME = co_name)


dbWriteTable(db, "town_populations", incorp)
  

