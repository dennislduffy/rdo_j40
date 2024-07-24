library(tidycensus)
library(tidyverse)
library(RSQLite)
library(rvest)

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

county_query <- dbSendQuery(warehouse, 
                            "
                            select co_name, co_code as data_warehouse_full_fips, co_code_3 as data_warehouse_short_fips 
                            from reference.county c 
                            order by co_name
                            "
                            )

warehouse_counties <- dbFetch(county_query) |> 
  tibble()

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
  rename(census_code = GEOID)

#pull counties and codes from MNIT site

url <- "https://mn.gov/mnit/government/policies/geo/mn-county-identification-codes.jsp"

mnit_table <- read_html(url) |> 
  html_element("table") |> 
  html_table(convert = FALSE) |> 
  rename(county_name = `COUNTY NAME`, 
         mnit_code = CODE)


problem_counties <- warehouse_counties |> 
  left_join(mnit_table, by = c("co_name" = "county_name")) |> 
  left_join(census_counties, by = c("co_name" = "NAME")) |> 
  mutate(short_census = str_sub(census_code, start = 3, end = 5)) |> 
  mutate(warehouse_long_match_census_long = data_warehouse_full_fips == census_code, 
         warehouse_short_match_census_short = data_warehouse_short_fips == short_census, 
         mnit_code_match_census_short = mnit_code == short_census) |> 
  filter(warehouse_short_match_census_short == FALSE)

write_csv(problem_counties, "Data Warehouse Wrong FIPS.csv")
