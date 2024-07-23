library(sf)
library(tidyverse)
library(RSQLite)

sf::sf_use_s2(FALSE)

mn <- st_read("j40_spatial/j40_spatial.gdb", layer = "usa", 
               query = "
    select
      GEOID10,
      CF as county_name, 
        DF_PFS + 
        AF_PFS + 
        HDF_PFS +
        DSF_PFS + 
        EBF_PFS +
        EALR_PFS +
        EBLR_PFS + 
        EPLR_PFS + 
        HBF_PFS + 
        LLEF_PFS +
        LIF_PFS + 
        LMI_PFS + 
        PM25F_PFS + 
        P100_PFS +
        P200_I_PFS +
        LPF_PFS + 
        KP_PFS +
        NPL_PFS + 
        RMP_PFS + 
        TSDF_PFS + 
        TF_PFS + 
        UF_PFS + 
        WF_PFS +
        UST_PFS + 
        TD_PFS + 
        FLD_PFS + 
        WFR_PFS +
        FUDS_ET +
        AML_ET +
      IS_PFS as total_percentiles,  
      HRS_ET as redlining, 
      SN_C as dac,
      FPL200S as low_income, 
      TA_PERC as tribal_percent
    from usa
    where SF = 'Minnesota'
                 ") |> 
  mutate(redlining = case_when(
    redlining == "1" ~ 1,
    TRUE ~ 0
  ), 
  total_percentiles = total_percentiles + redlining, 
  rank = cume_dist(total_percentiles), 
  county_name = str_replace(county_name, " County", ""), 
  dac = ifelse(dac == 1, "Disadvantaged", "Not Disadvantaged"), 
  low_income = ifelse(low_income == 1, "Low Income", "Not Low Income")) |> 
  select(-c(redlining))

#boundary database retrieved from https://gisdata.mn.gov/dataset/bdry-mn-city-township-unorg

boundaries <- st_read("town_boundaries/bdry_mn_city_township_unorg.gdb", layer = "city_township_unorg",
                      query = "
                      select FEATURE_NAME as city,
                      COUNTY_NAME as county
                      from city_township_unorg
                      where CTU_CLASS = 'CITY'
                      ") |>
  mutate(county = case_when(
    county == "Saint Louis" ~ "St. Louis",
    TRUE ~ county
  ), 
  city = case_when(
    city == "Saint Cloud" ~ "St. Cloud", 
    city == "Saint Louis Park" ~ "St. Louis Park",
    TRUE ~ city
  )) |> 
  st_transform("EPSG:4326") |>
  st_cast("MULTIPOLYGON")

boundary_tracts <- boundaries |> 
  st_join(mn, join = st_intersects) |> 
  group_by(city) |> 
  mutate(multi_county_flag = length(county) == TRUE, 
         drop_flag = case_when(
           multi_county_flag == FALSE & county != county_name ~ TRUE, 
           TRUE ~ FALSE
         )) |> 
  filter(drop_flag == FALSE) |> 
  tibble() |> 
  select(city, county, GEOID10)


# pull in city population levels

db <- dbConnect(RSQLite::SQLite(), "Databases/rdo_files.db")

pop_query <- dbSendQuery(db, 
                         "
                         select NAME as city, POPESTIMATE2023 as population, 
                          case when COUNTY_NAME = 'Saint Louis' then 'St. Louis'
                          else COUNTY_NAME end as county
                         FROM town_populations
                         "
                         )

city_populations <- dbFetch(pop_query) |> 
  tibble() 

boundary_tracts <- boundary_tracts |> 
  left_join(city_populations, by = c("city" = "city", "county" = "county"))

# pull in RDO territories

rdo_query <- dbSendQuery(db, 
                         "
                         select county, rdo
                         from rdo_counties
                         "
                         )

rdo_counties <- fetch(rdo_query) |> 
  tibble() |> 
  mutate(county = str_replace(county, " County", ""))

rdo_burdens <- mn |> 
  left_join(boundary_tracts, by = "GEOID10") |> 
  left_join(rdo_counties, by = c("county_name" = "county"))


st_write(rdo_burdens, "Datasets/shape/rdo_burdens.shp", delete_dsn = TRUE)



