### Load crime and greenspace data (if using)

criminalacts <- 
  readxl::read_xlsx("data/interventionscitoyendo.xlsx")

greenspace <-
  st_read("data/Espace_Vert.shp")


### Get census variables


View(list_census_regions("CA16"))

View(list_census_vectors("CA16"))

# visible minority 
# "v_CA16_3960", "v_CA16_3963", "v_CA16_3966", "v_CA16_3969", 
# "v_CA16_3972", "v_CA16_3975", "v_CA16_3978", "v_CA16_3981", 
# "v_CA16_3984", "v_CA16_3987", "v_CA16_3990", "v_CA16_3993"

# indigenous 
# "v_CA16_3855"

# immigrant 
# v_CA16_3411", "v_CA16_3435", "v_CA16_3846"   refugee is subset of immigrant

# LICO prevalence 
# "v_CA16_2555", "v_CA16_2558", "v_CA16_2564", "v_CA16_2567"

# overcrowding 
# "v_CA16_4858"   normalize with households not population

census <-
  get_census("CA16",
             regions = list(CMA = c("24462")),
             level = "CT",
             vectors = c("v_CA16_3960", "v_CA16_3963", "v_CA16_3966", "v_CA16_3969", 
                         "v_CA16_3972", "v_CA16_3975", "v_CA16_3978", "v_CA16_3981", 
                         "v_CA16_3984", "v_CA16_3987", "v_CA16_3990", "v_CA16_3993", 
                         "v_CA16_3855", "v_CA16_3411", "v_CA16_3435", "v_CA16_3846", 
                         "v_CA16_2555", "v_CA16_2558", "v_CA16_2564", "v_CA16_2567",
                         "v_CA16_4858"),
             geo_format = 'sf') %>% 
  st_transform(32618)


### Tidy census dataset

census <- census[ , -c(1, 2, 4, 6, 7, 9, 10, 11, 12, 13)]

census <-
  census %>% 
  set_names(c("dwellings", "population", "households", "south_asian", "chinese", "black", 
              "filipino", "latinx", "arab", "se_asian", "west_asian", "korean", "japanese", 
              "other_vm", "multiple_vm", "indigenous", "immigrant", "non_perm_res", "refugee", 
              "total_LICO", "LICO_0to17", "LICO_18to64", "LICO_65up", "overcrowding", "geometry"))

### Aggregate by borough

load("data/boroughs.Rdata")

censusbyborough <-
  st_join(census, boroughs) %>% 
  st_transform(32618) %>% 
  group_by(borough) %>% 
  summarise_if(is.numeric, funs(sum), na.rm = TRUE)

censusbyborough <- censusbyborough[-c(35), ]


### Manipulate census dataset

censusbyborough <-
  censusbyborough %>% 
  mutate(white = population - (south_asian + chinese + black + filipino + latinx + arab + se_asian + 
                                 west_asian + korean + japanese + other_vm + multiple_vm),
         overcrowding_pct_hh = overcrowding / households) %>% 
  mutate_at(.vars = c("south_asian", "chinese", "black", "filipino", "latinx", "arab", "se_asian", 
                      "west_asian", "korean", "japanese", "other_vm", "multiple_vm", "white", 
                      "indigenous", "immigrant", "non_perm_res", "refugee", "total_LICO", 
                      "LICO_0to17", "LICO_18to64", "LICO_65up"),
            .funs = list(`pct_pop` = ~{. / population * 100}))

censusbyborough <- censusbyborough[ , c(1:16, 28, 17:25, 30:50, 29, 26, 27)]


### Save datasets

save(census, file = "data/census.Rdata")
save(censusbyborough, file = "data/censusbyborough.Rdata")