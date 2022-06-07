## this is a spatialRF script for wellbeing

##  NEED TO COLLECT NUTS DATA FROM MORE THAN ONE YEAR AND BUILT A SPATIAL DATASET WITH AS MANY AS POSSIBLE NUTS2 REGIONS

library(tidyverse)
library(spatialRF)
library(eurostat)
library(sf)

# well being data
new <- read_rds("data/processed_data/regression_ready.rds")

#spatial data from eurostat

# after https://www.oecd.org/cfe/regionaldevelopment/territorial-grid.pdf tl2 (OECD) is mainly NUTS2016

# needed for greek regions
euro_nuts2_10 <- get_eurostat_geospatial(resolution = "01",
                                         year = 2010,
                                         nuts_level = 2)

euro_nuts2_13 <- get_eurostat_geospatial(resolution = "01",
                                         year = 2013,
                                         nuts_level = 2)

euro_nuts2_16 <- get_eurostat_geospatial(resolution = "01",
                                         year = 2016,
                                         nuts_level = 2)


euro_nuts1_16 <- get_eurostat_geospatial(resolution = "01",
                                         year = 2016,
                                         nuts_level = 1)

euros_nuts_16 <- rbind(euro_nuts2_16, euro_nuts1_16)


# euro_nuts2_21 <- get_eurostat_geospatial(resolution = "01",
#                                          year = 2021,
#                                          nuts_level = 2)

#all_euro_nuts2 <- euro_nuts2_10 %>% bind_rows(euro_nuts2_13, euro_nuts2_16,euro_nuts2_21, euro_nuts2_06 )





'%ni%' <- Negate('%in%')
missing_16 <- new$REG_ID[new$REG_ID %ni% euros_nuts_16$NUTS_ID]

# france is missing, take it from the 2013 version of eurostat
length(missing_16)

missing_from_16_in_13 <- rbind(euros_nuts_16 %>% select(names(euro_nuts2_13)), euro_nuts2_13[euro_nuts2_13$NUTS_ID %in% missing_16,])

missing_16 <- new$REG_ID[new$REG_ID %ni% missing_from_16_in_13$NUTS_ID]
length(missing_16)


# add greece from the 2010 tables
## need to rename greece regions: in the oecd they are gr## in the eurostat maps, they are el
# slovenia also from 2010 table



missing_from_10_in_16 <- rbind(
  euro_nuts2_10 %>% as_tibble() %>%
  mutate(NUTS_ID = ifelse(substr(NUTS_ID,1,2) == "EL", paste0("GR", substr(NUTS_ID,3,10)), NUTS_ID)) %>% 
  filter(NUTS_ID %in% missing_16),
  
  missing_from_16_in_13)
  

missing_still <- new$REG_ID[new$REG_ID %ni% missing_from_10_in_16$NUTS_ID]
length(missing_still)

## all missing are not european countries


## final (only_european) spatial dataset

spatial_data_oecd_euro <- 
  missing_from_10_in_16 %>% filter(NUTS_ID %in% new$REG_ID) %>% 
  select(3, "geometry") %>% st_as_sf()






distance_matrix <- st_distance(spatial_data_oecd_euro)

units(distance_matrix) <- NULL

row.names(distance_matrix) <- 1:dim(distance_matrix)[1]
colnames(distance_matrix) <- 1:dim(distance_matrix)[1]

# filter for avaiable data
variables.crval <- read_rds("data/processed_data/lasso_variables.rds")

spatialrf_data <- new %>% filter(REG_ID %in% spatial_data_oecd_euro$NUTS_ID) %>% 
  select(c(variables.crval, "SUBJ_LIFE_SAT", REG_ID)) %>% as.data.frame()

spatialrf_data <- spatialrf_data[match(spatial_data_oecd_euro$NUTS_ID, spatialrf_data$REG_ID),] %>% select(-REG_ID)

xy <- st_centroid(spatial_data_oecd_euro) %>% 
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2]) %>% 
  as.data.frame() %>% 
  select(3:4)


## new sample comes from different script "2_a_lasso_cforest.R" 
spatial.model <- spatialRF::rf_spatial(
  data = spatialrf_data,
  dependent.variable.name = "SUBJ_LIFE_SAT",
  predictor.variable.names = variables.crval,
  distance.matrix = distance_matrix,
  distance.thresholds = c(0, 100000, 300000, 500000, 598379, 1105724, 1400000, 2000000)
)


model.non.spatial <- spatialRF::rf_importance(
  model = spatial.model
)


spatialRF::plot_training_df_moran(
  data = spatialrf_data,
  dependent.variable.name = "SUBJ_LIFE_SAT",
  predictor.variable.names = variables.crval,
  distance.matrix = distance_matrix,
  distance.thresholds = c(0, 100000, 300000, 500000, 598379, 1105724, 1400000, 2000000),
  fill.color = viridis::viridis(
    100,
    option = "F",
    direction = -1
  ),
  point.color = "gray40"
)


spatialRF::plot_training_df(
  data = spatialrf_data,
  dependent.variable.name = "SUBJ_LIFE_SAT",
  predictor.variable.names = variables.crval,
  ncol = 3,
  point.color = viridis::viridis(100, option = "F"),
  line.color = "gray30"
)


# interactions
interactions <- rf_interactions(
  data = spatialrf_data,
  dependent.variable.name = "SUBJ_LIFE_SAT",
  predictor.variable.names = variables.crval
)

random.seed <- set.seed(1)

interactions <- spatialRF::the_feature_engineer(
  data = spatialrf_data,
  dependent.variable.name = "SUBJ_LIFE_SAT",
  predictor.variable.names = variables.crval,
  xy = xy,
  importance.threshold = 0.50, #uses 50% best predictors
  cor.threshold = 0.60, #max corr between interactions and predictors
  seed = random.seed,
  repetitions = 100,
  verbose = TRUE
)

