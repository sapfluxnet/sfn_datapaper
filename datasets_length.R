path.plant <- file.path('data/0.1.3/RData/plant')

sfn_sites_in_folder(path.plant)

# Test using Can Balasc data
esp_can <- read_sfn_data('ESP_CAN',folder=path.plant)

# For each tree get 
esp_can %>% 
  get_sapf_data() %>% 
  
daily_metrics()