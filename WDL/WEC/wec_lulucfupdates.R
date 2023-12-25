
## WEC income-emissions matrix LULUCF updates ==================================

## chapter 1: prelims ----------------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, ggrepel)

## loading paths
base_path <- worlddataverse::get_wdl_path()

## building function to load WEC data
load_wec_dat = function (){
  
  require (worlddataverse)
  
  # loading paths
  base_path <- worlddataverse::get_wdl_path()
  if (is.na (base_path)) {base_path = file.path("/Users",
                                                "teddyyankov",
                                                "Library",
                                                "CloudStorage",
                                                "GoogleDrive-teodor.yankov@worlddata.io",
                                                "Shared drives", 
                                                "DATA_WDL")}
  
  # load binary (Rda) file
  load (file.path (base_path,
                   "world_emissions_clock",
                   "01_data",
                   "WEC_data_binary_20230425.Rda"))
  
  # rename data file
  wec_dat = WDL_IIASA_data_consolidated_ind_essd
  
  # changing all 0s to NAs
  wec_dat [wec_dat == 0] <- NA
  
  .GlobalEnv$wec_dat = wec_dat
  .GlobalEnv$base_path = base_path
  
}

## the binary now has a classification df
load ("WEC_data_binary_20231120_lulucfREG.Rda")
wec_dat = WDL_IIASA_data_consolidated_ind_essd

# loading WEC data
load_wec_dat()

## income class objects
low_income = c("Afghanistan", "Korea, Dem. People's Rep", "South Sudan", "Burkina Faso", 
               "Liberia", "Sudan", "Burundi", "Madagascar", "Syrian Arab Republic", 
               "Central African Republic", "Malawi", "Togo", "Chad", "Mali", "Uganda", 
               "Congo, Dem. Rep", "Mozambique", "Yemen, Rep.", "Eritrea", "Niger", 
               "Ethiopia", "Rwanda", "Gambia, The", "Sierra Leone", "Guinea-Bissau", "Somalia")

low_income_iso3c <- countrycode (low_income, "country.name", "iso3c")

mid_income = c("Angola", "Jordan", "Philippines", "Algeria", "India", "Samoa", 
               "Bangladesh", "Iran, Islamic Rep", "São Tomé and Principe", "Benin", 
               "Kenya", "Senegal", "Bhutan", "Kiribati", "Solomon Islands", "Bolivia", 
               "Kyrgyz Republic", "Sri Lanka", "Cabo Verde", "Lao PDR", "Tanzania", 
               "Cambodia", "Lebanon", "Tajikistan", "Cameroon", "Lesotho", "Timor-Leste", 
               "Comoros", "Mauritania", "Tunisia", "Congo, Rep.", "Micronesia, Fed. Sts.", 
               "Ukraine", "Côte d'Ivoire", "Mongolia", "Uzbekistan", "Djibouti", "Morocco", 
               "Vanuatu", "Egypt, Arab Rep.", "Myanmar", "Vietnam", "Eswatini", "Nepal", 
               "Zambia", "Ghana", "Nicaragua", "Zimbabwe", "Guinea", "Nigeria", "Haiti", 
               "Pakistan", "Honduras", "Papua New Guinea", "Albania", "Fiji", "North Macedonia", 
               "Argentina", "Gabon", "Palau", "Armenia", "Georgia", "Paraguay", "Azerbaijan", 
               "Grenada", "Peru", "Belarus", "Guatemala", "Russian Federation", "Belize", 
               "Indonesia", "Serbia", "Bosnia and Herzegovina", "Iraq", "South Africa", 
               "Botswana", "Jamaica", "St. Lucia", "Brazil", "Kazakhstan", 
               "St. Vincent and the Grenadines", "Bulgaria", "Kosovo", "Suriname",
               "China", "Libya", "Thailand", "Colombia", "Malaysia", "Tonga", "Costa Rica", 
               "Maldives", "Turkey", "Cuba", "Marshall Islands", "Turkmenistan", "Dominica", 
               "Mauritius", "Tuvalu", "Dominican Republic", "Mexico", "West Bank and Gaza", 
               "El Salvador", "Moldova", "Equatorial Guinea", "Montenegro", "Ecuador", "Namibia")

mid_income_iso3c <- countrycode (mid_income, "country.name", "iso3c")

high_income = c("American Samoa", "Germany", "Oman", "Andorra", "Gibraltar", "Panama", 
                "Antigua and Barbuda", "Greece", "Poland", "Aruba", "Greenland", "Portugal", 
                "Australia", "Guam", "Puerto Rico", "Austria", "Hong Kong SAR, China", "Qatar", 
                "Bahamas, The", "Hungary", "Romania", "Bahrain", "Iceland", "San Marino", 
                "Barbados", "Ireland", "Saudi Arabia", "Belgium", "Isle of Man", "Seychelles", 
                "Bermuda", "Israel", "Singapore", "British Virgin Islands", "Italy", 
                "Sint Maarten (Dutch part)", "Brunei Darussalam", "Japan", "Slovak Republic", 
                "Canada", "Korea, Rep.", "Slovenia", "Cayman Islands", "Kuwait", "Spain", 
                "Channel Islands", "Latvia", "St. Kitts and Nevis", "Chile", "Liechtenstein", 
                "St. Martin (French part)", "Croatia", "Lithuania", "Sweden", "Curaçao", 
                "Luxembourg", "Switzerland", "Cyprus", "Macao SAR, China", "Taiwan, China", 
                "Czech Republic", "Malta", "Trinidad and Tobago", "Denmark", "Monaco", 
                "Turks and Caicos Islands", "Estonia", "Nauru", "United Arab Emirates", 
                "Faroe Islands", "Netherlands", "United Kingdom", "Finland", "New Caledonia", 
                "United States", "France", "New Zealand", "Uruguay", "French Polynesia", 
                "Northern Mariana Islands", "Virgin Islands (U.S.)", "Guyana", "Norway")

high_income_iso3c <- countrycode (high_income, "country.name", "iso3c")

## building classification table
wec_classification = wec_dat %>%
  
  # general aggregation
  dplyr::group_by (iso3c, year, pop) %>%
  dplyr::summarise (emissions = sum (base, na.rm = T)) %>%
  
  # computing emissions per capita
  mutate (em_pc = emissions / pop) %>%
  
  # converting to country name
  #mutate (country = countrycode (iso3c, "iso3c", "country.name")) %>%
  
  # creating income classifier variable
  mutate (income_class = ifelse (iso3c %in% low_income_iso3c, "low income", 
                                 ifelse (iso3c %in% mid_income_iso3c, "middle income", 
                                         ifelse (iso3c %in% high_income_iso3c, "high income", "unclassified"
                                         )))) %>%
  
  # adding emissions classifier variable 
  mutate (emissions_class = ifelse (em_pc < 5, "low emissions", 
                                    ifelse (em_pc >= 5 & em_pc <= 10, "middle emissions", 
                                            ifelse (em_pc > 10, "high emissions", NA 
                                            ))))

## function
income_emissions_class = function (income, emissions, year, data){
  
  ## creating main df
  wec_classification = data %>%
    
    # general aggregation
    dplyr::group_by (iso3c, year, pop) %>%
    dplyr::summarise (emissions = sum (base, na.rm = T)) %>%
    
    # computing emissions per capita
    mutate (em_pc = emissions / pop) %>%
    
    # converting to country name
    #mutate (country = countrycode (iso3c, "iso3c", "country.name")) %>%
    
    # creating income classifier variable
    mutate (income_class = ifelse (iso3c %in% low_income_iso3c, "low income", 
                                   ifelse (iso3c %in% mid_income_iso3c, "middle income", 
                                           ifelse (iso3c %in% high_income_iso3c, "high income", "unclassified"
                                           )))) %>%
    
    # adding emissions classifier variable 
    mutate (emissions_class = ifelse (em_pc < 5, "low emissions", 
                                      ifelse (em_pc >= 5 & em_pc <= 10, "middle emissions", 
                                              ifelse (em_pc > 10, "high emissions", NA 
                                              ))))
  
  ## filtering df
  filtered_data = wec_classification %>%
    filter(grepl (income, income_class, ignore.case = TRUE), 
           grepl (emissions, emissions_class, ignore.case = TRUE),
           year == year)
  
  ## computing total emissions, total population, and number of countries
  total_emissions <- sum (filtered_data$emissions, na.rm = TRUE)
  total_population <- sum (filtered_data$pop, na.rm = TRUE)
  num_countries <- nrow (filtered_data)
  
  # returning the results
  result <- list (
    income_class = income,
    emissions_class = emissions,
    total_emissions = total_emissions,
    total_population = total_population,
    num_countries = num_countries
  )
  
  return(result)
}

## iterating through for results

## storage
all_results <- data.frame()

# levels for income_class and emissions_class
income_levels <- c("low", "middle", "high")
emissions_levels <- c("low", "middle", "high")

# Iterate over all combinations
for (income in income_levels) {
  for (emissions in emissions_levels) {
    result <- income_emissions_class (income, emissions, 2023, wec_dat)
    all_results <- bind_rows (all_results, result)
  }
}

  









## testing
wec_classification_test = wec_classification

filtered_test = wec_classification_test %>%
  filter(grepl ("high", income_class, ignore.case = TRUE), 
         grepl ("high", emissions_class, ignore.case = TRUE),
         year == 2023)


total_emissions_test <- sum (filtered_test$emissions, na.rm = TRUE)
total_population_test <- sum (filtered_test$pop, na.rm = TRUE)
num_countries_test <- nrow (filtered_test)


result_test <- list (
  income_class = "high",
  emissions_class = "high",
  total_emissions = total_emissions_test,
  total_population = total_population_test,
  num_countries = num_countries_test
)

print (result_test)


result <- income_emissions_class (income = "high", emissions = "high", 2023, wec_dat)
print (result)












## creating main df
wec_classification = wec_dat %>%
  
  # general aggregation
  dplyr::group_by (iso3c, year, pop) %>%
  dplyr::summarise (emissions = sum (base, na.rm = T)) %>%
  
  # computing emissions per capita
  mutate (em_pc = emissions / pop) %>%
  
  # converting to country name
  #mutate (country = countrycode (iso3c, "iso3c", "country.name")) %>%
  
  # creating income classifier variable
  mutate (income_class = ifelse (iso3c %in% low_income_iso3c, "low income", 
                                 ifelse (iso3c %in% mid_income_iso3c, "middle income", 
                                         ifelse (iso3c %in% high_income_iso3c, "high income", "unclassified"
                                         )))) %>%
  
  # adding emissions classifier variable 
  mutate (emissions_class = ifelse (em_pc < 5, "low emissions", 
                                    ifelse (em_pc >= 5 & em_pc <= 10, "middle emissions", 
                                            ifelse (em_pc > 10, "high emissions", NA 
                                            ))))

## filtering df
filtered_data = wec_classification %>%
  filter(grepl ("high", income_class, ignore.case = TRUE), 
         grepl ("high", emissions_class, ignore.case = TRUE),
         year == year)

## computing total emissions, total population, and number of countries
total_emissions <- sum (filtered_data$emissions, na.rm = TRUE)
total_population <- sum (filtered_data$pop, na.rm = TRUE)
num_countries <- nrow (filtered_data)

# returning the results
result <- list (
  income_class = income,
  emissions_class = emissions,
  total_emissions = total_emissions,
  total_population = total_population,
  num_countries = num_countries
)

print (result)

















income_emissions_class <- function(income, emissions, year, data) {
  
  ## creating main df
  wec_classification <- data %>%
    
    # general aggregation
    dplyr::group_by(iso3c, year, pop) %>%
    dplyr::summarise(emissions = sum(base, na.rm = TRUE)) %>%
    
    # computing emissions per capita
    mutate(em_pc = emissions / pop) %>%
    
    # creating income classifier variable
    mutate(income_class = ifelse(
      iso3c %in% low_income_iso3c, "low income",
      ifelse(iso3c %in% mid_income_iso3c, "middle income",
             ifelse(iso3c %in% high_income_iso3c, "high income", "unclassified")
      )
    )) %>%
    
    # adding emissions classifier variable 
    mutate(emissions_class = ifelse(
      em_pc < 5, "low emissions",
      ifelse(em_pc >= 5 & em_pc <= 10, "middle emissions",
             ifelse(em_pc > 10, "high emissions", NA)
      )
    ))
  
  ## filtering df
  filtered_data <- wec_classification %>%
    filter(grepl(income, income_class, ignore.case = TRUE), 
           grepl(emissions, emissions_class, ignore.case = TRUE),
           year == year)
  
  ## computing total emissions, total population, and number of countries
  total_emissions <- sum(filtered_data$emissions, na.rm = TRUE)
  total_population <- sum(filtered_data$pop, na.rm = TRUE)
  num_countries <- nrow(filtered_data)
  
  # returning the results
  result <- list(
    income_class = income,
    emissions_class = emissions,
    total_emissions = total_emissions,
    total_population = total_population,
    num_countries = num_countries
  )
  
  return(result)
}

# Example usage
result <- income_emissions_class (income = "high", emissions = "high", year = 2023, data = wec_dat)
print(result)










income_emissions_class <- function (inc, em, target_year, data) {
  
  ## creating main df
  wec_classification <- data %>%
    
    # general aggregation
    dplyr::group_by(iso3c, year, pop) %>%
    dplyr::summarise(emissions = sum(base, na.rm = TRUE)) %>%
    
    # computing emissions per capita
    mutate(em_pc = emissions / pop) %>%
    
    # creating income classifier variable
    mutate(income_class = ifelse(
      iso3c %in% low_income_iso3c, "low income",
      ifelse(iso3c %in% mid_income_iso3c, "middle income",
             ifelse(iso3c %in% high_income_iso3c, "high income", "unclassified")
      )
    )) %>%
    
    # adding emissions classifier variable 
    mutate(emissions_class = ifelse(
      em_pc < 5, "low emissions",
      ifelse(em_pc >= 5 & em_pc <= 10, "middle emissions",
             ifelse(em_pc > 10, "high emissions", NA)
      )
    ))
  
  print("wec_classification:")
  print(wec_classification)
  
  ## filtering df
  filtered_data <- wec_classification %>%
    filter(grepl (inc, income_class, ignore.case = TRUE), 
           grepl (em, emissions_class, ignore.case = TRUE),
           year == target_year)
  
  print("filtered_data:")
  print(filtered_data)
  
  ## computing total emissions, total population, and number of countries
  total_emissions <- sum (filtered_data$emissions, na.rm = TRUE)
  total_population <- sum (filtered_data$pop, na.rm = TRUE)
  num_countries <- nrow (filtered_data)
  
  print("Intermediate Results:")
  print(total_emissions)
  print(total_population)
  print(num_countries)
  
  # returning the results
  result <- list(
    income_class = inc,
    emissions_class = em,
    total_emissions = total_emissions,
    total_population = total_population,
    num_countries = num_countries
  )
  
  return(result)
}

# Example usage
result <- income_emissions_class (inc = "high", em = "high", target_year = 2023, data = wec_dat)
print(result)

## for bullets 
class_filtered = classification_df %>%
  
  # keeping only high income 
  filter (income_class == "high_income") %>%
  
  # keeping only 2023
  filter (year == 2023) %>%
  
  # counting emission classes
  dplyr::group_by (base_class) %>%
  dplyr::summarise (count = n())


wec_dat_clean = wec_dat %>%
  
  # general aggregation
  dplyr::group_by (iso3c, year, pop) %>%
  dplyr::summarise (emissions = sum (base, na.rm = T)) %>%
  
  # computing emissions per capita
  mutate (em_pc = emissions / pop) %>%
  
  # keep 2023
  filter (year == 2023) %>%
  
  # keep relevant vars
  dplyr::select (iso3c, year, emissions, em_pc, pop)


## join class table with emissions
class_wemissions = classification_df %>% 
  
  # keeping only 2023
  filter (year == 2023) %>%
  
  # joining with wec dat
  left_join (wec_dat_clean, by = c("iso3c", "year"))

## plotting countries as dots
ggplot(class_wemissions %>% 
         filter (!iso3c %in% c("PRI", "MAC", "LVA", "MDV", "PAN", "LCA", "CRI")) %>%
         mutate (country = countrycode (iso3c, "iso3c", "country.name")), aes (x = gni_pc, y = em_pc)) +
  geom_point() +
  geom_hline(yintercept = c(5, 10), linetype = "dashed", color = "blue") +
  geom_vline(xintercept = c(1085, 13205), linetype = "dashed", color = "red") +
  geom_label_repel(aes (label = country),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  labs(title = "Top 10 Countries (high income, low emissions)",
       x = "GNI",
       y = "Emissions per Capita") + 
  theme_wdl() + 
  ylim (0, 6.5) + 
  xlim (11000, 100000)

## plotting the same but with inverted y-axis
ggplot(class_wemissions %>% 
         filter (!iso3c %in% c("PRI", "MAC", "LVA", "MDV", "PAN", "LCA", "CRI")) %>%
         mutate (country = countrycode (iso3c, "iso3c", "country.name")), aes (x = gni_pc, y = -em_pc)) +
  geom_point() +
  geom_hline(yintercept = c(-5, -10), linetype = "dashed", color = "blue") +
  geom_vline(xintercept = c(1085, 13205), linetype = "dashed", color = "red") +
  geom_label_repel(aes (label = country),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  labs(title = "Top 10 Countries (high income, low emissions)",
       x = "GNI",
       y = "Emissions per Capita") + 
  theme_wdl() + 
  ylim (0, -6.5) + 
  xlim (11000, 100000)
  
  
class_wemissions_experiment = class_wemissions %>% mutate (em_pc = -1 * em_pc)


