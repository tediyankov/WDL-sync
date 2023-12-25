
## WEC income-emissions matrix LULUCF updates ==================================

## chapter 1: prelims ----------------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode)

## loading paths
base_path <- worlddataverse::get_wdl_path()

## the binary now has a classification df
load ("WEC_data_binary_20231120_lulucfREG.Rda")
wec_dat = WDL_IIASA_data_consolidated_ind_essd

## chapter 2: function ---------------------------------------------------------

income_emissions_class <- function (inc, em, target_year, data) {
  
  ## classification vectors for income
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
    filter(grepl (inc, income_class, ignore.case = TRUE), 
           grepl (em, emissions_class, ignore.case = TRUE),
           year == target_year)
  
  ## computing total emissions, total population, and number of countries
  total_emissions <- sum (filtered_data$emissions, na.rm = TRUE)
  total_population <- sum (filtered_data$pop, na.rm = TRUE)
  num_countries <- nrow (filtered_data)
  
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

## iterating through for results
# empty df for storing the results
all_results <- data.frame()

## levels for income_class and emissions_class
income_levels <- c("low", "middle", "high")
emissions_levels <- c("low", "middle", "high")

## Iterate over all combinations
# for every income level
for (income in income_levels) {
  
  # for every emissions level
  for (emissions in emissions_levels) {
    
    # generate results object
    result <- income_emissions_class (income, emissions, 2023, wec_dat)
    
    # add to the main results df
    all_results <- bind_rows (all_results, result)
    
  }
  
}
