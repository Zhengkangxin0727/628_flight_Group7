
# 使用 reticulate 加载 Python 的 joblib
#joblib <- import("joblib")
#model_logistic <- joblib$load("logistic_regression_model.pkl")

library(jsonlite)
model_params <- fromJSON("logistic_regression_model.json")
coef <- unlist(model_params$coef)
coef <- as.numeric(coef)
intercept <- unlist(model_params$intercept)
classes <- unlist(model_params$classes)
predict_logistic <- function(new_data) {
  linear_combination <- as.matrix(new_data) %*% coef + intercept
  probabilities <- 1 / (1 + exp(-linear_combination))
  ifelse(probabilities > 0.5, classes[2], classes[1])
}

library(lightgbm)
model <- lgb.load("model.txt")

airport_data <- read.csv("airport_timezone_new.csv")
average_weather <- read.csv("avergae_for_logistic.csv")
actual_elapsed <- read.csv("average_actual_elapsed_time.csv")
average_visibility <- read.csv("averageHourlyVisibility.csv")
delay_time <- read.csv("average_delay_time.csv")

origin <- read.csv("origin_mean.csv")
dest <- read.csv("dest_mean.csv")
weather_type_origin <- read.csv("weathertype_origin_mean.csv")
weather_type_dest <- read.csv("weathertype_dest_mean.csv")
operating_airline <- read.csv("operating_airline_mean.csv")

ftoc <- function(fahrenheit) {
  celsius <- (fahrenheit - 32) * 5/9
  return(celsius)
}

convert_wind_direction_to_angle <- function(direction) {
  switch(direction,
         "N" = 0,
         "NNE" = 22.5,
         "NE" = 45,
         "ENE" = 67.5,
         "E" = 90,
         "ESE" = 112.5,
         "SE" = 135,
         "SSE" = 157.5,
         "S" = 180,
         "SSW" = 202.5,
         "SW" = 225,
         "WSW" = 247.5,
         "W" = 270,
         "WNW" = 292.5,
         "NW" = 315,
         "NNW" = 337.5,
         NA)  # Return NA if the input is not a valid wind direction
}

year_values <- c(2019, 2020, 2021, 2022, 2023, 2024)  # 可能的年份值
month_values <- c(11, 12)
airline_values <- c("AA", "AS", "AX", "B6", "C5", "CP", "DL", "EM", "EV", "F9", "G4", "G7", "HA", "KS", "MQ", "NK", "OH", "OO", "PT", "QX", "UA", "WN", "YV", "YX", "ZW")
airport_values_origin <- c("ABI", "ABQ", "ABR", "ABY", "ACT", "ACV", "ACY", "ADK", "ADQ", "AEX", 
                           "AGS", "AKN", "ALB", "ALO", "ALW", "AMA", "ANC", "APN", "ART", "ASE", 
                           "ATL", "ATW", "AUS", "AVL", "AVP", "AZA", "AZO", "BDL", "BET", "BFF", 
                           "BFL", "BGM", "BGR", "BHM", "BIH", "BIL", "BIS", "BLI", "BLV", "BMI", 
                           "BNA", "BOI", "BOS", "BPT", "BQK", "BQN", "BRD", "BRO", "BRW", "BTM", "BTR", 
                           "BTV", "BUF", "BUR", "BWI", "BZN", "CAE", "CAK", "CDV", "CGI", "CHA", 
                           "CHO", "CHS", "CID", "CKB", "CLE", "CLL", "CLT", "CMH", "CMX", 
                           "CNY", "COD", "COS", "COU", "CPR", "CRP", "CRW", "CSG", "CVG", "CWA", "CYS", 
                           "DAB", "DAL", "DAY", "DBQ", "DCA", "DEC", "DEN", "DFW", "DHN", "DIK", 
                           "DLG", "DLH", "DRO", "DRT", "DSM", "DTW", "DUT", "DVL", "EAR", "EAT", "EAU", 
                           "ECP", "EGE", "EKO", "ELM", "ELP", "ERI", "ESC", "EUG", "EVV", "EWN", "EWR", 
                           "EYW", "FAI", "FAR", "FAT", "FAY", "FCA", "FLG", "FLL", "FLO", "FNT", "FOD", 
                           "FSD", "FSM", "FWA", "GCC", "GCK", "GEG", "GFK", "GGG", "GNV", "GPT", 
                           "GRB", "GRI", "GRK", "GRR", "GSO", "GSP", "GTF", "GTR", "GUC", "GUM", "HDN", 
                           "HGR", "HIB", "HLN", "HNL", "HOU", "HPN", "HRL", "HSV", "HTS", 
                           "HVN", "HYS", "IAD", "IAG", "IAH", "ICT", "IDA", "ILM", "IMT", "IND", "INL", 
                           "IPT", "ISN", "ISP", "ITH", "ITO", "JAC", "JAN", "JAX", "JFK", "JHM", "JLN", 
                           "JMS", "JNU", "JST", "KOA", "KTN", "LAN", "LAR", "LAS", "LAW", "LAX", "LBB", 
                           "LBF", "LCH", "LCK", "LEX", "LFT", "LGA", "LGB", "LIH", "LIT", 
                           "LNK", "LNY", "LRD", "LSE", "LWB", "LWS", "LYH", "MAF", "MBS", "MCI", "MCO", 
                           "MCW", "MDT", "MDW", "MEI", "MEM", "MFE", "MFR", "MGM", "MHK", "MHT", "MIA", 
                           "MKE", "MKG", "MKK", "MLB", "MLI", "MLU", "MOB", "MOT", "MQT", "MRY", 
                           "MSN", "MSO", "MSP", "MSY", "MTJ", "MYR", "OAJ", "OAK", "OGD", "OGG", "OGS", "OKC", 
                           "OMA", "OME", "ONT", "ORD", "ORF", "ORH", "OTH", "OTZ", "PAE", "PAH", 
                           "PBG", "PBI", "PDX", "PGD", "PGV", "PHF", "PHL", "PHX", "PIA", "PIB", "PIE", 
                           "PIH", "PIT", "PLN", "PNS", "PQI", "PSG", "PSM", 
                           "PSP", "PUB", "PUW", "PVD", "PVU", "PWM", "RAP", "RDD", "RDM", "RDU", "RFD", 
                           "RHI", "RIC", "RKS", "RNO", "ROA", "ROC", "ROW", "RST", "RSW", "SAF", "SAN", 
                           "SAT", "SAV", "SBA", "SBN", "SBP", "SBY", "SCC", "SCE", "SCK", "SDF", "SEA", 
                           "SFB", "SFO", "SGF", "SGU", "SHD", "SHV", "SIT", "SJC", "SJU", "SLC", 
                           "SLN", "SMF", "SNA", "SPI", "SPS", "SRQ", "STC", "STL", "STS", 
                           "STT", "SUN", "SUX", "SWF", "SWO", "SYR", "TLH", "TOL", "TPA", "TRI", 
                           "TTN", "TUL", "TUS", "TVC", "TWF", "TXK", "TYR", "TYS", "UIN", "USA", "VCT", 
                           "VEL", "VLD", "VPS", "WRG", "XNA", "XWA", "YAK", "YKM", "YUM")

airport_values_dest <- c("ABI", "ABQ", "ABR", "ABY", "ACK", "ACT", "ACV", "ACY", "ADK", "ADQ", "AEX", 
                         "AGS", "AKN", "ALB", "ALO", "ALS", "ALW", "AMA", "ANC", "APN", "ART", "ASE", 
                         "ATL", "ATW", "ATY", "AUS", "AVL", "AVP", "AZA", "AZO", "BDL", "BET", "BFF", 
                         "BFL", "BGM", "BGR", "BHM", "BIH", "BIL", "BIS", "BJI", "BLI", "BLV", "BMI", 
                         "BNA", "BOI", "BOS", "BPT", "BQK", "BQN", "BRD", "BRO", "BRW", "BTM", "BTR", 
                         "BTV", "BUF", "BUR", "BWI", "BZN", "CAE", "CAK", "CDC", "CDV", "CGI", "CHA", 
                         "CHO", "CHS", "CID", "CIU", "CKB", "CLE", "CLL", "CLT", "CMH", "CMI", "CMX", 
                         "CNY", "COD", "COS", "COU", "CPR", "CRP", "CRW", "CSG", "CVG", "CWA", "CYS", 
                         "DAB", "DAL", "DAY", "DBQ", "DCA", "DDC", "DEC", "DEN", "DFW", "DHN", "DIK", 
                         "DLG", "DLH", "DRO", "DRT", "DSM", "DTW", "DUT", "DVL", "EAR", "EAT", "EAU", 
                         "ECP", "EGE", "EKO", "ELM", "ELP", "ERI", "ESC", "EUG", "EVV", "EWN", "EWR", 
                         "EYW", "FAI", "FAR", "FAT", "FAY", "FCA", "FLG", "FLL", "FLO", "FNT", "FOD", 
                         "FSD", "FSM", "FWA", "GCC", "GCK", "GEG", "GFK", "GGG", "GJT", "GNV", "GPT", 
                         "GRB", "GRI", "GRK", "GRR", "GSO", "GSP", "GTF", "GTR", "GUC", "GUM", "HDN", 
                         "HGR", "HHH", "HIB", "HLN", "HNL", "HOB", "HOU", "HPN", "HRL", "HSV", "HTS", 
                         "HVN", "HYS", "IAD", "IAG", "IAH", "ICT", "IDA", "ILM", "IMT", "IND", "INL", 
                         "IPT", "ISN", "ISP", "ITH", "ITO", "JAC", "JAN", "JAX", "JFK", "JHM", "JLN", 
                         "JMS", "JNU", "JST", "KOA", "KTN", "LAN", "LAR", "LAS", "LAW", "LAX", "LBB", 
                         "LBE", "LBF", "LBL", "LCH", "LCK", "LEX", "LFT", "LGA", "LGB", "LIH", "LIT", 
                         "LNK", "LNY", "LRD", "LSE", "LWB", "LWS", "LYH", "MAF", "MBS", "MCI", "MCO", 
                         "MCW", "MDT", "MDW", "MEI", "MEM", "MFE", "MFR", "MGM", "MHK", "MHT", "MIA", 
                         "MKE", "MKG", "MKK", "MLB", "MLI", "MLU", "MMH", "MOB", "MOT", "MQT", "MRY", 
                         "MSN", "MSO", "MSP", "MSY", "MTJ", "MYR", "OAJ", "OAK", "OGG", "OGS", "OKC", 
                         "OMA", "OME", "ONT", "ORD", "ORF", "ORH", "OTH", "OTZ", "OWB", "PAE", "PAH", 
                         "PBG", "PBI", "PDX", "PGD", "PGV", "PHF", "PHL", "PHX", "PIA", "PIB", "PIE", 
                         "PIH", "PIR", "PIT", "PLN", "PNS", "PQI", "PRC", "PSC", "PSE", "PSG", "PSM", 
                         "PSP", "PUB", "PUW", "PVD", "PVU", "PWM", "RAP", "RDD", "RDM", "RDU", "RFD", 
                         "RHI", "RIC", "RKS", "RNO", "ROA", "ROC", "ROW", "RST", "RSW", "SAF", "SAN", 
                         "SAT", "SAV", "SBA", "SBN", "SBP", "SBY", "SCC", "SCE", "SCK", "SDF", "SEA", 
                         "SFB", "SFO", "SGF", "SGU", "SHD", "SHV", "SIT", "SJC", "SJT", "SJU", "SLC", 
                         "SLN", "SMF", "SMX", "SNA", "SPI", "SPN", "SPS", "SRQ", "STC", "STL", "STS", 
                         "STT", "STX", "SUN", "SUX", "SWF", "SWO", "SYR", "TLH", "TOL", "TPA", "TRI", 
                         "TTN", "TUL", "TUS", "TVC", "TWF", "TXK", "TYR", "TYS", "UIN", "USA", "VCT", 
                         "VEL", "VLD", "VPS", "WRG", "XNA", "XWA", "YAK", "YKM", "YUM")

weather_type_values_1 <- c("BR", "DZ", "FG", "FU", "HZ", "NORM", "PL", "RA", "SH", "SN", "TS")
weather_type_values_2 <- c("BR", "DZ", "FG", "FU", "FZ", "HZ", "NORM", "PL", "RA", "SH", "SN", "TS")
Skycondition_value <- c("CLR.00", "FEW.02", "OVC.08", "SCT.04", "VV.09")

one_hot_columns <- c(
  "DayofMonth", "DayOfWeek", "CRSElapsedTime", "HourlyDryBulbTemperature_originairport", 
  "HourlyPrecipitation_originairport", "HourlyDryBulbTemperature_destairport", 
  "HourlyPrecipitation_destairport", "sin_HourlyWindDirection_originairport", 
  "cos_HourlyWindDirection_originairport", "sin_HourlyWindDirection_destairport", 
  "cos_HourlyWindDirection_destairport", "CRSDepTime_CST_hour", "CRSDepTime_CST_min", 
  "PCA_HourlyAltimeterSetting", "Visbility_Originairport_1", "Visbility_Originairport_2", 
  "Visbility_Destairport_1", "Visbility_Destairport_2", 
  paste0("Year_", year_values, ".0"),
  paste0("Month_", month_values, ".0"),
  paste0("Operating_Airline_", airline_values),
  paste0("Origin_", airport_values_origin),
  paste0("Dest_", airport_values_dest),
  paste0("HourlyPresentWeatherType_originairport_", weather_type_values_1),
  "HourlyWindGustSpeed_originairport_1.0",
  paste0("HourlyPresentWeatherType_destairport_", weather_type_values_2),
  "HourlyWindGustSpeed_destairport_1.0",
  paste0("SkyCondition_HourlySkyConditions_originairport_", Skycondition_value),
  paste0("SkyCondition_HourlySkyConditions_destairport_", Skycondition_value))


create_one_hot_dataframe <- function(X_logistic) {
  # 初始化所有列为 0 的数据框，使用 one_hot_columns 作为列名
  one_hot_df <- as.data.frame(matrix(0, nrow = 1, ncol = length(one_hot_columns)), stringsAsFactors = FALSE)
  colnames(one_hot_df) <- one_hot_columns
  
  one_hot_df$DayofMonth <- X_logistic$Day_of_Month
  one_hot_df$DayOfWeek <- X_logistic$Day_of_Week
  one_hot_df$CRSElapsedTime <- X_logistic$CRSElapsedTime
  one_hot_df$HourlyDryBulbTemperature_originairport <- X_logistic$HourlyDryBulbTemperature_originairport
  one_hot_df$HourlyPrecipitation_originairport <- X_logistic$HourlyPrecipitation_originairport
  one_hot_df$HourlyDryBulbTemperature_destairport <- X_logistic$HourlyDryBulbTemperature_destairport
  one_hot_df$HourlyPrecipitation_destairport <- X_logistic$HourlyPrecipitation_destairport
  one_hot_df$sin_HourlyWindDirection_originairport <- X_logistic$sin_HourlyWindDirection_originairport
  one_hot_df$cos_HourlyWindDirection_originairport <- X_logistic$cos_HourlyWindDirection_originairport
  one_hot_df$sin_HourlyWindDirection_destairport <- X_logistic$sin_HourlyWindDirection_destairport
  one_hot_df$cos_HourlyWindDirection_destairport <- X_logistic$cos_HourlyWindDirection_destairport
  one_hot_df$CRSDepTime_CST_hour <- X_logistic$CRSDepTime_CST_hour
  one_hot_df$CRSDepTime_CST_min <- X_logistic$CRSDepTime_CST_min
  one_hot_df$PCA_HourlyAltimeterSetting <- X_logistic$PCA_HourlyAltimeterSetting
  one_hot_df$Visbility_Originairport_1 <- X_logistic$Visibility_Originairport_1
  one_hot_df$Visbility_Originairport_2 <- X_logistic$Visibility_Originairport_2
  one_hot_df$Visbility_Destairport_1 <- X_logistic$Visibility_Destairport_1
  one_hot_df$Visbility_Destairport_2 <- X_logistic$Visibility_Destairport_2
  
  # 假设 X_logistic 中包含年份列
  for (year in year_values) {
    column_name <- paste0("Year_", year, ".0")
    if (year == X_logistic$Year) {
      one_hot_df[[column_name]] <- 1
    } else {
      one_hot_df[[column_name]] <- 0
    }
  }
  
  for (month in month_values) {
    column_name <- paste0("Month_", month, ".0")
    if (month == X_logistic$Month) {
      one_hot_df[[column_name]] <- 1
    } else {
      one_hot_df[[column_name]] <- 0
    }
  }
  
  for (airline in airline_values) {
    column_name <- paste0("Operating_Airline_", airline)
    if (airline == X_logistic$Operating_Airline) {
      one_hot_df[[column_name]] <- 1
    } else {
      one_hot_df[[column_name]] <- 0
    }
  }
  
  for (airport in airport_values_origin) {
    column_name <- paste0("Origin_", airport)
    if (airport == X_logistic$Origin) {
      one_hot_df[[column_name]] <- 1
    } else {
      one_hot_df[[column_name]] <- 0
    }
  }
  
  for (airport in airport_values_dest) {
    column_name <- paste0("Dest_", airport)
    if (airport == X_logistic$Dest) {
      one_hot_df[[column_name]] <- 1
    } else {
      one_hot_df[[column_name]] <- 0
    }
  }
  
  for (weather in weather_type_values_1) {
    column_name <- paste0("HourlyPresentWeatherType_originairport_", weather)
    if (weather == X_logistic$HourlyPresentWeatherType_originairport) {
      one_hot_df[[column_name]] <- 1
    } else {
      one_hot_df[[column_name]] <- 0
    }
  }
  
  if (format(round(X_logistic$HourlyWindGustSpeed_originairport,1), nsmall = 1) == "1.0"){
    one_hot_df[["HourlyWindGustSpeed_originairport_1.0"]] <- 1
  } else {
    one_hot_df[["HourlyWindGustSpeed_originairport_1.0"]] <- 0
  }
  
  for (weather in weather_type_values_2) {
    column_name <- paste0("HourlyPresentWeatherType_destairport_", weather)
    if (weather == X_logistic$HourlyPresentWeatherType_destairport) {
      one_hot_df[[column_name]] <- 1
    } else {
      one_hot_df[[column_name]] <- 0
    }
  }
  
  if (format(round(X_logistic$HourlyWindGustSpeed_destairport,1), nsmall = 1) == "1.0"){
    one_hot_df[["HourlyWindGustSpeed_destairport_1.0"]] <- 1
  } else {
    one_hot_df[["HourlyWindGustSpeed_destairport_1.0"]] <- 0
  }
  
  for (sky in Skycondition_value) {
    column_name <- paste0("SkyCondition_HourlySkyConditions_originairport_", sky)
    if (sky == X_logistic$SkyCondition_HourlySkyConditions_originairport) {
      one_hot_df[[column_name]] <- 1
    } else {
      one_hot_df[[column_name]] <- 0
    }
  }
  
  for (sky in Skycondition_value) {
    column_name <- paste0("SkyCondition_HourlySkyConditions_destairport_", sky)
    if (sky == X_logistic$SkyCondition_HourlySkyConditions_destairport) {
      one_hot_df[[column_name]] <- 1
    } else {
      one_hot_df[[column_name]] <- 0
    }
  }
  
  return(one_hot_df)
}

create_lgbm_dataframe <- function(X_lgbm){
  lgbm_df <- as.data.frame(matrix(0, nrow = 1, ncol = length(X_lgbm)), stringsAsFactors = FALSE)
  colnames(lgbm_df) <- colnames(X_lgbm)
  
  columns_to_replace <- c("Year", "Month", "Day_of_Month", "Day_of_Week", "DepHour", 
                          "ArrHour", "Avg_actual_elapsed_time", "Avg_visibility_origin", 
                          "Avg_visibility_dest", "HourlyWindSpeed_originairport", 
                          "HourlyWindSpeed_destairport", "HourlyDryBulbTemperature_originairport", 
                          "HourlyDryBulbTemperature_destairport", "HourlyRelativeHumidity_originairport", 
                          "HourlyRelativeHumidity_destairport", "HourlyPrecipitation_originairport", 
                          "HourlyPrecipitation_destairport")
  for (col in columns_to_replace) {
    lgbm_df[[col]] <- X_lgbm[[col]]
  }
  
  lgbm_df$Avg_visbility_origin <- X_lgbm$Avg_visbility_origin
  lgbm_df$Avg_visbility_dest <- X_lgbm$Avg_visbility_dest
  lgbm_df$Operating_Airline <- operating_airline$Airline[operating_airline$Operating_Airline == X_lgbm$Operating_Airline]
  lgbm_df$Origin <- origin$Origin_Airport[origin$Origin == X_lgbm$Origin]
  lgbm_df$Dest <- dest$Dest_Airport[dest$Dest == X_lgbm$Dest]
  lgbm_df$HourlyPresentWeatherType_originairport <- weather_type_origin$WeatherType_originairport[weather_type_origin$HourlyPresentWeatherType_originairport == X_lgbm$HourlyPresentWeatherType_originairport]
  lgbm_df$HourlyPresentWeatherType_destairport <- weather_type_dest$WeatherType_destairport[weather_type_dest$HourlyPresentWeatherType_destairport == X_lgbm$HourlyPresentWeatherType_destairport]
  
  return(lgbm_df)
}

# 天气预报函数
get_weather_forecast_for_time <- function(latitude, longitude, target_datetime) {
  base_url <- paste0("https://api.weather.gov/points/", latitude, ",", longitude)
  headers <- add_headers("User-Agent" = "(MyWeatherApp, rchen394@wisc.edu)")
  
  response <- GET(base_url, headers)
  if (status_code(response) != 200) return(NULL)
  
  data <- fromJSON(content(response, as = "text"))
  forecast_url <- data$properties$forecastHourly
  forecast_response <- GET(forecast_url, headers)
  if (status_code(forecast_response) != 200) return(NULL)
  
  forecast_data <- fromJSON(content(forecast_response, as = "text"))
  periods <- forecast_data$properties$periods
  target_tz <- attr(target_datetime, "tzone")
  
  for (i in 1:nrow(periods)) {
    start_time <- ymd_hms(substr(periods$startTime[i], 1, 19), tz = target_tz)
    end_time <- ymd_hms(substr(periods$endTime[i], 1, 19), tz = target_tz)
    if (target_datetime >= start_time && target_datetime < end_time) {
      return(list(
        timeRange = paste(start_time, "-", end_time),
        temperature = periods$temperature[i],
        temperatureUnit = periods$temperatureUnit[i],
        temperatureTrend = periods$temperatureTrend[i],
        probabilityOfPrecipitation = periods$probabilityOfPrecipitation$value[i],
        dewpoint = periods$dewpoint$value[i],
        relativeHumidity = periods$relativeHumidity$value[i],
        windSpeed = periods$windSpeed[i],
        windDirection = periods$windDirection[i],
        icon = periods$icon[i],
        shortForecast = periods$shortForecast[i],
        detailedForecast = periods$detailedForecast[i]
      ))
    }
  }
  return(NULL)
}

# 获取坐标
get_coordinates <- function(city_name, airport_name, airport_data) {
  city_row <- airport_data %>%
    filter(tolower(CityName) == tolower(city_name) & tolower(Airport) == tolower(airport_name))
  if (nrow(city_row) > 0) {
    return(list(lat = city_row$LATITUDE[1], lon = city_row$LONGITUDE[1]))
  }
  return(list(lat = NA, lon = NA))
}

# 生成动画帧
generate_animation_frames <- function(start_lat, start_lon, end_lat, end_lon, frames = 50) {
  lats <- seq(start_lat, end_lat, length.out = frames)
  lons <- seq(start_lon, end_lon, length.out = frames)
  return(data.frame(lat = lats, lon = lons))
}
