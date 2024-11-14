library(shiny)
library(leaflet)
library(dplyr)
library(shinyjs)
library(httr)
library(jsonlite)
library(lubridate)
library(shinyTime)
#library(reticulate)
library(dplyr)
library(ggplot2)

source("functions for shiny.R")


########### Shiny UI
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Holiday Season Flight Cancellation Delay Forecast"),
  
  tabsetPanel(
    
    # 第一页：描述部分
    tabPanel(
      title = "Introduction",
      fluidRow(
        column(
          width = 12,
          h3("Application Overview"),
          p("This Shiny application is designed to help users forecast potential flight cancellations and delays during the holiday season. Users can select departure and destination cities, specific airports, estimated departure and arrival times, and the operating airline. Based on user inputs, historical flight delay data, and weather conditions, the app utilizes two machine learning models to predict if a flight is likely to be canceled or delayed."),
          
          h4("Key Features:"),
          tags$ul(
            tags$li("Dynamic Airport Selection: Based on the chosen cities, relevant airports are dynamically loaded for user selection."),
            tags$li("Flight Route Animation: Displays the flight path as an animation on the map, providing a visual of the flight’s journey from origin to destination."),
            tags$li("Model Prediction: The app includes two prediction models. A logistic regression model predicts flight cancellation; if the flight is not canceled, a light gradient boosting model predicts expected delay time in minutes."),
            tags$li("Delay Data Visualization: Based on the selected route and airline, a bar chart of average delays by day of the week helps users understand delay trends and high-risk days.")
          ),
          
          h4("Warning Messages:"),
          tags$ul(
            tags$li("If the departure city and destination city are the same, a warning will display in red: 'Origin and destination city should be different!'"),
            tags$li("If there are missing or invalid weather data for the selected airports, the app will notify users with 'Oh! Can't find the weather data.'"),
            tags$li("When no historical data is available for the selected route, the delay plot will display 'OOPS! No historical data is available for this route!'")
          ),
          
          h4("Contact information:"),
          tags$ul(
            tags$li("STAT628 Group 7 rchen394@wisc.edu")
          )
        )
      )
    ),
    
    # 第二页：预测功能部分
    tabPanel(
      title = "Flight Delay Prediction",
      sidebarLayout(
        sidebarPanel(
          selectInput("start_city", "Departure City:", choices = unique(airport_data$CityName)),
          uiOutput("start_airport_ui"),
          selectInput("end_city", "Destination City:", choices = unique(airport_data$CityName)),
          uiOutput("end_airport_ui"),
          dateInput("departure_date", "Estimated Departure Date:", value = Sys.Date() + 1, min = Sys.Date() + 1, max = Sys.Date() + 6),
          timeInput("departure_time", "Estimated Departure Time:", value = strptime("12:00", format="%H:%M")),
          dateInput("arrival_date", "Estimated Arrival Date:", value = Sys.Date() + 1, min = Sys.Date() + 1, max = Sys.Date() + 6),
          timeInput("arrival_time", "Estimated Arrival Time:", value = strptime("14:00", format="%H:%M")),
          selectInput("operating_airline", "Operating Airline:", choices = c('9E', 'AA', 'PT', 'ZW', 'OO', 'YX', 'MQ', 'AS', 'QX', 
                                                                             'B6', 'UA', 'YV', 'C5', 'G7', 'WN', 'DL', 'F9', 'G4', 
                                                                             'HA', 'NK', 'OH', 'AX', 'EV', 'EM', 'CP', 'KS', 'VX', '9K')),
          actionButton("play_button", "Play Animation"),
          uiOutput("error_message")
        ),
        
        mainPanel(
          leafletOutput("flight_map"),
          tags$div(id = "animation_status", "Preparing to play animation...", style = "color:blue;"),
          textOutput("departure_info"),
          textOutput("departure_weather_info"),
          textOutput("arrival_weather_info"),
          verbatimTextOutput("model_output_logistic"), 
          verbatimTextOutput("model_output_lgbm"),
          plotOutput("delayPlot")
        )
      )
    )
  ),
  
  # JavaScript function for animation
  tags$script(HTML("
    function animateFlight(lats, lons) {
      var i = 0;
      function updateMarker() {
        if (i < lats.length) {
          Shiny.setInputValue('current_lat', lats[i]);
          Shiny.setInputValue('current_lon', lons[i]);
          i++;
          setTimeout(updateMarker, 100);
        } else {
          Shiny.setInputValue('animation_complete', true);
        }
      }
      updateMarker();
    }
  "))
)



### Shiny Server
server <- function(input, output, session) {
  plane_icon <- makeIcon(iconUrl = "plane_icon.jpg", iconWidth = 32, iconHeight = 32)
  
  observeEvent(input$play_button, {
    if (input$start_city == input$end_city) {
      output$error_message <- renderUI({
        HTML("<b style='color:red;'>Origin and destination city should be different!</b>")
      })
    } else {
      output$error_message <- renderUI({
        HTML("")
      })
    }
  })
  
  output$start_airport_ui <- renderUI({
    req(input$start_city)
    airports <- airport_data %>%
      filter(CityName == input$start_city) %>%
      pull(Airport) %>%
      unique()
    selectInput("start_airport", "Departure Airport:", choices = airports)
  })
  
  output$end_airport_ui <- renderUI({
    req(input$end_city)
    airports <- airport_data %>%
      filter(CityName == input$end_city) %>%
      pull(Airport) %>%
      unique()
    selectInput("end_airport", "Destination Airport:", choices = airports)
  })
  
  output$flight_map <- renderLeaflet({
    leaflet(options = leafletOptions(attributionControl = FALSE)) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -96, lat = 37.8, zoom = 4)
  })
  
  observeEvent(input$play_button, {
    req(input$start_city, input$start_airport, input$end_city, input$end_airport)
    
    start_coords <- get_coordinates(input$start_city, input$start_airport, airport_data)
    end_coords <- get_coordinates(input$end_city, input$end_airport, airport_data)
    
    if (is.na(start_coords$lat) || is.na(start_coords$lon) || is.na(end_coords$lat) || is.na(end_coords$lon)) {
      showModal(modalDialog(
        title = "Error",
        "Unable to get coordinates for departure or destination. Please check the city and airport names.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    
    frames <- generate_animation_frames(
      start_coords$lat, start_coords$lon,
      end_coords$lat, end_coords$lon
    )
    
    leafletProxy("flight_map") %>%
      clearShapes() %>%
      addPolylines(
        lng = c(start_coords$lon, end_coords$lon),
        lat = c(start_coords$lat, end_coords$lat),
        color = "blue", weight = 2
      )
    
    runjs(sprintf("animateFlight(%s, %s);",
                  jsonlite::toJSON(frames$lat),
                  jsonlite::toJSON(frames$lon)))
    
    observeEvent({
      input$play_button
      
      req(input$start_city,input$start_airport,input$end_city,input$end_airport,input$departure_date,input$departure_time,input$arrival_date,input$arrival_time,input$operating_airline)
      
    }, {
      tz_origin <- ifelse(
        any(airport_data$Airport == input$start_airport & airport_data$CityName == input$start_city),
        airport_data$IANA_Timezone[airport_data$Airport == input$start_airport & airport_data$CityName == input$start_city],
        "America/Chicago"
      )
      
      tz_dest <- ifelse(
        any(airport_data$Airport == input$end_airport & airport_data$CityName == input$end_city),
        airport_data$IANA_Timezone[airport_data$Airport == input$end_airport & airport_data$CityName == input$end_city],
        "America/Chicago"
      )      
      departure_datetime <- as.POSIXct(paste(input$departure_date, format(input$departure_time, "%H:%M:%S")), tz = tz_origin)
      arrival_datetime <- as.POSIXct(paste(input$arrival_date, format(input$arrival_time, "%H:%M:%S")), tz = tz_dest)
      
      # 转换为 CST 时间
      departure_datetime_CST <- with_tz(departure_datetime, "America/Chicago")
      arrival_datetime_CST <- with_tz(arrival_datetime, "America/Chicago")
      CRSElapsedTime <- as.numeric(with_tz(arrival_datetime, "America/Chicago") - with_tz(departure_datetime, "America/Chicago"), units = "mins")
      CRSDepTime_CST_hour <- hour(departure_datetime_CST)
      CRSDepTime_CST_min <- minute(departure_datetime_CST)
      DepHour <- as.numeric(format(departure_datetime, "%H"))  # 提取出小时部分并转换为数值型
      ArrHour <- as.numeric(format(arrival_datetime, "%H")) 
      
      # 显示出发时间的 Year, Month, DayofMonth, DayofWeek
      year <- year(departure_datetime)
      month <- month(departure_datetime)
      dayofmonth <- day(departure_datetime)
      dayofweek <- wday(departure_datetime, label = FALSE, week_start = 1)
      Operating_Airline <- input$operating_airline
      
      departure_weather <- get_weather_forecast_for_time(start_coords$lat, start_coords$lon, departure_datetime)
      arrival_weather <- get_weather_forecast_for_time(end_coords$lat, end_coords$lon, arrival_datetime)
      
      sin_HourlyWindDirection_originairport <- sin(
        ifelse(is.null(departure_weather$windDirection) || is.na(departure_weather$windDirection), 
               0, 
               convert_wind_direction_to_angle(departure_weather$windDirection)) * (pi / 180)
      )
      
      cos_HourlyWindDirection_originairport <- cos(
        ifelse(is.null(departure_weather$windDirection) || is.na(departure_weather$windDirection), 
               0, 
               convert_wind_direction_to_angle(departure_weather$windDirection)) * (pi / 180)
      )
      
      sin_HourlyWindDirection_destairport <- sin(
        ifelse(is.null(arrival_weather$windDirection) || is.na(arrival_weather$windDirection), 
               0, 
               convert_wind_direction_to_angle(arrival_weather$windDirection)) * (pi / 180)
      )
      
      cos_HourlyWindDirection_destairport <- cos(
        ifelse(is.null(arrival_weather$windDirection) || is.na(arrival_weather$windDirection), 
               0, 
               convert_wind_direction_to_angle(arrival_weather$windDirection)) * (pi / 180)
      )
      
      avg_origin_weather <- average_weather[average_weather$airport == input$start_airport, ]
      avg_dest_weather <- average_weather[average_weather$airport == input$start_airport, ]
      # 提取平均实际飞行时间，如果没有匹配的值，则设置为 0
      avg_actual_elapsed_time <- ifelse(
        length(actual_elapsed$AverageActualElapsedTime[actual_elapsed$Origin == input$start_airport & actual_elapsed$Dest == input$end_airport]) > 0,
        actual_elapsed$AverageActualElapsedTime[actual_elapsed$Origin == input$start_airport & actual_elapsed$Dest == input$end_airport],
        0
      )
      avg_visbility_origin <- average_visibility$averageHourlyVisibility [average_visibility$airport == input$start_airport]
      avg_visbility_dest <- average_visibility$averageHourlyVisibility [average_visibility$airport == input$end_airport]
      
      # 确保所有变量存在，否则使用 NA 作为默认值
      X_logistic <- data.frame(
        Year = if (exists("year")) year else NA,
        Month = if (exists("month")) month else NA,
        Day_of_Month = if (exists("dayofmonth")) dayofmonth else NA,
        Day_of_Week = if (exists("dayofweek")) dayofweek else NA,
        Operating_Airline = if (exists("Operating_Airline")) Operating_Airline else NA,
        Origin = input$start_airport,
        Dest = input$end_airport,
        CRSElapsedTime = if (exists("CRSElapsedTime")) CRSElapsedTime else NA,
        
        HourlyDryBulbTemperature_originairport = ftoc(departure_weather$temperature),
        HourlyPrecipitation_originairport = avg_origin_weather$HourlyPrecipitation,
        HourlyPresentWeatherType_originairport = avg_origin_weather$HourlyPresentWeatherType,
        HourlyWindGustSpeed_originairport = ifelse(avg_origin_weather$HourlyWindGustSpeed > 0.5, 1.0, 0.0),
        
        HourlyDryBulbTemperature_destairport = ftoc(arrival_weather$temperature),
        HourlyPrecipitation_destairport = avg_dest_weather$HourlyPrecipitation,
        HourlyPresentWeatherType_destairport = avg_dest_weather$HourlyPresentWeatherType,
        HourlyWindGustSpeed_destairport = ifelse(avg_dest_weather$HourlyWindGustSpeed > 0.5, 1.0, 0.0),
        
        SkyCondition_HourlySkyConditions_originairport = avg_origin_weather$SkyCondition,
        SkyCondition_HourlySkyConditions_destairport = avg_dest_weather$SkyCondition,
        
        sin_HourlyWindDirection_originairport = if (exists("sin_HourlyWindDirection_originairport")) sin_HourlyWindDirection_originairport else NA,
        cos_HourlyWindDirection_originairport = if (exists("cos_HourlyWindDirection_originairport")) cos_HourlyWindDirection_originairport else NA,
        sin_HourlyWindDirection_destairport = if (exists("sin_HourlyWindDirection_destairport")) sin_HourlyWindDirection_destairport else NA,
        cos_HourlyWindDirection_destairport = if (exists("cos_HourlyWindDirection_destairport")) cos_HourlyWindDirection_destairport else NA,
        
        CRSDepTime_CST_hour = if (exists("CRSDepTime_CST_hour")) CRSDepTime_CST_hour else NA,
        CRSDepTime_CST_min = if (exists("CRSDepTime_CST_min")) CRSDepTime_CST_min else NA,
        
        PCA_HourlyAltimeterSetting = 0.7139 * avg_origin_weather$PCA_HourlyAltimeterSetting + 0.7001 * avg_dest_weather$PCA_HourlyAltimeterSetting,
        
        Visibility_Originairport_1 = avg_origin_weather$Visbility_1,
        Visibility_Originairport_2 = avg_origin_weather$Visbility_2,
        Visibility_Destairport_1 = avg_dest_weather$Visbility_1,
        Visibility_Destairport_2 = avg_dest_weather$Visbility_2
      )
      
      
      # output$model_output_logistic <- renderPrint({
      #   if (any(is.na(X_logistic))) {
      #     # 如果 X_logistic 中有缺失值，输出提示信息
      #     paste("Oh! Can't find the weather data.")
      #   } else {
      #     # 如果没有缺失值，执行 one-hot 编码
      #     X_logistic_final <- create_one_hot_dataframe(X_logistic)
      #     
      #     # 检查编码后的数据框是否有空列
      #     if (ncol(X_logistic_final) == 0 || nrow(X_logistic_final) == 0) {
      #       paste("Error: One-hot encoded dataframe has missing data.")
      #     } else {
      #       # 进行预测
      #       predictions <- predict_logistic(as.matrix(X_logistic_final))
      #       
      #       # 根据预测结果输出取消信息
      #       cancel_message <- ifelse(predictions == 1, "This flight is predicted to be canceled.", "This flight is predicted not to be canceled.")
      #       
      #       # 输出预测结果和取消信息
      #       cat("Predict if this flight is canceled:", cancel_message)
      #     }
      #   }
      # })
      
      X_lgbm <- data.frame(
        Year = if (exists("year")) year else 0,
        Month = if (exists("month")) month else 0,
        Day_of_Month = if (exists("dayofmonth")) dayofmonth else 0,
        Day_of_Week = if (exists("dayofweek")) dayofweek else 0,
        DepHour = if (exists("DepHour")) DepHour else 0,
        ArrHour = if (exists("ArrHour")) ArrHour else 0,
        Operating_Airline = if (exists("Operating_Airline")) Operating_Airline else 0,
        Origin = input$start_airport,
        Dest = input$end_airport,
        Avg_actual_elapsed_time = if (exists("avg_actual_elapsed_time")) avg_actual_elapsed_time else 0,
        
        Avg_visbility_origin = if (exists("avg_visbility_origin")) avg_visbility_origin else 0,
        Avg_visbility_dest = if (exists("avg_visbility_dest")) avg_visbility_dest else 0,
        
        HourlyWindSpeed_originairport = departure_weather$windSpeed,
        HourlyWindSpeed_destairport = arrival_weather$windSpeed,
        
        HourlyDryBulbTemperature_originairport = ftoc(departure_weather$temperature),
        HourlyDryBulbTemperature_destairport = ftoc(arrival_weather$temperature),
        
        HourlyRelativeHumidity_originairport = departure_weather$relativeHumidity,
        HourlyRelativeHumidity_destairport = arrival_weather$relativeHumidity,
        
        HourlyPresentWeatherType_originairport = avg_origin_weather$HourlyPresentWeatherType,
        HourlyPresentWeatherType_destairport = avg_dest_weather$HourlyPresentWeatherType,
        
        HourlyPrecipitation_originairport = avg_origin_weather$HourlyPrecipitation,
        HourlyPrecipitation_destairport = avg_dest_weather$HourlyPrecipitation
      )
      
      output$model_output_logistic <- renderPrint({
        # 清除上次输出
        output$model_output_lgbm <- NULL
        
        if (any(is.na(X_logistic))) {
          cat("Oh! Can't find the weather data.")
        } else {
          # 第一次模型预测
          X_logistic_final <- create_one_hot_dataframe(X_logistic)
          
          if (ncol(X_logistic_final) == 0 || nrow(X_logistic_final) == 0) {
            cat("Error: One-hot encoded dataframe has missing data.")
          } else {
            predictions <- predict_logistic(as.matrix(X_logistic_final))
            first_prediction <- ifelse(predictions == 1, "not canceled", "canceled")
            
            # 拼接输出内容
            output_text <- paste("Prediction of cancellation:", first_prediction)
            
            # 判断是否需要进行第二次预测
            if (predictions == 1) {  # 如果未取消
              if (any(is.na(X_lgbm))) {
                output_text <- paste(output_text, "\nOh! Can't find the weather data for delay prediction.")
              } else {
                # 第二次模型预测
                X_lgbm_final <- create_lgbm_dataframe(X_lgbm)
                
                if (ncol(X_lgbm_final) == 0 || nrow(X_lgbm_final) == 0) {
                  output_text <- paste(output_text, "\nError: Processed encoded dataframe has missing data.")
                } else {
                  predictions_lgbm <- predict(model, newdata = as.matrix(X_lgbm_final))
                  output_text <- paste(output_text, "\nPredicted delay time:", round(predictions_lgbm,2), "mins")
                }
              }
            }
            
            # 输出组合结果
            cat(output_text)
          }
        }
      })
      
      
      
      output$delayPlot <- renderPlot({
        # Filter data based on selected Origin, Dest, and Operating_Airline
        filtered_data <- delay_time %>%
          filter(Origin == input$start_airport, 
                 Dest == input$end_airport, 
                 Operating_Airline == input$operating_airline)
        
        # Check if there is any data after filtering
        if (nrow(filtered_data) == 0) {
          # Display a message when no data is available
          ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = "OOPS! No historical data is available for this route!", size = 6, hjust = 0.5, vjust = 0.5) +
            theme_void()
        } else {
          # Extract the day of the week for the selected date
          selected_day <- wday(input$departure_date, label = TRUE, week_start = 1)
          
          # Create the plot
          ggplot(filtered_data, aes(x = factor(DayOfWeek, levels = 1:7, labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")), 
                                    y = AverageArrDelay, fill = factor(DayOfWeek == as.integer(selected_day)))) +
            geom_bar(stat = "identity") +
            scale_fill_manual(values = c("TRUE" = "darkblue", "FALSE" = "lightblue")) +
            labs(x = "Day of the Week", y = "Average Arrival Delay (min)", title = "Flight Route Delay (Mon-Sun)") +
            theme_minimal() +
            theme(legend.position = "none")
        }
      })
      
      
      # output$model_output_lgbm <- renderPrint({
      #   if (any(is.na(X_lgbm))) {
      #     # 如果 X_logistic 中有缺失值，输出提示信息
      #     paste("Oh! Can't find the weather data.")
      #   } else {
      #     # 如果没有缺失值，执行 one-hot 编码
      #     X_lgbm_final <- create_lgbm_dataframe(X_lgbm)
      #     
      #     # 检查编码后的数据框是否有空列
      #     if (ncol(X_lgbm_final) == 0 || nrow(X_lgbm_final) == 0) {
      #       paste("Error: Processed encoded dataframe has missing data.")
      #     } else {
      #       # 进行预测
      #       predictions <- predict(model, newdata = as.matrix(X_lgbm_final))
      #       
      #       cat("Predicted delay time:", predictions, " mins")
      #     }
      #   }
      # })
      
      # output$departure_info <- renderText({
      #   if (nrow(avg_origin_weather) > 0 && nrow(avg_dest_weather) > 0) {
      #     paste(
      #       "Departure Date Information:",
      #       "\nYear:", year,
      #       "\nMonth:", month,
      #       "\nDay of Month:", dayofmonth,
      #       "\nDay of Week:", dayofweek,
      #       "\nOperating Airline:", Operating_Airline,
      #       "\nOrigin:", input$start_airport,
      #       "\nDest:", input$end_airport,
      #       "\nCRSElapsedTime:", CRSElapsedTime,
      # 
      #       "\nHourlyDryBulbTemperature_originairport:", departure_weather$temperature,
      #       "\nHourlyPrecipitation_originairport:", avg_origin_weather$HourlyPrecipitation,
      #       "\nHourlyPresentWeatherType_originairport:", avg_origin_weather$HourlyPresentWeatherType,
      #       "\nHourlyWindGustSpeed_originairport:", ifelse(avg_origin_weather$HourlyWindGustSpeed > 0.5, 1.0, 0.0),
      # 
      #       "\nHourlyDryBulbTemperature_destairport:", arrival_weather$temperature,
      #       "\nHourlyPrecipitation_destairport:", avg_dest_weather$HourlyPrecipitation,
      #       "\nHourlyPresentWeatherType_destairport:", avg_dest_weather$HourlyPresentWeatherType,
      #       "\nHourlyWindGustSpeed_destairport:", ifelse(avg_dest_weather$HourlyWindGustSpeed > 0.5, 1.0, 0.0),
      # 
      #       "\nSkyCondition_HourlySkyConditions_originairport:", avg_origin_weather$SkyCondition,
      #       "\nSkyCondition_HourlySkyConditions_destairport:", avg_dest_weather$SkyCondition,
      # 
      #       "\nsin_HourlyWindDirection_originairport:", sin_HourlyWindDirection_originairport,
      #       "\ncos_HourlyWindDirection_originairport:", cos_HourlyWindDirection_originairport,
      #       "\nsin_HourlyWindDirection_destairport:", sin_HourlyWindDirection_destairport,
      #       "\ncos_HourlyWindDirection_destairport:", cos_HourlyWindDirection_destairport,
      # 
      #       "\nCRSDepTime_CST_hour:", CRSDepTime_CST_hour,
      #       "\nCRSDepTime_CST_min:", CRSDepTime_CST_min,
      # 
      #       "\nPCA_HourlyAltimeterSetting:", 0.7139 * avg_origin_weather$PCA_HourlyAltimeterSetting + 0.7001 * avg_dest_weather$PCA_HourlyAltimeterSetting,
      #       "\nVisibility_Originairport_1:", avg_origin_weather$Visbility_1,
      #       "\nVisibility_Originairport_2:", avg_origin_weather$Visbility_2,
      #       "\nVisibility_Destairport_1:", avg_dest_weather$Visbility_1,
      #       "\nVisibility_Destairport_2:", avg_dest_weather$Visbility_2
      #       #"\nstr:",str(X_logistic)
      #     )
      #   } else {
      #     "No data available for selected airports."
      #   }
      # })
      
      # output$departure_info <- renderText({
      #   if (nrow(avg_origin_weather) > 0 && nrow(avg_dest_weather) > 0) {
      #     paste(
      #       "Departure Date Information:",
      #       "\nYear:", year,
      #       "\nDay of Week:", dayofweek,
      #       "\nDay of Month:", dayofmonth,
      #       "\nMonth:", month,
      #       "\nDepHour:",DepHour,
      #       "\nArrHour:",ArrHour,
      #       "\nOperating Airline:", Operating_Airline,
      #       "\nOrigin:", input$start_airport,
      #       "\nDest:", input$end_airport,
      #       "\nAvg_actual_elapsed_time:",avg_actual_elapsed_time,
      #       "\nAvg_visbility_origin",avg_visbility_origin,
      #       "\nAvg_visbility_dest",avg_visbility_dest,
      #       "\nHourlyWindSpeed_originalairport:",departure_weather$windSpeed,
      #       "\nHourlyWindSpeed_destairport:",arrival_weather$windSpeed,
      #       "\nHourlyDryBulbTemperature_destairport:", arrival_weather$temperature,
      #       "\nHourlyDryBulbTemperature_originairport:", departure_weather$temperature,
      #       "\nHourlyRelativeHumidity_destairport:",arrival_weather$relativeHumidity,
      #       "\nHourlyRelativeHumidity_originairport:",departure_weather$relativeHumidity,
      #       "\nHourlyPresentWeatherType_originairport:", avg_origin_weather$HourlyPresentWeatherType,
      #       "\nHourlyPresentWeatherType_destairport:", avg_dest_weather$HourlyPresentWeatherType,
      #       "\nHourlyPrecipitation_originairport:", avg_origin_weather$HourlyPrecipitation,
      #       "\nHourlyPrecipitation_destairport:", avg_dest_weather$HourlyPrecipitation
      #     )
      #   } else {
      #     "No data available for selected airports."
      #   }
      # })
      
      
      
      # output$departure_weather_info <- renderText({
      #   if (is.null(departure_weather)) {
      #     return("No weather data available for departure.")
      #   }
      #   paste(
      #     "Departure Weather Information:\n",
      #     "Time Range:", departure_weather$timeRange, "\n",
      #     "Temperature:", departure_weather$temperature, departure_weather$temperatureUnit, "\n",
      #     "Precipitation Probability:", departure_weather$probabilityOfPrecipitation, "%", "\n",
      #     "Dew Point:", departure_weather$dewpoint, "°C", "\n",
      #     "Relative Humidity:", departure_weather$relativeHumidity, "%", "\n",
      #     "Wind Speed:", departure_weather$windSpeed, "\n",
      #     "Wind Direction:", convert_wind_direction_to_angle(departure_weather$windDirection), "\n"
      #   )
      # })
      
      # output$arrival_weather_info <- renderText({
      #   if (is.null(arrival_weather)) {
      #     return("No weather data available for arrival.")
      #   }
      #   paste(
      #     "Arrival Weather Information:\n",
      #     "Time Range:", arrival_weather$timeRange, "\n",
      #     "Temperature:", arrival_weather$temperature, arrival_weather$temperatureUnit, "\n",
      #     "Precipitation Probability:", arrival_weather$probabilityOfPrecipitation, "%", "\n",
      #     "Dew Point:", arrival_weather$dewpoint, "°C", "\n",
      #     "Relative Humidity:", arrival_weather$relativeHumidity, "%", "\n",
      #     "Wind Speed:", arrival_weather$windSpeed, "\n",
      #     "Wind Direction:", convert_wind_direction_to_angle(arrival_weather$windDirection), "\n"
      #   )
      # })
    })
  })
  
  observeEvent(input$current_lat, {
    leafletProxy("flight_map") %>%
      clearMarkers() %>%
      addMarkers(
        lng = input$current_lon, lat = input$current_lat, 
        icon = plane_icon
      )
    shinyjs::html("animation_status", sprintf("Playing: Current Latitude %f, Longitude %f", input$current_lat, input$current_lon))
  })
  
  observeEvent(input$animation_complete, {
    shinyjs::html("animation_status", "Animation finished")
  })
}

shinyApp(ui, server)

