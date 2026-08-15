# ============================================================
# server.R
# Defines the reactive logic, calculations and rendered outputs.
# ============================================================

server <- function(input, output, session) {
  
  # ---- Reset Filters ----
  observeEvent(input$reset_filters, {
    updatePickerInput(session, "station_filter", selected = "All")
    updatePickerInput(session, "season_filter", selected = "All")
    updatePickerInput(session, "pollutant_filter", selected = "aqi_site")
    updateDateRangeInput(session, "date_filter", start = date_min, end = date_max)
    updateRadioButtons(session, "radius_filter", selected = 10)
  })
  
  
  # ---- Reactive Filtered Dataset ----
  filtered_data <- reactive({
    
    data <- air_data
    
    if (!is.null(input$station_filter) && input$station_filter != "All") {
      data <- data %>%
        filter(station_name == input$station_filter)
    }
    
    if (!is.null(input$season_filter) && input$season_filter != "All") {
      data <- data %>%
        filter(season == input$season_filter)
    }
    
    data <- data %>%
      filter(
        date >= input$date_filter[1],
        date <= input$date_filter[2]
      )
    
    data
  })
  
  # ---- Helper: Toggle Station Selection ----
  update_station_selection <- function(clicked_station) {
    
    if (is.null(clicked_station) ||
        !(clicked_station %in% levels(air_data$station_name))) {
      return(NULL)
    }
    
    new_selection <- if (!is.null(input$station_filter) &&
                         input$station_filter == clicked_station) {
      "All"
    } else {
      clicked_station
    }
    
    updatePickerInput(
      session,
      "station_filter",
      selected = new_selection
    )
  }
  
  
  # ---- Selected Metric Display Settings ----
  selected_metric_label <- reactive({
    req(input$pollutant_filter)
    names(pollutant_choices)[pollutant_choices == input$pollutant_filter]
  })
  
  selected_metric_accuracy <- reactive({
    req(input$pollutant_filter)
    
    if (input$pollutant_filter == "o3_8hr") {
      0.01
    } else {
      0.1
    }
  })
  
  # ---- Dynamic KPI Labels ----
  output$avg_metric_label <- renderText({
    paste("Avg", selected_metric_label())
  })
  
  output$max_metric_label <- renderText({
    paste("Max", selected_metric_label())
  })
  
  # ---- KPI: Average Selected Metric ----
  output$avg_aqi <- renderText({
    selected_metric <- input$pollutant_filter
    
    value <- filtered_data() %>%
      summarise(avg_value = mean(.data[[selected_metric]], na.rm = TRUE)) %>%
      pull(avg_value)
    
    number(value, accuracy = selected_metric_accuracy())
  })
  
  
  # ---- KPI: Maximum Selected Metric ----
  output$max_aqi <- renderText({
    selected_metric <- input$pollutant_filter
    
    value <- filtered_data() %>%
      summarise(max_value = max(.data[[selected_metric]], na.rm = TRUE)) %>%
      pull(max_value)
    
    number(value, accuracy = selected_metric_accuracy())
  })
  
  
  # ---- KPI: Poor+ Days ----
  output$poor_days <- renderText({
    value <- filtered_data() %>%
      filter(aqi_band %in% c("Poor", "Very Poor", "Hazardous")) %>%
      nrow()
    
    comma(value)
  })
  
  
  # ---- KPI: Record Count ----
  output$record_count <- renderText({
    comma(nrow(filtered_data()))
  })
  
  
  # ---- Preview Seasonal AQI Plot ----
  output$preview_season_plot <- renderPlotly({
    
    plot_data <- filtered_data() %>%
      group_by(station_name, season) %>%
      summarise(
        avg_aqi = mean(aqi_site, na.rm = TRUE),
        record_count = n(),
        .groups = "drop"
      )
    
    p <- ggplot(
      plot_data,
      aes(
        x = season,
        y = avg_aqi,
        fill = station_name,
        customdata = station_name,
        text = paste0(
          "Station: ", station_name,
          "<br>Season: ", season,
          "<br>Average AQI: ", round(avg_aqi, 1),
          "<br>Records: ", record_count
        )
      )
    ) +
      geom_col(
        position = position_dodge(width = 0.75),
        width = 0.65
      ) +
      scale_fill_manual(values = station_palette, drop = FALSE) +
      labs(
        title = "Average AQI by Monitoring Station and Season",
        x = "Season",
        y = "Average AQI",
        fill = "Station"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 17),
        axis.title = element_text(face = "bold"),
        legend.position = "bottom"
      )
    
    ggplotly(p, tooltip = "text", source = "season_bar") %>%
      layout(
        legend = list(orientation = "h", x = 0.25, y = -0.2)
      )
  })
  
  # ---- AQI Band Distribution Plot ----
  output$aqi_band_plot <- renderPlotly({
    
    band_data <- filtered_data() %>%
      filter(!is.na(aqi_band)) %>%
      mutate(
        aqi_band = factor(
          aqi_band,
          levels = c("Hazardous", "Very Poor", "Poor", "Fair", "Good")
        )
      ) %>%
      group_by(station_name, aqi_band) %>%
      summarise(
        day_count = n(),
        .groups = "drop"
      )
    
    p <- ggplot(
      band_data,
      aes(
        x = station_name,
        y = day_count,
        fill = aqi_band,
        customdata = station_name,
        text = paste0(
          "Station: ", station_name,
          "<br>AQI Band: ", aqi_band,
          "<br>Days: ", day_count
        )
      )
    ) +
      geom_col(width = 0.65) +
      scale_fill_manual(values = aqi_band_palette, drop = FALSE) +
      labs(
        title = "AQI Band Distribution by Monitoring Station",
        x = "Monitoring Station",
        y = "Number of Daily Records",
        fill = "AQI Band"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 17),
        axis.title = element_text(face = "bold"),
        legend.position = "bottom"
      )
    
    ggplotly(p, tooltip = "text", source = "aqi_band_bar") %>%
      layout(
        legend = list(orientation = "h", x = 0.05, y = -0.25)
      )
  })
  
  # ---- Linked Interaction: AQI Band Bar Click Toggles Station Filter ----
  observeEvent(
    event_data("plotly_click", source = "aqi_band_bar", priority = "event"),
    {
      click_data <- event_data(
        "plotly_click",
        source = "aqi_band_bar",
        priority = "event"
      )
      
      if (!is.null(click_data$customdata)) {
        update_station_selection(as.character(click_data$customdata))
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )
  
  # ---- Linked Interaction: Seasonal Bar Click Toggles Station Filter ----
  observeEvent(event_data("plotly_click", source = "season_bar"), {
    
    click_data <- event_data("plotly_click", source = "season_bar")
    
    if (!is.null(click_data$customdata)) {
      update_station_selection(as.character(click_data$customdata))
    }
  })
  
  # ---- Spatial Map: Monitoring Stations and NPI Facilities ----
  output$station_npi_map <- renderLeaflet({
    
    selected_station <- input$station_filter
    selected_radius_km <- as.numeric(input$radius_filter)
    
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      # Add NPI facility points
      addCircleMarkers(
        data = npi_data,
        lng = ~longitude,
        lat = ~latitude,
        radius = 4,
        color = "#d97706",
        fillColor = "#f59e0b",
        fillOpacity = 0.75,
        stroke = TRUE,
        weight = 1,
        group = "NPI facilities",
        popup = ~paste0(
          "<b>", facility_name, "</b><br>",
          "Business: ", registered_business_name, "<br>",
          "Suburb: ", suburb, "<br>",
          "Industry: ", primary_anzsic_class_name
        )
      ) %>%
      
      # Add monitoring station points
      addCircleMarkers(
        data = station_summary,
        lng = ~station_longitude,
        lat = ~station_latitude,
        layerId = ~as.character(station_name),
        radius = 9,
        color = "#0f172a",
        fillColor = ~dplyr::case_when(
          station_name == "Civic" ~ "#1f78b4",
          station_name == "Florey" ~ "#33a02c",
          station_name == "Monash" ~ "#e31a1c",
          TRUE ~ "#0f172a"
        ),
        fillOpacity = 0.95,
        stroke = TRUE,
        weight = 2,
        group = "Monitoring stations",
        label = ~lapply(
          paste0(
            "<div class='map-tooltip'>",
            "<div class='map-tooltip-title'>", station_name, " Monitoring Station</div>",
            "<div><b>Average AQI:</b> ", round(avg_aqi, 1), "</div>",
            "<div><b>Maximum AQI:</b> ", round(max_aqi, 1), "</div>",
            "<div><b>Facilities within 5 km:</b> ", facilities_5km, "</div>",
            "<div><b>Facilities within 10 km:</b> ", facilities_10km, "</div>",
            "<div><b>Nearest facility:</b> ", nearest_facility, "</div>",
            "<div><b>Nearest distance:</b> ", round(nearest_facility_distance_km, 2), " km</div>",
            "</div>"
          ),
          htmltools::HTML
        ),
        labelOptions = labelOptions(
          direction = "auto",
          textsize = "13px",
          opacity = 1,
          className = "custom-leaflet-label"
        )
      )
    
    # Add radius circle only when one station is selected
    if (!is.null(selected_station) && selected_station != "All") {
      
      selected_station_data <- station_summary %>%
        filter(station_name == selected_station)
      
      map <- map %>%
        addCircles(
          data = selected_station_data,
          lng = ~station_longitude,
          lat = ~station_latitude,
          radius = selected_radius_km * 1000,
          color = "#0f766e",
          fillColor = "#0f766e",
          fillOpacity = 0.06,
          weight = 2,
          group = "Selected radius",
          options = pathOptions(interactive = FALSE)
        ) %>%
        setView(
          lng = selected_station_data$station_longitude[1],
          lat = selected_station_data$station_latitude[1],
          zoom = ifelse(selected_radius_km == 5, 12, 11)
        )
      
    } else {
      
      map <- map %>%
        fitBounds(
          lng1 = min(station_summary$station_longitude, na.rm = TRUE) - 0.08,
          lat1 = min(station_summary$station_latitude, na.rm = TRUE) - 0.08,
          lng2 = max(station_summary$station_longitude, na.rm = TRUE) + 0.08,
          lat2 = max(station_summary$station_latitude, na.rm = TRUE) + 0.08
        )
    }
    
    map %>%
      addLayersControl(
        overlayGroups = c(
          "Monitoring stations",
          "NPI facilities",
          "Selected radius"
        ),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # ---- Linked Interaction: Map Station Click Toggles Station Filter ----
  observeEvent(input$station_npi_map_marker_click, {
    
    clicked_marker <- input$station_npi_map_marker_click
    
    if (!is.null(clicked_marker$id)) {
      update_station_selection(clicked_marker$id)
    }
  })
  
  # ---- Spatial Insight Panel ----
  output$map_insight <- renderUI({
    
    selected_station <- input$station_filter
    selected_radius_km <- as.numeric(input$radius_filter)
    
    # Case 1: All stations selected
    if (is.null(selected_station) || selected_station == "All") {
      
      highest_aqi_station <- station_summary %>%
        arrange(desc(avg_aqi)) %>%
        slice(1)
      
      highest_facility_station <- station_summary %>%
        arrange(desc(facilities_10km)) %>%
        slice(1)
      
      HTML(
        paste0(
          "<p><b>All stations are currently shown.</b></p>",
          
          "<p>The map compares the three Canberra monitoring stations: ",
          "<b>Civic</b>, <b>Florey</b>, and <b>Monash</b>, together with nearby ",
          "National Pollutant Inventory facilities.</p>",
          
          "<p><b>", highest_aqi_station$station_name, "</b> has the highest average AQI ",
          "in the selected data, with an average AQI of <b>",
          round(highest_aqi_station$avg_aqi, 1), "</b>.</p>",
          
          "<p><b>", highest_facility_station$station_name, "</b> has the largest number of ",
          "NPI facilities within 10 km, with <b>",
          highest_facility_station$facilities_10km, "</b> facilities.</p>",
          
          "<p class='insight-note'>Spatial proximity is shown as environmental context only. ",
          "It should not be interpreted as direct evidence that nearby facilities caused the ",
          "observed AQI pattern.</p>"
        )
      )
      
    } else {
      
      # Case 2: Specific station selected
      selected_station_data <- station_summary %>%
        filter(station_name == selected_station)
      
      facility_count <- if (selected_radius_km == 5) {
        selected_station_data$facilities_5km
      } else {
        selected_station_data$facilities_10km
      }
      
      HTML(
        paste0(
          "<p><b>", selected_station_data$station_name, " station is selected.</b></p>",
          
          "<p>The selected station has an average AQI of <b>",
          round(selected_station_data$avg_aqi, 1), "</b> and a maximum AQI of <b>",
          round(selected_station_data$max_aqi, 1), "</b> during the selected period.</p>",
          
          "<p>Within the selected <b>", selected_radius_km, " km</b> radius, there are <b>",
          facility_count, "</b> NPI facilities near this station.</p>",
          
          "<p>The nearest listed facility is <b>",
          selected_station_data$nearest_facility, "</b>, approximately <b>",
          round(selected_station_data$nearest_facility_distance_km, 2),
          " km</b> away.</p>",
          
          "<p class='insight-note'>This view supports comparison between local monitoring ",
          "conditions and surrounding industrial context, while avoiding causal claims from ",
          "distance alone.</p>"
        )
      )
    }
  })
  
  # ---- Weekly AQI Trend Plot ----
  output$weekly_aqi_plot <- renderPlotly({
    
    weekly_data <- filtered_data() %>%
      mutate(
        week_start = lubridate::floor_date(date, unit = "week", week_start = 1)
      ) %>%
      group_by(station_name, week_start) %>%
      summarise(
        avg_aqi = mean(aqi_site, na.rm = TRUE),
        avg_pm25 = mean(pm25_24hr, na.rm = TRUE),
        avg_pm10 = mean(pm10_24hr, na.rm = TRUE),
        record_count = n(),
        .groups = "drop"
      )
    
    peak_point <- weekly_data %>%
      arrange(desc(avg_aqi)) %>%
      slice(1)
    
    # Dynamic annotation offsets based on visible date range
    date_span_days <- as.numeric(
      max(weekly_data$week_start, na.rm = TRUE) -
        min(weekly_data$week_start, na.rm = TRUE)
    )
    
    x_offset_text <- max(7, date_span_days * 0.06)
    x_offset_arrow <- max(4, date_span_days * 0.035)
    
    y_span <- max(weekly_data$avg_aqi, na.rm = TRUE) -
      min(weekly_data$avg_aqi, na.rm = TRUE)
    
    y_offset_text <- max(4, y_span * 0.12)
    y_offset_arrow <- max(2, y_span * 0.06)
    
    p <- ggplot(
      weekly_data,
      aes(
        x = week_start,
        y = avg_aqi,
        colour = station_name,
        group = station_name,
        customdata = station_name,
        text = paste0(
          "Station: ", station_name,
          "<br>Week starting: ", week_start,
          "<br>Weekly average AQI: ", round(avg_aqi, 1),
          "<br>Average PM2.5: ", round(avg_pm25, 1),
          "<br>Average PM10: ", round(avg_pm10, 1),
          "<br>Records: ", record_count
        )
      )
    ) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 1.8, alpha = 0.8) +
      geom_point(
        data = peak_point,
        aes(
          x = week_start,
          y = avg_aqi
        ),
        inherit.aes = FALSE,
        size = 4.2,
        shape = 21,
        stroke = 1.2,
        fill = "white",
        colour = "#111827"
      ) +
      geom_segment(
        data = peak_point,
        aes(
          x = week_start + x_offset_arrow,
          y = avg_aqi + y_offset_arrow,
          xend = week_start + 1,
          yend = avg_aqi + 1
        ),
        inherit.aes = FALSE,
        arrow = arrow(length = unit(0.12, "cm"), type = "closed"),
        linewidth = 0.45,
        colour = "#374151"
      ) +
      geom_text(
        data = peak_point,
        aes(
          x = week_start + x_offset_text + 2,
          y = avg_aqi + y_offset_text,
          label = "Late autumn\nAQI peak"
        ),
        inherit.aes = FALSE,
        hjust = 0,
        size = 3.3,
        fontface = "bold",
        colour = "#111827"
      ) +
      scale_colour_manual(values = station_palette, drop = FALSE) +
      labs(
        title = "Weekly Average AQI by Monitoring Station",
        x = "Week",
        y = "Weekly Average AQI",
        colour = "Station"
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.02, 0.25))) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 17),
        axis.title = element_text(face = "bold"),
        legend.position = "bottom"
      )
    
    ggplotly(p, tooltip = "text", source = "weekly_trend") %>%
      layout(
        legend = list(orientation = "h", x = 0.25, y = -0.25)
      )
  })
  
  # ---- Linked Interaction: Weekly Trend Click Toggles Station Filter ----
  observeEvent(
    event_data("plotly_click", source = "weekly_trend", priority = "event"),
    {
      click_data <- event_data(
        "plotly_click",
        source = "weekly_trend",
        priority = "event"
      )
      
      if (!is.null(click_data$customdata)) {
        update_station_selection(as.character(click_data$customdata))
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )
  
  # ---- Weekly Trend Insight Panel ----
  output$weekly_trend_insight <- renderUI({
    
    selected_station <- input$station_filter
    
    weekly_summary <- filtered_data() %>%
      mutate(
        week_start = lubridate::floor_date(date, unit = "week", week_start = 1)
      ) %>%
      group_by(station_name, week_start) %>%
      summarise(
        avg_aqi = mean(aqi_site, na.rm = TRUE),
        avg_pm25 = mean(pm25_24hr, na.rm = TRUE),
        avg_pm10 = mean(pm10_24hr, na.rm = TRUE),
        .groups = "drop"
      )
    
    peak_week <- weekly_summary %>%
      arrange(desc(avg_aqi)) %>%
      slice(1)
    
    if (is.null(selected_station) || selected_station == "All") {
      
      HTML(
        paste0(
          "<p><b>All stations are currently shown.</b></p>",
          
          "<p>The weekly trend shows that AQI values generally fluctuate between ",
          "moderate ranges, with a clear peak around late autumn to early winter 2025.</p>",
          
          "<p>The highest weekly average AQI in the selected data occurs at <b>",
          peak_week$station_name, "</b> during the week starting <b>",
          peak_week$week_start, "</b>, with an average AQI of <b>",
          round(peak_week$avg_aqi, 1), "</b>.</p>",
          
          "<p>This peak is also associated with elevated particulate values, including ",
          "weekly average PM2.5 of <b>", round(peak_week$avg_pm25, 1),
          "</b> and PM10 of <b>", round(peak_week$avg_pm10, 1), "</b>.</p>",
          
          "<p class='insight-note'>The trend view highlights timing of pollution events. ",
          "It does not by itself prove the cause of the peak, but it helps identify periods ",
          "that require closer interpretation with weather and particulate data.</p>"
        )
      )
      
    } else {
      
      HTML(
        paste0(
          "<p><b>", selected_station, " station is selected.</b></p>",
          
          "<p>For this station, the highest weekly average AQI occurs during the week starting <b>",
          peak_week$week_start, "</b>, with an average AQI of <b>",
          round(peak_week$avg_aqi, 1), "</b>.</p>",
          
          "<p>During that week, the average PM2.5 value is <b>",
          round(peak_week$avg_pm25, 1), "</b>, and the average PM10 value is <b>",
          round(peak_week$avg_pm10, 1), "</b>.</p>",
          
          "<p class='insight-note'>Click the same station line again to return to the ",
          "all-station comparison view.</p>"
        )
      )
    }
  })
  
  # ---- Weather Relationship Plot: Wind Speed vs PM2.5 ----
  output$wind_pm25_plot <- renderPlotly({
    
    scatter_data <- filtered_data() %>%
      filter(
        !is.na(wspd),
        !is.na(pm25_24hr),
        !is.na(station_name)
      )
    
    p <- ggplot(
      scatter_data,
      aes(
        x = wspd,
        y = pm25_24hr,
        colour = station_name,
        customdata = station_name,
        text = paste0(
          "Station: ", station_name,
          "<br>Date: ", date,
          "<br>Wind speed: ", round(wspd, 1),
          "<br>PM2.5: ", round(pm25_24hr, 1),
          "<br>AQI: ", round(aqi_site, 1),
          "<br>Season: ", season
        )
      )
    ) +
      geom_point(alpha = 0.42, size = 1.8) +
      geom_smooth(
        aes(group = station_name),
        method = "lm",
        se = FALSE,
        linewidth = 1.3,
        alpha = 0.95
      ) +
      scale_colour_manual(values = station_palette, drop = FALSE) +
      labs(
        title = "Wind Speed and PM2.5 across Monitoring Stations",
        x = "Wind Speed",
        y = "PM2.5 24-hour Concentration",
        colour = "Station"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 17),
        axis.title = element_text(face = "bold"),
        legend.position = "bottom"
      )
    
    ggplotly(p, tooltip = "text", source = "wind_pm25") %>%
      layout(
        legend = list(orientation = "h", x = 0.25, y = -0.25)
      )
  })
  
  # ---- Linked Interaction: Weather Scatter Click Toggles Station Filter ----
  observeEvent(
    event_data("plotly_click", source = "wind_pm25", priority = "event"),
    {
      click_data <- event_data(
        "plotly_click",
        source = "wind_pm25",
        priority = "event"
      )
      
      if (!is.null(click_data$customdata)) {
        update_station_selection(as.character(click_data$customdata))
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )
  
  # ---- Weather Insight Panel ----
  output$weather_insight <- renderUI({
    
    selected_station <- input$station_filter
    
    weather_data <- filtered_data() %>%
      filter(
        !is.na(wspd),
        !is.na(pm25_24hr)
      )
    
    # Correlation for the currently filtered data
    overall_cor <- cor(
      weather_data$wspd,
      weather_data$pm25_24hr,
      use = "complete.obs"
    )
    
    # Highest PM2.5 day in the current filtered data
    peak_pm25_day <- weather_data %>%
      arrange(desc(pm25_24hr)) %>%
      slice(1)
    
    # Average PM2.5 under low wind conditions
    low_wind_summary <- weather_data %>%
      mutate(
        wind_group = if_else(
          wspd <= median(wspd, na.rm = TRUE),
          "Lower wind days",
          "Higher wind days"
        )
      ) %>%
      group_by(wind_group) %>%
      summarise(
        avg_pm25 = mean(pm25_24hr, na.rm = TRUE),
        avg_aqi = mean(aqi_site, na.rm = TRUE),
        day_count = n(),
        .groups = "drop"
      )
    
    lower_wind <- low_wind_summary %>%
      filter(wind_group == "Lower wind days")
    
    higher_wind <- low_wind_summary %>%
      filter(wind_group == "Higher wind days")
    
    if (is.null(selected_station) || selected_station == "All") {
      
      HTML(
        paste0(
          "<p><b>All stations are currently shown.</b></p>",
          
          "<p>The scatterplot shows a generally negative relationship between ",
          "wind speed and PM2.5. The correlation in the selected data is <b>",
          round(overall_cor, 2), "</b>.</p>",
          
          "<p>Lower-wind days have an average PM2.5 value of <b>",
          round(lower_wind$avg_pm25, 1), "</b>, compared with <b>",
          round(higher_wind$avg_pm25, 1), "</b> on higher-wind days.</p>",
          
          "<p>The highest PM2.5 value in the selected data occurs at <b>",
          peak_pm25_day$station_name, "</b> on <b>",
          peak_pm25_day$date, "</b>, with PM2.5 of <b>",
          round(peak_pm25_day$pm25_24hr, 1), "</b>.</p>",
          
          "<p class='insight-note'>This supports the interpretation that calmer ",
          "conditions can be associated with higher particulate levels, although ",
          "wind speed is only one part of the air-quality system.</p>"
        )
      )
      
    } else {
      
      HTML(
        paste0(
          "<p><b>", selected_station, " station is selected.</b></p>",
          
          "<p>For this station, the wind speed and PM2.5 correlation is <b>",
          round(overall_cor, 2), "</b>.</p>",
          
          "<p>Lower-wind days have an average PM2.5 value of <b>",
          round(lower_wind$avg_pm25, 1), "</b>, compared with <b>",
          round(higher_wind$avg_pm25, 1), "</b> on higher-wind days.</p>",
          
          "<p>The highest PM2.5 value for this selection occurs on <b>",
          peak_pm25_day$date, "</b>, with PM2.5 of <b>",
          round(peak_pm25_day$pm25_24hr, 1), "</b> and AQI of <b>",
          round(peak_pm25_day$aqi_site, 1), "</b>.</p>",
          
          "<p class='insight-note'>Click the same station point again to return ",
          "to the all-station comparison view.</p>"
        )
      )
    }
  })
  
  # ---- PCA Cluster Scatterplot ----
  output$pca_cluster_plot <- renderPlotly({
    
    pca_filtered <- pca_data %>%
      filter(
        date >= input$date_filter[1],
        date <= input$date_filter[2]
      )
    
    if (!is.null(input$station_filter) && input$station_filter != "All") {
      pca_filtered <- pca_filtered %>%
        filter(station_name == input$station_filter)
    }
    
    if (!is.null(input$season_filter) && input$season_filter != "All") {
      pca_filtered <- pca_filtered %>%
        filter(season == input$season_filter)
    }
    
    p <- ggplot(
      pca_filtered,
      aes(
        x = PC1,
        y = PC2,
        colour = cluster_name,
        customdata = station_name,
        text = paste0(
          "Station: ", station_name,
          "<br>Date: ", date,
          "<br>Cluster: ", cluster_name,
          "<br>AQI: ", round(aqi_site, 1),
          "<br>PM2.5: ", round(pm25_24hr, 1),
          "<br>PM10: ", round(pm10_24hr, 1),
          "<br>O3: ", round(o3_8hr, 3),
          "<br>Wind speed: ", round(wspd, 1),
          "<br>Temperature: ", round(tavg, 1)
        )
      )
    ) +
      geom_point(alpha = 0.55, size = 2.1) +
      scale_colour_manual(values = cluster_palette, drop = FALSE) +
      labs(
        title = "PCA of Daily Air Quality and Weather Conditions",
        x = paste0("Pollution Intensity Pattern - PC1 (", round(pca_variance[1], 1), "% variance)"),
        y = paste0("Weather and Dispersion Pattern - PC2 (", round(pca_variance[2], 1), "% variance)"),
        colour = "Cluster"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 15),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10),
        legend.position = "right"
      )
    
    ggplotly(p, tooltip = "text", source = "pca_cluster") %>%
      layout(
        legend = list(
          orientation = "v",
          x = 1.02,
          y = 0.9
        ),
        margin = list(r = 170, b = 70)
      )
  })
  
  # ---- Linked Interaction: PCA Point Click Toggles Station Filter ----
  observeEvent(
    event_data("plotly_click", source = "pca_cluster", priority = "event"),
    {
      click_data <- event_data(
        "plotly_click",
        source = "pca_cluster",
        priority = "event"
      )
      
      if (!is.null(click_data$customdata)) {
        update_station_selection(as.character(click_data$customdata))
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )
  
  # ---- PCA and Cluster Insight Panel ----
  output$pca_cluster_insight <- renderUI({
    
    selected_station <- input$station_filter
    
    pca_filtered <- pca_data %>%
      filter(
        date >= input$date_filter[1],
        date <= input$date_filter[2]
      )
    
    if (!is.null(selected_station) && selected_station != "All") {
      pca_filtered <- pca_filtered %>%
        filter(station_name == selected_station)
    }
    
    if (!is.null(input$season_filter) && input$season_filter != "All") {
      pca_filtered <- pca_filtered %>%
        filter(season == input$season_filter)
    }
    
    cluster_summary <- pca_filtered %>%
      group_by(cluster_name) %>%
      summarise(
        day_count = n(),
        avg_aqi = mean(aqi_site, na.rm = TRUE),
        avg_pm25 = mean(pm25_24hr, na.rm = TRUE),
        avg_pm10 = mean(pm10_24hr, na.rm = TRUE),
        avg_wind = mean(wspd, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(day_count))
    
    dominant_cluster <- cluster_summary %>%
      slice(1)
    
    extreme_cluster <- cluster_summary %>%
      filter(cluster_name == "Rare Extreme Particulate")
    
    extreme_days <- ifelse(
      nrow(extreme_cluster) == 0,
      0,
      extreme_cluster$day_count
    )
    
    if (is.null(selected_station) || selected_station == "All") {
      
      HTML(
        paste0(
          "<p><b>All stations are currently shown.</b></p>",
          
          "<p>PC1 explains <b>", round(pca_variance[1], 1),
          "%</b> of the variation and PC2 explains <b>",
          round(pca_variance[2], 1), "%</b>. Together, these two dimensions ",
          "summarise the main air-quality and weather differences in the dataset.</p>",
          
          "<p>The most common pattern in the current selection is <b>",
          dominant_cluster$cluster_name, "</b>, with <b>",
          dominant_cluster$day_count, "</b> days.</p>",
          
          "<p>The <b>Rare Extreme Particulate</b> cluster contains <b>",
          extreme_days, "</b> days in the current selection. These days represent ",
          "higher AQI and particulate conditions compared with the typical cluster.</p>",
          
          "<p class='insight-note'>PCA reduces several variables into two dimensions, ",
          "while k-means clustering groups days with similar profiles. The clusters should ",
          "be interpreted as descriptive patterns rather than fixed pollution categories.</p>"
        )
      )
      
    } else {
      
      HTML(
        paste0(
          "<p><b>", selected_station, " station is selected.</b></p>",
          
          "<p>For this station and filter selection, the dominant cluster is <b>",
          dominant_cluster$cluster_name, "</b>, containing <b>",
          dominant_cluster$day_count, "</b> days.</p>",
          
          "<p>This dominant cluster has an average AQI of <b>",
          round(dominant_cluster$avg_aqi, 1), "</b>, average PM2.5 of <b>",
          round(dominant_cluster$avg_pm25, 1), "</b>, average PM10 of <b>",
          round(dominant_cluster$avg_pm10, 1), "</b>, and average wind speed of <b>",
          round(dominant_cluster$avg_wind, 1), "</b>.</p>",
          
          "<p>The Rare Extreme Particulate cluster contains <b>",
          extreme_days, "</b> days for this selection.</p>",
          
          "<p class='insight-note'>Click the same station point again to return ",
          "to the all-station PCA comparison view.</p>"
        )
      )
    }
  })
  
  # ---- Cluster Profile Comparison Plot ----
  output$cluster_profile_plot <- renderPlotly({
    
    pca_filtered <- pca_data %>%
      filter(
        date >= input$date_filter[1],
        date <= input$date_filter[2]
      )
    
    if (!is.null(input$station_filter) && input$station_filter != "All") {
      pca_filtered <- pca_filtered %>%
        filter(station_name == input$station_filter)
    }
    
    if (!is.null(input$season_filter) && input$season_filter != "All") {
      pca_filtered <- pca_filtered %>%
        filter(season == input$season_filter)
    }
    
    profile_data <- pca_filtered %>%
      select(
        cluster_name,
        aqi_site,
        pm25_24hr,
        pm10_24hr,
        o3_8hr,
        tavg,
        wspd,
        pres
      ) %>%
      pivot_longer(
        cols = c(aqi_site, pm25_24hr, pm10_24hr, o3_8hr, tavg, wspd, pres),
        names_to = "variable",
        values_to = "value"
      ) %>%
      group_by(variable) %>%
      mutate(
        standardised_value = as.numeric(scale(value))
      ) %>%
      ungroup() %>%
      group_by(cluster_name, variable) %>%
      summarise(
        avg_standardised_value = mean(standardised_value, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        variable = factor(
          variable,
          levels = c(
            "aqi_site",
            "pm25_24hr",
            "pm10_24hr",
            "o3_8hr",
            "tavg",
            "wspd",
            "pres"
          ),
          labels = c(
            "AQI",
            "PM2.5",
            "PM10",
            "Ozone O3",
            "Temperature",
            "Wind speed",
            "Pressure"
          )
        )
      )
    
    p <- ggplot(
      profile_data,
      aes(
        x = variable,
        y = avg_standardised_value,
        colour = cluster_name,
        group = cluster_name,
        text = paste0(
          "Cluster: ", cluster_name,
          "<br>Variable: ", variable,
          "<br>Average standardised value: ",
          round(avg_standardised_value, 2)
        )
      )
    ) +
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        linewidth = 0.5,
        colour = "#64748b"
      ) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2.4) +
      scale_colour_manual(values = cluster_palette, drop = FALSE) +
      labs(
        title = "Cluster Profiles of Standardised Air Quality and Weather Variables",
        x = "Variable",
        y = "Mean Standardised Value",
        colour = "Cluster"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 25, hjust = 1),
        legend.position = "bottom"
      )
    
    ggplotly(p, tooltip = "text", source = "cluster_profile") %>%
      layout(
        legend = list(orientation = "h", x = 0.05, y = -0.25),
        margin = list(b = 95)
      )
  })
  
  # ---- Sankey Final Flow Summary ----
  output$season_wind_aqi_sankey <- renderSankeyNetwork({
    
    sankey_data <- filtered_data() %>%
      filter(
        !is.na(season),
        !is.na(wspd),
        !is.na(aqi_band)
      ) %>%
      mutate(
        wind_condition = case_when(
          wspd <= quantile(wspd, 1/3, na.rm = TRUE) ~ "Low wind",
          wspd <= quantile(wspd, 2/3, na.rm = TRUE) ~ "Normal wind",
          TRUE ~ "High wind"
        ),
        
        # Rename Fair to Moderate for audience-friendly Sankey labelling
        aqi_display = case_when(
          aqi_band == "Fair" ~ "Moderate",
          TRUE ~ as.character(aqi_band)
        ),
        
        season_node = as.character(season),
        wind_node = wind_condition,
        aqi_node = aqi_display
      )
    
    # ---- Links: Season to Wind ----
    links_season_wind <- sankey_data %>%
      count(season_node, wind_node, name = "value") %>%
      rename(source = season_node, target = wind_node)
    
    # ---- Links: Wind to AQI Band ----
    links_wind_aqi <- sankey_data %>%
      count(wind_node, aqi_node, name = "value") %>%
      rename(source = wind_node, target = aqi_node)
    
    links <- bind_rows(
      links_season_wind,
      links_wind_aqi
    )
    
    # ---- Node order similar to Sheet 5 sketch ----
    node_order <- c(
      "Summer", "Autumn", "Winter", "Spring",
      "Low wind", "Normal wind", "High wind",
      "Good", "Moderate", "Poor", "Very Poor", "Hazardous"
    )
    
    nodes <- data.frame(
      name = node_order[node_order %in% unique(c(links$source, links$target))]
    ) %>%
      mutate(
        group = case_when(
          name %in% c("Summer", "Autumn", "Winter", "Spring") ~ "Season",
          name %in% c("Low wind", "Normal wind", "High wind") ~ "Wind",
          name == "Good" ~ "Good",
          name == "Moderate" ~ "Moderate",
          name == "Poor" ~ "Poor",
          name == "Very Poor" ~ "Very Poor",
          name == "Hazardous" ~ "Hazardous",
          TRUE ~ "Other"
        )
      )
    
    links <- links %>%
      mutate(
        IDsource = match(source, nodes$name) - 1,
        IDtarget = match(target, nodes$name) - 1,
        
        link_group = case_when(
          target == "Good" ~ "Good",
          target == "Moderate" ~ "Moderate",
          target == "Poor" ~ "Poor",
          target == "Very Poor" ~ "Very Poor",
          target == "Hazardous" ~ "Hazardous",
          target %in% c("Low wind", "Normal wind", "High wind") ~ "WindLink",
          TRUE ~ "Other"
        )
      )
    
    # ---- Colour palette inspired by your sketch ----
    colour_scale <- networkD3::JS(
      'd3.scaleOrdinal()
        .domain([
          "Season", "Wind", "Good", "Moderate", "Poor", "Very Poor", "Hazardous",
          "WindLink"
        ])
        .range([
          "#64748B",  /* season nodes */
          "#475569",  /* wind nodes */
          "#22C55E",  /* good */
          "#FACC15",  /* moderate */
          "#FB923C",  /* poor */
          "#EF4444",  /* very poor */
          "#7F1D1D",  /* hazardous */
          "#94A3B8"   /* season-to-wind links */
        ])'
    )
    
    sankey <- sankeyNetwork(
      Links = links,
      Nodes = nodes,
      Source = "IDsource",
      Target = "IDtarget",
      Value = "value",
      NodeID = "name",
      NodeGroup = "group",
      LinkGroup = "link_group",
      colourScale = colour_scale,
      fontSize = 15,
      nodeWidth = 22,
      nodePadding = 26,
      sinksRight = FALSE
    )
    
    htmlwidgets::onRender(
      sankey,
      '
      function(el, x) {
        
        // Make links softer and more polished
        d3.select(el).selectAll(".link")
          .style("stroke-opacity", 0.35)
          .style("mix-blend-mode", "multiply");
        
        d3.select(el).selectAll(".link")
          .on("mouseover", function() {
            d3.select(this)
              .style("stroke-opacity", 0.70);
          })
          .on("mouseout", function() {
            d3.select(this)
              .style("stroke-opacity", 0.35);
          });
        
        // Rounded-looking, cleaner node styling
        d3.select(el).selectAll(".node rect")
          .style("stroke", "#111827")
          .style("stroke-width", "0.8px")
          .style("rx", "5px")
          .style("ry", "5px");
        
        // Professional label styling
        d3.select(el).selectAll(".node text")
          .style("font-family", "Inter, Arial, sans-serif")
          .style("font-size", "14px")
          .style("font-weight", "700")
          .style("fill", "#0F172A");
      }
      '
    )
  })
  
  # ---- Sankey Flow Insight Panel ----
  output$sankey_insight <- renderUI({
    
    sankey_data <- filtered_data() %>%
      filter(
        !is.na(season),
        !is.na(wspd),
        !is.na(aqi_band)
      ) %>%
      mutate(
        wind_condition = case_when(
          wspd <= quantile(wspd, 1/3, na.rm = TRUE) ~ "Low wind",
          wspd <= quantile(wspd, 2/3, na.rm = TRUE) ~ "Normal wind",
          TRUE ~ "High wind"
        ),
        aqi_display = case_when(
          aqi_band == "Fair" ~ "Moderate",
          TRUE ~ as.character(aqi_band)
        )
      )
    
    selected_station <- input$station_filter
    selected_season <- input$season_filter
    
    filter_context <- case_when(
      selected_station != "All" && selected_season != "All" ~ paste0(
        "<p><b>", selected_station, " station and ", selected_season,
        " season are selected.</b></p>"
      ),
      selected_station != "All" && selected_season == "All" ~ paste0(
        "<p><b>", selected_station, " station is selected. All seasons are currently shown.</b></p>"
      ),
      selected_station == "All" && selected_season != "All" ~ paste0(
        "<p><b>All stations are shown for ", selected_season, " season.</b></p>"
      ),
      TRUE ~ "<p><b>All stations and all seasons are currently shown.</b></p>"
    )
    
    dominant_season <- sankey_data %>%
      count(season, name = "records") %>%
      arrange(desc(records)) %>%
      slice(1)
    
    dominant_wind <- sankey_data %>%
      count(wind_condition, name = "records") %>%
      arrange(desc(records)) %>%
      slice(1)
    
    dominant_aqi <- sankey_data %>%
      count(aqi_display, name = "records") %>%
      arrange(desc(records)) %>%
      slice(1)
    
    poor_or_worse <- sankey_data %>%
      filter(aqi_display %in% c("Poor", "Very Poor", "Hazardous")) %>%
      nrow()
    
    total_records <- nrow(sankey_data)
    
    low_wind_poor <- sankey_data %>%
      filter(
        wind_condition == "Low wind",
        aqi_display %in% c("Poor", "Very Poor", "Hazardous")
      ) %>%
      nrow()
    
    HTML(
      paste0(
        filter_context,
        "<p><b>The flow summary is based on ",
        comma(total_records), " daily station records in the current selection.</b></p>",
        
        "<p>The largest seasonal contribution comes from <b>",
        dominant_season$season, "</b>, while the most common wind condition is <b>",
        dominant_wind$wind_condition, "</b>.</p>",
        
        "<p>The most frequent AQI severity outcome is <b>",
        dominant_aqi$aqi_display, "</b>, with <b>",
        comma(dominant_aqi$records), "</b> records.</p>",
        
        "<p>Across the current selection, <b>",
        comma(poor_or_worse), "</b> records fall into Poor, Very Poor or Hazardous ",
        "air-quality categories.</p>",
        
        "<p>Of these poorer-air-quality records, <b>",
        comma(low_wind_poor), "</b> occur under low-wind conditions.</p>",
        
        "<p class='insight-note'>The Sankey diagram is intended as a final narrative overview. ",
        "It shows associations between season, wind condition and AQI severity, but it should not ",
        "be interpreted as proof of causation.</p>"
      )
    )
  })
  
  # ---- Final Takeaway Cards ----
  output$summary_station_season <- renderUI({
    
    station_peak <- station_summary %>%
      arrange(desc(avg_aqi)) %>%
      slice(1)
    
    seasonal_peak <- air_data %>%
      group_by(station_name, season) %>%
      summarise(
        avg_aqi = mean(aqi_site, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(avg_aqi)) %>%
      slice(1)
    
    HTML(
      paste0(
        "<p>The dashboard shows that AQI varies by both monitoring station and season. ",
        "<b>", station_peak$station_name, "</b> has the highest overall average AQI of <b>",
        round(station_peak$avg_aqi, 1), "</b>.</p>",
        
        "<p>The strongest seasonal pattern occurs at <b>",
        seasonal_peak$station_name, "</b> during <b>",
        seasonal_peak$season, "</b>, with an average AQI of <b>",
        round(seasonal_peak$avg_aqi, 1), "</b>.</p>"
      )
    )
  })
  
  
  output$summary_weather_time <- renderUI({
    
    weekly_summary <- air_data %>%
      mutate(
        week_start = lubridate::floor_date(date, unit = "week", week_start = 1)
      ) %>%
      group_by(station_name, week_start) %>%
      summarise(
        avg_aqi = mean(aqi_site, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(avg_aqi)) %>%
      slice(1)
    
    weather_data <- air_data %>%
      filter(
        !is.na(wspd),
        !is.na(pm25_24hr)
      )
    
    wind_pm25_cor <- cor(
      weather_data$wspd,
      weather_data$pm25_24hr,
      use = "complete.obs"
    )
    
    HTML(
      paste0(
        "<p>The clearest temporal signal is the late-autumn to early-winter pollution peak. ",
        "The highest weekly average AQI occurs at <b>",
        weekly_summary$station_name, "</b> during the week starting <b>",
        weekly_summary$week_start, "</b>.</p>",
        
        "<p>Weather also matters: wind speed and PM2.5 show a negative relationship, ",
        "with correlation <b>", round(wind_pm25_cor, 2),
        "</b>. This supports the interpretation that calmer conditions are associated ",
        "with higher particulate levels.</p>"
      )
    )
  })
  
  
  output$summary_cluster_pattern <- renderUI({
    
    cluster_summary <- pca_data %>%
      group_by(cluster_name) %>%
      summarise(
        day_count = n(),
        avg_aqi = mean(aqi_site, na.rm = TRUE),
        avg_pm25 = mean(pm25_24hr, na.rm = TRUE),
        avg_pm10 = mean(pm10_24hr, na.rm = TRUE),
        .groups = "drop"
      )
    
    common_cluster <- cluster_summary %>%
      arrange(desc(day_count)) %>%
      slice(1)
    
    extreme_cluster <- cluster_summary %>%
      filter(cluster_name == "Rare Extreme Particulate")
    
    HTML(
      paste0(
        "<p>The PCA and clustering section shows that most days belong to the <b>",
        common_cluster$cluster_name, "</b> pattern, with <b>",
        comma(common_cluster$day_count), "</b> records.</p>",
        
        "<p>A smaller but important group is the <b>Rare Extreme Particulate</b> cluster, ",
        "with <b>", comma(extreme_cluster$day_count), "</b> records and much higher average ",
        "AQI, PM2.5 and PM10 levels.</p>"
      )
    )
  })
  
  
  output$summary_takeaway <- renderUI({
    
    HTML(
      paste0(
        "<p style='color:#ffffff; margin-bottom:16px;'>This dashboard shows that Canberra's air quality patterns are shaped by a ",
        "combination of <b>station location</b>, <b>seasonal timing</b>, <b>weather conditions</b>, ",
        "and <b>broader multivariate pollution profiles</b>. Monash and Florey tend to show ",
        "higher pollution levels than Civic during several elevated periods, while low-wind ",
        "conditions are associated with higher particulate matter.</p>",
        
        "<p style='color:#ffffff; margin-bottom:0;'>The final message is that poor-air-quality events should not be interpreted ",
        "from a single factor alone. A narrative dashboard combining temporal, spatial, weather, ",
        "and cluster-based evidence provides a more complete picture for environmental monitoring ",
        "and public communication.</p>"
      )
    )
  })
}