# ============================================================
# ui.R
# Defines the layout and visual structure of the Shiny app.
# ============================================================

ui <- fluidPage(
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  # ---- Hero Header ----
  div(
    class = "hero-section",
    h1("Canberra Air Quality Explorer"),
    p(
      "An interactive narrative visualisation of air pollution patterns, ",
      "weather conditions, and industrial emission context across Canberra ",
      "monitoring stations from 2025 to 2026."
    ),
    
    div(
      class = "interaction-guide",
      strong("How to use this dashboard: "),
      span(
        "Use the sidebar filters to change the view. Click station marks in the charts or map ",
        "to focus on one monitoring station; click the same station again to return to all stations. ",
        "Hover over visual elements for detailed values."
      )
    )
  ),
  
  # ---- Main Layout ----
  fluidRow(
    
    # ---- Sidebar Controls ----
    column(
      width = 3,
      
      div(
        class = "control-panel",
        
        h4("Explore the Data"),
        p(
          class = "small-muted",
          "Use these controls to filter the dashboard by station, season, pollutant and date range."
        ),
        
        pickerInput(
          inputId = "station_filter",
          label = "Monitoring station",
          choices = station_choices,
          selected = "All"
        ),
        
        pickerInput(
          inputId = "season_filter",
          label = "Season",
          choices = season_choices,
          selected = "All"
        ),
        
        pickerInput(
          inputId = "pollutant_filter",
          label = "Main pollutant / metric",
          choices = pollutant_choices,
          selected = "aqi_site"
        ),
        
        dateRangeInput(
          inputId = "date_filter",
          label = "Date range",
          start = date_min,
          end = date_max,
          min = date_min,
          max = date_max
        ),
        
        radioButtons(
          inputId = "radius_filter",
          label = "NPI facility radius",
          choices = c("5 km" = 5, "10 km" = 10),
          selected = 10
        ),
        
        actionButton(
          inputId = "reset_filters",
          label = "Reset filters",
          class = "btn-primary"
        ),
        
        br(),
        
        hr(),
        
        div(
          class = "data-scope-card",
          
          h4("Data and Scope"),
          
          p(
            "This dashboard uses daily Canberra air-quality records, weather observations, ",
            "and National Pollutant Inventory facility context for 2025–2026."
          ),
          
          tags$ul(
            tags$li("ACT Air Quality Monitoring Data"),
            tags$li("Meteostat Canberra Airport weather data"),
            tags$li("National Pollutant Inventory facility locations")
          ),
          
          p(
            class = "small-muted",
            "The visualisation is descriptive and exploratory. Spatial proximity and weather ",
            "patterns are shown as contextual associations, not proof of causation."
          )
        )
      )
    ),
    
    # ---- Main Content ----
    column(
      width = 9,
      
      # ---- KPI Cards ----
      fluidRow(
        column(
          width = 3,
          div(
            class = "kpi-card",
            h5(textOutput("avg_metric_label")),
            textOutput("avg_aqi")
          )
        ),
        column(
          width = 3,
          div(
            class = "kpi-card",
            h5(textOutput("max_metric_label")),
            textOutput("max_aqi")
          )
        ),
        column(
          width = 3,
          div(
            class = "kpi-card",
            h5("Poor+ Days"),
            textOutput("poor_days")
          )
        ),
        column(
          width = 3,
          div(
            class = "kpi-card",
            h5("Records"),
            textOutput("record_count")
          )
        )
      ),
      
      br(),
      
      # ---- Seasonal Comparison Section ----
      div(
        class = "section-card",
        id = "seasonal-section",
        
        h2("1. Seasonal Comparison: Which station and season show higher AQI?"),
        p(
          "Compare seasonal AQI patterns across Canberra monitoring stations. ",
          "Click a station bar to focus the dashboard on that station; click the same ",
          "station again to return to the all-station view."
        ),
        
        plotlyOutput("preview_season_plot", height = "430px"),
        
        br(),
        
        h3("AQI Band Distribution"),
        p(
          "Explore how daily records are distributed across AQI severity bands. ",
          "This adds frequency context to the seasonal average AQI comparison."
        ),
        
        plotlyOutput("aqi_band_plot", height = "390px")
      ),
      
      # ---- Spatial Context Section ----
      div(
        class = "section-card",
        id = "spatial-section",
        
        h2("2. Spatial Context: Where are pollution patterns observed?"),
        p(
          "View each monitoring station in relation to nearby National Pollutant Inventory ",
          "facilities. Select a station or radius to explore local industrial-emission context."
        ),
        
        fluidRow(
          column(
            width = 8,
            leafletOutput("station_npi_map", height = "520px")
          ),
          
          column(
            width = 4,
            div(
              class = "insight-box",
              h4("Spatial Insight"),
              htmlOutput("map_insight")
            )
          )
        )
      ),
      
      # ---- Weekly AQI Trend Section ----
      div(
        class = "section-card",
        id = "weekly-trend-section",
        
        h2("3. Weekly AQI Trend: When do pollution peaks occur?"),
        p(
          "Track weekly AQI changes and identify peak pollution periods. Hover for weekly ",
          "values, or click a station line to link the whole dashboard to that station."
        ),
        
        fluidRow(
          column(
            width = 8,
            plotlyOutput("weekly_aqi_plot", height = "460px")
          ),
          
          column(
            width = 4,
            div(
              class = "insight-box compact-insight",
              h4("Trend Insight"),
              htmlOutput("weekly_trend_insight")
            )
          )
        )
      ),
      
      # ---- Weather Relationship Section ----
      div(
        class = "section-card",
        id = "weather-section",
        
        h2("4. Weather Relationship: How does wind speed relate to PM2.5?"),
        p(
          "Explore how wind speed relates to PM2.5 levels. The trend lines help show whether ",
          "calmer conditions are associated with higher particulate concentrations."
        ),
        
        fluidRow(
          column(
            width = 8,
            plotlyOutput("wind_pm25_plot", height = "460px")
          ),
          
          column(
            width = 4,
            div(
              class = "insight-box compact-insight",
              h4("Weather Insight"),
              htmlOutput("weather_insight")
            )
          )
        )
      ),
      
      # ---- PCA Cluster Explorer Section ----
      div(
        class = "section-card",
        id = "pca-section",
        
        h2("5. PCA Cluster Explorer: What broader air-quality patterns emerge?"),
        p(
          "Use Principal Component Analysis to summarise multiple air-quality and weather ",
          "variables, then compare clusters of days with similar environmental profiles."
        ),
        
        fluidRow(
          column(
            width = 8,
            plotlyOutput("pca_cluster_plot", height = "500px")
          ),
          
          column(
            width = 4,
            div(
              class = "insight-box compact-insight",
              h4("PCA and Cluster Insight"),
              htmlOutput("pca_cluster_insight")
            )
          )
        ),
        
        br(),
        
        h3("Cluster Profile Comparison"),
        p(
          "Compare standardised air-quality and weather variables across clusters. ",
          "This explains why each cluster represents a different type of daily condition."
        ),
        
        plotlyOutput("cluster_profile_plot", height = "420px"),
        
        p(
          class = "small-muted",
          "Note: Values are standardised, so 0 represents the average level for each variable. ",
          "Positive values indicate above-average levels and negative values indicate below-average levels."
        )
      ),
      
      # ---- Sankey Final Summary Section ----
      div(
        class = "section-card",
        id = "sankey-section",
        
        h2("6. Final Flow Summary: How do season and wind conditions connect to AQI severity?"),
        p(
          "Follow the final flow from season, to wind condition, to AQI severity. ",
          "This summarises how seasonal context and dispersion conditions align with ",
          "daily air-quality outcomes."
        ),
        
        fluidRow(
          column(
            width = 8,
            sankeyNetworkOutput("season_wind_aqi_sankey", height = "520px")
          ),
          
          column(
            width = 4,
            div(
              class = "insight-box compact-insight",
              h4("Flow Insight"),
              htmlOutput("sankey_insight")
            )
          )
        ),
        
        br(),
        
        # ---- Overall Dashboard Takeaways ----
        div(
          class = "takeaway-section",
          style = "margin-top: 32px;",
          
          h3(
            "Overall Dashboard Takeaways",
            style = "font-weight: 800; color: #0b2f4f; margin-bottom: 22px;"
          ),
          
          fluidRow(
            column(
              width = 4,
              div(
                class = "summary-card",
                style = "
                  background: #ffffff;
                  border-radius: 18px;
                  padding: 24px;
                  min-height: 230px;
                  box-shadow: 0 12px 30px rgba(15, 23, 42, 0.10);
                  border-left: 6px solid #0f766e;
                  margin-bottom: 20px;
                ",
                h4(
                  "Spatial and Seasonal Pattern",
                  style = "font-weight: 800; color: #0b2f4f; margin-bottom: 14px;"
                ),
                htmlOutput("summary_station_season")
              )
            ),
            
            column(
              width = 4,
              div(
                class = "summary-card",
                style = "
                  background: #ffffff;
                  border-radius: 18px;
                  padding: 24px;
                  min-height: 230px;
                  box-shadow: 0 12px 30px rgba(15, 23, 42, 0.10);
                  border-left: 6px solid #0f766e;
                  margin-bottom: 20px;
                ",
                h4(
                  "Time and Weather Pattern",
                  style = "font-weight: 800; color: #0b2f4f; margin-bottom: 14px;"
                ),
                htmlOutput("summary_weather_time")
              )
            ),
            
            column(
              width = 4,
              div(
                class = "summary-card",
                style = "
                  background: #ffffff;
                  border-radius: 18px;
                  padding: 24px;
                  min-height: 230px;
                  box-shadow: 0 12px 30px rgba(15, 23, 42, 0.10);
                  border-left: 6px solid #0f766e;
                  margin-bottom: 20px;
                ",
                h4(
                  "Multivariate Pattern",
                  style = "font-weight: 800; color: #0b2f4f; margin-bottom: 14px;"
                ),
                htmlOutput("summary_cluster_pattern")
              )
            )
          ),
          
          br(),
          
          div(
            class = "final-takeaway",
            style = "
              background: linear-gradient(135deg, #0f766e, #164e63);
              color: #ffffff;
              border-radius: 22px;
              padding: 28px 32px;
              box-shadow: 0 14px 34px rgba(15, 23, 42, 0.14);
            ",
            h3(
              "Final Interpretation",
              style = "color: #ffffff; font-weight: 800; margin-bottom: 14px;"
            ),
            div(
              style = "color: #ffffff; font-size: 1.05rem; line-height: 1.65;",
              htmlOutput("summary_takeaway")
            )
          )
        )
      )
    )
  )
)