required_packages <- c("shiny", "bslib", "ggplot2", "dplyr")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

# ---- 1. Data ----
health_data <- data.frame(
  Country = rep(c("United States", "United Kingdom", "Germany"), each = 6),
  Sex = rep(rep(c("Male", "Female"), each = 3), 3),
  Year = rep(c("2000", "2012", "2022"), 6),
  Tobacco_Prevalence = c(
    33.8, 24.3, 23.0,
    27.6, 19.4, 15.0,
    36.8, 25.1, 18.0,
    34.0, 22.0, 14.8,
    41.5, 34.2, 27.8,
    29.5, 23.7, 18.8
  ),
  COPD_Positive = c(
    6.1, 6.5, 6.6,
    6.0, 6.8, 7.2,
    5.8, 6.0, 5.8,
    4.7, 5.5, 5.9,
    4.5, 4.3, 3.9,
    2.6, 2.7, 2.7
  )
)

# ---- 2. UI ----
ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bg = "#f8f9fa",
    fg = "#1a1a2e",
    primary = "#16213e",
    secondary = "#0f3460",
    success = "#2ecc71",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),

  tags$head(tags$style("
    body { background-color: #f0f2f5 !important; }

    .app-header {
      background: linear-gradient(135deg, #16213e 0%, #0f3460 60%, #533483 100%);
      color: white;
      padding: 28px 36px 22px 36px;
      margin-bottom: 24px;
      border-radius: 0 0 18px 18px;
      box-shadow: 0 4px 20px rgba(0,0,0,0.15);
    }
    .app-header h1 { font-size: 1.7rem; font-weight: 700; margin: 0; letter-spacing: -0.3px; }
    .app-header p  { font-size: 0.88rem; margin: 6px 0 0 0; opacity: 0.8; }

    .filter-card {
      background: white;
      border-radius: 14px;
      padding: 24px;
      box-shadow: 0 2px 12px rgba(0,0,0,0.07);
      border: none !important;
      height: 100%;
    }
    .filter-title {
      font-size: 0.7rem;
      font-weight: 700;
      text-transform: uppercase;
      letter-spacing: 1.2px;
      color: #888;
      margin-bottom: 18px;
    }
    .filter-card .form-label { font-weight: 600; font-size: 0.85rem; color: #333; }
    .filter-card .form-select, .filter-card .form-control {
      border-radius: 8px;
      border: 1.5px solid #e0e0e0;
      font-size: 0.9rem;
    }
    .filter-card .form-check-label { font-size: 0.9rem; }

    .kpi-card {
      background: white;
      border-radius: 14px;
      padding: 20px 24px;
      box-shadow: 0 2px 12px rgba(0,0,0,0.07);
      border-left: 5px solid #0f3460;
      margin-bottom: 0;
    }
    .kpi-card.copd { border-left-color: #e74c3c; }
    .kpi-label { font-size: 0.72rem; font-weight: 700; text-transform: uppercase;
                 letter-spacing: 1px; color: #999; margin-bottom: 4px; }
    .kpi-value { font-size: 2.2rem; font-weight: 800; color: #1a1a2e; line-height: 1; }
    .kpi-unit  { font-size: 0.85rem; color: #888; margin-top: 4px; }

    .chart-card {
      background: white;
      border-radius: 14px;
      padding: 24px;
      box-shadow: 0 2px 12px rgba(0,0,0,0.07);
    }
    .chart-title { font-size: 1rem; font-weight: 700; color: #1a1a2e; margin-bottom: 4px; }
    .chart-subtitle { font-size: 0.8rem; color: #999; margin-bottom: 16px; }

    .disclaimer {
      font-size: 0.78rem; color: #aaa; text-align: center;
      padding: 16px 0 8px 0;
    }
    .selectize-input { border-radius: 8px !important; }
  ")),

  # ---- Header ----
  div(class = "app-header",
    h1("\U0001F6AC Tobacco Use & COPD Tracker"),
    p("Comparing historical tobacco usage and respiratory health across three nations.")
  ),

  # ---- Main layout ----
  layout_columns(
    col_widths = c(3, 9),
    gap = "20px",

    # Left: Filters
    div(class = "filter-card",
      div(class = "filter-title", "Filters"),
      selectInput("country_input", "Country",
        choices = c("United States", "United Kingdom", "Germany"),
        selected = "United States"
      ),
      tags$div(style = "margin-top: 16px;",
        tags$label(class = "form-label", "Demographic"),
        radioButtons("sex_input", label = NULL,
          choices = c("Male", "Female"), selected = "Male", inline = TRUE
        )
      ),
      tags$div(style = "margin-top: 16px;",
        selectInput("year_input", "Year",
          choices = c("2000", "2012", "2022"), selected = "2022"
        )
      )
    ),

    div(
      # KPI row
      layout_columns(
        col_widths = c(6, 6),
        gap = "16px",
        div(class = "kpi-card",
          div(class = "kpi-label", "\U0001F6AC Tobacco Prevalence"),
          div(class = "kpi-value", textOutput("kpi_tobacco", inline = TRUE)),
          div(class = "kpi-unit", "% of population (age 15+)")
        ),
        div(class = "kpi-card copd",
          div(class = "kpi-label", "\U0001FAC1 COPD Prevalence"),
          div(class = "kpi-value", textOutput("kpi_copd", inline = TRUE)),
          div(class = "kpi-unit", "% of population diagnosed")
        )
      ),

      # Chart
      div(style = "margin-top: 20px;", class = "chart-card",
        div(class = "chart-title", textOutput("chart_title", inline = TRUE)),
        div(class = "chart-subtitle",
          "Bars = tobacco use. Dashed line = COPD prevalence for selected country."
        ),
        plotOutput("comparePlot", height = "280px")
      )
    )
  ),

  tags$div(class = "disclaimer", style = "padding: 16px 0 8px 0; text-align: center; font-size: 0.78rem; color: #aaa;",
    "Data shown is for educational purposes only and tracks population-level trends, not individual risk."
  )
)

# ---- 3. Server ----
server <- function(input, output, session) {

  selected_data <- reactive({
    health_data %>%
      filter(Country == input$country_input,
             Sex == input$sex_input,
             Year == input$year_input)
  })

  output$kpi_tobacco <- renderText({
    req(nrow(selected_data()) > 0)
    paste0(selected_data()$Tobacco_Prevalence, "%")
  })

  output$kpi_copd <- renderText({
    req(nrow(selected_data()) > 0)
    paste0(selected_data()$COPD_Positive, "%")
  })

  output$chart_title <- renderText({
    paste0("Country Comparison \u2014 ", input$year_input, " (", input$sex_input, ")")
  })

  output$comparePlot <- renderPlot({
    plot_df <- health_data %>%
      filter(Sex == input$sex_input, Year == input$year_input) %>%
      mutate(is_selected = (Country == input$country_input))

    current_copd <- selected_data()$COPD_Positive

    ggplot(plot_df, aes(x = Country, y = Tobacco_Prevalence, fill = is_selected)) +
      geom_col(width = 0.5, show.legend = FALSE) +
      geom_hline(yintercept = current_copd, linetype = "dashed",
                 linewidth = 0.9, color = "#e74c3c") +
      annotate("text", x = Inf, y = current_copd + 2,
               label = paste0("COPD: ", current_copd, "%"),
               hjust = 1.05, size = 3.5, color = "#e74c3c", fontface = "bold") +
      scale_fill_manual(values = c("TRUE" = "#0f3460", "FALSE" = "#a8c0d6")) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(x = NULL, y = "Tobacco Use Prevalence (%)") +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_line(color = "#f0f0f0"),
        axis.text.x  = element_text(size = 11, face = "bold", color = "#333"),
        axis.text.y  = element_text(size = 10, color = "#888"),
        axis.title.y = element_text(size = 10, color = "#888"),
        plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
  }, bg = "white")
}

shinyApp(ui = ui, server = server)
