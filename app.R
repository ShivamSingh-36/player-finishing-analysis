library(glue)
library(tidyverse)
library(ggsoccer)
library(TTR)
library(ggtext)
library(patchwork)
library(hexbin)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(ggbraid)
library(shinycssloaders)
library(logger) 

if(!file.exists("Final.csv")) {
  stop("DEPLOYMENT ERROR: Final.csv not found!")
}

csv_size <- file.size("Final.csv") / 1024^2
if(csv_size > 100) {
  warning("Large CSV (", round(csv_size, 1), "MB). May be slow.")
}

cat("✓ Loading Final.csv...\n")
log_info("App started. CSV: {round(csv_size, 1)}MB")

player_data_full <- read.csv("Final.csv", stringsAsFactors = FALSE)

required_columns <- c("Player.ID", "x", "y", "Shot.xG", "Shot.Result", 
                      "Shot.Type", "Situation", "Season", "Player")
missing_cols <- setdiff(required_columns, names(player_data_full))
if(length(missing_cols) > 0) {
  stop("CSV missing: ", paste(missing_cols, collapse=", "))
}
log_info("CSV validated. {nrow(player_data_full)} rows.")

player_data_full <- player_data_full %>%
  mutate(player_id_numeric = as.integer(sub(".*\\((\\d+)\\).*", "\\1", Player.ID)))

player_data_full <- player_data_full %>%
  group_by(Season, Player.ID) %>%
  arrange(Season, Player.ID) %>%
  mutate(
    shot_index = row_number(),
    total_shots = n(),
    days_into_season = (shot_index - 1) * (270 / max(1, total_shots - 1)), 
    date = as.Date(paste0(Season, "-08-01")) + days_into_season
  ) %>%
  ungroup() %>%
  select(-shot_index, -total_shots, -days_into_season)

if(any(is.na(player_data_full$date))) {
  warning("NA dates detected")
  player_data_full <- player_data_full %>% filter(!is.na(date))
}

player_data_full <- player_data_full %>%
  rename(
    X = x, Y = y, xG = Shot.xG,
    result = Shot.Result, shotType = Shot.Type,
    situation = Situation, year = Season,
    player_name = Player
  )

PITCH_LENGTH <- 120
PITCH_WIDTH <- 80
PITCH_HALF_START <- 60
PITCH_Y_MARGIN <- -2
PLOT_WIDTH <- 16
PLOT_HEIGHT <- 12
PLOT_DPI <- 600

MIN_SHOTS_FOR_SMOOTHING <- 10
MIN_ROLL_WINDOW <- 2

validate_download <- function(filtered_data, line_data, plot_selection) {
  if(nrow(filtered_data) == 0) {
    showNotification(
      "No data to download. Please adjust your filters.", 
      type = "error", 
      duration = 5
    )
    return(FALSE)
  }
  
  if(("Line Chart" %in% plot_selection || "All" %in% plot_selection) && nrow(line_data) == 0) {
    showNotification(
      "Insufficient data for line chart. Try reducing the rolling average or adjusting filters.", 
      type = "error", 
      duration = 5
    )
    return(FALSE)
  }
  
  return(TRUE)
}

calculate_stats <- function(data) {
  if(nrow(data) == 0) {
    return(list(
      goals = 0,
      xg_total = 0,
      shots = 0,
      conversion_rate = 0,
      avg_xg = 0,
      performance = 0
    ))
  }
  
  goals <- sum(data$isGoal)
  xg_total <- sum(data$xG)
  shots <- nrow(data)
  
  list(
    goals = goals,
    xg_total = xg_total,
    shots = shots,
    conversion_rate = if(shots > 0) (goals / shots) * 100 else 0,
    avg_xg = if(shots > 0) mean(data$xG) else 0,
    performance = goals - xg_total
  )
}
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      @media (max-width: 768px) {
        .container-fluid { padding: 10px; }
      }
      .help-text { color: #B0BEC5; font-size: 12px; margin-top: 5px; }
      .warning-text { color: #f59e0b; font-size: 13px; margin-top: 5px; font-weight: bold; }
      .stats-box { 
        background: linear-gradient(135deg, #1e3a8a 0%, #0f172a 100%);
        padding: 20px; 
        border-radius: 10px; 
        border-left: 5px solid #10b981;
        box-shadow: 0 4px 15px rgba(16, 185, 129, 0.2);
        margin-bottom: 20px;
        color: white;
      }
      .stat-item { margin: 8px 0; font-size: 15px; }
      .stat-label { font-weight: bold; color: #10b981; }
      
      .js-irs-0 .irs-bar,
      .js-irs-0 .irs-from,
      .js-irs-0 .irs-to,
      .js-irs-0 .irs-single {
        background: #10b981 !important;
      }
      
      .modal-content {
        background-color: #1e293b;
        color: white;
      }
      .modal-header {
        border-bottom: 2px solid #10b981;
      }
      .modal-body {
        font-size: 14px;
        line-height: 1.6;
      }
      .modal-body code {
        background-color: #334155;
        padding: 2px 6px;
        border-radius: 4px;
        color: #10b981;
      }
      .modal-body ol {
        padding-left: 20px;
      }
      .modal-body li {
        margin-bottom: 10px;
      }
      
      /* Loading overlay */
      .loading-overlay {
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        background: rgba(15, 23, 42, 0.9);
        display: flex;
        align-items: center;
        justify-content: center;
        z-index: 9999;
        color: #10b981;
        font-size: 20px;
        font-weight: bold;
      }
    "))
  ),
  
  setBackgroundColor("#0f172a"),
  titlePanel(
    div("Player Finishing Analysis", style = "color:#10b981; text-shadow: 0 0 10px rgba(16, 185, 129, 0.3);"), 
    windowTitle = "Player Finishing Analysis"
  ),
  
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #212121; color: white;",
      
      fluidRow(
        column(8,
          numericInput("player", "Understat Player ID:", 
             value = 1228, min = 1, max = 99999, step = 1)
        ),
        column(4, style = "padding-top: 25px;",
          actionButton("load_player", "Load", 
                       icon = icon("search"),
                       style = "width: 100%; background-color: #10b981; color: white; border: none; font-weight: bold; box-shadow: 0 2px 8px rgba(16, 185, 129, 0.3);")
        )
      ),
      
      actionButton("help", "How to find Player IDs", 
                   icon = icon("question-circle"),
                   style = "color: white; background-color: #6366f1; border: none; margin-bottom: 15px; box-shadow: 0 2px 8px rgba(99, 102, 241, 0.3);"),
      
      radioButtons("goal_filter", "Show Shots:", 
                   choices = c("All Shots" = "all", 
                               "Goals Only" = "goals", 
                               "Misses Only" = "misses"),
                   selected = "all",
                   inline = FALSE),
      
      uiOutput("goal_filter_warning"),
      
      selectizeInput("situation", "Situation:", 
                     choices = c("OpenPlay", "DirectFreekick", "FromCorner", "SetPiece", "Penalty"), 
                     multiple = TRUE, 
                     selected = c("OpenPlay", "DirectFreekick", "FromCorner", "SetPiece", "Penalty")),
      
      selectizeInput("shotType", "Shot Type:", 
                     choices = c("LeftFoot", "RightFoot", "Head", "OtherBodyPart"), 
                     multiple = TRUE, 
                     selected = c("LeftFoot", "RightFoot", "Head", "OtherBodyPart")),
      
      sliderInput("year", "Year:",
                  min = 2014, max = 2024,
                  value = c(2014, 2024),
                  sep = ""),
      
      numericInput("roll_avg", "Rolling Average of Line Chart:", 
                   value = 50, min = 5, max = 300, step = 5),
      div(class = "help-text", "Note: Higher values = smoother line"),
      
      selectInput("shots", "Shot Map Type:", 
                  choices = c("Point", "Hexbin", "Density", "Goal Zone Analysis", "Shot Type Breakdown"), 
                  selected = "Point"),
      
      selectizeInput("plots", "Plots:", 
                     choices = c("All", "Line Chart", "Shot Map", "Histogram"),
                     selected = "All"),
      
      radioButtons("theme", "Background Theme:", 
                   choices = c("Dark", "Light"), 
                   selected = "Dark"),
      
      downloadButton("download", "Download Plot", 
                     style = "width: 100%; margin-top: 10px; background-color: #FF0000; color: white; font-weight: bold; border: none; box-shadow: 0 2px 8px rgba(255, 0, 0, 0.3);"),
      
      downloadButton("download_data", "Download Data (CSV)", 
                     style = "width: 100%; margin-top: 10px; background-color: #10b981; border: none; box-shadow: 0 2px 8px rgba(16, 185, 129, 0.3);"),
      
      downloadButton("download_svg", "Download SVG", 
                     style = "width: 100%; margin-top: 10px; background-color: #8b5cf6; color: white; border: none; box-shadow: 0 2px 8px rgba(139, 92, 246, 0.3);"),
      
      downloadButton("download_pdf", "Download PDF", 
                     style = "width: 100%; margin-top: 10px; background-color: #f59e0b; color: white; border: none; box-shadow: 0 2px 8px rgba(245, 158, 11, 0.3);")
    ),
    
    mainPanel(
      h2("Player Shot Analysis", align = "center", style = "color:white"),
      h4("This Shiny app generates an interactive dashboard that visualizes a player's finishing profile. Adjust the filters and settings to explore trends and discover valuable insights about their shooting performance.", 
         style = "color:white"),
      h5("Created by Shivam Singh", style = "color:white; margin-bottom: 20px;"),
      
      uiOutput("stats_box"),
      
      withSpinner(
        plotOutput("plot", height = "800px"),
        color = "#10b981",
        type = 4
      )
    )
  )
)
server <- function(input, output, session) {
  
  loading <- reactiveVal(FALSE)
  cached_plot <- reactiveVal(NULL)
  observeEvent(input$help, {
    showModal(modalDialog(
      title = div(icon("info-circle"), " How to Find Understat Player IDs"),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      HTML("
        <div style='color: white;'>
          <h4 style='color: #10b981;'>Method 1: Direct from Understat.com</h4>
          <ol>
            <li>Go to <a href='https://understat.com' target='_blank' style='color: #10b981;'>understat.com</a></li>
            <li>Search for any player using the search bar</li>
            <li>Look at the URL - it will be like: <code>understat.com/player/<strong>1250</strong></code></li>
            <li>The number at the end (<strong>1250</strong>) is the Player ID</li>
          </ol>
          
          <h4 style='color: #10b981; margin-top: 20px;'>Method 2: Popular Player IDs</h4>
          <ul>
            <li><strong>647</strong> - Harry Kane</li>
            <li><strong>1250</strong> - Mohamed Salah</li>
            <li><strong>447</strong> - Kevin De Bruyne</li>
            <li><strong>8260</strong> - Erling Haaland</li>
            <li><strong>3423</strong> - Kylian Mbappé</li>
            <li><strong>2371</strong> - Cristiano Ronaldo</li>
            <li><strong>2097</strong> - Lionel Messi</li>
          </ul>
          
          <h4 style='color: #10b981; margin-top: 20px;'>Tips</h4>
          <ul>
            <li>Only players from top 5 European leagues are available</li>
            <li>Data goes back to 2014/15 season</li>
            <li>Invalid IDs will show an error - just try another player</li>
          </ul>
        </div>
      ")
    ))
  })

  observeEvent(input$player, {
    if(!is.na(input$player) && (input$player < 1 || input$player > 99999)) {
      showNotification("Invalid Player ID. Must be between 1-99999", 
                       type = "warning", duration = 3)
    }
  })

  dataset <- eventReactive(input$load_player, {

    loading(TRUE)
    shinyjs::disable("load_player")
    
    showNotification(
      paste("Loading player", input$player, "..."),
      id = "loading",
      duration = NULL,
      type = "message"
    )
    
    tryCatch({
      data <- player_data_full %>%
        filter(player_id_numeric == input$player)
  
      if(nrow(data) == 0) {
        stop("Player has no shot data")
      }

      required_cols <- c("X", "Y", "result", "xG")
      missing_cols <- setdiff(required_cols, names(data))
      if(length(missing_cols) > 0) {
        stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
      }
      
      removeNotification("loading")
      
      showNotification(
        paste("✅ Loaded", nrow(data), "shots!"),
        type = "message",
        duration = 3
      )
      
      return(data)
      
    }, error = function(e) {
      removeNotification("loading")
      
      error_msg <- e$message
      if(grepl("ERR_NAME_NOT_RESOLVED|ERR_CONNECTION", error_msg, ignore.case = TRUE)) {
        error_msg <- "Cannot connect to Understat. Please check your internet connection."
      } else if(grepl("timeout|timed out", error_msg, ignore.case = TRUE)) {
        error_msg <- "Request timed out. The server might be slow. Please try again."
      } else if(grepl("No data returned|no shot data", error_msg, ignore.case = TRUE)) {
        error_msg <- "This player has no shot data. Try a different player ID."
      }
      
      showNotification(
        paste("❌", error_msg,
              "\n\nSuggestions:",
              "\n• Try a different player ID (e.g., 1228 for Bruno Fernandes)",
              "\n• Check your internet connection",
              "\n• Click 'How to find Player IDs' for help"), 
        type = "error",
        duration = 10
      )
      return(NULL)
    }, finally = {

      loading(FALSE)
      shinyjs::enable("load_player")
    })
  }, ignoreNULL = FALSE)
  processed_data <- reactive({
    req(dataset())
    
    data <- dataset()
    
    data <- data %>%
      mutate(
        X = X * PITCH_LENGTH,
        Y = (1 - Y) * PITCH_WIDTH
      )
    
    if("result" %in% names(data)) {
      unique_results <- unique(data$result)
      
      data <- data %>%
        mutate(result = case_when(
          result %in% c("Goal", "Scored") ~ "Goal",
          TRUE ~ "No Goal"
        ))
    } else {
      stop("Missing 'result' column in data")
    }
    
    data <- data %>%
      mutate(isGoal = ifelse(result == "Goal", 1, 0))
    
    data
  })
  filtered_data <- reactive({
    req(processed_data())
    
    if(length(input$situation) == 0 || length(input$shotType) == 0) {
      showNotification(
        "⚠️ You must select at least one situation and shot type",
        type = "warning",
        duration = 5
      )
      return(data.frame())
    }
    
    data <- processed_data() %>%
      filter(
        year >= input$year[1],
        year <= input$year[2],
        situation %in% input$situation,
        shotType %in% input$shotType
      )
    
    if(input$goal_filter == "goals") {
      data <- data %>% filter(result == "Goal")
    } else if(input$goal_filter == "misses") {
      data <- data %>% filter(result == "No Goal")
    }
    
    data
  })
  output$goal_filter_warning <- renderUI({
    if(input$goal_filter != "all" && ("Line Chart" %in% input$plots || "All" %in% input$plots)) {
      div(
        class = "warning-text",
        icon("exclamation-triangle"),
        " Note: Performance metric (Goals - xG) is biased when filtering by result"
      )
    }
  })
  line_data <- reactive({
    req(filtered_data())
    req(input$roll_avg)
    
    if(nrow(filtered_data()) == 0) {
      return(data.frame(date = as.Date(character()), GxGSM = numeric(), GxG = numeric()))
    }
    
    data <- filtered_data() %>%
      mutate(GxG = isGoal - xG) %>%
      mutate(date = as.Date(date)) %>%
      arrange(date)
    
    n_shots <- nrow(data)
    
    if(n_shots < MIN_SHOTS_FOR_SMOOTHING) {
      data <- data %>% mutate(GxGSM = GxG)
      
    } else if(n_shots < input$roll_avg) {

      roll_window <- max(MIN_ROLL_WINDOW, floor(n_shots / 2))

      if(roll_window > n_shots) {
        roll_window <- n_shots
      }
      
      data <- data %>%
        mutate(GxGSM = TTR::SMA(GxG, n = roll_window))
      
    } else {
      data <- data %>%
        mutate(GxGSM = TTR::SMA(GxG, n = input$roll_avg))
    }

    data <- data %>% filter(!is.na(GxGSM))
    
    if(nrow(data) == 0) {
      return(data.frame(date = as.Date(character()), GxGSM = numeric(), GxG = numeric()))
    }
    
    data
  })
  get_theme_settings <- function(theme_name) {
    if(theme_name == "Dark") {
      list(
        fill_b = "#212121",
        colorText = "#f1f5f9",
        colorLine = "#ffffff",
        gridline = "#334155",
        accent1 = "#FF0000",
        accent2 = "#00FFFF",
        gradient_low = "#FFFFFF",
        gradient_high = "#FF0000",
        line_color = "#000000",
        shottype_colors = c("LeftFoot" = "#3b82f6", "RightFoot" = "#10b981", 
                           "Head" = "#f59e0b", "OtherBodyPart" = "#8b5cf6")
      )
    } else {
      list(
        fill_b = "floralwhite",
        colorText = "black",
        colorLine = "black",
        gridline = "#9E9E9E",
        accent1 = "#FF0000",
        accent2 = "#000000",
        gradient_low = "#000000",
        gradient_high = "#FF0000",
        line_color = "black",
        shottype_colors = c("LeftFoot" = "#2563eb", "RightFoot" = "#059669", 
                           "Head" = "#d97706", "OtherBodyPart" = "#7c3aed")
      )
    }
  }
  create_theme_function <- function(ts) {
    function() {
      theme_minimal() +
        theme(plot.background = element_rect(colour = ts$fill_b, fill = ts$fill_b),
              panel.background = element_rect(colour = ts$fill_b, fill = ts$fill_b)) +
        theme(plot.title = element_text(colour = ts$colorText, size = 21, face = "bold", hjust = 0.5),
              plot.subtitle = element_text(colour = ts$colorText, size = 16, hjust = 0.5),
              plot.caption = element_text(colour = ts$colorText, size = 12, hjust = 1),
              axis.title.x = element_text(colour = ts$colorText, face = "bold", size = 12),
              axis.title.y = element_text(colour = ts$colorText, face = "bold", size = 12),
              axis.text.x = element_text(colour = ts$colorText, size = 8),
              axis.text.y = element_text(colour = ts$colorText, size = 8)) +
        theme(panel.grid.major = element_line(colour = ts$gridline, linewidth = 0.4, linetype = "dashed"),
              panel.grid.minor = element_line(colour = ts$gridline, linewidth = 0.4, linetype = "dashed")) +
        theme(panel.grid.major.x = element_line(colour = ts$gridline, linewidth = 0.4, linetype = "dashed"),
              panel.background = element_blank()) +
        theme(legend.title = element_text(colour = ts$colorText),
              legend.text = element_text(colour = ts$colorText))
    }
  }
  player_name <- reactive({
    req(processed_data())
    data <- processed_data()

    name_cols <- c("player", "player_name", "name")
    name_col <- intersect(name_cols, names(data))
    
    if(length(name_col) > 0 && nrow(data) > 0) {
      name <- unique(data[[name_col[1]]])[1]
      if(!is.na(name) && nchar(as.character(name)) > 0) {
        return(as.character(name))
      }
    }
    
    paste("Player ID:", input$player)
  })

  output$stats_box <- renderUI({
    req(filtered_data())
    
    data <- filtered_data()
    
    if(nrow(data) == 0) {
      return(div(class = "stats-box",
                 h4(style = "margin-top: 0; color: #f59e0b; text-shadow: 0 0 10px rgba(245, 158, 11, 0.3);", 
                    "⚠ No Data Available"),
                 div(style = "color: #f59e0b; font-size: 14px; margin-top: 10px;", 
                     "No shots match your current filter selections.",
                     tags$br(), tags$br(),
                     "Please check:",
                     tags$ul(
                       tags$li("Year range includes player's active seasons"),
                       tags$li("At least one situation is selected"),
                       tags$li("At least one shot type is selected"),
                       tags$li("'Show Shots' filter is appropriate")
                     ))))
    }
    
    stats <- calculate_stats(data)
    
    div(class = "stats-box",
        h4(style = "margin-top: 0; color: #10b981; text-shadow: 0 0 10px rgba(16, 185, 129, 0.2);", 
           "Summary Statistics"),
        div(class = "stat-item",
            span(class = "stat-label", "Player: "),
            span(player_name())),
        div(class = "stat-item",
            span(class = "stat-label", "Total Shots: "),
            span(stats$shots)),
        div(class = "stat-item",
            span(class = "stat-label", "Goals: "),
            span(stats$goals)),
        div(class = "stat-item",
            span(class = "stat-label", "Expected Goals (xG): "),
            span(round(stats$xg_total, 2))),
        div(class = "stat-item",
            span(class = "stat-label", "Conversion Rate: "),
            span(sprintf("%.1f%%", stats$conversion_rate))),
        div(class = "stat-item",
            span(class = "stat-label", "Average xG/Shot: "),
            span(round(stats$avg_xg, 3))),
        div(class = "stat-item",
            span(class = "stat-label", "Performance (Goals - xG): "),
            span(round(stats$performance, 2), 
                 style = if(stats$performance > 0) "color: #4CAF50;" else "color: #F44336;"))
    )
  })
  create_plot <- function(plot_type, data, line_df, ts, tc, pname, year_range, roll_info) {
    
    stats <- calculate_stats(data)
    stats_label <- glue("{stats$goals} Goals with {round(stats$xg_total, 1)} xG\nfrom {stats$shots} Shots.")
    data_source <- glue("Data via Understat\nAccurate as of {Sys.Date()}")
    
    if(plot_type == "line") {
      if(nrow(line_df) == 0 || all(is.na(line_df$GxGSM))) {
        return(ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                   label = "Insufficient data for line chart\n\nTry:\n• Reducing rolling average\n• Adjusting filters\n• Selecting 'All Shots'", 
                   size = 6, color = ts$colorText) +
          theme_void() +
          theme(plot.background = element_rect(fill = ts$fill_b, color = ts$fill_b)))
      }

      n_shots <- nrow(line_df)
      
      if(n_shots == 1) {
        g <- ggplot(line_df, aes(x = date, y = GxGSM)) +
          geom_hline(yintercept = 0, linewidth = 1, colour = ts$colorText, linetype = "longdash", alpha = 0.5) +
          geom_point(size = 6, color = ts$accent1, alpha = 0.9) +
          scale_y_continuous(limits = c(min(-0.5, min(line_df$GxGSM) - 0.2), 
                                       max(0.5, max(line_df$GxGSM) + 0.2))) +
          labs(title = pname, 
               subtitle = glue("{year_range} | League Games Only | Single Shot"), 
               x = "Date (Single observation - no smoothing possible)", 
               y = "Goals - xG") +
          tc() +
          theme(legend.position = "none",
                plot.title = element_text(size = 24, face = "bold"),
                plot.subtitle = element_text(size = 18))
        
      } else if(n_shots <= 3) {
        g <- ggplot(line_df, aes(x = date, y = GxGSM)) +
          geom_hline(yintercept = 0, linewidth = 1, colour = ts$colorText, linetype = "longdash", alpha = 0.5) +
          geom_line(linewidth = 2, color = ts$line_color, alpha = 0.9) +
          geom_point(size = 4, color = ts$accent1, alpha = 0.7) +
          labs(title = pname, 
               subtitle = glue("{year_range} | League Games Only"), 
               x = roll_info, 
               y = "Goals - xG") +
          tc() +
          theme(legend.position = "none",
                plot.title = element_text(size = 24, face = "bold"),
                plot.subtitle = element_text(size = 18))
        
      } else {
        g <- ggplot(line_df, aes(x = date, y = GxGSM)) +
          geom_hline(yintercept = 0, linewidth = 1, colour = ts$colorText, linetype = "longdash", alpha = 0.5) +
          geom_line(linewidth = 2, color = ts$line_color, alpha = 0.9) +
          geom_braid(aes(ymin = 0, ymax = GxGSM, fill = GxGSM > 0), alpha = 0.7) +
          scale_fill_manual(values = c(ts$accent2, ts$accent1)) +
          labs(title = pname, 
               subtitle = glue("{year_range} | League Games Only"), 
               x = roll_info, 
               y = "Goals - xG") +
          tc() +
          theme(legend.position = "none",
                plot.title = element_text(size = 24, face = "bold"),
                plot.subtitle = element_text(size = 18))
      }
      
      return(g)
    }
    
if(plot_type == "shotmap") {
  shot_type <- input$shots
  
  stats_label <- glue("{stats$goals} Goals with {round(stats$xg_total, 1)} xG\nfrom {stats$shots} Shots.")
  data_source <- glue("Data via Understat\nAccurate as of {Sys.Date()}")
  
  if(shot_type == "Point") {
    g <- ggplot() +
      annotate_pitch(dimensions = pitch_statsbomb, fill = ts$fill_b, colour = ts$colorLine) +
      coord_flip(xlim = c(60,120), ylim = c(-2, 80)) +
      theme_pitch() +
      geom_point(data = data, aes(x = X, y = Y, fill = result, size = xG), 
                 colour = ts$colorLine, shape = 21, alpha = 0.8, stroke = 1.2, show.legend = FALSE) +
      scale_fill_manual(values = c("Goal" = ts$accent1, "No Goal" = ts$fill_b)) +
      scale_size_continuous(range = c(2, 12)) +
      labs(y = stats_label,
           x = data_source) +
      tc() + 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title.y = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size = 10, face = "bold", hjust = 0.5),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            aspect.ratio = 0.5)
    
  } else if(shot_type == "Hexbin") {
    g <- ggplot() +
      annotate_pitch(dimensions = pitch_statsbomb, fill = ts$fill_b, colour = ts$colorLine) +
      coord_flip(xlim = c(60,120), ylim = c(-2, 80)) +
      theme_pitch() +
      geom_hex(data = data, aes(x = X, y = Y), bins = 25, colour = ts$colorLine, linewidth = 0.8, show.legend = FALSE) +
      scale_fill_gradient(low = ts$gradient_low, high = ts$gradient_high) +
      labs(y = stats_label,
           x = data_source) +
      tc() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title.y = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size = 10, face = "bold", hjust = 0.5),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            aspect.ratio = 0.5)
    
  } else if(shot_type == "Density") {
    goals <- data %>% filter(result == "Goal")
    g <- ggplot() +
      annotate_pitch(dimensions = pitch_statsbomb, fill = ts$fill_b, colour = ts$colorLine) +
      coord_flip(xlim = c(60,120), ylim = c(-2, 80)) +
      theme_pitch() +
      stat_density_2d(data = data, aes(x = X, y = Y, fill = after_stat(level)), 
                      geom = "polygon", alpha = 0.7, colour = ts$colorLine, linewidth = 0.5) +
      scale_fill_gradient(low = ts$gradient_low, high = ts$gradient_high)
    
    if(nrow(goals) > 0) {
      g <- g + geom_point(data = goals, aes(x = X, y = Y), 
                         color = ts$accent1, size = 4, shape = 17, alpha = 0.9)
    }
    
    g <- g +
      labs(y = stats_label,
           x = data_source) +
      tc() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "none",
            axis.title.y = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size = 10, face = "bold", hjust = 0.5),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            aspect.ratio = 0.5)
    
  } else if(shot_type == "Goal Zone Analysis") {
    data_zones <- data %>%
      mutate(zone = case_when(
        xG >= 0.3 ~ "High Quality (xG ≥ 0.3)",
        xG >= 0.15 ~ "Medium Quality (0.15-0.3)",
        TRUE ~ "Low Quality (xG < 0.15)"
      ))
    
    goals <- data_zones %>% filter(result == "Goal")
    
    g <- ggplot() +
      annotate_pitch(dimensions = pitch_statsbomb, fill = ts$fill_b, colour = ts$colorLine) +
      coord_flip(xlim = c(60,120), ylim = c(-2, 80)) +
      theme_pitch() +
      geom_point(data = data_zones, aes(x = X, y = Y, color = zone, size = xG), 
                 alpha = 0.6, stroke = 1) +
      scale_color_manual(values = c("High Quality (xG ≥ 0.3)" = ts$accent1,
                                    "Medium Quality (0.15-0.3)" = "#fbbf24",
                                    "Low Quality (xG < 0.15)" = ts$accent2)) +
      scale_size_continuous(range = c(2, 10))
    
    if(nrow(goals) > 0) {
      g <- g + geom_point(data = goals, aes(x = X, y = Y), 
                         color = ts$accent1, size = 6, shape = 17, stroke = 2)
    }
    
    g <- g +
      labs(y = stats_label,
           x = data_source,
           color = "Shot Quality",
           caption = "Circles = All shots (colored by quality) | Triangles = Goals (Red overlay)") +
      tc() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = c(0.15, 0.14),
            legend.background = element_rect(fill = ts$fill_b, color = ts$colorLine, linewidth = 0.5),
            plot.caption = element_text(size = 9, hjust = 0.5, margin = margin(t = 5)),
            axis.title.y = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size = 10, face = "bold", hjust = 0.5),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            aspect.ratio = 0.5) +
      guides(size = "none")
    
  } else if(shot_type == "Shot Type Breakdown") {
    goals <- data %>% filter(result == "Goal")
    
    g <- ggplot() +
      annotate_pitch(dimensions = pitch_statsbomb, fill = ts$fill_b, colour = ts$colorLine) +
      coord_flip(xlim = c(60,120), ylim = c(-2, 80)) +
      theme_pitch() +
      geom_point(data = data, aes(x = X, y = Y, color = shotType, size = xG), 
                 alpha = 0.7, stroke = 1.2) +
      scale_color_manual(values = ts$shottype_colors,
                        labels = c("LeftFoot" = "Left Foot",
                                 "RightFoot" = "Right Foot",
                                 "Head" = "Header",
                                 "OtherBodyPart" = "Other")) +
      scale_size_continuous(range = c(3, 12))
    
    if(nrow(goals) > 0) {
      g <- g + geom_point(data = goals, aes(x = X, y = Y), 
                         color = ts$accent1, size = 7, shape = 17, stroke = 2.5)
    }
    
    g <- g +
      labs(y = stats_label,
           x = data_source,
           color = "Shot Type",
           caption = "Circles = Shots by body part | Red triangles = Goals") +
      tc() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = c(0.15, 0.16),
            legend.background = element_rect(fill = ts$fill_b, color = ts$colorLine, linewidth = 0.5),
            plot.caption = element_text(size = 9, hjust = 0.5, margin = margin(t = 5)),
            axis.title.y = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size = 10, face = "bold", hjust = 0.5),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            aspect.ratio = 0.5) +
      guides(size = "none")
  }
  
  return(g)
}
    if(plot_type == "histogram") {
      show_legend <- input$goal_filter == "all"
      g <- ggplot() +
        geom_histogram(data = data, aes(x = xG, fill = result), 
                       bins = 20, position = position_stack(reverse = TRUE), 
                       color = ts$colorLine, linewidth = 0.5, alpha = 0.85) +
        scale_fill_manual(values = c("Goal" = ts$accent1, "No Goal" = ts$accent2)) +
        labs(x = "Individual Shot xG", y = "Frequency") +
        tc()
      
      if(show_legend) {
        g <- g + theme(legend.position = c(0.85, 0.85),
                      legend.title = element_blank(),
                      legend.background = element_rect(fill = ts$fill_b, color = ts$colorLine, linewidth = 0.5),
                      legend.text = element_text(size = 12))
      } else {
        g <- g + theme(legend.position = "none")
      }
      
      if(nrow(data) > 0) {
        g <- g +
          geom_vline(xintercept = mean(data$xG), colour = ts$colorText, 
                     linewidth = 1.5, linetype = "longdash", alpha = 0.7) +
          annotate(geom = "text", x = mean(data$xG) + 0.03, 
                   y = max(table(cut(data$xG, 20))) * 0.9, 
                   label = glue("xG/Shot = {round(mean(data$xG), 3)}"), 
                   colour = ts$colorText, size = 5, fontface = "bold", hjust = 0)
      }
      
      return(g)
    }
  }

output$plot <- renderPlot({
  tryCatch({
    req(filtered_data())
    
    withProgress(message = 'Generating plot...', value = 0, {
      
      incProgress(0.1, detail = "Loading data...")
      plot_start_time <- Sys.time()
      data <- filtered_data()
      
      if(nrow(data) == 0) {
        ts <- get_theme_settings(input$theme)
        par(bg = ts$fill_b)
        plot.new()
        text(0.5, 0.6, "No data available for selected filters", 
             cex = 2, col = ts$colorText, family = "sans", font = 2)
        text(0.5, 0.4, "Please adjust your filter selections above", 
             cex = 1.3, col = ts$colorText, family = "sans")
        return()
      }
      
      incProgress(0.2, detail = "Preparing theme...")
      ts <- get_theme_settings(input$theme)
      tc <- create_theme_function(ts)
      pname <- player_name()
      year_range <- glue("{input$year[1]} - {input$year[2]}")
      line_df <- line_data()
      n_filtered <- nrow(data)
      
      if(n_filtered < MIN_SHOTS_FOR_SMOOTHING) {
        roll_info <- "Raw data (too few shots for smoothing)"
      } else if(n_filtered < input$roll_avg) {
        actual_window <- max(MIN_ROLL_WINDOW, floor(n_filtered / 2))
        roll_info <- glue("Rolling Average: {actual_window} shots (adjusted from {input$roll_avg})")
      } else {
        roll_info <- glue("Rolling Average: {input$roll_avg} shots")
      }
      
      plots_to_create <- input$plots
      final_plot <- NULL
      
      incProgress(0.3, detail = "Creating plots...")
      
      if("All" %in% plots_to_create) {
        incProgress(0.4, detail = "Line chart...")
        g1 <- create_plot("line", data, line_df, ts, tc, pname, year_range, roll_info)
        
        incProgress(0.6, detail = "Shot map...")
        g2 <- create_plot("shotmap", data, line_df, ts, tc, pname, year_range, roll_info)
        
        incProgress(0.8, detail = "Histogram...")
        g3 <- create_plot("histogram", data, line_df, ts, tc, pname, year_range, roll_info)
        
        combined <- g1 / (g2 | g3)
        final_plot <- combined & plot_annotation(
          caption = "Created by Shivam Singh",
          theme = theme(
            plot.background = element_rect(fill = ts$fill_b, colour = ts$fill_b),
            plot.caption = element_text(colour = ts$colorText, hjust = 1, size = 14)
          )
        )
        
      } else if("Line Chart" %in% plots_to_create) {
        incProgress(0.6, detail = "Line chart...")
        g1 <- create_plot("line", data, line_df, ts, tc, pname, year_range, roll_info)
        final_plot <- g1 & plot_annotation(
          caption = "Created by Shivam Singh",
          theme = theme(
            plot.background = element_rect(fill = ts$fill_b, colour = ts$fill_b),
            plot.caption = element_text(colour = ts$colorText, hjust = 1, size = 14)
          )
        )
        
      } else if("Shot Map" %in% plots_to_create) {
        incProgress(0.6, detail = "Shot map...")
        g2 <- create_plot("shotmap", data, line_df, ts, tc, pname, year_range, roll_info)
        final_plot <- (g2 + theme(axis.title.y = element_blank())) &
          plot_annotation(
            caption = "Created by Shivam Singh",
            title = pname,
            subtitle = glue("{year_range} | League Games Only"),
            theme = theme(
              plot.background = element_rect(fill = ts$fill_b, colour = ts$fill_b),
              plot.caption = element_text(colour = ts$colorText, hjust = 1, size = 14),
              plot.title = element_text(colour = ts$colorText, hjust = 0.5, size = 21, face = "bold"),
              plot.subtitle = element_text(colour = ts$colorText, hjust = 0.5, size = 16)
            )
          )
        
      } else if("Histogram" %in% input$plots) {
        incProgress(0.6, detail = "Histogram...")
        g3 <- create_plot("histogram", data, line_df, ts, tc, pname, year_range, roll_info)
        final_plot <- g3 & plot_annotation(
          caption = "Created by Shivam Singh",
          title = pname,
          subtitle = glue("{year_range} | League Games Only"),
          theme = theme(
            plot.background = element_rect(fill = ts$fill_b, colour = ts$fill_b),
            plot.caption = element_text(colour = ts$colorText, hjust = 1, size = 14),
            plot.title = element_text(colour = ts$colorText, hjust = 0.5, size = 21, face = "bold"),
            plot.subtitle = element_text(colour = ts$colorText, hjust = 0.5, size = 16)
          )
        )
      }
      
      incProgress(0.9, detail = "Finalizing...")
      
      plot_time <- as.numeric(difftime(Sys.time(), plot_start_time, units = "secs"))
      log_info("Plot rendered in {round(plot_time, 2)} seconds for {nrow(data)} shots")
      cached_plot(final_plot)
      incProgress(1.0, detail = "Complete!")
      print(final_plot)
      
    })
    
  }, error = function(e) {
    par(bg = "#0f172a")
    plot.new()
    text(0.5, 0.5, paste("Plot Error:", e$message), 
         cex = 1.5, col = "white", family = "sans")
  })
})


  output$download <- downloadHandler(
  filename = function() { 
    paste(player_name(), "_", Sys.Date(), ".png", sep="") 
  },
  content = function(file) {
    if(!validate_download(filtered_data(), line_data(), input$plots)) {
      return(NULL)
    }

    final_plot <- cached_plot()
    
    if(is.null(final_plot)) {
      showNotification("Please generate a plot first", type = "warning")
      return(NULL)
    }
    
    ts <- get_theme_settings(input$theme)
    
    device <- function(..., width, height) {
      grDevices::png(..., width = PLOT_WIDTH, height = PLOT_HEIGHT, 
                    res = PLOT_DPI, units = "in")
    }
    ggsave(file, plot = final_plot, device = device, bg = ts$fill_b)
  }
)

  output$download_svg <- downloadHandler(
  filename = function() { 
    paste(player_name(), "_", Sys.Date(), ".svg", sep="") 
  },
  content = function(file) {
    if(!validate_download(filtered_data(), line_data(), input$plots)) {
      return(NULL)
    }

    final_plot <- cached_plot()
    
    if(is.null(final_plot)) {
      showNotification("Please generate a plot first", type = "warning")
      return(NULL)
    }
    
    ts <- get_theme_settings(input$theme)
    
    ggsave(file, plot = final_plot, device = "svg", 
           width = PLOT_WIDTH, height = PLOT_HEIGHT, bg = ts$fill_b)
  }
)

  output$download_pdf <- downloadHandler(
  filename = function() { 
    paste(player_name(), "_", Sys.Date(), ".pdf", sep="") 
  },
  content = function(file) {
    if(!validate_download(filtered_data(), line_data(), input$plots)) {
      return(NULL)
    }
    final_plot <- cached_plot()
    
    if(is.null(final_plot)) {
      showNotification("Please generate a plot first", type = "warning")
      return(NULL)
    }
    
    ts <- get_theme_settings(input$theme)
    
    ggsave(file, plot = final_plot, device = "pdf", 
           width = PLOT_WIDTH, height = PLOT_HEIGHT, bg = ts$fill_b)
  }
)

  output$download_data <- downloadHandler(
    filename = function() {
      paste(player_name(), "_shots_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      if(nrow(filtered_data()) == 0) {
        showNotification(
          "No data to download. Please adjust your filters.", 
          type = "error", 
          duration = 5
        )
        return(NULL)
      }
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  session$onSessionEnded(function() {
    gc()
    }
  )
}

shinyApp(ui = ui, server = server)