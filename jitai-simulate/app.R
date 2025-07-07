library(shiny)
library(plotly)
library(dplyr)
library(purrr)
library(RColorBrewer)
library(scales)
library(httr)
library(markdown)
library(waiter)
library(shinyjs)
remotes::install_github("deepanshu88/shinyCopy2clipboard")
library(shinyCopy2clipboard)

api_key <- trimws(readLines("apikey.txt"))

jscode <- '
var container = document.getElementById("chat-container");
if (container) {
  var elements = container.getElementsByClassName("user-message");
  if (elements.length > 1) {
    var lastElement = elements[elements.length - 1];
    lastElement.scrollIntoView({
      behavior: "smooth"
    });
  }
}
'

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      h2, h3 { color: #0072B2; }
      ul { margin-left: 20px; }
      .img-wrapper { display: inline-block; vertical-align: top; margin-right: 10px; }
      .img-wrapper2 { width: 40px; height: 40px; border-radius: 50%; }
      .user-message, .bot-message { margin-bottom: 15px; padding: 10px; border-radius: 10px; }
      .user-message { background-color: #e0f7fa; }
      .bot-message { background-color: #f1f8e9; }

      #main_tabs .tab-pane {
        padding-top: 15px;
      }
      .help-section {
        max-width: 900px;
        margin: auto;
        padding: 20px;
        line-height: 1.6;
        font-size: 16px;
        background-color: #f9fbfc;
        border-radius: 8px;
        box-shadow: 0 4px 10px rgba(0,0,0,0.05);
      }
      .help-section h3 {
        border-bottom: 2px solid #0072B2;
        padding-bottom: 6px;
        margin-bottom: 15px;
      }
      .formula-list li {
        margin-bottom: 18px;
      }
      .formula-list b {
        color: #0072B2;
      }
      .formula-mathjax {
        display: block;
        margin-top: 6px;
        background-color: #e8f0fe;
        padding: 8px 12px;
        border-radius: 6px;
        font-family: 'Courier New', Courier, monospace;
        font-size: 16px;
      }
      .symbol-list li {
        margin-bottom: 8px;
      }

      .sidebar-section {
        background-color: #f9fbfc;
        border: 1px solid #cde3e6;
        border-radius: 8px;
        padding: 12px 15px;
        margin-bottom: 15px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.05);
      }
      .sidebar-section h4 {
        color: #0072B2;
        margin-bottom: 10px;
        border-bottom: 1px solid #cde3e6;
        padding-bottom: 4px;
        font-weight: 600;
      }
      .small-help-text {
        font-size: 12px;
        color: #666666;
        margin-top: -8px;
        margin-bottom: 8px;
        font-style: italic;
      }
    "))
  ),
  sidebarLayout(
    sidebarPanel(
      div(
        class = "sidebar-section",
        h4("Simulation Settings"),
        sliderInput("n_people", "Number of Individuals:", min = 1, max = 20, value = 5, step = 1),
        sliderInput("hours", "Duration (hours):", min = 6, max = 48, value = 12, step = 1),
        sliderInput("freq", "Measurements per hour:", min = 1, max = 12, value = 4, step = 1)
      ),
      div(
        class = "sidebar-section",
        h4("Moderator Input"),
        sliderInput("moderator_value", "Moderator (0-100):", min = 0, max = 100, value = 50, step = 10)
      ),
      div(
        class = "sidebar-section",
        h4("Decision Rules"),
        checkboxGroupInput(
          "decision_rules", "Select Decision Rules:",
          choices = c(
            "Threshold-based",
            "Slope-based",
            "Combined (Threshold + Slope)",
            "Cumulative Increase",
            "Randomized Trigger",
            "Exp Smoothing Threshold",
            "Adaptive Threshold"
          ),
          selected = "Adaptive Threshold"
        )
      ),
      div(
        class = "sidebar-section",
        h4("Parameters"),
        sliderInput("slope_threshold", "Slope Threshold (per Measurement):", min = 0, max = 2, value = 0.5, step = 0.05),
        sliderInput("cooldown", "Cooldown Period (minutes):", min = 5, max = 60, value = 15, step = 5),
        sliderInput("effect_sd", "Intervention Effect Variability (SD):", min = 0, max = 2, value = 0.5, step = 0.1),
        sliderInput("threshold_adjust_up", "Threshold Increase Factor (No Intervention):", min = 1, max = 1.1, value = 1.02, step = 0.01),
        sliderInput("threshold_adjust_down", "Threshold Decrease Factor (Successful Intervention):", min = 0.8, max = 1, value = 0.95, step = 0.01)
      ),
      div(
        class = "sidebar-section",
        h4("Plot Options"),
        checkboxInput("group_plot", "Show Group Plot", value = FALSE),
        checkboxInput("use_fixed_seed", "Use Fixed Random Seed", value = FALSE),
        numericInput("fixed_seed", "Set Seed Value:", value = 1234, min = 1, max = 1e6, step = 1),
        uiOutput("person_selector"),
        actionButton("simulate", "Simulate New Group", icon = icon("sync"))
      ),
      div(
        class = "small-help-text",
        "Adaptive Threshold adjusts dynamically based on intervention success."
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        tabPanel(
          "Simulator",
          uiOutput("plots_ui")
        ),
        tabPanel(
          "Moderator Effect",
          plotlyOutput("moderatorEffectPlot"),
          p(
            "This plot illustrates how different moderator levels influence the variability and intensity of suicidal urge over time. Higher moderator values increase the amplitude of fluctuations, representing greater instability in urge levels across the day. The highlighted line corresponds to the moderator level closest to the selected input.",
            style = "font-style: italic; color: grey; margin-top: 10px; max-width: 700px;"
          )
        ),
        tabPanel(
          "AI Chat Assistant",
          tags$div(
            id = "chat-container",
            tags$div(
              id = "chat-header",
              tags$h3("Mental Health AI Chat Assistant")
            ),
            tags$div(
              id = "chat-history",
              uiOutput("chatThread")
            ),
            tags$div(
              id = "chat-input",
              textAreaInput(
                inputId = "prompt", label = NULL,
                placeholder = "Type your message here...", width = "100%", resize = "vertical"
              ),
              actionButton(inputId = "submit", label = "Send", icon = icon("paper-plane")),
              actionButton(inputId = "remove_chatThread", label = "Clear History", icon = icon("trash-can")),
              shinyCopy2clipboard::CopyButton("clipbtn", label = "Copy Last Response", icon = icon("clipboard"))
            )
          )
        ),
        tabPanel(
          "Help",
          tags$div(
            class = "help-section",
            withMathJax(),
            tags$h2("\ud83d\udcda JITAI Simulator"),
            tags$p("This app allows you to simulate real-time suicidal urge intervention scenarios using multiple decision rules, and also has a built-in AI chat assistant."),
            tags$h3("\ud83c\udf9b\ufe0f Simulator Features"),
            tags$ul(
              tags$li(tags$b("Number of Individuals:"), " Set how many simulated individuals will be generated."),
              tags$li(tags$b("Duration (hours):"), " Total simulation time."),
              tags$li(tags$b("Measurements per Hour:"), " Number of suicidal urge measurements collected each hour."),
              tags$li(tags$b("Decision Rules:"), " Select one or more intervention decision rules to test (e.g., threshold-based, slope-based, adaptive thresholds)."),
              tags$li(tags$b("Cooldown Period:"), " Minimum time required between interventions."),
              tags$li(tags$b("Intervention Effect Variability:"), " Simulates individual differences in how effective the intervention is."),
              tags$li(tags$b("Adaptive Threshold:"), " Allows intervention thresholds to adjust dynamically based on recent success or failure."),
              tags$li(tags$b("Moderator Influence:"), " Adjusts how strongly individual or environmental factors influence the variability of suicidal urge levels over time.")
            ),
            tags$h3("\ud83d\udcca Decision Rule Formulas"),
            tags$ul(
              class = "formula-list",
              tags$li(
                tags$b("Threshold-based: "),
                "Intervene if \\( U_t > T_t \\)",
                tags$span(class = "formula-mathjax", "\\[ \\text{Intervene when suicidal urge level } U_t \\text{ exceeds threshold } T_t. \\]")
              ),
              tags$li(
                tags$b("Slope-based: "),
                "Intervene if \\( \\frac{dU}{dt} = U_t - U_{t-1} > \\theta_s \\)",
                tags$span(class = "formula-mathjax", "\\[ \\text{Intervene when change in suicidal urge between measurements exceeds slope threshold } \\theta_s. \\]")
              ),
              tags$li(
                tags$b("Combined (Threshold + Slope): "),
                "Intervene if \\( U_t > T_t \\) and \\( \\frac{dU}{dt} > \\theta_s \\)",
                tags$span(class = "formula-mathjax", "\\[ \\text{Intervene when both suicidal urge exceeds threshold and slope exceeds threshold.} \\]")
              ),
              tags$li(
                tags$b("Cumulative Increase: "),
                "Intervene if \\( \\sum_{i=t-n}^t (U_i - U_{i-1}) > \\theta_s \\)",
                tags$span(class = "formula-mathjax", "\\[ \\text{Intervene when cumulative increase over last } n \\text{ measurements exceeds } \\theta_s. \\]")
              ),
              tags$li(
                tags$b("Randomized Trigger: "),
                "Intervene with probability \\( P = \\min(1, \\frac{U_t}{10}) \\)",
                tags$span(class = "formula-mathjax", "\\[ \\text{Probability of intervention increases with suicidal urge level } U_t. \\]")
              ),
              tags$li(
                tags$b("Exp Smoothing Threshold: "),
                "Intervene if \\( E_t > T_t \\), where \\( E_t = \\alpha U_t + (1-\\alpha) E_{t-1} \\)",
                tags$span(class = "formula-mathjax", "\\[ \\text{Exponentially smoothed suicidal urge } E_t \\text{ compared to threshold } T_t. \\]")
              ),
              tags$li(
                tags$b("Adaptive Threshold: "),
                "Threshold updated by:",
                tags$span(class = "formula-mathjax", "
                  \\[
                  T_{t+1} = \\begin{cases}
                    T_t \\times \\delta_d & \\text{if intervention successful} \\\\
                    T_t \\times \\delta_u & \\text{if no intervention or unsuccessful}
                  \\end{cases}
                  \\]
                ")
              )
            ),
            tags$h3("\ud83d\udd39 Symbol Definitions"),
            tags$ul(
              class = "symbol-list",
              tags$li("\\( U_t \\): Suicidal urge level at time \\( t \\)"),
              tags$li("\\( T_t \\): Intervention threshold at time \\( t \\)"),
              tags$li("\\( \\frac{dU}{dt} \\): Change in suicidal urge level between consecutive measurements"),
              tags$li("\\( \\theta_s \\): Slope threshold (user-defined sensitivity)"),
              tags$li("\\( n \\): Number of prior timepoints considered in cumulative increase"),
              tags$li("\\( P \\): Probability of triggering intervention (in randomized trigger)"),
              tags$li("\\( E_t \\): Exponentially smoothed suicidal urge value at time \\( t \\)"),
              tags$li("\\( \\alpha \\): Smoothing parameter (fixed at 0.3 in this app)"),
              tags$li("\\( \\delta_d \\): Threshold decrease factor (applied after successful intervention)"),
              tags$li("\\( \\delta_u \\): Threshold increase factor (applied when no intervention occurs)")
            )
          )
        )
      )
    )
  )
)



chatGPT_R <- function(api_key, prompt, model = "gpt-3.5-turbo") {
  library(httr)
  library(jsonlite)

  body <- list(
    model = model,
    messages = list(list(role = "user", content = prompt)),
    max_tokens = 300,
    temperature = 0.7
  )

  res <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    body = body,
    encode = "json"
  )

  stop_for_status(res)

  content <- content(res, as = "parsed", simplifyVector = TRUE)
  response_text <- content$choices[[1]]$message$content
  return(response_text)
}

server <- function(input, output, session) {
  random_seed <- reactiveVal(sample(1:10000, 1))

  observeEvent(input$simulate, {
    if (input$use_fixed_seed) {
      random_seed(input$fixed_seed)
    } else {
      random_seed(sample(1:10000, 1))
    }
  })

  observe({
    shinyjs::toggleState("fixed_seed", condition = input$use_fixed_seed)
  })

  # Function to simulate stress and interventions for one person
  simulate_person <- function(person_id, hours, freq, decision_rule, slope_threshold, cooldown,
                              effect_sd, threshold_adjust_up, threshold_adjust_down, fixed_start,
                              baseline_shift, phase_shift, noise_vector, amplitude_scale = 1,
                              moderator_value = 0) {
    total_points <- hours * freq
    time_seq <- seq(0, hours * 60, length.out = total_points) # in minutes

    individual_threshold <- runif(1, 6, 9)
    intervention_effect <- runif(1, 1.5, 3)

    amplitude <- 3 * (1 + moderator_value) * amplitude_scale

    base_urge <- 5 + baseline_shift + amplitude * sin(2 * pi * time_seq / (60 * 24) + phase_shift) + noise_vector
    base_urge <- pmax(0, pmin(10, base_urge))
    base_urge[1] <- fixed_start

    df <- data.frame(
      person_id = person_id,
      time_min = time_seq,
      stress = base_urge,
      intervention = FALSE,
      effect = NA,
      threshold = NA,
      intervention_effect = intervention_effect,
      decision_rule = decision_rule
    )

    current_threshold <- individual_threshold
    last_intervention_time <- -Inf

    for (i in seq_len(nrow(df))) {
      df$threshold[i] <- current_threshold

      if (i == 1) next

      cooldown_passed <- (df$time_min[i] - last_intervention_time) >= cooldown
      threshold_triggered <- df$stress[i] > current_threshold
      should_intervene <- FALSE

      if (decision_rule == "Adaptive Threshold") {
        if (threshold_triggered && cooldown_passed) should_intervene <- TRUE
      } else if (decision_rule == "Threshold-based") {
        if (threshold_triggered && cooldown_passed) should_intervene <- TRUE
      } else if (decision_rule == "Slope-based") {
        slope <- df$stress[i] - df$stress[i - 1]
        if ((slope >= slope_threshold) && cooldown_passed) should_intervene <- TRUE
      } else if (decision_rule == "Combined (Threshold + Slope)") {
        slope <- df$stress[i] - df$stress[i - 1]
        if (threshold_triggered && (slope >= slope_threshold) && cooldown_passed) should_intervene <- TRUE
      } else if (decision_rule == "Cumulative Increase") {
        start_idx <- max(1, i - 4)
        cum_inc <- sum(diff(df$stress[start_idx:i]))
        if ((cum_inc > slope_threshold) && cooldown_passed) should_intervene <- TRUE
      } else if (decision_rule == "Randomized Trigger") {
        prob <- min(1, df$stress[i] / 10)
        if ((runif(1) < prob) && cooldown_passed) should_intervene <- TRUE
      } else if (decision_rule == "Exp Smoothing Threshold") {
        smooth_vals <- stats::filter(df$stress[1:i], filter = 0.3, method = "recursive")
        smooth_val <- ifelse(is.na(smooth_vals[length(smooth_vals)]), 0, smooth_vals[length(smooth_vals)])
        if ((smooth_val > current_threshold) && cooldown_passed) should_intervene <- TRUE
      }

      if (should_intervene) {
        df$intervention[i] <- TRUE
        last_intervention_time <- df$time_min[i]

        sampled_effect <- max(0, rnorm(1, mean = intervention_effect, sd = effect_sd))
        df$effect[i] <- sampled_effect

        if (i < nrow(df)) df$stress[i + 1] <- max(0, df$stress[i + 1] - sampled_effect)

        stress_drop <- if (i < nrow(df)) df$stress[i] - df$stress[i + 1] else 0

        if (decision_rule == "Adaptive Threshold") {
          if (stress_drop >= sampled_effect * 0.5) {
            current_threshold <- current_threshold * threshold_adjust_down
          } else {
            current_threshold <- current_threshold * threshold_adjust_up
          }
          current_threshold <- max(3, min(9, current_threshold))
        }
      } else {
        if (decision_rule == "Adaptive Threshold") {
          current_threshold <- current_threshold * threshold_adjust_up
          current_threshold <- max(3, min(9, current_threshold))
        }
      }
    }

    df
  }

  all_data <- reactive({
    req(input$decision_rules)
    set.seed(random_seed())
    people <- seq_len(input$n_people)
    total_points <- input$hours * input$freq

    starting_points <- runif(input$n_people, min = 2, max = 6)
    baseline_shifts <- rnorm(input$n_people, 0, 1.5)
    amplitude_scaling <- runif(input$n_people, 0.5, 1.5)
    noise_scale <- runif(input$n_people, 0.7, 1.3)
    noise_mat <- replicate(input$n_people, rnorm(total_points, 0, 1))
    phase_shifts <- runif(input$n_people, 0, 2 * pi)

    df_list <- purrr::cross2(people, input$decision_rules) %>%
      purrr::map(function(pr) {
        person_id <- pr[[1]]
        decision_rule <- pr[[2]]

        simulate_person(
          person_id = person_id,
          hours = input$hours,
          freq = input$freq,
          decision_rule = decision_rule,
          slope_threshold = input$slope_threshold,
          cooldown = input$cooldown,
          effect_sd = input$effect_sd,
          threshold_adjust_up = input$threshold_adjust_up,
          threshold_adjust_down = input$threshold_adjust_down,
          fixed_start = starting_points[person_id],
          baseline_shift = baseline_shifts[person_id],
          phase_shift = phase_shifts[person_id],
          noise_vector = noise_mat[, person_id] * noise_scale[person_id],
          amplitude_scale = amplitude_scaling[person_id],
          moderator_value = input$moderator_value
        )
      })

    bind_rows(df_list)
  })

  output$person_selector <- renderUI({
    if (input$group_plot) {
      return(NULL)
    }
    selectInput("selected_person", "Select Individual:", choices = seq_len(input$n_people), selected = 1)
  })

  output$plots_ui <- renderUI({
    req(input$decision_rules)
    tabs <- lapply(input$decision_rules, function(rule) {
      plotname <- paste0("stressPlot_", gsub("[^a-zA-Z0-9]", "_", rule))
      thresholdplotname <- paste0("thresholdPlot_", gsub("[^a-zA-Z0-9]", "_", rule))
      tab_content <- tagList(
        plotlyOutput(plotname)
      )
      if (rule == "Adaptive Threshold") {
        tab_content <- tagList(
          tab_content,
          plotlyOutput(thresholdplotname),
          p("Visualizes the evolving intervention threshold over time for the selected individual.")
        )
      }
      tabPanel(
        title = rule,
        tab_content
      )
    })
    do.call(tabsetPanel, c(id = "simulator_tabs", tabs))
  })

  observe({
    req(all_data())
    df <- all_data()

    for (rule in input$decision_rules) {
      local({
        decision_rule_selected <- rule
        plotname <- paste0("stressPlot_", gsub("[^a-zA-Z0-9]", "_", decision_rule_selected))
        thresholdplotname <- paste0("thresholdPlot_", gsub("[^a-zA-Z0-9]", "_", decision_rule_selected))

        output[[plotname]] <- renderPlotly({
          df_sub <- df %>% filter(decision_rule == decision_rule_selected)

          color_palette <- c(RColorBrewer::brewer.pal(8, "Set1"), RColorBrewer::brewer.pal(8, "Set2"))
          all_ids <- sort(unique(df_sub$person_id))
          if (length(all_ids) > length(color_palette)) {
            color_palette <- rep(color_palette, length.out = length(all_ids))
          }
          color_map <- setNames(color_palette[1:length(all_ids)], all_ids)

          if (input$group_plot) {
            p <- plot_ly()
            for (pid in all_ids) {
              df_person <- df_sub %>% filter(person_id == pid)
              p <- p %>%
                add_lines(
                  data = df_person, x = ~time_min, y = ~stress, name = paste("Person", pid),
                  line = list(color = color_map[as.character(pid)])
                ) %>%
                add_markers(
                  data = df_person %>% filter(intervention == TRUE),
                  x = ~time_min, y = ~stress,
                  marker = list(size = 10, color = color_map[as.character(pid)]),
                  name = paste("Intervention: Person", pid),
                  showlegend = FALSE
                )
            }
            p %>% layout(
              title = paste("Group Plot:", decision_rule_selected),
              xaxis = list(title = "Time (minutes)"),
              yaxis = list(title = "Suicidal Urge Level", range = c(0, 15)),
              legend = list(x = 1, y = 1),
              margin = list(t = 80, b = 70)
            )
          } else {
            req(input$selected_person)
            df_person <- df_sub %>% filter(person_id == input$selected_person)
            color_indiv <- color_map[as.character(input$selected_person)]

            p <- plot_ly(df_person,
              x = ~time_min, y = ~stress,
              type = "scatter", mode = "lines",
              line = list(color = color_indiv, width = 2),
              name = "Suicidal Urge Level"
            )

            interventions <- df_person %>% filter(intervention == TRUE)
            if (nrow(interventions) > 0) {
              effect_range <- range(interventions$effect, na.rm = TRUE)
              effect_colors <- scales::col_numeric("Viridis", effect_range)(interventions$effect)
              for (i in seq_len(nrow(interventions))) {
                p <- add_trace(
                  p,
                  x = interventions$time_min[i],
                  y = interventions$stress[i],
                  type = "scatter",
                  mode = "markers",
                  marker = list(
                    size = 14,
                    color = effect_colors[i],
                    line = list(width = 2, color = "black")
                  ),
                  name = "Intervention",
                  showlegend = (i == 1),
                  text = paste0("Effect size: ", round(interventions$effect[i], 2)),
                  hoverinfo = "text"
                )
              }
            }

            p %>% layout(
              title = paste("Suicidal Urge Trajectory for Individual", input$selected_person, "-", decision_rule_selected),
              xaxis = list(title = "Time (minutes)"),
              yaxis = list(title = "Suicidal Urge Level", range = c(0, 15)),
              legend = list(x = 0.8, y = 0.95),
              margin = list(t = 80, b = 70)
            )
          }
        })


        if (decision_rule_selected == "Adaptive Threshold") {
          output[[thresholdplotname]] <- renderPlotly({
            req(!input$group_plot, input$selected_person)
            df_sub <- df %>% filter(decision_rule == decision_rule_selected, person_id == input$selected_person)
            plot_ly(df_sub,
              x = ~time_min, y = ~threshold, type = "scatter", mode = "lines+markers",
              line = list(color = "orange", width = 3), name = "Adaptive Threshold"
            ) %>%
              layout(
                title = paste("Evolving Intervention Threshold for Individual", input$selected_person, "-", decision_rule_selected),
                xaxis = list(title = "Time (minutes)"),
                yaxis = list(title = "Threshold", range = c(3, 10)),
                margin = list(t = 80, b = 70)
              )
          })
        }
      })
    }
  })

  output$moderatorEffectPlot <- renderPlotly({
    req(input$moderator_value)

    moderators <- c(0, 50, 100)
    moderator_levels <- paste0("Moderator = ", moderators)
    mod_input <- as.numeric(input$moderator_value)

    time_seq <- seq(0, 24 * 60, length.out = 200)

    df_mod <- purrr::map(moderators, function(mod_val) {
      amplitude <- 3 * (1 + mod_val / 100)
      urge <- 5 + amplitude * sin(2 * pi * time_seq / (24 * 60))
      data.frame(
        time_min = time_seq,
        urge = urge,
        moderator = paste0("Moderator = ", mod_val)
      )
    })

    closest_idx <- which.min(abs(moderators - mod_input))

    colors <- c("#0072B2", "#56B4E9", "#CC79A7")
    line_widths <- rep(2, length(moderators))
    line_widths[closest_idx] <- 4

    p <- plot_ly() %>%
      layout(
        title = "Effect of Moderator on Suicidal Urge Variability",
        xaxis = list(title = "Time (minutes)"),
        yaxis = list(title = "Suicidal Urge Level", range = c(0, 15)),
        margin = list(t = 80, b = 70),
        annotations = list(
          x = max(time_seq) * 0.9,
          y = 9,
          text = paste("Selected moderator:", moderators[closest_idx]),
          showarrow = FALSE,
          font = list(size = 16, color = colors[closest_idx])
        )
      )

    for (i in seq_along(df_mod)) {
      df_sub <- df_mod[[i]]
      p <- p %>% add_lines(
        data = df_sub,
        x = ~time_min,
        y = ~urge,
        name = moderator_levels[i],
        line = list(color = colors[i], width = line_widths[i])
      )
    }

    p
  })

  historyALL <- reactiveValues(df = data.frame(), val = character(0))

  observeEvent(input$submit, {
    req(nchar(trimws(input$prompt)) > 0)

    w <- waiter::Waiter$new(
      id = "chat-history",
      html = waiter::spin_3(),
      color = "rgba(255,255,255,0.5)"
    )
    w$show()

    # Call OpenAI API
    chat_response <- chatGPT_R(api_key, input$prompt, "gpt-3.5-turbo")
    historyALL$val <- chat_response

    new_entries <- data.frame(
      users = c("Human", "AI"),
      content = c(input$prompt, markdown::mark_html(text = chat_response)),
      stringsAsFactors = FALSE
    )
    historyALL$df <- rbind(historyALL$df, new_entries)

    updateTextAreaInput(session, "prompt", value = "")
    w$hide()

    session$sendCustomMessage(type = "jsCode", list(code = jscode))
  })

  output$chatThread <- renderUI({
    if (nrow(historyALL$df) == 0) {
      return(NULL)
    }

    conversations <- lapply(seq_len(nrow(historyALL$df)), function(i) {
      user_type <- historyALL$df$users[i]
      content <- historyALL$df$content[i]
      tags$div(
        class = ifelse(user_type == "Human", "user-message", "bot-message"),
        HTML(
          paste0(
            ifelse(user_type == "Human",
              "<div class='img-wrapper'><img src='girl.avif' class='img-wrapper2'></div>",
              "<div class='img-wrapper'><img src='boy.avif' class='img-wrapper2'></div>"
            ),
            content
          )
        )
      )
    })
    do.call(tagList, conversations)
  })

  observeEvent(input$remove_chatThread, {
    historyALL$df <- data.frame()
    historyALL$val <- character(0)
    output$chatThread <- renderUI({
      NULL
    })
  })

  observe({
    req(historyALL$val)
    shinyCopy2clipboard::CopyButtonUpdate(session,
      id = "clipbtn",
      label = "Copy Last Response",
      icon = icon("clipboard"),
      text = as.character(historyALL$val)
    )
  })
}

shinyApp(ui = ui, server = server)
