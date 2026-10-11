library(shiny)
library(bslib)
library(dplyr)
library(ranger)

pokemon_theme <- bs_theme(
  version = 5,
  bootswatch = "darkly",
  primary = "#E74C3C",
  secondary = "#3498DB",
  base_font = font_google("Montserrat"),
  heading_font = font_google("Press Start 2P")
)

ui <- page_fluid(
  theme = pokemon_theme,
  
  tags$head(
    tags$style(HTML("
      body { background-color: #121212; }
      .card { border-radius: 14px; box-shadow: 0 4px 15px rgba(0,0,0,0.5); }
      .btn-run { font-size: 1.1rem; padding: 10px; font-weight: 800; }
    "))
  ),
  
  titlePanel(
    div(
      h2("BLIND AI POKÉMON CLASSIFIER", style = "letter-spacing: 2px; text-shadow: 0 0 10px #E74C3C;"),
      p("Blind Test AI Engine • Evaluate Only Raw In-Game Mechanics", style = "color: #B0BEC5;")
    )
  ),
  
  layout_sidebar(
    sidebar = sidebar(
      title = "Input Mechanics & Controls",
      width = 340,
      
      # Prediction Trigger Button (for custom input suspense)
      actionButton("btn_run", "⚡ Evaluate Stats", class = "btn btn-danger btn-run w-100 mb-3 text-white"),
      
      # Preset Actions
      div(
        class = "d-grid gap-2 mb-3",
        actionButton("btn_random", "🎲 Load Random Mystery Mon", class = "btn btn-warning text-dark font-weight-bold"),
        actionButton("btn_reset", "🔄 Reset Default Sliders", class = "btn btn-outline-secondary btn-sm")
      ),
      
      hr(),
      # Mechanics
      numericInput("base_egg_steps", "Egg Steps (Breeding)", value = 30720, min = 1000, max = 40000, step = 1280),
      sliderInput("capture_rate", "Capture Rate (Catch Difficulty)", min = 3, max = 255, value = 3, step = 1),
      selectInput("is_genderless", "Is Genderless?", choices = c("Yes", "No"), selected = "Yes"),
      numericInput("base_happiness", "Base Happiness", value = 0, min = 0, max = 140, step = 5),
      
      hr(),
      # Combat Stats
      sliderInput("hp", "HP", min = 1, max = 255, value = 106),
      sliderInput("attack", "Attack", min = 5, max = 190, value = 110),
      sliderInput("defense", "Defense", min = 5, max = 230, value = 90),
      sliderInput("sp_attack", "Sp. Attack", min = 10, max = 194, value = 154),
      sliderInput("sp_defense", "Sp. Defense", min = 20, max = 230, value = 90),
      sliderInput("speed", "Speed", min = 5, max = 180, value = 130)
    ),
    
    # Results View
    layout_column_wrap(
      width = 1,
      
      # Verdict Card
      card(
        card_header("AI Assessment Output"),
        uiOutput("verdict_box"),
        br(),
        uiOutput("prob_progress")
      ),
      
      # Metric Profile & Identity Reveal
      card(
        card_header("Metric Profile & Reveal"),
        layout_column_wrap(
          width = 1/2,
          value_box(
            title = "Base Stat Total (BST)",
            value = textOutput("bst_value"),
            showcase = bsicons::bs_icon("lightning-charge-fill")
          ),
          value_box(
            title = "Capture Difficulty",
            value = textOutput("difficulty_tier"),
            showcase = bsicons::bs_icon("controller")
          )
        ),
        hr(),
        div(
          class = "text-center",
          actionButton("btn_reveal", "👁️ Reveal Pokémon Identity", class = "btn btn-info text-dark mb-2"),
          uiOutput("revealed_identity")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive state for mystery mode
  current_subject <- reactiveVal(list(
    name = "Custom Entry",
    actual_class = "Unknown"
  ))
  
  revealed <- reactiveVal(FALSE)
  
  # Trigger evaluation on button click OR when loading random mon
  trigger_eval <- reactiveVal(0)
  
  observeEvent(input$btn_run, {
    trigger_eval(trigger_eval() + 1)
  })
  
  # Event: Load Random Mon
  observeEvent(input$btn_random, {
    revealed(FALSE)
    
    random_row <- pokemon_ml %>% slice_sample(n = 1)
    
    current_subject(list(
      name = random_row$name,
      actual_class = as.character(random_row$is_legendary)
    ))
    
    # Update inputs
    updateNumericInput(session, "base_egg_steps", value = random_row$base_egg_steps)
    updateSliderInput(session, "capture_rate", value = random_row$capture_rate)
    updateSelectInput(session, "is_genderless", selected = as.character(random_row$is_genderless))
    updateNumericInput(session, "base_happiness", value = random_row$base_happiness)
    
    updateSliderInput(session, "hp", value = random_row$hp)
    updateSliderInput(session, "attack", value = random_row$attack)
    updateSliderInput(session, "defense", value = random_row$defense)
    updateSliderInput(session, "sp_attack", value = random_row$sp_attack)
    updateSliderInput(session, "sp_defense", value = random_row$sp_defense)
    updateSliderInput(session, "speed", value = random_row$speed)
    
    # Automatically evaluate the random subject
    trigger_eval(trigger_eval() + 1)
  })
  
  # Event: Reset Custom Inputs
  observeEvent(input$btn_reset, {
    revealed(FALSE)
    current_subject(list(name = "Custom Entry", actual_class = "Unknown"))
    
    updateNumericInput(session, "base_egg_steps", value = 5120)
    updateSliderInput(session, "capture_rate", value = 45)
    updateSelectInput(session, "is_genderless", selected = "No")
    updateNumericInput(session, "base_happiness", value = 70)
    
    updateSliderInput(session, "hp", value = 70)
    updateSliderInput(session, "attack", value = 80)
    updateSliderInput(session, "defense", value = 70)
    updateSliderInput(session, "sp_attack", value = 80)
    updateSliderInput(session, "sp_defense", value = 70)
    updateSliderInput(session, "speed", value = 75)
    
    trigger_eval(trigger_eval() + 1)
  })
  
  # Event: Reveal Identity
  observeEvent(input$btn_reveal, {
    revealed(TRUE)
  })
  
  output$revealed_identity <- renderUI({
    if (!revealed()) {
      p("Identity hidden! Press to reveal true name.", style = "color: #7F8C8D; font-style: italic;")
    } else {
      info <- current_subject()
      div(
        style = "background-color: #212529; padding: 12px; border-radius: 8px; border: 1px solid #495057;",
        h4(paste0("Subject: ", info$name), style = "color: #F1C40F; margin: 0; font-weight: bold;"),
        p(paste0("Actual In-Game Classification: ", ifelse(info$actual_class == "Yes", "Legendary", "Standard Pokémon")),
          style = "color: #CED4DA; margin: 4px 0 0 0;")
      )
    }
  })
  
  # Prediction model evaluated only when trigger fires
  model_results <- eventReactive(trigger_eval(), {
    sample_mon <- tibble(
      hp             = input$hp,
      attack         = input$attack,
      defense        = input$defense,
      sp_attack      = input$sp_attack,
      sp_defense     = input$sp_defense,
      speed          = input$speed,
      capture_rate   = input$capture_rate,
      base_egg_steps = input$base_egg_steps,
      base_happiness = input$base_happiness,
      is_genderless  = factor(input$is_genderless, levels = c("No", "Yes"))
    )
    
    prob <- predict(rf_model, data = sample_mon)$predictions[1, "Yes"]
    bst  <- sum(input$hp, input$attack, input$defense, input$sp_attack, input$sp_defense, input$speed)
    
    list(prob = prob, bst = bst)
  }, ignoreNULL = FALSE)
  
  output$bst_value <- renderText({
    paste0(model_results()$bst, " / 720")
  })
  
  output$difficulty_tier <- renderText({
    if (input$capture_rate <= 3) "Ultra Hard (Rate 3)"
    else if (input$capture_rate <= 45) "Rare (Rate <= 45)"
    else "Standard Encounter"
  })
  
  output$verdict_box <- renderUI({
    res <- model_results()
    is_leg <- res$prob >= 0.50
    
    color_bg <- if (is_leg) "#E74C3C" else "#3498DB"
    label    <- if (is_leg) "⭐ LEGENDARY TIER" else "STANDARD SPECIES"
    subtitle <- if (is_leg) "Metrics match Legendary profile" else "Metrics match Regular Pokémon profile"
    
    div(
      style = paste0("padding: 24px; border-radius: 12px; text-align: center; background-color: ", color_bg, ";"),
      h2(label, style = "color: white; font-weight: 900; margin-bottom: 8px;"),
      p(subtitle, style = "color: #F8F9FA; font-size: 1.05rem; margin: 0;")
    )
  })
  
  output$prob_progress <- renderUI({
    res <- model_results()
    pct <- round(res$prob * 100, 1)
    
    div(
      p(paste0("Model Confidence: ", pct, "%"), style = "font-weight: bold; margin-bottom: 6px;"),
      div(
        class = "progress",
        style = "height: 25px; background-color: #2C3E50; border-radius: 10px;",
        div(
          class = "progress-bar progress-bar-striped progress-bar-animated bg-danger",
          role = "progressbar",
          style = paste0("width: ", pct, "%; font-size: 0.9rem; font-weight: bold;"),
          paste0(pct, "%")
        )
      )
    )
  })
}

shinyApp(ui = ui, server = server)