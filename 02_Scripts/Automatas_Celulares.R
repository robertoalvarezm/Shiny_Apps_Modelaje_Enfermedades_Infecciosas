library(shiny)
library(ggplot2)
library(bslib)

# Function to generate the next generation based on the rule
generate_next_generation <- function(current_gen, rule) {
  rule_binary <- intToBits(rule)[1:8]
  next_gen <- numeric(length(current_gen))
  
  for (i in 1:length(current_gen)) {
    left <- if (i == 1) current_gen[length(current_gen)] else current_gen[i-1]
    center <- current_gen[i]
    right <- if (i == length(current_gen)) current_gen[1] else current_gen[i+1]
    
    pattern <- 4*left + 2*center + right + 1
    next_gen[i] <- as.numeric(rule_binary[pattern])
  }
  
  return(next_gen)
}

# Function to generate all generations
generate_automaton <- function(initial_state, rule, generations) {
  automaton <- matrix(0, nrow = generations, ncol = length(initial_state))
  automaton[1,] <- initial_state
  
  for (i in 2:generations) {
    automaton[i,] <- generate_next_generation(automaton[i-1,], rule)
  }
  
  return(automaton)
}

# UI
ui <- page_sidebar(
  title = "1-D Autómatas celulares",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  sidebar = sidebar(
    sliderInput("rule", "Regla (0-255)", min = 0, max = 255, value = 30, step = 1),
    sliderInput("generations", "Número de generaciones", min = 10, max = 200, value = 50, step = 10),
    numericInput("cells", "Número de celdas", min = 10, max = 200, value = 100),
    selectInput("initial_state", "Estado inicial", 
                choices = c("Una celda ocupada", "Aleatorio", "Todxs negrxs"), 
                selected = "Single Cell"),
    actionButton("generate", "Correr autómata")
  ),
  
  plotOutput("automaton_plot"),
  
  card(
    card_header("Info"),
    card_body(
      "Esta aplicación muestra un autómata celular unidimensional basado en las reglas 0-255.  
Selecciona un número de regla, define el número de generaciones y células,  
elige un estado inicial y haz clic en 'Correr autómata' para visualizar el patrón.  
El autómata tiene  condiciones de frontera circulares.

Elaborado por Roberto Álvarez, 2024."
    )
  )
)

# Server
server <- function(input, output, session) {
  
  automaton_data <- reactiveVal(NULL)
  
  observeEvent(input$generate, {
    initial_state <- switch(input$initial_state,
                            "Una celda ocupada" = c(rep(0, floor(input$cells/2)), 1, rep(0, ceiling(input$cells/2) - 1)),
                            "Aleatorio" = sample(c(0, 1), input$cells, replace = TRUE),
                            "Todxs negrxs" = rep(1, input$cells))
    
    automaton <- generate_automaton(initial_state, input$rule, input$generations)
    automaton_data(automaton)
  })
  
  output$automaton_plot <- renderPlot({
    req(automaton_data())
    
    df <- expand.grid(generation = 1:nrow(automaton_data()), 
                      cell = 1:ncol(automaton_data()))
    df$state <- as.vector(automaton_data())
    
    ggplot(df, aes(x = cell, y = generation, fill = factor(state))) +
      geom_tile() +
      scale_fill_manual(values = c("0" = "white", "1" = "black")) +
      scale_y_reverse() +
      labs(title = paste("1-D Autómata celular - Regla", input$rule),
           x = "Celdas", y = "Generacion") +
      theme_minimal() +
      theme(legend.position = "none",
            panel.grid = element_blank(),
            aspect.ratio = 1)
  })
}

# Run the app
shinyApp(ui = ui, server = server)