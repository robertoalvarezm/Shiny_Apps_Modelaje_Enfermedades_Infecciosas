library(shiny)
library(deSolve)
library(ggplot2)
library(bslib)

# Define the models
si_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I
    dI <- beta * S * I
    return(list(c(dS, dI)))
  })
}

sis_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I + gamma * I
    dI <- beta * S * I - gamma * I
    return(list(c(dS, dI)))
  })
}

sir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I
    dI <- beta * S * I - gamma * I
    dR <- gamma * I
    return(list(c(dS, dI, dR)))
  })
}

# UI
ui <- page_navbar(
  title = "Modelos básicos por compartimentos",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  nav_panel("SI", 
            layout_sidebar(
              sidebar = sidebar(
                sliderInput("si_beta", "Transmission rate (β)", 0.001, 1, 0.3, step = 0.001),
                sliderInput("si_I0", "Proporción inicial infectada", 0.001, 0.1, 0.01, step = 0.001),
                sliderInput("si_tmax", "Rango de tiempo", 1, 100, 50)
              ),
              plotOutput("si_plot")
            )
  ),
  
  nav_panel("SIS ",
            layout_sidebar(
              sidebar = sidebar(
                sliderInput("sis_beta", "Transmission rate (β)", 0.001, 1, 0.3, step = 0.001),
                sliderInput("sis_gamma", "Tasa de recuperación (γ)", 0.001, 1, 0.1, step = 0.001),
                sliderInput("sis_I0", "Proporción inicial infectada", 0.001, 0.1, 0.01, step = 0.001),
                sliderInput("sis_tmax", "Rango de tiempo", 1, 100, 50)
              ),
              plotOutput("sis_plot")
            )
  ),
  
  nav_panel("SIR ",
            layout_sidebar(
              sidebar = sidebar(
                sliderInput("sir_beta", "Transmission rate (β)", 0.001, 1, 0.3, step = 0.001),
                sliderInput("sir_gamma", "Tasa de recuperación (γ)", 0.001, 1, 0.1, step = 0.001),
                sliderInput("sir_I0", "Proporción inicial infectada", 0.001, 0.1, 0.01, step = 0.001),
                sliderInput("sir_tmax", "Rango de tiempo", 1, 100, 50)
              ),
              plotOutput("sir_plot")
            )
  ),
  
  nav_panel("Ecuaciones diferenciales",
            card(
              card_header("Ecuaciones de los modelos"),
              card_body(
                h4("Modelo SI :"),
                withMathJax("$$\\frac{dS}{dt} = -\\beta S I$$"),
                withMathJax("$$\\frac{dI}{dt} = \\beta S I$$"),
                hr(),
                h4("Model SIS:"),
                withMathJax("$$\\frac{dS}{dt} = -\\beta S I + \\gamma I$$"),
                withMathJax("$$\\frac{dI}{dt} = \\beta S I - \\gamma I$$"),
                hr(),
                h4("Model SIR :"),
                withMathJax("$$\\frac{dS}{dt} = -\\beta S I$$"),
                withMathJax("$$\\frac{dI}{dt} = \\beta S I - \\gamma I$$"),
                withMathJax("$$\\frac{dR}{dt} = \\gamma I$$")
              )
            )
  )
)

# Server
server <- function(input, output) {
  
  output$si_plot <- renderPlot({
    parameters <- c(beta = input$si_beta)
    initial_state <- c(S = 1 - input$si_I0, I = input$si_I0)
    times <- seq(0, input$si_tmax, by = 0.1)
    
    out <- ode(y = initial_state, times = times, func = si_model, parms = parameters)
    df <- as.data.frame(out)
    
    ggplot(df, aes(x = time)) +
      geom_line(aes(y = S, color = "Susceptibles")) +
      geom_line(aes(y = I, color = "Infectados")) +
      labs(title = "SI Model", x = "Tiempo", y = "Proporción", color = "Compartimento") +
      theme_minimal() +
      scale_color_manual(values = c("Susceptibles" = "blue", "Infectados" = "red"))
  })
  
  output$sis_plot <- renderPlot({
    parameters <- c(beta = input$sis_beta, gamma = input$sis_gamma)
    initial_state <- c(S = 1 - input$sis_I0, I = input$sis_I0)
    times <- seq(0, input$sis_tmax, by = 0.1)
    
    out <- ode(y = initial_state, times = times, func = sis_model, parms = parameters)
    df <- as.data.frame(out)
    
    ggplot(df, aes(x = time)) +
      geom_line(aes(y = S, color = "Susceptibles")) +
      geom_line(aes(y = I, color = "Infectados")) +
      labs(title = "SIS ", x = "Tiempo", y = "Proporción", color = "Compartimento") +
      theme_minimal() +
      scale_color_manual(values = c("Susceptibles" = "blue", "Infectados" = "red"))
  })
  
  output$sir_plot <- renderPlot({
    parameters <- c(beta = input$sir_beta, gamma = input$sir_gamma)
    initial_state <- c(S = 1 - input$sir_I0, I = input$sir_I0, R = 0)
    times <- seq(0, input$sir_tmax, by = 0.1)
    
    out <- ode(y = initial_state, times = times, func = sir_model, parms = parameters)
    df <- as.data.frame(out)
    
    ggplot(df, aes(x = time)) +
      geom_line(aes(y = S, color = "Susceptibles")) +
      geom_line(aes(y = I, color = "Infectados")) +
      geom_line(aes(y = R, color = "Recuperados")) +
      labs(title = "SIR Model", x = "Tiempo", y = "Proporción", color = "Compartimento") +
      theme_minimal() +
      scale_color_manual(values = c("Susceptibles" = "blue", "Infectados" = "red", "Recuperados" = "green"))
  })
}

# Run the app
shinyApp(ui = ui, server = server)