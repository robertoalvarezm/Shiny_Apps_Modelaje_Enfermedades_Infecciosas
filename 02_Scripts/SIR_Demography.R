library(shiny)
library(ggplot2)
library(bslib)
library(deSolve)

# SIR model with demography
sir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- mu * N - beta * S * I / N - mu * S
    dI <- beta * S * I / N - gamma * I - mu * I
    dR <- gamma * I - mu * R
    return(list(c(dS, dI, dR)))
  })
}

# UI
ui <- page_navbar(
  title = "SIR Model with Demography",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  nav_panel(
    title = "Simulation",
    page_sidebar(
      sidebar = sidebar(
        sliderInput("beta", "Transmission rate (β)", 0.1, 1, 0.3, step = 0.05),
        sliderInput("gamma", "Recovery rate (γ)", 0.01, 0.5, 0.1, step = 0.01),
        sliderInput("mu", "Birth/Death rate (μ)", 0.001, 0.1, 0.01, step = 0.001),
        sliderInput("N", "Total population (N)", 1000, 10000, 1000, step = 1000),
        sliderInput("I0", "Initial infected (I₀)", 1, 100, 10, step = 1),
        sliderInput("time", "Simulation time", 100, 1000, 365, step = 100),
        actionButton("run", "Run Simulation")
      ),
      card(
        card_header("SIR Model Plots"),
        card_body(
          plotOutput("separate_plot"),
          plotOutput("combined_plot"),
          plotOutput("phase_plot")
        )
      )
    )
  ),
  
  nav_panel(
    title = "Model Equations",
    card(
      card_header("SIR Model with Demography - Differential Equations"),
      card_body(
        withMathJax(
          "$$\\frac{dS}{dt} = \\mu N - \\beta \\frac{SI}{N} - \\mu S$$",
          "$$\\frac{dI}{dt} = \\beta \\frac{SI}{N} - \\gamma I - \\mu I$$",
          "$$\\frac{dR}{dt} = \\gamma I - \\mu R$$",
          "Where:",
          "\\(S\\) is the number of susceptible individuals",
          "\\(I\\) is the number of infected individuals",
          "\\(R\\) is the number of recovered individuals",
          "\\(N\\) is the total population (\\(N = S + I + R\\))",
          "\\(\\beta\\) is the transmission rate",
          "\\(\\gamma\\) is the recovery rate",
          "\\(\\mu\\) is the birth/death rate"
        )
      )
    )
  ),
  
  nav_panel(
    title = "About",
    card(
      card_header("About"),
      card_body(
        "This app simulates the SIR (Susceptible, Infected, Recovered) model with demography. 
        Adjust the parameters and click 'Run Simulation' to see the results. 
        The plots show the dynamics of S, I, and R over time, both separately and combined, 
        as well as a phase plot to visualize the spiral attractor. 
        The 'Model Equations' tab displays the differential equations governing the model."
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  sim_data <- reactiveVal(NULL)
  
  observeEvent(input$run, {
    parameters <- c(
      beta = input$beta,
      gamma = input$gamma,
      mu = input$mu,
      N = input$N
    )
    
    initial_state <- c(
      S = input$N - input$I0,
      I = input$I0,
      R = 0
    )
    
    times <- seq(0, input$time, by = 0.1)
    
    out <- ode(y = initial_state, times = times, func = sir_model, parms = parameters)
    sim_data(as.data.frame(out))
  })
  
  output$separate_plot <- renderPlot({
    req(sim_data())
    df <- sim_data()
    df_long <- tidyr::pivot_longer(df, cols = c("S", "I", "R"), names_to = "Variable", values_to = "Value")
    
    ggplot(df_long, aes(x = time, y = Value, color = Variable)) +
      geom_line() +
      scale_color_manual(values = c("S" = "blue", "I" = "red", "R" = "green")) +
      labs(title = "SIR Model - Separate Variables", x = "Time", y = "Population") +
      theme_minimal()
  })
  
  output$combined_plot <- renderPlot({
    req(sim_data())
    df <- sim_data()
    
    ggplot(df) +
      geom_line(aes(x = time, y = S, color = "Susceptible")) +
      geom_line(aes(x = time, y = I, color = "Infected")) +
      geom_line(aes(x = time, y = R, color = "Recovered")) +
      scale_color_manual(values = c("Susceptible" = "blue", "Infected" = "red", "Recovered" = "green")) +
      labs(title = "SIR Model - Combined Variables", x = "Time", y = "Population") +
      theme_minimal()
  })
  
  output$phase_plot <- renderPlot({
    req(sim_data())
    df <- sim_data()
    
    ggplot(df, aes(x = S, y = I)) +
      geom_path(color = "purple") +
      geom_point(data = df[1,], aes(x = S, y = I), color = "red", size = 3) +
      labs(title = "SIR Model - Phase Plot (Spiral Attractor)", x = "Susceptible", y = "Infected") +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
