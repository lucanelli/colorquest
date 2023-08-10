
###
library(shiny)
library(colourpicker)
library(colorBlindness)
library(ggplot2)

library(shiny)
library(colourpicker)
library(colorBlindness)
library(ggplot2)

ui <- fluidPage(
  titlePanel("🌈 Color Quest"),
  h3("Version 1.0"),
 
  div(class = "container", 
      sidebarLayout(
        sidebarPanel(
          uiOutput("logo"),
          br(),
          h4("Choose Your Colors"),
          p("Select up to 4 colors for exploration and visualization."),
          colourInput("col1", "Color 1:", value = "red"),
          colourInput("col2", "Color 2:", value = "darkturquoise"),
          colourInput("col3", "Color 3:", value = "darkgreen"),
          colourInput("col4", "Color 4:", value = "darkorange"),
          br(),
          div(class = "citation-box",
              tags$strong("For Citations:"),
              tags$em("Nelli (2023). \"Color Quest: An Interactive Tool for Exploring Color Palettes and Enhancing Visualization Accessibility.\" ",
                      tags$span("Journal of XXX"), ", XX(X), XXX-XXX."),
              br(),
              p(textOutput("counter"))
          )),
        mainPanel(
          h4("Welcome to Color Quest!"),
          p("This interactive tool, presented in", tags$a(ref="link to publication", "Nelli (2023),"),  "helps you choose perfect colors for visualizations and simulates how they appear to individuals with ", strong("color blindness"), "."),
          p("Select your preferred colors from the color pickers, and witness the magic of colorful exploration 
            across various plots. Discover the secret codes of your chosen colors and experience their enchanting 
            transformation through color-blind friendly visuals. Embrace accessibility and creativity in one 
            fascinating journey!"),
          p("Explore the realms of art and data science, where colors weave the tale of your visual journey. The legends of your chosen colors will be revealed below, in all their hexadecimal glory:"),
          verbatimTextOutput("color_codes"),
          
          tabsetPanel(
            id = "plots_tabset",
            
            tabPanel("Scatter Plot", 
                     br(),
                     fluidRow(
                       column(width = 6, plotOutput("scatter_plot", height = "300px")),
                       column(width = 6, plotOutput("scatter_plot_deuteranopia", height = "300px"))
                     ),
                     br(),
                     fluidRow(
                       column(width = 6, plotOutput("scatter_plot_protanopia", height = "300px")),
                       column(width = 6, plotOutput("scatter_plot_desaturated", height = "300px"))
                     )
                     
            ),
          
      
            tabPanel("Line Plot", 
                     br(),
                     fluidRow(
                       column(width = 6, plotOutput("line_plot", height = "300px")),
                       column(width = 6, plotOutput("line_plot_deuteranopia", height = "300px"))
                     ),
                     br(),
                     fluidRow(
                       column(width = 6, plotOutput("line_plot_protanopia", height = "300px")),
                       column(width = 6, plotOutput("line_plot_desaturated", height = "300px"))
                     )
                     
            ),
 
            tabPanel("Box Plot", 
                     br(),
                     fluidRow(
                       column(width = 6, plotOutput("boxplot", height = "300px")),
                       column(width = 6, plotOutput("boxplot_deuteranopia", height = "300px"))
                     ),
                     br(),
                     fluidRow(
                       column(width = 6, plotOutput("boxplot_protanopia", height = "300px")),
                       column(width = 6, plotOutput("boxplot_desaturated", height = "300px"))
                     )
                     
            ),
            
            
            tabPanel("Histogram", 
                     br(),
                     column(width = 12, plotOutput("histogram", height = "300px")) ,
                     br(),
                     br(),
                     column(width = 12, plotOutput("histogram_deuteranopia", height = "300px")),
                     br(),
                     br(),
                     column(width = 12, plotOutput("histogram_protanopia", height = "300px")),
                     br(),
                     br(),
                     column(width = 12, plotOutput("histogram_desaturated", height = "300px"))
                     
            )
            
          ),
          p("Congratulations! You've embarked on a Color Quest! 
            The mystical color pickers above hold the key to a world of wonders.
            Each visualization you encounter is imbued with the essence of your chosen colors! 
            Marvel at the scatter plot, delve into the histogram, dance with the box plot, 
            and follow the lines of the line plot, all painted in the vivid hues of your imagination!"),
          div(class = "legend"),
          
          
          div(class = "contact",
              h3("Contact me:"),
              p("If you have any suggestions, questions, or need assistance, please feel free to write me at",
                a("luca.nelli@glasgow.ac.uk", href = "mailto:luca.nelli@glasgow.ac.uk"), "."))
        )
      )
  )
      )



server <- function(input, output) {
  # Render the logo based on the size of the window
  output$logo <- renderUI({
    img(src = "logo.png", height = "auto", width = "100%")  # Logo will resize automatically
  })
  
  output$counter <- 
    renderText({
      if (!file.exists("counter.Rdata")) 
        counter <- 0
      else
        load(file="counter.Rdata")
      counter  <- counter + 1
      save(counter, file="counter.Rdata")     
      paste("You are visitor number: ", counter)
    })
  
  random_data <- reactive({
    data.frame(
      x = c(rnorm(50, 0.5, 0.1), rnorm(50, 1, 0.2), rnorm(50, 1.5, 0.2), rnorm(50, 2, 0.2)),
      y = c(rnorm(50, 1.5, 0.1), rnorm(50, 0.8, 0.2), rnorm(50, 0.4, 0.2), rnorm(50, 1.5, 0.2)),
      col = c(rep(input$col1, 50),rep(input$col2, 50), rep(input$col3, 50), rep(input$col4, 50))
    )
  })
  
  # scatter plot
  
  scatter_plot <- reactive({
    
    ggplot(data = random_data()) +
      geom_point(mapping = aes(x = x, y = y, colour = col), size=1.8) +
      scale_color_identity(guide = "legend",limits = c(input$col1, input$col2, input$col3, input$col4)) +
      labs(color = "")
    
  })
  
  
  output$scatter_plot <- renderPlot({
    cvdPlot(plot = scatter_plot(), layout = "origin") 
  })
  
  output$scatter_plot_deuteranopia <- renderPlot({
    cvdPlot(plot = scatter_plot(), layout = "deuteranope") 
  })
  
  output$scatter_plot_protanopia <- renderPlot({
    cvdPlot(plot = scatter_plot(), layout = "protanope") 
  })
  
  output$scatter_plot_desaturated <- renderPlot({
    cvdPlot(plot = scatter_plot(), layout = "desaturate") 
  })
  
  
  
  # histogram
  
  histogram_plot <- reactive({
    ggplot(data = random_data()) +
      geom_histogram(mapping = aes(x = x, fill = col, color = "black"), position = "dodge", bins = 10) +
      scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4), 
                        labels = c(input$col1, input$col2, input$col3, input$col4)) +
      scale_color_identity() +
      labs(fill = "") +
      facet_wrap(~ col, scales = "free")
  })
  
  
  output$histogram <- renderPlot({
    cvdPlot(plot = histogram_plot(), layout = "origin") 
  })
  
  output$histogram_deuteranopia <- renderPlot({
    cvdPlot(plot = histogram_plot(), layout = "deuteranope") 
  })
  
  output$histogram_protanopia <- renderPlot({
    cvdPlot(plot = histogram_plot(), layout = "protanope") 
  })
  
  output$histogram_desaturated <- renderPlot({
    cvdPlot(plot = histogram_plot(), layout = "desaturate") 
  })
  
  # BOXPLOT
  
  boxplot_plot <- reactive({
    ggplot(data = random_data(), aes(x = col, y = x, fill = col)) +
      geom_boxplot() +
      scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4),
                        labels = c(input$col1, input$col2, input$col3, input$col4)) +
      labs(fill = "") 
    
  })
  
  
  output$boxplot <- renderPlot({
    cvdPlot(plot = boxplot_plot(), layout = "origin") 
  })
  
  output$boxplot_deuteranopia <- renderPlot({
    cvdPlot(plot = boxplot_plot(), layout = "deuteranope") 
  })
  
  output$boxplot_protanopia <- renderPlot({
    cvdPlot(plot = boxplot_plot(), layout = "protanope") 
  })
  
  output$boxplot_desaturated <- renderPlot({
    cvdPlot(plot = boxplot_plot(), layout = "desaturate") 
  })
  
  
  
  
  # LINES
  random_data.L <- reactive({
    data.frame(
      x = rep(seq(-3, 3, length.out = 20), 4),  # Creating x-values
      col = rep(c(input$col1, input$col2, input$col3, input$col4), each = 20),
      y = ifelse(rep(c(input$col1, input$col2, input$col3, input$col4), each = 20) %in% c(input$col1, input$col2),
                 0.9 * rep(seq(-3, 3, length.out = 20), 2), -0.9 * rep(seq(-3, 3, length.out = 20), 2)) +
        scale(arima.sim(list(ar = c(0.1)), n = 80))
    )
  })
  
  line_plot <- reactive({
    ggplot(data = random_data.L()) +
      geom_ribbon(aes(x = x, ymin = y - runif(1, 0.3, 0.6), ymax = y + runif(1, 0.3, 0.6), fill = col), alpha = 0.4, colour = NA) +
      geom_line(mapping = aes(x = x, y = y, colour = col), lwd = 1.2) +
      scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4),
                         labels = c(input$col1, input$col2, input$col3, input$col4)) +
      scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4),
                        labels = c(input$col1, input$col2, input$col3, input$col4)) +
      labs(color = "") +
      guides(fill = guide_legend(title = ""))
  })
  
  output$line_plot <- renderPlot({
    cvdPlot(plot = line_plot(), layout = "origin") 
  })
  
  output$line_plot_deuteranopia <- renderPlot({
    cvdPlot(plot = line_plot(), layout = "deuteranope") 
  })
  
  output$line_plot_protanopia <- renderPlot({
    cvdPlot(plot = line_plot(), layout = "protanope") 
  })
  
  output$line_plot_desaturated <- renderPlot({
    cvdPlot(plot = line_plot(), layout = "desaturate") 
  })
  
  
  # Displaying color codes
  output$color_codes <- renderText({
    paste(" Color 1: ", input$col1, "\n",
          "Color 2: ", input$col2, "\n",
          "Color 3: ", input$col3, "\n",
          "Color 4: ", input$col4)
  })
}

shinyApp(ui = ui, server = server, options = list(name = "Color Finder App"))
