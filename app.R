
library(shiny)
library(colourpicker)
library(colorBlindness)
library(ggplot2)
library(terra)
library(KernSmooth)
library(tidyterra)
library(grid)
library(jpeg)
library(shinycssloaders)



environ <- function(d, x, bw, mini, maxi) {
  cox <- cbind(runif(x, min = 1, max = d), runif(x, min = 1, max = d))
  smooth <- bkde2D(cox, bandwidth = c(bw, bw), gridsize = c(d, d), range.x = list(c(1, d), c(1, d)))
  layer <- smooth$fhat
  minl <- min(layer)
  maxl <- max(layer)
  layer <- ((layer - minl) / (maxl - minl)) * (maxi - mini) + mini
  return(layer)
}


d <- max(60, 60)
rng <- (d + 1):(2 * d)


ui <- fluidPage(
  titlePanel("🌈 Color Quest"),
  h3("Version 1.0"),
  p(textOutput("counter")),
  
  div(class = "container", 
      sidebarLayout(
        sidebarPanel(
          uiOutput("logo"),
          br(),
          h4("Choose Your Colors"),
          p("Select up to 4 colors for exploration and visualization."),
          colourInput("col1", "Color 1:", value = "red"),
          colourInput("col2", "Color 2:", value = "yellow"),
          colourInput("col3", "Color 3:", value = "green"),
          colourInput("col4", "Color 4:", value = "darkblue"),
          br()
        ),
        mainPanel(
          h4("Welcome to Color Quest!"),
          p("This interactive tool, presented in", tags$a(ref="link to publication", "Nelli (2023),"),  "helps you choose perfect colors for visualizations and simulates how they appear to individuals with ", strong("color blindness"), "."),
          p("Embrace the power of accessible and creative data visualization with Color Quest. Choose your colors, explore the plots, and gain insights into the world of color harmony and inclusivity."),
          
          
          HTML("
<ol>
  <li>Use the color pickers in the left sidebar to select up to four colors. These colors will define your custom palettes and will be used across the app's visualizations.</li>
  <li>Navigate through the tabs such as 'Scatter Plot,' 'Line Plot,' 'Box Plot,' 'Histogram,' and 'Heatmap.' Each tab offers different visualization types to showcase your chosen colors.</li>
  <li>Discover how your color choices impact visualizations with the added benefit of simulating how these visuals appear to people with color vision deficiencies.</li>
  <li>In the <b>'Test your plot'</b> tab, you can submit your own JPG or PNG image. <u>Rest assured that your uploaded images will not be saved on the server, ensuring your security and privacy!</u>
               The app will then generate color-blindness simulations of the image using your selected colors.</li>
  <li>Explore the dynamic visualizations to assess the clarity and accessibility of your chosen color palettes for various types of data representations. </li>
</ol>
"),
          
          
          # verbatimTextOutput("color_codes"),
          
          tabsetPanel(
            id = "plots_tabset",
            
            tabPanel("Scatter Plot", 
                     br(),
                     fluidRow(
                       column(width = 6, withSpinner(plotOutput("scatter_plot", height = "300px"))),
                       column(width = 6, withSpinner(plotOutput("scatter_plot_deuteranopia", height = "300px")))
                     ),
                     br(),
                     fluidRow(
                       column(width = 6, withSpinner(plotOutput("scatter_plot_protanopia", height = "300px"))),
                       column(width = 6, withSpinner(plotOutput("scatter_plot_desaturated", height = "300px")))
                     )
                     
            ),
            
            
            tabPanel("Line Plot", 
                     br(),
                     fluidRow(
                       column(width = 6, withSpinner(plotOutput("line_plot", height = "300px"))),
                       column(width = 6, withSpinner(plotOutput("line_plot_deuteranopia", height = "300px")))
                     ),
                     br(),
                     fluidRow(
                       column(width = 6, withSpinner(plotOutput("line_plot_protanopia", height = "300px"))),
                       column(width = 6, withSpinner(plotOutput("line_plot_desaturated", height = "300px")))
                     )
                     
            ),
            
            tabPanel("Box Plot", 
                     br(),
                     fluidRow(
                       column(width = 6, withSpinner(plotOutput("boxplot", height = "300px"))),
                       column(width = 6, withSpinner(plotOutput("boxplot_deuteranopia", height = "300px")))
                     ),
                     br(),
                     fluidRow(
                       column(width = 6, withSpinner(plotOutput("boxplot_protanopia", height = "300px"))),
                       column(width = 6, withSpinner(plotOutput("boxplot_desaturated", height = "300px")))
                     )
                     
            ),
            
            
            tabPanel("Histogram", 
                     br(),
                     column(width = 12, withSpinner(plotOutput("histogram", height = "300px"))) ,
                     br(),
                     br(),
                     column(width = 12, withSpinner(plotOutput("histogram_deuteranopia", height = "300px"))),
                     br(),
                     br(),
                     column(width = 12, withSpinner(plotOutput("histogram_protanopia", height = "300px"))),
                     br(),
                     br(),
                     column(width = 12, withSpinner(plotOutput("histogram_desaturated", height = "300px")))
                     
            ),
            tabPanel("Heatmap", 
                     br(),
                     fluidRow(
                       column(width = 6, withSpinner(plotOutput("heatmap", height = "300px"))),
                       column(width = 6, withSpinner(plotOutput("heatmap_deuteranopia", height = "300px"))),
                       column(width = 6, withSpinner(plotOutput("heatmap_protanopia", height = "300px"))),
                       column(width = 6, withSpinner(plotOutput("heatmap_desaturated", height = "300px")))
                     )
                     
            ),
            
            tabPanel(HTML("<b>Test your plot!</b>"),
                     br(),
                     p("Upload your image, and see how it appears for individuals with color-blindness"),
                     fileInput("image_upload", "Browse (jpg or png)"),
                     br(),
                   
                       column(width = 12, withSpinner(plotOutput("uploaded_image_plot", height = "600px"))),
                       column(width = 12, withSpinner(plotOutput("uploaded_image_plot_deuteranopia", height = "600px")))
                     ,
                     br(),
                    
                       column(width = 12, withSpinner(plotOutput("uploaded_image_plot_protanopia", height = "600px"))),
                       column(width = 12, withSpinner(plotOutput("uploaded_image_plot_desaturated", height = "600px")))
                     
            )
            
          ),
          
          
          
          div(class = "contact",
              h3("Contact me:"),
              p("If you have any suggestions, questions, or need assistance, please feel free to write me at",
                a("luca.nelli@glasgow.ac.uk", href = "mailto:luca.nelli@glasgow.ac.uk"), ".")),
          
          div(class = "citation-box",
              style = "border: 1px solid #ddd; padding: 10px;",
              tags$strong("Suggested Citation:"),
              p("Color-blindness friendly plots were generated using ColorQuest software (Nelli, 2023)"),
              tags$em("Nelli (2023). \"Color Quest: An Interactive Tool for Exploring Color Palettes and Enhancing Visualization Accessibility.\" ",
                      tags$span("Journal of XXX"), ", XX(X), XXX-XXX.")
          )
          
          
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
      geom_line(mapping = aes(x = x, y = y, colour = col), linewidth = 1.2) +
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
  
  
  
  
  
  # Heatmap
  
  
  smooth_surface <-  reactive({
    rast(scales::rescale(environ(3 * d, 2000, 10, 0, 1)[rng, rng]))
  })
  
  heatmap <- reactive({
    
    ggplot() +
      geom_spatraster(data= smooth_surface()) +
      scale_fill_gradientn(
        colors = rev(c(input$col1, input$col2, input$col3, input$col4)),
        breaks = rev(c(0.01, 0.333, 0.666, 0.99)),
        labels = rev(c(input$col1, input$col2, input$col3, input$col4)),
        guide = guide_colorbar(title = "")
      ) +
      labs(title = "", x = "", y = "") +
      theme_minimal()
    
  })
  
  output$heatmap <- renderPlot({
    cvdPlot(plot = heatmap(), layout = "origin") 
  })
  
  output$heatmap_deuteranopia <- renderPlot({
    cvdPlot(plot = heatmap(), layout = "deuteranope") 
  })
  
  output$heatmap_protanopia <- renderPlot({
    cvdPlot(plot = heatmap(), layout = "protanope") 
  })
  
  output$heatmap_desaturated <- renderPlot({
    cvdPlot(plot = heatmap(), layout = "desaturate") 
  })
  
  
  # IMAGE UPLOAD 
  
  uploaded_image_plot_reactive <- reactive({
    req(input$image_upload)
    
    if (grepl("\\.jpg$", input$image_upload$name, ignore.case = TRUE)) {
      img <- jpeg::readJPEG(input$image_upload$datapath)
    } else if (grepl("\\.(png|PNG)$", input$image_upload$name)) {
      img <- png::readPNG(input$image_upload$datapath)
    } else {
      return(NULL)  # Unsupported format
    }
    
    img_plot <- rasterGrob(img, interpolate = TRUE)
    
    cvdPlot(plot = img_plot, layout = "origin")
  })
  
  output$uploaded_image_plot <- renderPlot({
    uploaded_image_plot_reactive()
  })
  
  
  uploaded_image_plot_reactive_deuteranopia <- reactive({
    req(input$image_upload)
    
    if (grepl("\\.jpg$", input$image_upload$name, ignore.case = TRUE)) {
      img <- jpeg::readJPEG(input$image_upload$datapath)
    } else if (grepl("\\.(png|PNG)$", input$image_upload$name)) {
      img <- png::readPNG(input$image_upload$datapath)
    } else {
      return(NULL)  # Unsupported format
    }
    
    img_plot <- rasterGrob(img, interpolate = TRUE)
    
    cvdPlot(plot = img_plot, layout = "deuteranope")
  })
  
  
  output$uploaded_image_plot_deuteranopia <- renderPlot({
    uploaded_image_plot_reactive_deuteranopia()
  })
  
  
  uploaded_image_plot_reactive_protanopia <- reactive({
    req(input$image_upload)
    
    if (grepl("\\.jpg$", input$image_upload$name, ignore.case = TRUE)) {
      img <- jpeg::readJPEG(input$image_upload$datapath)
    } else if (grepl("\\.(png|PNG)$", input$image_upload$name)) {
      img <- png::readPNG(input$image_upload$datapath)
    } else {
      return(NULL)  # Unsupported format
    }
    
    img_plot <- rasterGrob(img, interpolate = TRUE)
    
    cvdPlot(plot = img_plot, layout = "protanope")
  })
  
  output$uploaded_image_plot_protanopia <- renderPlot({
    uploaded_image_plot_reactive_protanopia()
  })
  
  uploaded_image_plot_reactive_desaturated <- reactive({
    req(input$image_upload)
    
    if (grepl("\\.jpg$", input$image_upload$name, ignore.case = TRUE)) {
      img <- jpeg::readJPEG(input$image_upload$datapath)
    } else if (grepl("\\.(png|PNG)$", input$image_upload$name)) {
      img <- png::readPNG(input$image_upload$datapath)
    } else {
      return(NULL)  # Unsupported format
    }
    
    img_plot <- rasterGrob(img, interpolate = TRUE)
    
    cvdPlot(plot = img_plot, layout = "desaturate")
  })
  
  output$uploaded_image_plot_desaturated <- renderPlot({
    uploaded_image_plot_reactive_desaturated()
  })
  
  outputOptions(output, "uploaded_image_plot", suspendWhenHidden = FALSE)
  outputOptions(output, "uploaded_image_plot_deuteranopia", suspendWhenHidden = FALSE)
  outputOptions(output, "uploaded_image_plot_protanopia", suspendWhenHidden = FALSE)
  outputOptions(output, "uploaded_image_plot_desaturated", suspendWhenHidden = FALSE)
  
  observeEvent(input$image_upload, {
    shinyjs::enable("uploaded_image_spinner")
  })
  
  
  
  # Displaying color codes
  # output$color_codes <- renderText({
  #   paste(" Color 1: ", input$col1, "\n",
  #         "Color 2: ", input$col2, "\n",
  #         "Color 3: ", input$col3, "\n",
  #         "Color 4: ", input$col4)
  # })
}

shinyApp(ui = ui, server = server, options = list(name = "Color Finder App"))
