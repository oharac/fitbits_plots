#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
library(cowplot)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("FITBITS examples"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            includeMarkdown('instructions.Md'),
            h4('Species A'),
            textInput("sppA_name", label = NULL,
                      value = 'Triceratops'),
            textInput("sppA_coords", label = NULL,
                      value = '(0.25, 0.50, 0.75)'),
            h4('Species B'),
            textInput("sppB_name", label = NULL,
                      value = '(name)'),
            textInput("sppB_coords", label = NULL,
                      value = '(U, M, NCP)'),
            h4('Species C'),
            textInput("sppC_name", label = NULL,
                      value = '(name)'),
            textInput("sppC_coords", label = NULL,
                      value = '(U, M, NCP)'), 
            h4('Species D'),
            textInput("sppD_name", label = NULL,
                      value = '(name)'),
            textInput("sppD_coords", label = NULL,
                      value = '(U, M, NCP)'),
            h4('Species E'),
            textInput("sppE_name", label = NULL,
                      value = '(name)'),
            textInput("sppE_coords", label = NULL,
                      value = '(U, M, NCP)')
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("ncp2dPlot"),
            plotlyOutput("ncp3dPlotly")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    spp_df <- reactive({
        message('in spp_df()')
        input_df <- data.frame(spp_id = c(input$sppA_name,
                                          input$sppB_name,
                                          input$sppC_name,
                                          input$sppD_name,
                                          input$sppE_name),
                               coords = c(input$sppA_coords,
                                          input$sppB_coords,
                                          input$sppC_coords,
                                          input$sppD_coords,
                                          input$sppE_coords)) %>%
            rowwise() %>%
            mutate(coord_extract = str_split(coords, ','),
                   u   = coord_extract[[1]], 
                   m   = ifelse(length(coord_extract) > 1, coord_extract[[2]], NA),
                   ncp = ifelse(length(coord_extract) > 2, coord_extract[[3]], NA)) %>%
            mutate(u   = as.numeric(str_extract(u, '[0-9\\.]+')),
                   m   = as.numeric(str_extract(m, '[0-9\\.]+')),
                   ncp = as.numeric(str_extract(ncp, '[0-9\\.]+'))) %>%
            ### constrain to 0-1
            mutate(u   = max(min(u, 1), 0),
                   m   = max(min(m, 1), 0),
                   ncp = max(min(ncp, 1), 0)) %>%
            ungroup() %>%
            select(-coord_extract) %>%
            drop_na()
        print(knitr::kable(input_df))
        return(input_df)
    })
    
    output$ncp2dPlot <- renderPlot({

        plot_df <- spp_df()
        
        p1 <- ggplot(plot_df, aes(x = u, y = ncp, color = spp_id, shape = spp_id)) +
            theme_classic() +
            theme(legend.title = element_blank()) +
            scale_x_continuous(breaks = seq(0, 1, .2), limits = c(0, 1)) +
            scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0, 1)) +
            labs(x = 'Uniqueness/irreplaceability', 
                 y = 'Magn. of global effect on NCP') +
            geom_point(size = 3)
        p2 <- ggplot(plot_df, aes(x = m, y = ncp, color = spp_id, shape = spp_id)) +
            theme_classic() +
            scale_x_continuous(breaks = seq(0, 1, .2), limits = c(0, 1)) +
            scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0, 1)) +
            labs(x = 'Multifunctionality', y = 'Magn. of global effect on NCP') +
            geom_point(size = 3, show.legend = FALSE)
        
        ggdraw() +
            draw_plot(p1 + theme(legend.position = 'none'), 
                      x = 0.0, y = 0.0, height = 1, width = 0.4) +
            draw_plot(p2, x = 0.4, y = 0.0, height = 1, width = 0.4) +
            draw_plot(get_legend(p1), x = 0.8, y = 0, height = 1, width = 0.2)
    })
    output$ncp3dPlotly <- renderPlotly({
        plotly_df <- spp_df()
        
        plot_ly(x = plotly_df$u, 
                y = plotly_df$m, 
                z = plotly_df$ncp, 
                type = "scatter3d", mode = "markers", name = plotly_df$spp_id)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
