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
library(DT)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("FITBITS Prioritization"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            includeMarkdown('instructions.Md'),
            actionButton("go", label = "Plot data"),
            hr(),
            DTOutput("dt_out")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            # plotOutput("dt_plot")
            plotOutput("ncp2dPlot"),
            plotlyOutput("ncp3dPlotly")
        )
    )
)

server <- function(input, output) {

    ### initialize a blank dataframe
    fb_df <- reactiveValues(
        data = { 
            # message('inside reactiveValues')
            data.frame(
                bit_name = c('e.g.: Bee richness', rep('(name/desc of bit)', 9)),
                U        = c(.3,                   rep(NA_real_, 9)),
                M        = c(.5,                   rep(NA_real_, 9)),
                NCP      = c(.8,                   rep(NA_real_, 9)))
    })
    
    output$dt_out <- renderDT({
        message('inside renderDT')
        DT::datatable(fb_df$data, editable = TRUE)
    })
    
    ### when there is any edit to a cell, write that edit to the initial dataframe
    ### check to make sure it's positive, if not convert
    observeEvent(input$dt_out_cell_edit, {
        message('inside observeEvent')
        ### get values
        info <- input$dt_out_cell_edit
        i <- as.numeric(info$row)
        j <- as.numeric(info$col)
        k <- ifelse(j > 1, 
                    max(min(as.numeric(info$value), 1), 0),
                    as.character(info$value))

        #write values to reactive
        fb_df$data[i, j] <- k
    })
    
    #render plot
    output$ncp2dPlot <- renderPlot({

        message('inside renderPlot')
        ###  require the input button to be non-0 (ie: don't load the plot when the app first loads)
        req(input$go + 1)
        tmp_df <- isolate(fb_df$data) %>%  ### don't react to any changes in the data
            drop_na()
        print(knitr::kable(tmp_df))
        
        p1 <- ggplot(tmp_df, aes(x = U, y = NCP, color = bit_name, shape = bit_name)) +
            theme_classic() +
            theme(legend.title = element_blank()) +
            scale_x_continuous(breaks = seq(0, 1, .2), limits = c(0, 1)) +
            scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0, 1)) +
            labs(x = 'Uniqueness/irreplaceability',
                 y = 'Magn. of global effect on NCP') +
            geom_point(size = 3)
        p2 <- ggplot(tmp_df, aes(x = M, y = NCP, color = bit_name, shape = bit_name)) +
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
        message('inside renderPlotly')
        ###  require the input button to be non-0 (ie: don't load the plot when the app first loads)
        req(input$go + 1)
        tmp_df <- isolate(fb_df$data) %>%  ### don't react to any changes in the data
            drop_na()
        
        plot_ly(x = tmp_df$U,
                y = tmp_df$M,
                z = tmp_df$NCP,
                type = "scatter3d", mode = "markers", name = tmp_df$bit_name)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
