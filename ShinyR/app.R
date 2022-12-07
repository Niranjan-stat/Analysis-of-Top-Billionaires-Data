library(shiny)
library(shinythemes)
library(shinyWidgets)

library(gifski)
library(plotly)

library(tidyverse)
library(gganimate)


load("./../Data/finaldata.RData")

## The App:
## 
## It uses Tabs to separate:
##  - Home: A bit of an introduction.
##  - Visualisations: The main application
##  - Summary: The dataset
##  - About: About the group members and references

ui <- navbarPage(
    title = "Billionaire data",
    theme = shinytheme("flatly"),
    tabPanel(
        "Home",
        h3("R Shiny App"),
        br(),
        h1("WORLD OF THE RICH"),
        br(),
        h4("MTH208A"),
        h4("Guide: Dootika Vats"),
    ),
    tabPanel(
        "Visualisations",
        
        tags$head(tags$style(
            HTML("hr {border-top: 1px solid #000000;}")
        )),
        
        titlePanel("Visualizing the Data"),
        
        sidebarLayout(
            sidebarPanel(
                selectInput(
                    "question",
                    h3("View plots:"),
                    choices = list(
                        "Select" = 1,
                        "Net Worth with Rank" = 2,
                        "Age Distributions" = 3,
                        "Net Worth with Source" = 4,
                        "Numbers v/s Time" = 5,
                        "Scatter Plots" = 6,
                        "Map Distribution" = 7,
                        "Top 10 Billionaires Race" = 8
                    )
                ),
                
                
                conditionalPanel(
                    condition = "input.question == 2",
                    sliderInput(
                        "rank.2",
                        h4("Ranks:"),
                        min = 1,
                        max = 500,
                        value = c(1, 500),
                        step = 1,
                    ),
                    checkboxGroupInput(
                        "year.2",
                        "Year:",
                        choices =  c(2011:2019, 2021),
                        selected = c(2011:2019, 2021)
                    )
                ),
                conditionalPanel(
                    condition = "input.question == 3",
                    sliderTextInput(
                        "year.3",
                        h4("Year:"),
                        choices = c(2011:2019, 2021),
                        selected = 2011,
                        animate = animationOptions(loop = TRUE)
                    )
                ),
                conditionalPanel(
                    condition = "input.question == 4",
                    sliderTextInput(
                        "year.4",
                        h4("Year:"),
                        choices = c(2011:2019, 2021),
                        selected = 2011,
                        animate = animationOptions(loop = TRUE)
                    ),
                    selectInput(
                        "source.4",
                        h4("Source of Income:"),
                        choices = as.character(unique(tables[[2011]]$`Source of Wealth`)),
                        selected = as.character(unique(tables[[2011]]$`Source of Wealth`)),
                        multiple = TRUE
                    )
                ),
                conditionalPanel(
                    condition = "input.question == 5",
                    selectInput(
                        "country.5",
                        h4("Countries:"),
                        choices = as.character(unique(tables[[2011]]$Citizenship)),
                        selected = c("India", "Russia", "China"),
                        multiple = TRUE
                    )
                ),
                conditionalPanel(
                    condition = "input.question == 6",
                    selectInput(
                        "axis1.6",
                        "Select variable X:",
                        choices = c(
                            "Age",
                            "Net Worth (in billion $US)",
                            "Source of Wealth",
                            "Citizenship",
                            "Population",
                            "GDP",
                            "Population Density",
                            "Last Change YTD",
                            "GDP Growth Rate",
                            "Number of Billionares"
                        )
                    ),
                    selectInput(
                        "axis2.6",
                        "Select variable Y:",
                        choices = c(
                            "Age",
                            "Net Worth (in billion $US)",
                            "Source of Wealth",
                            "Citizenship",
                            "Population",
                            "GDP",
                            "Population Density",
                            "Last Change YTD",
                            "GDP Growth Rate",
                            "Number of Billionares"
                        )
                    )
                ),
                conditionalPanel(
                    condition = "input.question == 7",
                    sliderTextInput(
                        "year.7",
                        "Year:",
                        choices = c(2011:2019, 2021),
                        selected = 2011,
                        animate = animationOptions(loop = TRUE)
                    ),
                    p("We can ignore USA to remove the discrepancy caused by it."),
                    radioButtons(
                        "ignore.usa.7",
                        "Ignore USA:",
                        c("Yes",
                          "No")
                    ),
                    radioButtons(
                        "data_map.7",
                        "Select Data:",
                        c("Count",
                          "Total Net Worth",
                          "Average Net Worth")
                    )
                )
            ),
            
            mainPanel(
                conditionalPanel(condition = "input.question == 2",
                                 plotOutput("NetWorthWithRank")),
                
                conditionalPanel(
                    condition = "input.question == 3",
                    plotOutput("AgeWithTime"),
                    plotOutput("AgeHistogram")
                ),
                
                conditionalPanel(
                    condition = "input.question == 4",
                    plotOutput("NetWorthWithSource"),
                    plotOutput("NetWorthBoxWithSource")
                ),
                
                conditionalPanel(condition = "input.question == 5",
                                 plotlyOutput("CountWithTime")),
                
                conditionalPanel(condition = "input.question == 6",
                                 plotlyOutput("ScatterPlot")),
                
                conditionalPanel(
                    condition = "input.question == 7",
                    plotlyOutput("MapPlot")
                ),
                conditionalPanel(condition = "input.question == 8",
                                 imageOutput("RacePlot"))
            )
            
        )
    ),
    tabPanel(
        "Summary",
        
        selectInput(
            "dataset",
            "Year :",
            choices =  c(
                "2011",
                "2012",
                "2013",
                "2014",
                "2015",
                "2016",
                "2017",
                "2018",
                "2019",
                "2021"
            )
        ),
        numericInput(
            inputId = "obs",
            label = "Number of observations to view:",
            value = 10
        ),
        mainPanel(verbatimTextOutput("suma"),
                  tableOutput("view"), )
    ),
    tabPanel(
        "About",
        h3("Credits:"),
        h5("Amitesh Singh"),
        h5("Niranjan Dey"),
        h5("Om Shivam Verma"),
        h5("Vikram Kumar"),
        br(),
        br(),
        h3("Resource:"),
        h6("https://www.wikipedia.org/"),
        h6(
            "https://stats.areppim.com/stats/links_billionairexlists.htm"
        )
    )
)


server <- function(input, output) {
    datasetInput <- reactive({
        switch(
            input$dataset,
            "2011" = tables[[2011]],
            "2012" = tables[[2012]],
            "2013" = tables[[2013]],
            "2014" = tables[[2014]],
            "2015" = tables[[2015]],
            "2016" = tables[[2016]],
            "2017" = tables[[2017]],
            "2018" = tables[[2018]],
            "2019" = tables[[2019]],
            "2021" = tables[[2021]],
        )
    })
    
    output$suma <- renderPrint({
        summary(datasetInput())
    })
    
    output$view <- renderTable({
        head(datasetInput(), n = input$obs)
    })
    
    common_theme = theme(
        text = element_text(size = 20),
        
        plot.title = element_text(face = "bold"),
        axis.text = element_text(colour = "black", size = 15),
        legend.text = element_text(size = 15),
        
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey")
    )
    
    get_scatterplot_colname <- function(label) {
        return(switch(
            label,
            "Age" = "Age",
            "Net Worth (in billion $US)" = "Net.Worth..in.billion..US.",
            "Source of Wealth" = "Source.of.Wealth",
            "Citizenship" = "Citizenship",
            "Population" = "population",
            "GDP" = "GDP",
            "Population Density" = "Population_Density",
            "Last Change YTD" = "LastChangeYTD",
            "GDP Growth Rate" = "GDPGR",
            "Number of Billionares" = "No.of.Billionares"
        ))
    }
    
    NetWorthWithRankData <- reactive({
        gfg_data <- data.frame()
        for (i in as.integer(input$year.2)) {
            gfg_data <- rbind(
                gfg_data,
                data.frame(
                    Net.Worth = tables[[i]]$`Net Worth (in billion $US)`[input$rank.2[1]:input$rank.2[2]],
                    Rank = input$rank.2[1]:input$rank.2[2],
                    Year = rep(i, input$rank.2[2] - input$rank.2[1] + 1)
                )
            )
        }
        gfg_data$Year <- factor(gfg_data$Year)
        gfg_data
    })
    output$NetWorthWithRank <- renderPlot({
        ggplot(NetWorthWithRankData()) +
            geom_line(aes(
                x = Rank,
                y = Net.Worth,
                color = Year
            )) +
            labs(title = "Net Worth v/s Rank",
                 y = "Net Worth") +
            common_theme
    })
    
    output$AgeWithTime <- renderPlot({
        gfg_data <- data.frame()
        for (i in c(2011:2019, 2021)) {
            gfg_data <- rbind(gfg_data,
                              data.frame(Year = rep(i, 500),
                                         Age = tables[[i]]$Age))
        }
        
        gfg_data$Year <- factor(gfg_data$Year)
        
        gfg_plot <- ggplot(gfg_data) +
            geom_boxplot(
                mapping = aes(
                    x = Year,
                    y = Age,
                    fill = Year
                ),
                lwd = 1,
                color = "#212121"
            ) +
            labs(title = "Age v/s Year") +
            common_theme +
            theme(legend.position = "none")
        
        gfg_plot
    })
    
    AgeHistogramData <- reactive({
        gfg_data <- data.frame(tables[[as.integer(input$year.3)]])
        gfg_data
    })
    output$AgeHistogram <- renderPlot({
        gfg_plot <- ggplot(AgeHistogramData()) +
            geom_histogram(
                mapping = aes(x = Age),
                color = "white",
                binwidth = 5
            ) +
            xlim(0, 100) +
            ylim(0, 80) +
            labs(title = paste("Histogram of Age:", as.character(input$year.3)),
                 y = "Number of Billionaires") +
            common_theme
        
        gfg_plot
    })
    
    NetWorthWithSourceData <- reactive({
        gfg_data <- data.frame(tables[[input$year.4]]) %>%
            group_by(`Source.of.Wealth`) %>%
            summarise(total.net.worth = sum(`Net.Worth..in.billion..US.`)) %>%
            arrange(total.net.worth)
        
        gfg_data$Source.of.Wealth <-
            factor(gfg_data$Source.of.Wealth,
                   levels = as.character(gfg_data$Source.of.Wealth))
        
        gfg_data
    })
    output$NetWorthWithSource <- renderPlot({
        gfg_data <- NetWorthWithSourceData()
        gfg_plot <-
            ggplot(gfg_data, aes(x = total.net.worth, y = Source.of.Wealth)) +
            geom_col(orientation = "y") +
            geom_text(mapping = aes(label = total.net.worth),
                      hjust = -0.2,
            ) +
            labs(
                title = paste("Total Net Worth:", as.character(input$year.4)),
                x = "Total Net Worth",
                y = "Source of Wealth"
            ) +
            xlim(0, 2400) +
            common_theme
        
        gfg_plot
    })
    
    NetWorthBoxWithSourceData <- reactive({
        gfg_data <- data.frame(tables[[input$year.4]])
        gfg_data <-
            filter(gfg_data, Source.of.Wealth %in% input$source.4)
        gfg_data
    })
    output$NetWorthBoxWithSource <- renderPlot({
        gfg_plot <- ggplot(NetWorthBoxWithSourceData()) +
            geom_boxplot(
                mapping = aes(x = `Source.of.Wealth`, y = `Net.Worth..in.billion..US.`),
                width = 0.5
            ) +
            labs(
                title = paste(
                    "Net Worth v/s Source of Wealth:",
                    as.character(input$year.4)
                ),
                x = "Source of Wealth",
                y = "Net Worth in Billion US$"
            ) +
            common_theme +
            theme(legend.position = "none",
                  axis.text.x = element_text(angle = 90))
        
        gfg_plot
    })
    
    CountWithTimeData <- reactive({
        gfg_data <- data.frame(Country = input$country.5)
        
        for (i in c(2011:2019, 2021)) {
            nation_count <- data.frame(table(tables[[i]]$Citizenship))
            colnames(nation_count)[colnames(nation_count) == "Freq"] <-
                as.character(i)
            gfg_data <- merge(
                x = gfg_data,
                y = nation_count,
                by.x = "Country",
                by.y = "Var1",
                all.x = TRUE
            )
        }
        
        gfg_data[is.na(gfg_data)] <- 0
        
        gfg_data <-
            cbind(gfg_data["Country"], stack(gfg_data[as.character(c(2011:2019, 2021))])) %>%
            mutate(`Year` = `ind`,
                   `Count` = `values`,
                   .keep = "unused") %>%
            arrange(`Country`, `Year`)
        
        gfg_data$Year <- as.integer(as.character(gfg_data$Year))
        
        gfg_data <- data.frame(gfg_data)
        
        gfg_data
    })
    output$CountWithTime <- renderPlotly({
        gfg_data <- CountWithTimeData()
        
        fig <- plot_ly(data = gfg_data) %>%
            add_trace(
                data = gfg_data,
                type = "scatter",
                mode = 'lines+markers',
                x = ~ Year,
                y = ~ Count,
                color = ~ Country,
                hovertemplate = paste("<b>Count:</b> %{y}",
                                      "<br><b>Year:</b> %{x}")
            ) %>%
            layout(
                title = list(
                    text = "Count v/s Year",
                    x = 0,
                    font = list(color = "black")
                ),
                xaxis = list(title = "Year"),
                yaxis = list(title = "Number of Billionaires"),
                font = list(size = 15)
            )
        
        fig
    })
    
    output$ScatterPlot <- renderPlotly({
        fig <- plot_ly(type = 'scatter',
                       mode = 'markers')
        
        fig <- fig %>%
            add_trace(
                x = dat2021[, get_scatterplot_colname(input$axis1.6)],
                y = dat2021[, get_scatterplot_colname(input$axis2.6)],
                color = factor(dat2021$Citizenship),
                hovertemplate = paste("<b>X</b>: %{x}",
                                      "<br><b>Y</b>: %{y}")
            ) %>%
            layout(
                xaxis = list(title = input$axis1.6),
                yaxis = list(title = input$axis2.6)
            )
        
        fig
    })
    
    MapPlotData <- reactive({
        if (input$data_map.7 == "Count") {
            gfg_data <- data.frame(country_codes)
            
            nation_count <-
                data.frame(table(tables[[input$year.7]]$Citizenship))
            
            gfg_data <- merge(
                x = gfg_data,
                y = nation_count,
                by.x = "Country",
                by.y = "Var1",
                all.x = TRUE
            )
            
            gfg_data[is.na(gfg_data)] <- 0
            
            gfg_data[gfg_data$Country %in% "Russia", "Freq"] <-
                sum(gfg_data[gfg_data$Code %in% "RUS", "Freq"])
            gfg_data <-
                filter(gfg_data, Country != "Russian Federation")
        } else if (input$data_map.7 == "Total Net Worth"){
            gfg_data <- data.frame(country_codes)
            
            total_worths <- tables[[input$year.7]] %>%
                select(Citizenship, `Net Worth (in billion $US)`) %>%
                group_by(Citizenship) %>%
                summarise(`Total Net Worth` = sum(`Net Worth (in billion $US)`))
            
            gfg_data <- merge(
                x = gfg_data,
                y = total_worths,
                by.x = "Country",
                by.y = "Citizenship",
                all.x = TRUE
            )
            
            gfg_data[is.na(gfg_data)] <- 0
            
            gfg_data[gfg_data$Country %in% "Russia", "Total Net Worth"] <-
                sum(gfg_data[gfg_data$Code %in% "RUS", "Total Net Worth"])
            gfg_data <-
                filter(gfg_data, Country != "Russian Federation")
        } else if (input$data_map.7 == "Average Net Worth"){
            gfg_data_worth <- data.frame(country_codes)
            
            total_worths <- tables[[input$year.7]] %>%
                select(Citizenship, `Net Worth (in billion $US)`) %>%
                group_by(Citizenship) %>%
                summarise(`Total Net Worth` = sum(`Net Worth (in billion $US)`))
            
            gfg_data_worth <- merge(
                x = gfg_data_worth,
                y = total_worths,
                by.x = "Country",
                by.y = "Citizenship",
                all.x = TRUE
            )
            
            gfg_data_worth[is.na(gfg_data_worth)] <- 0
            
            gfg_data_worth[gfg_data_worth$Country %in% "Russia", "Total Net Worth"] <-
                sum(gfg_data_worth[gfg_data_worth$Code %in% "RUS", "Total Net Worth"])
            
            
            
            gfg_data_worth <-
                filter(gfg_data_worth, Country != "Russian Federation")
            
            gfg_data_count <- data.frame(country_codes)
            
            nation_count <-
                data.frame(table(tables[[input$year.7]]$Citizenship))
            
            gfg_data_count <- merge(
                x = gfg_data_count,
                y = nation_count,
                by.x = "Country",
                by.y = "Var1",
                all.x = TRUE
            )
            
            gfg_data_count[is.na(gfg_data_count)] <- 0
            
            gfg_data_count[gfg_data_count$Country %in% "Russia", "Freq"] <-
                sum(gfg_data_count[gfg_data_count$Code %in% "RUS", "Freq"])
            gfg_data_count <-
                filter(gfg_data_count, Country != "Russian Federation")
            
            gfg_data <- merge(
                x = gfg_data_count,
                y = gfg_data_worth,
                by = c("Country", "Code")
            )
            
            gfg_data <- gfg_data %>%
                filter(`Freq` > 0) %>%
                mutate(`Average Net Worth` = `Total Net Worth`/`Freq` , .keep = "unused")
        }
        
        if (input$ignore.usa.7 == "Yes"){
            gfg_data <- filter(gfg_data, Code != "USA")
        }
        
        gfg_data
    })
    output$MapPlot <- renderPlotly({
        gfg_data <- MapPlotData()
        
        color_bar_title <- switch(
            input$data_map.7,
            "Count" = "Number of Billionaires",
            "Total Net Worth" = "Total Net Worth",
            "Average Net Worth" = "Average Net Worth"
        )
        
        plot_ly(
            data = gfg_data,
            type = "choropleth",
            locations = gfg_data$Code,
            z = switch(
                input$data_map.7,
                "Count" = gfg_data$Freq,
                "Total Net Worth" = gfg_data$`Total Net Worth`,
                "Average Net Worth" = gfg_data$`Average Net Worth`
            ),
            zmin = 0,
            zmax = switch(
                input$data_map.7,
                "Count" = ifelse(input$ignore.usa.7 == "Yes", 80, 180),
                "Total Net Worth" = ifelse(input$ignore.usa.7 == "Yes", 1500, 3500),
                "Average Net Worth" = 20
            ),
            text = gfg_data$Country,
            colorscale = "Reds"
        ) %>%
            colorbar(title = color_bar_title) %>%
            layout(title = list(
                text = paste("Country Heat Map:", as.character(input$year.7)),
                x = 0,
                font = list(color = "black")
            ),
            font = list(size = 15))
    })
    
    output$RacePlot <- renderImage({
        Name_tidy <- TS_data
        
        Name_formatted <- Name_tidy %>%
            group_by(Year) %>%
            mutate(
                rank = rank(-Net_Worth),
                Value_rel = Net_Worth / Net_Worth[rank == 1],
                Value_lbl = paste0(" ", round(Net_Worth))
            ) %>%
            group_by(Name) %>%
            filter(rank <= 10) %>%
            ungroup()
        outfile <- tempfile(fileext = '.gif')
        staticplot = ggplot(Name_formatted,
                            aes(
                                rank,
                                group = Name,
                                fill = as.factor(Name),
                                color = as.factor(Name)
                            )) +
            geom_tile(
                aes(
                    y = Net_Worth / 2,
                    height = Net_Worth,
                    width = 0.9
                ),
                alpha = 0.8,
                color = NA
            ) +
            geom_text(
                aes(y = 0, label = paste(Name, " ")),
                vjust = 0.2,
                hjust = 1,
                size = 5
            ) +
            geom_text(aes(
                y = Net_Worth,
                label = Value_lbl,
                hjust = 0
            ),
            size = 5) +
            coord_flip(clip = "off", expand = FALSE) +
            scale_y_continuous(labels = scales::comma) +
            scale_x_reverse() +
            guides(color = "none", fill = "none") +
            theme(
                axis.line = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                legend.position = "none",
                panel.background = element_blank(),
                panel.border = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_line(size = 1, color = "grey"),
                panel.grid.minor.x = element_line(size = 1, color = "grey"),
                plot.title  = element_text(
                    size = 25,
                    hjust = 0.5,
                    face = "bold",
                    colour = "grey",
                    vjust = -1
                ),
                plot.subtitle = element_text(
                    size = 18,
                    hjust = 0.5,
                    face = "italic",
                    color = "grey"
                ),
                plot.caption = element_text(
                    size = 8,
                    hjust = 0.5,
                    face = "italic",
                    color = "grey"
                ),
                plot.background = element_blank(),
                plot.margin = margin(2, 2, 2, 4, "cm")
            )
        
        anim = staticplot + transition_states(Year,
                                              transition_length = 4,
                                              state_length = 1) +
            view_follow(fixed_x = TRUE)  + ease_aes('cubic-in-out') +
            labs(title = ' Billioniare in  : {closest_state}',
                 subtitle = "Top 10  Billioniare",
                 caption = "Net_WOrth in Billions | Data Source:Statsapperim ")
        anim_save("outfile.gif", animate(anim, fps = 5),)
        list(src = "outfile.gif",
             contentType = 'image/gif')
    }, deleteFile = TRUE)
}


shinyApp(ui = ui, server = server)
