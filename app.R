library(shiny)  # Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(ggimage)
library(dplyr)  # For filtering and manipulating data
library(png)
library(grid)
library(stringr)
library(plotly)
library(ggrepel)
library(magick)

dataset1<-read.csv("/Users/walkerbasham/Desktop/ActionNetwork/ShinyApps/RecruitingPostSeasonRankings/RecruitingPostSeasonRankings/rsconnect/shinyapps.io/actionanalyticscfb/HistoricalRecruitingandRakingsDataWithLogos.csv")
dataset1$Rank[is.na(dataset1$Rank)] = 'UR'


# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel(
        div(
            tags$em(style = "font-size: 30px; display: block; font-style: normal",
                    "Recruiting's Correlation with Postseason Rankings",
                    br()
            ),
            fluidRow(
                column(
                    width = 6,
                    div(style = "font-size:14px; text-align:left;",
                        "Dashboard built by: ",
                        tags$a(href = "https://www.actionnetwork.com/ncaaf",
                               "Action Analytics", target = "_blank")
                    ),
                ),
            )
        )
    ),
    title = "Recruiting's Correlation with Postseason Rankings",
    plotOutput("plot", hover = NULL, height = "auto"), #need height to be auto for the function below to work
    hr(),
    fluidRow(  
        column(4,
               #allows the user to filter by school or conference
               selectInput(
                    inputId = "TeamChoices",        
                    label = "Filter by Teams",
                    choices = unique(dataset1$Team),
                    selected = c("Alabama"),
                    multiple = TRUE),
               
            ),
        column(3,
               sliderInput("YearChoice", "Choose Year Graph Starts", value = 2014, min = 2002, max = 2021, sep = "")
        )
        )
    
           
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    Team <-reactive({
        df<-filter(dataset1,Team %in% input$TeamChoices)
        df<-df[
            with(df, order(-Year, Team)),
        ]
    }) 
    ht<-reactive(input$plotHt)
    choice <- reactive({
        req(input$YearChoice)
        filter(Team(), Year >= input$YearChoice)
    })
    yearStart <- reactive(as.integer(paste0(input$YearChoice)))
    
    
    output$plot <- renderPlot({
        ggplot(choice(),aes(x = Year,y=points, label = Rank)) + 
            scale_fill_continuous(guide = FALSE) +
            geom_line(aes(x = Year, Y = points, color = Team)) +
            ggimage::geom_image(x=yearStart(), y = 380, adj.x = 10, image="/Users/walkerbasham/Desktop/ActionNetwork/ShinyApps/RecruitingPostSeasonRankings/RecruitingPostSeasonRankings/rsconnect/shinyapps.io/actionanalyticscfb/ActionLogo.png", size = .174, by = 'height', asp = 2.2) +
            ggimage::geom_image(aes(image = logo), size = .05, by = 'height', asp = 16/9) +
            geom_label_repel(aes(color = Team))+
            #geom_point(aes(shape = Top4, color = Team), alpha = .75, size = 4) +
            scale_color_manual(values = choice()$color) +
            #geom_point(aes(shape = Top4, color = Team), size = 4) +
            scale_y_continuous(limits=c(0, 400), breaks = seq(0, 400, by = 50)) +
            scale_x_continuous(limits = c(yearStart(),2020), breaks = seq(yearStart(),2020, by = 1))+
            labs(title="Yearly Recruiting Chart",
                 subtitle="Compared to Postseason Finish",
                 caption = "Figure: @ActionAnalytics with Shiny | Data: @CFB_data with #cfbfastr",
                 y="Recruiting Points",
                 #color = "Team",
                 shape = "Top 4 Finish?") +  # title and caption  #
            theme(
                axis.text.x = element_text(angle = 90, vjust=0.5, size = 10, face = "bold"),  # rotate x axis text
                panel.grid.minor = element_blank(),
                axis.ticks.y =  element_blank(),
                axis.title.x = element_text(size = 12, face = "bold"),
                axis.title.y = element_text(size = 12, face = "bold"),
                axis.text.y = element_text(size = 10, face = "bold", color = "black"),
                plot.title = element_text(size = 16, face = "bold"),
                plot.subtitle = element_text(size = 14, face = "bold"),
                plot.caption = element_text(size = 10, face = "bold"),
                legend.title = element_blank(),
                legend.position = "none")  # turn off minor grid

        },
        height = 500
    )

}

# Run the application 
shinyApp(ui = ui, server = server)
