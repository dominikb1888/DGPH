pacman::p_load(
    shiny, 
    bslib
    )

source("scripts/clean.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Histograms in Tabs"),

    sidebarLayout(
        sidebarPanel(
            selectInput("hospital","Hospital",levels(data$hospital)),
            sliderInput("age", "Age", min(data$age,na.rm=T), max(data$age, na.rm=T), c(0,70)),
        ),
        mainPanel(
            navset_card_underline(
                title = "Visualizations",
                nav_panel("Plot", titlePanel("Age Classes"), plotOutput("ageClasses")),
                nav_panel("Summary", titlePanel("Test Change"), plotOutput("ageTable")),
                nav_panel("Table", titlePanel("Test2"))
            ),
            p("This is a paragraph"),
            tags$ul(
                tags$li(tags$b("location_name"), " - the facility that the data were collected at"),
                tags$li(tags$b("data_date"), " - the date the data were collected at"),
                tags$li(tags$b("submitted_daate"), " - the date the data were submitted at"),
                tags$li(tags$b("Province"), " - the province the data were collected at (all 'North' for this dataset)"),
                tags$li(tags$b("District"), " - the district the data were collected at"),
                tags$li(tags$b("age_group"), " - the age group the data were collected for (0-5, 5-14, 15+, and all ages)"),
                tags$li(tags$b("cases_reported"), " - the number of cases reported for the facility/age group on the given date")
            ),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    filtered_data <- reactive({
       data <- data |> filter(hospital == input$hospital & age >= input$age[1] & age <= input$age[2])
    })

    output$ageClasses <- renderPlot({
        age_classes <- filtered_data() |> 
            group_by(
                age_class = ifelse(adult, "adult", "child")) |> 
            tally(sort = T)
        
        ggplot(age_classes, aes(x=age_class, y=n)) + geom_col()
    })
    
    output$ageTable <- renderPlot({
        age_table <- filtered_data() |>
            tabyl(age_cat, gender) |>
            adorn_totals(where="both") |>
            adorn_percentages(denominator = "col") |>
            adorn_pct_formatting() |>
            adorn_ns(position = "front") |>
            adorn_title(                                # adjust titles
                row_name = "Age Category",
                col_name = "Gender")
        
        ggtexttable(age_table)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
