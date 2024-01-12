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
            selectInput("hospital","Hospital",levels(data$hospital))
            ### Create a dropdown with options from our dataset ###
            ### levels(data$hospital) -> this populates input$hospital ###
        ),
        mainPanel(
            navset_card_underline(
                title = "Visualizations",
                nav_panel("Plot", titlePanel("Age Classes"), plotOutput("ageClasses")),
                nav_panel("Summary", titlePanel("Test Change"), plotOutput("ageTable")),
                nav_panel("Table", titlePanel("Test2"))
            )    
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$ageClasses <- renderPlot({
        filtered_data <- data |> filter(hospital == input$hospital)
        age_classes <- filtered_data |> 
            group_by(
                age_class = ifelse(adult, "adult", "child")) |> 
            tally(sort = T)
        
        ggplot(age_classes, aes(x=age_class, y=n)) + geom_col()
    })
    
    output$ageTable <- renderPlot({
        filtered_data <- data |> filter(hospital == input$hospital)
        age_table <- filtered_data |>
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
