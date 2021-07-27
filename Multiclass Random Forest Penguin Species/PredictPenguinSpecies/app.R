library(shiny)
library(tidymodels)
library(tidyverse)
library(palmerpenguins)
library(shinydashboard)

penguins_df <- penguins %>% drop_na()

model <- readRDS("Multiclass_RF_Model.rds")
modelspec <- readRDS("Multiclass_RF_varimp.rds")


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Predict Palmer Penguin's Species From Body Measurements Using Multiclass Random Forest Model"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            h2("Enter Body Measurements, Sex, Island of the Penguin"),
            
            selectInput("sex",
                        "Sex", 
                        choices = unique(penguins_df$sex)
            ),
            
            selectInput("island",
                        "Island", 
                        choices = unique(penguins_df$island)
            ),
            
            sliderInput("bill_length",
                        "BIll Length mm",
                        min = min(penguins_df$bill_length_mm),
                        max = max(penguins_df$bill_length_mm),
                        value = mean(penguins_df$bill_length_mm), round = TRUE, step = 1),
            
            sliderInput("bill_depth",
                        "BIll Depth mm",
                        min = min(penguins_df$bill_depth_mm),
                        max = max(penguins_df$bill_depth_mm),
                        value = mean(penguins_df$bill_depth_mm), round = TRUE, step = 0.5),
            
            sliderInput("flipper_length",
                        "Flipper Length mm",
                        min = min(penguins_df$flipper_length_mm),
                        max = max(penguins_df$flipper_length_mm),
                        value = mean(penguins_df$flipper_length_mm), round = TRUE, step = 1),
            
            sliderInput("body_mass",
                        "Body Mass g",
                        min = min(penguins_df$body_mass_g  ),
                        max = max(penguins_df$body_mass_g  ),
                        value = mean(penguins_df$body_mass_g  ), round = TRUE, step = 5)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Prediction", 
                                 h1(" "),
                                 plotOutput("distPlot", height = "520px"),
                                 h1(" "),
                                 h1(" "),
                                 h1(" "),
                                 h3("Variable Importance Plot, via Permutation"),
                                 plotOutput("varPlot", height = "370px")
                                 
                        ),
                        
                        tabPanel("About", 
                                 h3("Model Fitting"),
                                 h6("To find out how the model is fit and evaluated, visit the link below"),
                                 tags$a(href="https://rpubs.com/faiazrmn/Multiclass_RF_Penguins", "Click here!"),
                                 h3("Penguins Data"),
                                 h6("To know more about the penguins data, visit the link below"),
                                 tags$a(href="https://allisonhorst.github.io/palmerpenguins/", "Click here!"),
                        )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ### Variable Importance Plot
    output$varPlot <- renderPlot({
        library(vip)
        modelspec %>%
            set_engine("ranger", importance = "permutation") %>%
            fit(species ~ . ,
                data = penguins_df %>% select(-year)
            ) %>%
            vip(geom = "col")
    })
    
    ### Predicted Species Plot
    output$distPlot <- renderPlot({
        
        predicted_prob <- predict(model,
                                  tibble(sex = input$sex,
                                         bill_length_mm = input$bill_length,
                                         bill_depth_mm = input$bill_depth,
                                         flipper_length_mm = input$flipper_length,
                                         body_mass_g = input$body_mass,
                                         island = input$island),
                                  type = "prob") %>% 
            gather() %>% 
            mutate(Species = c("Adelies", "Chinstrap", "Gentoo"))
        
        ggplot(predicted_prob, aes(Species, value, fill = Species, color = "white")) + 
            geom_bar(stat = "identity", show.legend = FALSE, width = 0.6) +
            labs(title = "Predicted Probability of Penguin's Sex",
                 subtitle = "From the Input Data",
                 x = "Species", 
                 y = "Probability") +
            ylim(0, 1) +
            geom_text(aes(y = value, 
                          x = Species, 
                          label = round(value, 2)), 
                      size = 6, 
                      color = "white", 
                      nudge_y = -0.1, 
                      fontface = "bold") +
            theme_light() +
            theme(axis.text.x = element_text(face="bold", color="#663333", 
                                             size=15, angle=0),
                  axis.text.y = element_text(face="bold", color="#993333", 
                                             size=12, angle=0),
                  axis.title=element_text(size=14,face="bold"),
                  plot.title=element_text(size=20),
                  plot.subtitle = element_text(size=14, color="#808080")
            )
        
    }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
