library(shiny)
library(shinyjs)
library(dplyr)
library(googlesheets4)
library(shinythemes)
library(ggplot2)

#This application collects data on EM fellowships and produces an analysis

# Define UI for application
ui <- fluidPage(theme = shinytheme("yeti"),
                
                shinyjs::useShinyjs(),
                
                # Application title
                titlePanel("Return on Investment: EM Fellowships"),
                
                verticalLayout(
                    #Prelim question
                    div(id = "resident",
                        inputPanel(
                            selectInput("resident_entry", "Have you completed a residency in emergency medicine?", choices = c('--', 'Yes', 'No'))
                        )
                    ),
                    #First question: subdivides those who did a fellowship from those who did not.
                    shinyjs::hidden(
                        div(id = "fship_now",
                            inputPanel(
                                selectInput("fship_entry", "Are you currently enrolled in emergency medicine fellowship training?", choices = c('--', 'Yes', 'No'))
                            )
                        )
                    ),
                    shinyjs::hidden(
                        div(id = "fship_ever",
                            inputPanel(
                                selectInput("fship_ever_entry", "Have you ever completed a fellowship in emergency medicine?", choices = c('--', 'Yes', 'No'))
                            )
                        )
                    ),
                    shinyjs::hidden(
                        div(id = "fship_type",
                            inputPanel(
                                verticalLayout(
                                selectInput("fship_type_entry", "Which EM fellowship have you completed/are your completing?", choices = NULL),
                                shinyjs::hidden(
                                div(id = "fship_other",
                                    textInput("fship_other_entry", "EM Fellowship Subspecialty:"))),
                                selectInput("fship_length_entry", "Full duration of fellowship in years:", choices = c(1,2,3,4,5,6,7)),
                                numericInput("fhsip_salary", "Annual salary during fellowship (in dollars):", value = NULL),
                                numericInput("fship_hours", "Average number of hours worked each week during fellowship:", value = NULL),
                                numericInput("fship_vaca", "Number of annual weeks vacation during fellowship:", value = NULL)
                            )
                            )
                        )
                    ),
                    shinyjs::hidden(
                        div(id = "full_pay",
                            inputPanel(
                                verticalLayout(
                                    div(id = "everybody_qs",
                                        selectInput("gender", "Gender:", choices = c('Female', 'Male', 'Other')),
                                        numericInput("age", "Your current age:", value = NULL),
                                        selectInput("married", "Are you currently married?", choices = c('Yes', 'No')),
                                        selectInput("kids", "Do you have any children?", choices = c('Yes', 'No')),
                                    selectInput("state", "Current state of residence:", choices = c('AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'FL', 'GA', 'HI', 'ID', 'IL', 'IN', 'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA', 'MI', 'MN', 'MS', 'MO', 'MT', 'NE', 'NV', 'NH', 'NJ', 'NM', 'NY', 'NC', 'ND', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 'TX', 'UT', 'VT', 'VA', 'WA', 'WV', 'WI', 'WY')),
                                    numericInput("since_res", "Number of years since residency:", value = NULL),
                                    numericInput("til_death", "Number of years (estimated) until retirement:", value = NULL)
                                    ),
                                    div(id = "full_out",
                                        selectInput("emp_type", "Select the best description of your employment model:",
                                                    choices = c("Democratic Group", "Hospital Employee", "Contract Management Group", "Independent or Locums", "Academic", "Government")),
                                        selectInput("admin", "Do you have an administrative role currently?", choices = c('No', 'Yes')),
                                        selectInput("local_power", "How much ability do you have to effect change in emergency medicine locally?", choices = c('None', 'Minimal', 'Some', 'A Lot')),
                                        selectInput("em_power", "How much ability do you have to effect change in emergency medicine locally?", choices = c('None', 'Minimal', 'Some', 'A Lot')),
                                        selectInput("pay_type", "Would you like us to calculate your pay using your salary or an hourly rate?", choices = c('Salary', 'Hourly')),
                                        hr(),
                                        shinyjs::hidden(
                                        div(id = "by_salary",
                                            verticalLayout(
                                            numericInput("salary_entry", "Enter the amount you make in one year:", value = NULL),
                                            numericInput("salary_hours", "How many hours do you work in a typical week?", value = NULL),
                                            numericInput("salary_vaca", "How many weeks vacation do you get in a typical year?", value = NULL)
                                            ))),
                                        shinyjs::hidden(
                                        div(id = "by_hourly",
                                            verticalLayout(
                                            numericInput("hourly_entry", "What is your average hourly pay?", value = NULL),
                                            numericInput("hourly_monthly", "How many hours do you work in a typical month?", value = NULL)
                                        ))),
                                        hr(),
                                        numericInput("if_fired", "If you were fired today, how many weeks would it take you to find a job with a satisfying combination of pay and location?", value = NULL),
                                        numericInput("net_worth", "If you subtracted all of your current debts from your current assets, what would be your approximate net worth (in dollars)?", value = NULL)
                                )
                            )
                            )
                    )),
                    shinyjs::hidden(
                        div(id = "submit",
                            actionButton("submit_entry", "SUBMIT")
                            )
                    )
                )
)

# Define server logic
server <- function(input, output, session) {
    
    v <- reactiveValues()
    v$full <- temp_frame
    
    observeEvent(input$resident_entry, {
        if(input$resident_entry != '--') {
            shinyjs::toggle(id = "resident", anim = TRUE)
        }
        if(input$resident_entry == 'Yes') {
            shinyjs::toggle(id = "fship_now", anim = TRUE)
        }
        #Will need an if statement here that displays the overall output for residents.
    })
    
    observeEvent(input$fship_entry, {
        if(input$fship_entry != '--') {
            shinyjs::toggle(id = "fship_now", anim = TRUE)
        }
        if(input$fship_entry == 'Yes') {
            shinyjs::toggle(id = "fship_type", anim = TRUE)
            fship_type_choices <- unique(v$full$FShip)
            updateSelectInput(session, "fship_type_entry",
                              choices = c('--',
                                          fship_type_choices,
                                          'Other'))
            shinyjs::toggle("full_pay", anim = TRUE)
            shinyjs::hide("full_out", anim = FALSE)
            shinyjs::toggle("submit", anim = TRUE)
        }
        if(input$fship_entry == 'No') {
            shinyjs::toggle(id = "fship_ever", anim = TRUE)
        }
    })
    
    observeEvent(input$fship_ever_entry, {
        if(input$fship_ever_entry != '--') {
            shinyjs::toggle(id = "fship_ever", anim = TRUE)
        }
        if(input$fship_ever_entry == 'Yes') {
            shinyjs::toggle(id = "fship_type", anim = TRUE)
            fship_type_choices <- unique(v$full$FShip)
            updateSelectInput(session, "fship_type_entry",
                              choices = c('--',
                                          fship_type_choices,
                                          'Other'))
            shinyjs::toggle(id = "full_pay", anim = TRUE)
            shinyjs::toggle(id = "submit", anim = TRUE)
        }
        if(input$fship_ever_entry == 'No') {
            shinyjs::toggle(id = "full_pay", anim = TRUE)
            shinyjs::toggle(id = "submit", anim = TRUE)
        }
    }) 
    
    #observeEvent for entry of other for Fellowship type
    
    observeEvent(input$fship_type_entry, {
        if(input$fship_type_entry == 'Other') {
            shinyjs::show(id = "fship_other", anim = TRUE)
        }
        if(input$fship_type_entry != 'Other') {
            shinyjs::hide(id = "fship_other", anim = TRUE)
        }
    })
    
    observeEvent(input$pay_type, {
        if(input$pay_type == 'Salary') {
            shinyjs::hide(id = "by_hourly", anim = FALSE)
            shinyjs::show(id = "by_salary", anim = TRUE)
        }
        if(input$pay_type == 'Hourly') {
            shinyjs::hide(id = "by_salary", anim = FALSE)
            shinyjs::show(id = "by_hourly", anim = TRUE)
        }
    })

}

# Run the application 
shinyApp(ui = ui, server = server)