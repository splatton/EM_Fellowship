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
                    ),
                    shinyjs::hidden(
                        div(id = "thanks",
                            wellPanel(
                                helpText("Thanks for completing this survey! Please tell your friends - the more data we have, the better for all of us.")
                            ))
                    )
                )
)

# Define server logic
server <- function(input, output, session) {
    
    v <- reactiveValues()
    v$new <- data.frame(Time = NA, FShip.Current = NA, FShip.Ever = NA, FShip.Type = NA, FShip.Length = NA, FShip.Salary = NA, FShip.Hours = NA, FShip.Vaca = NA, Gender = NA, Age = NA, Married = NA, Kids = NA, State = NA, Since.Res = NA, Til.Retirement = NA, Emp.Model = NA, Admin = NA, Local.Power = NA, EM.Power = NA, Hourly = NA, Yearly.Hours = NA, If.Fired.Weeks = NA, Net.Worth = NA)
    
    #The following code all reflects the survey flow.
    
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
    
    #The following code now reflects the entry of the data into the database. This will all happen when the 'Submit' button is hit.
    
    observeEvent(input$submit_entry, {
        shinyjs::toggle(id = "submit", anim = FALSE)
        shinyjs::hide(id = "fship_type", anim = FALSE)
        shinyjs::hide(id = "full_pay", anim = FALSE)
        shinyjs::show(id = "thanks", anim = TRUE)
        v$new[1,'Time'] <- Sys.time()
        v$new[1,'FShip.Current'] <- input$fship_entry
        if(input$fship_entry == 'Yes') {
            v$new[1,'FShip.Ever'] <- input$fship_entry
        }
        else {
            v$new[1,'FShip.Ever'] <- input$fship_ever_entry
        }
        
        #These next entries are only for fellows
        
        if(input$fship_entry == 'Yes' | input$fship_ever_entry == 'Yes') {
            if(input$fship_type_entry == 'Other') {
                v$new[1,'FShip.Type'] <- input$fship_other_entry
            }
            else {
                v$new[1,'FShip.Type'] <- input$fship_type_entry
            }
            v$new[1,'FShip.Length'] <- input$fship_length_entry
            v$new[1,'FShip.Salary'] <- input$fship_salary
            v$new[1,'FShip.Hours'] <- input$fship_hours
            v$new[1,'FShip.Vaca'] <- input$fship_vaca
        }
        
        #These next entries are for all comers.
        
        v$new[1,'Gender'] <- input$gender
        v$new[1,'Age'] <- input$age
        v$new[1,'Married'] <- input$married
        v$new[1,'Kids'] <- input$kids
        v$new[1,'State'] <- input$state
        v$new[1,'Since.Res'] <- input$since_res
        v$new[1,'Til.Retirement'] <- input$til_death
        
        #These next entries are only for non-fellows
        
        if(input$fship_entry == 'No') {
            v$new[1,'Emp.Model'] <- input$emp_type
            v$new[1,'Admin'] <- input$admin
            v$new[1,'Local.Power'] <- input$local_power
            v$new[1,'EM.Power'] <- input$em_power
            v$new[1,'If.Fired.Weeks'] <- input$if_fired
            v$new[1,'Net.Worth'] <- input$net_worth
            if(input$pay_type == 'Salary') {
                v$new[1,'Hourly'] <- input$salary_entry/(input$salary_hours*(52-input$salary_vaca))
                v$new[1,'Yearly.Hours'] <- input$salary_hours*(52-input$salary_vaca)
            }
            else {
                v$new[1,'Hourly'] <- input$hourly_entry
                v$new[1,'Yearly.Hours'] <- 12*input$hourly_monthly
            }
        }
        
        #Next we will enter this into the database.
        
    })

}

# Run the application 
shinyApp(ui = ui, server = server)