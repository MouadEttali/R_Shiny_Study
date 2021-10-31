
# Import libraries
library(shiny)
library(data.table)
library(randomForest)

# Read in the Lasso model
model <- readRDS("model.rds")


####################################
# User interface                   #
####################################

ui <- pageWithSidebar(
  
  # Page header
  headerPanel('Attrition Predictor'),
  
  # Input values
  sidebarPanel(
    #HTML("<h3>Input parameters</h3>"),
    tags$label(h3('Input parameters')),
    numericInput("age", 
                 label = "Age", 
                 value = 5.1),
    
    selectInput("buisness_travel", "Buisness Travels:",
                c("Doesn't travel" = "Non-Travel",
                  "Rarely travels" = "Travel_Rarely",
                  "Frequent Travels" = "Travel_Frequently")),
    
    selectInput("department", "Department:",
                c("Research & Development" = "Research & Development",
                  "Sales" = "Sales",
                  "Human Resources" = "Human Resources")),
    
    selectInput("education", "Education Level:",
                c("Bac" = 1,
                  "Bac + 2" = 2,
                  "Bac + 3" = 3,
                  "Bac + 4" = 4,
                  "Bac + 5" = 5)),
    
    selectInput("education_field", "Education Field:",
                c("Technical Degree" = "Technical Degree",
                  "Human Resources " = "Human Resources",
                  "Marketing" = "Marketing",
                  "Life Sciences" = "Life Sciences",
                  "Medical" = "Medical",
                  "Other"="Other")),
    
    selectInput("envir_satis", "Environment Satisfaction:",
                c("Not satisfied" = 1,
                  "Neutral" = 2,
                  "Satisfied" = 3,
                  "Very satisfied" = 4)),
                
    selectInput("gender", "Gender:",
                c("Male" = "Male",
                  "Female" = "Female"
                  )),
    
    selectInput("Job_inv", "Job involvement:",
                c("Not very involved" = 1,
                  "Neutral" = 2,
                  "Involved" = 3,
                  "Very involved" = 4)),
                
    selectInput("job_lev", "Job Level:",
              c("Very Low" = 1,
                "Low" = 2,
                "Medium" = 3,
                "High" = 4,
                "Very High" = 5)),
                
                
    selectInput("job_role", "Job Role:",
            c( "Research Scientist "  = "Research Scientist "   ,
               "Laboratory Technician" = "Laboratory Technician"  ,
               "Manufacturing Director" ="Manufacturing Director" ,
               "Healthcare Representative"= "Healthcare Representative",
               "Research Director"        = "Research Director"  ,
               "Sales Representative "    ="Sales Representative " ,
               "Sales Executive"          ="Sales Executive"   ,
               "Manager"                  = "Manager"  ,
               "Human Resources"="Human Resources" )),
            
            selectInput("job_satis", "Job Satisfaction:",
                        c("Not satisfied" = 1,
                          "Neutral" = 2,
                          "Satisfied" = 3,
                          "Very satisfied" = 4)), 
            
            selectInput("marital_stat", "Marital Status:",
                        c("Divorced" = "Divorced",
                          "Single" ="Single" ,
                          "Married" = "Married")),  
    
    sliderInput("monthly_income", "Monthly Income :", 
                min = 0, max = 25000, value = 6000),
    
    numericInput("num_companies_worked", 
                 label = "Number of previous experiences", 
                 value = 2),
    selectInput("overtime", "works overtime", 
                c("No" = 0 ,
                "Yes" = 1 )),
    numericInput("total_years_working", "Total years of experience", 
                 value = 2),
    
    selectInput("work_life_balance", "work life balance:",
                c("very unbalanced" =1,
                  "Unbalanced" = 2,
                  "balanced" =3 ,
                  "very balanced" = 4)),
    
    numericInput("years_in_curr_role", "years working in current Role: ", 
                 value = 2),
    numericInput("years_since_last_promotion", "years since last promotion: ", 
                 value = 2),
    numericInput("years_with_curr_manager", "years with current manager : ", 
                 value = 2),
    
    sliderInput("total_satis", "total satisfaction :", 
                min = 0, max = 20, value = 10),
    
    actionButton("submitbutton", "Submit", 
                 class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
    
  )
)

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c("age",
               "buisness_travel",
               "department",
               "education",
               "education_field",
               "employee_number",
               "environment_satisfaction",
               "gender",
               "job_involvement",
               "job_level",
               "job_role",
               "job_satisfaction",
               "marital_status",
               "monthly_income",
               "num_companies_worked",
               "overtime",
               "performance_rating",
               "relationship_satisfaction",
               "stock_option_level",
               "total_working_years",
               "work_life_balance",
               "years_in_current_role",
               "years_since_last_promotion",
               "years_with_curr_manager",
               "total_satisfaction"),
      Value = as.character(c(input$age,
                             input$buisness_travel,
                             input$department,
                             input$education,
                             input$education_field,
                             sample(0:4000,1),
                             input$envir_satis,
                             input$gender,
                             input$job_inv,
                             input$job_lev,
                             input$job_role,
                             input$job_satis,
                             input$marital_stat,
                             input$monthly_income,
                             input$num_companies_worked,
                             input$overtime,
                             sample(3:4,1),
                             sample(1:4,1),
                             sample(0:3,1),
                             input$total_working_years,
                             input$work_life_balance,
                             input$years_in_curr_role,
                             input$years_since_last_promotion,
                             input$years_with_curr_manager,
                             input$total_satis
                             )),
      stringsAsFactors = TRUE)
    
    Attrition <- 0
    df <- rbind(df, Attrition)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)