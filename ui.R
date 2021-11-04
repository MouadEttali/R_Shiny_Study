library(shinydashboard)
require(shiny)
library(shinyjs)
library(tidyr)
library(data.table)
library(workflows)
library("glmnet")
library(DT)


# Read in the Lasso model
model <- readRDS("final_model.rds")
#defining character vectors for select inputs
variab<-c("age", "distance_from_home","hourly_rate",
           "daily_rate", "monthly_rate","monthly_income",
           "percent_salary_hike","years_at_company",
           "years_in_current_role","years_since_last_promotion",
           "years_with_curr_manager","total_working_years",
           "num_companies_worked","training_times_last_year",
           "gender","over_time","department",
           "education_field", "job_role", "marital_status",
           "environment_satisfaction", "job_satisfaction",
           "relationship_satisfaction","work_life_balance",
           "job_involvement","performance_rating",
           "business_travel", "education","job_level",
           "stock_option_level")
country<-variab


dashboardPage(
  #defines header
  skin = "red",
  dashboardHeader(
    title="Projet Attrition IBM HR" ,
    dropdownMenu()
  ),
  #defines sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Rapport du Projet", tabName = "presentation", icon = icon("th")),
      menuItem("Dataset utilisee",tabName="dataset",icon=icon("globe")),
      menuItem("Analyse Univariee", tabName = "univariee", icon = icon("dashboard")),
      menuItem("Analyse Bivariee",tabName="bivariee",icon=icon("signal")),
      menuItem("Analyse Predictive",tabName="predictive",icon=icon("globe")),
      useShinyjs()  
    )
  ),
  
  #defines bodys
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    tabItems(
      
      #First TAB Menu-Dashboard  
      tabItem(tabName = "univariee",
              
              fluidRow(
                
                
                column(2,
                       
                       box(selectInput("country",label="Choisir la Variable",choices=country),width = 12) 
                       
                ),#end column
                
                #box for plotting the time series plot
                column(5,
                       
                       box(
                         
                        # highchartOutput("hcontainer"),
                         plotOutput(outputId = "distPlot"),
                         
                         
                         
                         width=12) #end box2
                       
                ), #end 
                
                #hr(),
                # h4("Relative inflation rates time series plot",align="center"),
        
                #br(),
                #column(3,),#end column
                column(5,
                       
                       box(
                         plotOutput(outputId = "distPlot2"),width=12
                         
                       ) ),
                hr(),
                # h4("Relative inflation rates time series plot",align="center"),
                
                br(),
                column(2,),#end column
                column(5,
                       box(id = "myBox1",
                         plotOutput(outputId = "distPlot3"),width=12
                       ),
                       box(id = "myBox2",
                         tableOutput(outputId = "table") ,width=10
                       ),
                       ),
                column(5,
                       
                       box(id = "myBox",
                         plotOutput(outputId = "distPlot4"),width=12
                         
                       ) )
                
              )
      ),
      #second tab menu- PRESENTATION
      tabItem(tabName="presentation",
              
              h2("Introduction",style="text-align:center"),
              br(),
              br(),
              box(width=12,height="245px",
                  p(style="font-size:20px",strong("Les demissions des employes "),"sont une realite auquelle toute entreprise fait face quotidienement.Toutefois, si la situation n'est pas geree correctement, le depart de membres importants du personnel peut entrainer une baisse de la productivite. L'organisation peut etre amenee a employer de nouvelles personnes et a les former aux outils utilises, ce qui prend du temps.
                     Les raisons pour lesquelles les employes quittent l'entreprise sont nombreuses : insatisfaction salariale, stagnation de la carriere, etc. La perte n'est pas seulement financiere, mais l'entreprise perd aussi parfois les employes qualifies qui sont les actifs les plus precieux de l'entreprise. Si l'entreprise arrive a prevoir l'attrition des employes dans un avenir proche et determiner les causes de leur depart, elle peut travailler au prealable sur le maintien en poste et eviter la perte de ses employes de valeur. En se basant sur les donnees fournies par une base de donnees creee par IBM, que vous pouvez trouver sur le lien suivant : https://www.kaggle.com/pavansubhasht/ibm-hr-analytics-attrition-dataset, on va essayer de decouvrir les facteurs qui expliquent l'attrition des employes dans cette etude de cas, et par ailleurs, explorer la possibilite de predire le depart des employes en se basant sur les variables disponibles."),
                  
                  
              ),#end row
              h2("Methodes utilises",style="text-align:center"),
              box(width=12,height="650px",
                  p(style="font-size:20px",strong("1- Statistiques descriptives"),br(),"Durant tout le long de notre etude, on a utilise un ensemble de methodes et techniques permettant de decrire de facon synthetique et parlante les donnees observees pour mieux les analyser. En effet, pour les variables qualitatives on a effectue des distributions de frequences et realise des graphiques representatifs associes e savoir les diagrammes en batons. Quand aux variables quantitatives, on a calcule quelques indicateurs de tendances centrales (la Moyenne et la Mediane) et des indicateurs de dispersion (ecart-type, coefficient de variation), des indicateurs, de meme on a etudie quelques representations graphiques telle la boite e moustache. D'autre part, pour etudier la relation entre deux variables et determiner l'existence d'une eventuelle liaison entre elles, on a utilise les methodes suivantes : tableau croise accompagne des indicateurs Khi-deux et v de cramer, et la correlation."),
                  br(),
                  p(style="font-size:20px",strong("2- Khi-deux "),br(),"Un test d'independance, qui permet de demontrer si une relation existe et unit les modalites de deux variables qualitatives et de determiner la force du lien."),
                  br(),
                  p(style="font-size:20px",strong("3- Correlation "),br(),"Un test de correlation permettant de verifier l'existence d'une relation significative entre deux variables quantitatives continues."),
                  br(),
                  p(style="font-size:20px",strong("4- Regression Lineaire "),br(),"Dans le cas ou deux variables sont fortement correlees, on essaie de modeliser la relation entre la variable e expliquer et celle explicative e travers la regression lineaire, afin de predire ses valeurs."),
                  br(),
                  p(style="font-size:20px",strong("5- Prediction de l'Attrition avec la Regression Lasso "),br(),"Comme on a de multiples variables dans notre modele de regression logistique, il peut etre utile de trouver un ensemble reduit de variables resultant en un modele a performance optimale.
La regression logistique penalisee impose une penalite au modele logistique pour avoir trop de variables. Cela a pour consequence de reduire a zero les coefficients des variables les moins contributives. Ceci est egalement connu sous le nom de regularisation.
La regression penalisee utilisee pour ce projet  est la Regression lasso : les coefficients de certaines variables moins contributives sont forces a etre exactement nuls. Seules les variables les plus significatives sont conservees dans le modele final.
"),
                  
                  
                  ),
              h2("Resultats et Interpretations",style="text-align:center"),
              box(width=12,height="5000px",
                  
                  p(style="font-size:20px","Tout d'abord, on a explore le pourcentage des employes qui sont partis et ceux qui sont restes. On a trace alors le graphique suivant :"),
                  
                  plotOutput(outputId ="barp", width=840),br(),
                  p(style="font-size:20px","On remarque que 83.88% des employes n'ont pas quitte l'organisation alors que 16.12% l'ont fait. Cette base de donnees peut representer alors un probleme de donnees desequilibrees. Le fait de savoir qu'on pourrait avoir affaire a un tel probleme nous aidera a determiner la meilleure approche pour mettre en oeuvre une conclusion significative. Par la suite, nous avons applique un filtre, de telle facon a limiter l'etude aux personnes qui ont quitte l'entreprise (autrement dit : attrition=oui), afin d'identifier les facteurs qui les poussent a ceci. On a cherche a connaitre qui a le plus tendance a quitter l'entreprise, les hommes ou bien les femmes. On a realise ainsi le graphique suivant :"),
                  br(),
                  plotOutput(outputId ="gend", width=840),br(),
                  p(style="font-size:20px"," On constate que 60% des employes qui quittent l'entreprise sont des hommes contre 40% qui sont des femmes. On peut se demander alors : pourquoi cette disproportion ? Est-ce qu'il y a des facteurs lies aux hommes qui les poussent a quitter, ou d'autres au contraire qui retiennent les femmes dans l'entreprise ?", br(), "Pour cela, on a decide d'etudier chaque genre a part. On a commence par comparer la distribution de 
tranches age des employes qui ont quitte. On a retrouve le resultat suivant :"),
                  br(),
                  plotOutput(outputId ="pyr", width=840),br(),
                  p(style="font-size:20px"," On constate que 60% des employes qui quittent l'entreprise sont des hommes contre 40% qui sont des femmes. On peut se demander alors : pourquoi cette disproportion ? Est-ce qu'il y a des facteurs lies aux hommes qui les poussent a quitter, ou d'autres au contraire qui retiennent les femmes dans l'entreprise ?", br(), " On remarque que la tranche d'age qui a le plus tendance a quitter l'entreprise, que ce soit pour les hommes ou pour les femmes, est celle approximativement entre 28 ans et 32 ans, suivie par celle entre 33 et 37. On remarque aussi que le pourcentage des hommes qui quittent l'entreprise est toujours plus grand que celui des femmes, quelque soit la tranche sauf pour la premiere tranche. Le pourcentage majoritaire des hommes et  femmes qui sont partis etaient ages approximativement entre 23 ans et 37 ans. Ceci montre qu'il est indispensable de donner plus d'importance a cette tranche d'age afin d'essayer de les retenir.
                    ", br(),br(), "Maintenant, on va comparer la satisfaction professionnelle des deux genres. On commence d'abord par identifier le pourcentage de non-satisfaction au travail. On retrouve le resultat suivant :"),
                  br(),
                  
                  plotOutput(outputId ="satisf", width=840),br(),
                  p(style="font-size:20px"," On remarque que les hommes sont moins satisfaits de leur travail que les femmes au sein de l'entreprise. Et comme on a constate au debut que les employes qui quittaient l'entreprise, etaient plus des hommes que de femmes (60% des hommes contre 40% des femmes), alors l'insatisfaction est peut etre un facteur qui poussent les hommes a demissoner."),
                  br(),
                  
                  plotOutput(outputId ="qual", width=840),br(),
 
                  plotOutput(outputId ="qualh", width=840),br(),
          
                  
                  plotOutput(outputId ="qualf", width=840),br(),
                  
                  
                  plotOutput(outputId ="quale", width=840),br(),
                  p(style="font-size:20px"," On constate que la grande majorite des employes ayant quitte l'entreprise, travaillait dans le departement de recherche et developpement, avec un pourcentage egal a 56.3%, suivie par celui des ventes avec 23%. On note que le pourcentage des hommes travaillant dans le departement de recherche et developpement et ayant quitte, est deux fois plus grand que celui des femmes. Les hommes ont plus tendance a quitter l'entreprise que les femmes lorsqu'ils travaillent dans le departement de recherche et developpement. Le departement des ressources humaines connait rarement des departs pour les deux genres. On peut conclure que le departement de recherche et developpement souffre d'un eventuel probleme qui pousse les employes a quitter leur poste la-bas, et surtout les hommes.",br(),"On peut se demander si le salaire est l'un des facteurs qui causent le depart des employes de cette entreprise ? Pour repondre a cette question on a effectue la Boite a moustache suivante qui compare le salaire des employes partis et ceux restes:"),
                  br(),
                  
                  plotOutput(outputId ="qualm", width=840),br(),
                  p(style="font-size:20px"," On a constate que le groupe d'employes n'ayant pas quitter l'entreprise, avaient un indicateur de dispersion plus eleve avec une fourchette statistique egale approximativement a 16 500 $ sans tenir en compte les valeurs aberrantes par rapport au groupe 2 qui a quitte son poste pour approximativement 10 000 $. 25% des employes du groupe (attrition = non) ont un revenu mensuel moins de 3000 $ par mois, tandis que pour le groupe 2, nous trouvons 50% des employes ayant un salaire inferieur approximativement a 3000 $. 50% des employes restes touchent un salaire au plus egal a 5204 $, tandis que 50% des employes partis touchaient un salaire au plus egal approximativement a 3202 $. Les employes qui ont demissionne ont tendance a avoir un salaire bien inferieur a celui des autres employes."),
                  ),
              
              h2("Conclusion",style="text-align:center"),
             
            
              box(width=12,height="430px",
                  p(style="font-size:20px","En guise de conclusion, l'etude de cette base de donnees nous a permis d'identifier quelques facteurs qui apparemment causent le depart des employes. Ces facteurs la peuvent etre classes en deux categories : une, generale, commune a tous les employes sans exception, et puis une specifique aux hommes. En effet, les facteurs communs concernent le salaire ; comme on a pu voir les employes qui ont demissionne avaient un salaire bien inferieur a celui des employes restes, les heures supplementaires ; la majorite des employes qui quittent font des heures supplementaires, et l'age ; la tranche d'age quittant le plus l'entreprise est celle 28 ans et 32 ans. C'est la periode durant laquelle les employes cherchent a evoluer et chercher de meilleures opportunites pour booster leur carriere, et ameliorer leur situation financiere. Il faut donc songer a offrir aux employes plus de chances et de garanties d'evolution de carriere au sein de l'entreprise. Quant aux facteurs lies aux hommes, il est d'abord question de leur non-satisfaction au travail. Ce facteur etant un peu faible, mais ayant quand meme un impact significatif. Aussi, il est tres clair que le departement de recherche et developpement connait un reel probleme qui pousse les employes a quitter leur poste, et surtout les hommes ; leur nombre etant le double de celui des femmes parties. Il est donc necessaire de fournir plus d'efforts et de se concentrer sur la satisfaction des employes de ce departement. On a pu determiner a la fin, qu'il est possible de predire les annees qu'un employe va passer au sein de l'entreprise a partir du nombre total d'annees qu'il a travaille dans sa vie. Il est vrai que cette prediction n'est pas la meilleure qu'on aurait aime avoir, mais c'est la meilleure qu'on a pu extraire de cette base de donnees. Finalement, Il ne faut pas oublier que lors de cette etude, 83.88% des employes sont toujours au sein de l'entreprise, contre seulement 16.12% des employes qui ont demissionne. Les resultats auraient ete plus pertinents si le pourcentage de ces derniers etait plus grand, et s'il y avait encore plus de variables concernant par exemple la qualite de communication, la relation des employes avec la direction, le manque d'evolution dans un poste, etc. De plus, il serait possible de predire l'attrition de maniere plus precise. Cela dit, les resultats qu'on a pu obtenir sont satisfaisants et ont permis d'identifier plusieurs facteurs jouant un role important dans l'attrition des employes de cette entreprise. "),
                  
                  
              ),#end row
              
              h4("Made with love from", strong("Abdellah Madane & Mouad Et-tali")),
              a("Code R du projet bientot sur github",target="_blank",href="LINK Github")
              
      ),
      
      tabItem(tabName = "bivariee",
              
              #h3("Time series of Inflation rates of Economic trade unions",align="center") ,
              
              fluidRow(
                
                
                column(3,
                       
                       box(selectInput("Var1",label="Choisir variable 1:",choices=country),width = 12),br(),
                       box(selectInput("Var2",label="Choisir variable 2:",choices=country),width = 12) 
                       
                ),
                column(9,                
                       box(id = "BM1",plotOutput(outputId ="quant_quant"),width=12),
                       box(id = "BM2",plotOutput(outputId ="quant_qualit"),width=12),
                       box(id = "BM3",plotOutput(outputId ="qualit_qualit"),width=12)
                       
                )# end row

              )),
      tabItem(tabName = "dataset",
              
              #h3("Time series of Inflation rates of Economic trade unions",align="center") ,
              
              fluidRow(
                br(),br(),br(), 
                column(10, 
                       div(DT::dataTableOutput("txtout"),style = "font-size: 75%; width: 75%", )
                       # box(id = "BM1",plotOutput(outputId ="quant_quant"),width=12),
                       # box(id = "BM2",plotOutput(outputId ="quant_qualit"),width=12),
                       # box(id = "BM3",plotOutput(outputId ="qualit_qualit"),width=12)
                )
                
                
              )),
      

      tabItem(tabName = "predictive",
              
              h3("Prediction de l'Attrition",align="center") ,
              fluidRow(
              box(width=12,height="2000px",
              # Input values
              column(4,
                #HTML("<h3>Input parameters</h3>"),
                tags$label(h3('Input parameters')),
                numericInput("age", 
                             label = "Age", 
                             value = 5.1),
                
                selectInput("business_travel", "Buisness Travels:",
                            c("Doesn't travel" = "Non-Travel",
                              "Rarely travels" = "Travel_Rarely",
                              "Frequent Travels" = "Travel_Frequently")),
                
                selectInput("department", "Department:",
                            c("Research & Development" = "Research & Development",
                              "Sales" = "Sales",
                              "Human Resources" = "Human Resources")),
                
                selectInput("education", "Education Level:",
                            c("Bac" = "1",
                              "Bac + 2" = "2",
                              "Bac + 3" = "3",
                              "Bac + 4" = "4",
                              "Bac + 5" = "5")),
                
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
                
                selectInput("job_inv", "Job involvement:",
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
                            c( "Research Scientist"  = "Research Scientist"   ,
                               "Laboratory Technician" = "Laboratory Technician"  ,
                               "Manufacturing Director" ="Manufacturing Director" ,
                               "Healthcare Representative"= "Healthcare Representative",
                               "Research Director"        = "Research Director"  ,
                               "Sales Representative"    ="Sales Representative" ,
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
                selectInput("over_time", "works overtime", 
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
              
              
                tags$label(h3('Status/Output')), # Status/Output Text Box
                verbatimTextOutput('contents'),
                tableOutput('tabledata') # Prediction results table
                
              ))
              
      )
    )#end tabitems
    
    
  )#end body
  
)#end dashboard
