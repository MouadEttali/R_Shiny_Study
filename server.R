library(tidyverse)
require(shinydashboard)
require(ggplot2)
require(dplyr)
library(readxl)
require(tidyr)
library(tidymodels)
library(scales)
library(janitor)
library(gridExtra)
library(glue)
library(ggcorrplot)
library(vip)
library(dplyr)
library(RColorBrewer)
library(hrbrthemes)
library(shinyjs)
library(DT)
library(tidyr)
library(data.table)
library(workflows)
library("glmnet")
# Read in the Lasso model
model <- readRDS("final_model.rds")



myPalette <- brewer.pal(5, "Set2")
data <- read_csv("dataPreproc.csv")
data2 <- read_csv("HR-Employee-Attrition.csv") %>% clean_names()
# Dividing features into vectors to faciltate plotting
numerical <- c("age", "distance_from_home","hourly_rate",
               "daily_rate", "monthly_rate","monthly_income",
               "percent_salary_hike","years_at_company",
               "years_in_current_role","years_since_last_promotion",
               "years_with_curr_manager","total_working_years",
               "num_companies_worked","training_times_last_year")

categorical <- c("gender","over_time","department",
                 "education_field", "job_role", "marital_status","environment_satisfaction", "job_satisfaction",
                 "relationship_satisfaction","work_life_balance",
                 "job_involvement","performance_rating",
                 "business_travel", "education","job_level",
                 "stock_option_level")

ordinal <- c("environment_satisfaction", "job_satisfaction",
             "relationship_satisfaction","work_life_balance",
             "job_involvement","performance_rating",
             "business_travel", "education","job_level",
             "stock_option_level")
server <- function(input, output) {
  
  observeEvent(input$country, {
    
    if(input$country %in% categorical){
      shinyjs::hide(id = "myBox")
      shinyjs::hide(id = "myBox1")
      shinyjs::show(id = "myBox2")
    }else{
      shinyjs::hide(id = "myBox2")
      shinyjs::show(id = "myBox1")
      shinyjs::show(id = "myBox")
      
    }
  })
  output$txtout <- renderDataTable({
    data
    
  })
    output$distPlot <- renderPlot({
    inp<-input$country
    x    <-  data[,input$country]
    if(inp %in% numerical){ 
      hist(x[[1]], col = "#75AADB", border = "white",
      xlab = paste(inp, "(ans)"),
      main = paste("Histograme de la variable:",inp))
    }else{
      y    <-  data2[,input$country]
      effectifs <- reactive({table(y)})
      pie(effectifs(), labels = substr(names(effectifs()), 1, 16), 
          main = paste("Diagramme en secteur de la variable:", inp), border="white", col=myPalette, radius = 1)

      
      }

    
  })
    
  output$distPlot2 <- renderPlot({
  inp<-input$country
  if(inp %in% numerical){ 
  ggplot(data, aes(x=age)) +
    geom_density(position="identity", alpha=0.5) + 
    theme_bw() + 
    scale_fill_brewer(palette="Set1") +
    ggtitle(paste("Courbe de densite de la variable:", inp)) + 
    theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15), legend.position="bottom") +
    labs(x = "Age") 
  }else{
    y    <-  data2[,input$country]
    effectifs <- reactive({table(y)})
    barplot(effectifs(), main = paste("Diagramme en baton de la variable:", inp), border="white", col=myPalette, radius = 1,
            ylab="Effectifs", las = 1,
            names.arg = substr(names(effectifs()), 1, 14))
    
    
  }
    
  })
  

  output$distPlot3 <- renderPlot({
    inp<-input$country
    if(inp %in% numerical){ 
    x    <-  data[,input$country]
    plot(ecdf(x[[1]]),
         xlab=inp,
         ylab="Effectif cumule",
         main=paste("Courbe cumule de la variable:", inp)
         )
    }
    
  })
  
  output$table <- renderTable(
    {
      inp<-input$country
      if(inp %in% categorical){
      y    <-  data2[,input$country]
      effectifs <- reactive({table(y)})
      effectifs()
   
    }}, colnames = TRUE, bordered = TRUE, striped = TRUE,  width = "100%",height = "100%", 
    align = 'c',
    
    )
  

  
  output$distPlot4 <- renderPlot({
    inp<-input$country
    if(inp %in% numerical){ 
    d    <-  data[,input$country]
    ggplot(d) +
      aes(y = d[[1]]) +
      geom_boxplot() +
      ylab("age") +
      ggtitle(paste("Boite a moustache de la variable:", inp)) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15), legend.position="bottom")
    
    }
  })
  
  #####################Bivariee
  observeEvent(c(input$Var1, input$Var2), {
    
    if(input$Var1 %in% numerical & input$Var2 %in% numerical){
      shinyjs::hide(id = "BM3")
      shinyjs::hide(id = "BM2")
      shinyjs::show(id = "BM1")

    }else if(input$Var1 %in% categorical & input$Var2 %in% categorical){
      shinyjs::hide(id = "BM2")
      shinyjs::hide(id = "BM1")
      shinyjs::show(id = "BM3")

    }else{
      shinyjs::hide(id = "BM3")
      shinyjs::hide(id = "BM1")
      shinyjs::show(id = "BM2")
      
    }
  })
  
  output$quant_quant <- renderPlot({
    var1<-input$Var1
    var2<-input$Var2
    if(var1 %in% numerical & var2 %in% numerical  ){ 
      x    <-  data[,var1]
      y    <-  data[,var2]
      fit1 <- lm(x[[1]] ~ y[[1]], data = data)
      summary(fit1)
      ggplot(data, aes(x = x[[1]], y = y[[1]])) + 
        geom_point() +
        stat_smooth(method = "lm", col = "red") +ylab(var2) + xlab(var1)+
        labs(title =paste(var1,"en fonction de", var2),subtitle = paste("Adj R2 = ",signif(summary(fit1)$adj.r.squared, 4),
                           "Intercept =",signif(fit1$coef[[1]],4 ),
                           " Slope =",signif(fit1$coef[[2]], 4),
                           " P =",signif(summary(fit1)$coef[2,4], 4)))+
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),plot.subtitle = element_text(face = "bold", hjust = 0.5, size = 15))
      
    }
  })
    
    
    output$quant_qualit <- renderPlot({
      var1<-input$Var1
      var2<-input$Var2
      if(xor((var1 %in% numerical & var2 %in% categorical),(var2 %in% numerical & var1 %in% categorical))){ 
        if (var1 %in% categorical){
          categ    <-  var1
          quanti    <-  var2
        }else{
          categ    <-  var2
          quanti    <-  var1
        }
        x    <-  data[,categ]
        y    <-  data[,quanti]

        ggplot(data, aes(x = as.factor(x[[1]]), y = y[[1]])) + 
          geom_boxplot( ) +
          labs(title = paste("Boite a moustache de", quanti, "en fonction de", categ),
               x = categ, 
               y = quanti)+
          theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20))
        
        
    
        
      }
    })

    
    output$qualit_qualit <- renderPlot({
      var1<-input$Var1
      var2<-input$Var2
      if(input$Var1 %in% categorical & input$Var2 %in% categorical){ 
        x    <-  data[,var1]
        y    <-  data[,var2]

        ggplot(data, aes(x = as.factor(x[[1]]), fill = as.factor(y[[1]]), label = scales::percent(prop.table(stat(count))) ))+ 
          geom_text(stat = 'count',
                    position = position_dodge(.9), 
                    vjust = -0.5, 
                    size = 3) + geom_bar(position = "dodge")+
          ylab(var2) + xlab(var1)+ ggtitle(paste("Type de", var1, "selon", var2))+
          theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20))+ scale_fill_discrete(name = var2)
      }
    })
    
    
    
    
  #########PRESENTATION
    summarise_att <- function(tbl) {
      tbl %>%
        summarise(att = sum(attrition == 1),
                  n = n()) %>% 
        mutate(pct_att = att/n)
    }
    
    output$barp<- renderPlot(

      
      data2 %>%
        group_by(attrition) %>%
        summarise_att() %>%
        ggplot(aes(x = "",y = n,fill = attrition)) +
        geom_bar(width = 2,stat = "identity", color="white") +
        coord_polar(theta = "y") +
        geom_text(aes(label = paste0(round(n/sum(n),4)*100,"%")),
                  position = position_stack(vjust = 0.5),color = "white", size=5) +
        theme_minimal()+
        
        coord_polar("y", start=0) +
        theme_void()  +
        theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
              panel.border = element_blank(),panel.grid=element_blank(),
              axis.ticks = element_blank(),
              plot.title=element_text(size=14, face="bold")) +
        labs(title = "Diagramme en secteur representant le pourcentage des employes partis" )
      +   scale_fill_brewer(palette="Set2")

      
      
    )
    gend_attr <- data2 %>% 
      group_by(gender, attrition) %>% 
      summarise(n = n())
    gend_tots <- data2 %>% 
      group_by(gender) %>% 
      summarise(nums = n())
    gend_attr <- left_join(gend_attr, gend_tots, by = "gender")
    gend_attr <- gend_attr %>% 
      mutate(attr_perc = n / nums * 100)

    output$gend <- renderPlot(
      ggplot(data2, aes(x = as.factor(data2$attrition), fill = as.factor(data2$gender), label = scales::percent(prop.table(stat(count))) ))+ 
        geom_text(stat = 'count',
                  position = position_dodge(.9), 
                  vjust = -0.5, 
                  size = 3) + geom_bar(position = "dodge")+
        ylab("Genre") + xlab("Attrition")+ ggtitle(paste("Type de", "Attrition", "selon Genre"))+
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20))+ scale_fill_discrete(name = "Genre")
    )
    
    GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, draw_group = function(self, data, ..., draw_quantiles = NULL){
      data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
      grp <- data[1,'group']
      newdata <- plyr::arrange(transform(data, x = if(grp%%2==1) xminv else xmaxv), if(grp%%2==1) y else -y)
      newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
      newdata[c(1,nrow(newdata)-1,nrow(newdata)), 'x'] <- round(newdata[1, 'x']) 
      if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
        stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 
                                                  1))
        quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
        aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
        aesthetics$alpha <- rep(1, nrow(quantiles))
        both <- cbind(quantiles, aesthetics)
        quantile_grob <- GeomPath$draw_panel(both, ...)
        ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
      }
      else {
        ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
      }
    })
    
    geom_split_violin <- function (mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
      layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, position = position, show.legend = show.legend, inherit.aes = inherit.aes, params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
    }
    
    output$pyr <- renderPlot(
      data2 %>% ggplot(aes(x=gender, y=age, fill=attrition)) +
        geom_split_violin() + 
        theme_bw() + 
        scale_fill_brewer(palette="Set1") +
        scale_color_manual(values=c("#661304", "#040242")) + 
        ggtitle("Sex and Age vs Survived") + 
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15), legend.position="bottom") +
        labs(x = "Sex", y = "Age")
      
    )
    
    output$satisf <- renderPlot(
      ggplot(data2, aes(x = as.factor(data2$job_satisfaction), fill = as.factor(data2$gender), label = scales::percent(prop.table(stat(count))) ))+ 
        geom_text(stat = 'count',
                  position = position_dodge(.9), 
                  vjust = -0.5, 
                  size = 3) + geom_bar(position = "dodge")+
        ylab("Genre") + xlab("Satisfaction du travail")+ ggtitle(paste("Satisfaction du travail", "selon le genre"))+
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20))+ scale_fill_discrete(name = "Genre")
    )
    
    output$qual <- renderPlot({

        ggplot(data2, aes(x = data2$department, fill = data2$attrition, label = scales::percent(prop.table(stat(count))) ))+ 
          geom_text(stat = 'count',
                    position = position_dodge(.9), 
                    vjust = -0.5, 
                    size = 3) + geom_bar(position = "dodge")+
          ylab("attrition") + xlab("departement")+ ggtitle(paste("Visualisation de l'attrition selons les departements"))+
          theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20))+ scale_fill_discrete(name = "attrition")
        })
    
    
    ##CASHOMMES
    output$qualh <- renderPlot({
      t<- filter(data2, gender=="Male")
      ggplot(t, aes(x = t$department, fill = t$attrition, label = scales::percent(prop.table(stat(count))) ))+ 
        geom_text(stat = 'count',
                  position = position_dodge(.9), 
                  vjust = -0.5, 
                  size = 3) + geom_bar(position = "dodge")+
        ylab("attrition") + xlab("departement")+ ggtitle(paste("Visualisation de l'attrition selons les departements - Cas des Hommes"))+
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20))+ scale_fill_discrete(name = "attrition")
    })
    
    ##CASFEMMES
    output$qualf <- renderPlot({
      t<- filter(data2, gender=="Female")
      ggplot(t, aes(x = t$department, fill = t$attrition, label = scales::percent(prop.table(stat(count))) ))+ 
        geom_text(stat = 'count',
                  position = position_dodge(.9), 
                  vjust = -0.5, 
                  size = 3) + geom_bar(position = "dodge")+
        ylab("attrition") + xlab("departement")+ ggtitle(paste("Visualisation de l'attrition selons les departements - Cas des Femmes"))+
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20))+ scale_fill_discrete(name = "attrition")
    })
    
    output$quale <- renderPlot({
      ggplot(data2, aes(x = data2$department, fill = data2$gender, label = scales::percent(prop.table(stat(count))) ))+ 
        geom_text(stat = 'count',
                  position = position_dodge(.9), 
                  vjust = -0.5, 
                  size = 3) + geom_bar(position = "dodge")+
        ylab("genre") + xlab("departement")+ ggtitle(paste("Comparaison du pourcentage des hommes et des femmes selons les departements"))+
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20))+ scale_fill_discrete(name = "genre")
    })
    
    output$qualm <- renderPlot({
      ggplot(data2, aes(x = as.factor(data2$attrition), y = data2$monthly_income)) + 
        geom_boxplot( ) +
        labs(title = paste("Boite a moustache salaire - attrition"),
             x = "attrition", 
             y = "salaire")+
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20))
    })
    
    
    #######################PREDICTION
    # Input Data
    datasetInput <- reactive({  
      
      df <- data.frame(
        age = as.numeric(input$age),
        attrition = as.factor(sample(0:1,1)),
        business_travel=as.ordered(input$business_travel),
        department= as.factor(input$department),
        education=as.factor(input$education),
        education_field=as.factor(input$education_field),
        employee_number=sample(0:4000,1),
        environment_satisfaction=as.factor(input$envir_satis),
        gender= as.character(input$gender),
        job_involvement=as.factor(input$job_inv),
        job_level=as.factor(input$job_lev),
        job_role=as.factor(input$job_role),
        job_satisfaction=as.factor(input$job_satis),
        marital_status=as.factor(input$marital_stat),
        monthly_income=as.numeric(input$monthly_income),
        num_companies_worked=as.numeric(input$num_companies_worked),
        over_time=as.factor(input$over_time),
        performance_rating=as.factor(sample(3:4,1)),
        relationship_satisfaction=as.factor(sample(1:4,1)),
        stock_option_level=as.factor(sample(0:3,1)),
        total_working_years=as.numeric(input$total_years_working),
        work_life_balance=as.factor(input$work_life_balance),
        years_in_current_role=as.numeric(input$years_in_curr_role),
        years_since_last_promotion=as.numeric(input$years_since_last_promotion),
        years_with_curr_manager=as.numeric(input$years_with_curr_manager),
        total_satisfaction=as.numeric(input$total_satis),
        
        
        stringsAsFactors = TRUE)
      
      df <-
        df %>%
        # Binary categorical
        mutate(across(c(department, education_field,
                        job_role, marital_status),~ as.factor(.))) %>%
        # Nominal categorical
        mutate(across(c(environment_satisfaction, job_satisfaction,
                        relationship_satisfaction,
                        work_life_balance,business_travel, education ,
                        job_involvement,job_level, stock_option_level,
                        performance_rating),
                      ~as.ordered(.))) %>%
        # Ordinal categorical
        mutate(business_travel = factor(business_travel, ordered = TRUE,
                                        levels = c("Non-Travel",
                                                   "Travel_Rarely","Travel_Frequently")))%>%
        
        mutate(education = factor(education, ordered = TRUE,
                                  levels = c("1",
                                             "2","3","4","5"))) %>%
        
        mutate(environment_satisfaction = factor(environment_satisfaction, ordered = TRUE,
                                                 levels = c("1",
                                                            "2","3","4"))) %>%
        mutate(job_involvement = factor(job_involvement, ordered = TRUE,
                                        levels = c("1",
                                                   "2","3","4"))) %>%
        mutate(job_level = factor(job_level, ordered = TRUE,
                                  levels = c("1",
                                             "2","3","4","5"))) %>%
        mutate(job_satisfaction = factor(job_satisfaction, ordered = TRUE,
                                         levels = c("1",
                                                    "2","3","4"))) %>%
        mutate(performance_rating = factor(performance_rating, ordered = TRUE,
                                           levels = c("3","4")))   %>% 
        mutate(relationship_satisfaction = factor(relationship_satisfaction, ordered = TRUE,
                                                  levels = c("1",
                                                             "2","3","4")))%>% 
        mutate(stock_option_level = factor(stock_option_level, ordered = TRUE,
                                           levels = c("0","1","2","3"))) %>%
        mutate(work_life_balance = factor(work_life_balance, ordered = TRUE,
                                          levels = c("1",
                                                     "2","3","4"))) 
      post_eda_processing <- function(tbl) {
        tbl %>%
          mutate(total_satisfaction =
                   as.numeric(environment_satisfaction) +
                   as.numeric(job_satisfaction) +
                   as.numeric(relationship_satisfaction) +
                   as.numeric(work_life_balance) +
                   as.numeric(job_involvement)) }
      
      
      df <- post_eda_processing(df)
      
      write.table(df,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = TRUE)
      
      test <- read.csv(paste("input", ".csv", sep=""), header = FALSE)
      
      output <- data.frame(predict(model,df),round(predict(model,df,type="prob"),3))
      rename(output,   Predicted_Class=.pred_class, Churned_Probability=.pred_1, Stayed_Probability= .pred_0 )
      #Output <- class(as.data.frame(Output))
      #colnames(Output)[0] <- "Predicted_Class
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

