library(shiny)
library(shinyjs)
library(csv)
library(dplyr)
library(readr)
library(magrittr)
library(gbm)
library(shinyFiles)
library(Metrics)

ui <- fluidPage(
    titlePanel("Prediction of Generation Time and Yield from Genomic Attributes"),
    sidebarLayout(
        sidebarPanel(
            useShinyjs(),  # Set up shinyjs
            
            fileInput("file1", "Choose labeled TSV File",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            disabled(
                fileInput("file2", "Choose unlabeled TSV File",
                          multiple = TRUE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv"))
            ),
            tags$hr(),
            tags$h3("Model Specification"),
            fluidRow(
                sliderInput("slider1", label = h4("Training/Test split"), min = 0.30, 
                            max = 0.7, value = 45)
            ),
            textInput("iteration", "Iteration", "100"),
            textInput("shrinkage", "Shrinkage", "0.01"),
            textInput("interaction", "Interaction Depth", "3,5"),
            textInput("bagfraction", "Bag fraction", "0.65"),
            tags$hr(),
            tags$h3("Assay Specification"),
            selectInput("Assay", "Choose an assay:",
                        list("Yield", "Generation")
            ),
            selectInput("Antibiotic", "Choose an antibiotic:",
                        list("KAN", "TET", "CTX", "TRIM", "CAM","CIP")
            ),
            selectInput("Concentration", "Choose a concentration level:",
                        list("Low", "Medium","High")
            ),
            disabled(
                actionButton("go", "Start Training/Fitting")
            ),
            tags$h3("Save Model Output"),
            disabled(
                shinySaveButton("save", "Save labeled predictions", "Save file as ...", filetype=list(csv="csv"))
            ),
            disabled(
                shinySaveButton("save_un", "Save unlabeled predictions", "Save file as ...", filetype=list(csv="csv"))
            )
        ),
        
        mainPanel(
            tabsetPanel(position = "below",
                        tabPanel("Plot", plotOutput("plot")), 
                        tabPanel("Summary", plotOutput("summary")),
                        tabPanel("Importance", plotOutput("importance")),
                        tabPanel("Table", tableOutput("table")),
                        tabPanel("Report", fluidRow(
                            column(width = 7, tableOutput("report_train")),
                            column(width = 5, tableOutput("report_test")))),
                        tabPanel("Model Performance", plotOutput("plot_performance"))
            )
        )
        
    )
)

server <- function(input, output, session) {
    
    observeEvent(req(input$file1), {enable("go")})
    observeEvent(req(input$file1), {enable("file2")})
    
    
    attach_metadata_predictors<-function(predictors, assay, antibiotic, concentration){
        library(dplyr)
        library(tidyverse)
        library(readr)
        metadata<-read_tsv("Metadata_median.txt")
        colnames(metadata)<-c("Lane", "Generation_CAM-Low","Yield_CAM-Low","Generation_CAM-Medium","Yield_CAM-Medium","Generation_CAM-High","Yield_CAM-High","Generation_CIP-Low","Yield_CIP-Low","Generation_CIP-Medium","Yield_CIP-Medium","Generation_CIP-High","Yield_CIP-High","Generation_CTX-Low",   
                              "Yield_CTX-Low","Generation_CTX-Medium", "Yield_CTX-Medium","Generation_CTX-High", "Yield_CTX-High","Generation_KAN-High","Yield_KAN-High","Generation_KAN-Low", "Yield_KAN-Low", "Generation_KAN-Medium","Yield_KAN-Medium","Generation_TET-Low","Yield_TET-Low","Generation_TET-Medium",  
                              "Yield_TET-Medium","Generation_TET-High","Yield_TET-High","Generation_TRIM-Low","Yield_TRIM-Low", "Generation_TRIM-Medium",  "Yield_TRIM-Medium", "Generation_TRIM-High","Yield_TRIM-High")
        
        id<-match(paste0(assay,"_",antibiotic,"-",concentration), colnames(metadata))
        metadata<- metadata %>% select(c(1,id))
        label<-inner_join(metadata, predictors, by= c('Lane' = 'name')) %>% drop_na() %>% select(-1) %>% rename(., label=colnames(.)[1] )
        return(label)
    }
    test_train_split<-function(dataset, random_split){
        library(datasets)
        library(caret)
        library(readr)
        validation_index <- createDataPartition(dataset$label, p=random_split, list=FALSE)
        validation <- dataset[-validation_index,]
        dataset <- dataset[validation_index,]
        return(list(validation,dataset))
    }
    model_fit<-function(dataset){
        set.seed(123)
        library(gbm) 
        return(gbm(
            formula = label ~ .,
            distribution = "gaussian",
            data = dataset,
            n.trees = 100,
            interaction.depth = 3,
            shrinkage = 0.1,
            cv.folds = 5,
            n.cores = NULL, # will use all cores by default
            verbose = FALSE
        ))
    }
    model_predict<-function(tuned_model, validation){
        pred <- predict(tuned_model, n.trees = tuned_model$n.trees, validation[-1])
        results<-data.frame(cbind( validation[1],pred))
        colnames(results)<-c('test', 'predicted')
        return(results)
    }
    get_binmodal_distribution<-function(results){
        library(mixtools)
        mixmdl = normalmixEM(results$predicted)
        return(mixmdl)
    }
    predict_label_bimodal<-function(mixmdl){
        return(ifelse(mixmdl$posterior[,2]<0.5,'S', 'R'))
    }
    plot_bimodal<-function(x,mixmdl, title){
        sdnorm =function(x, mean=0, sd=1, lambda=1){lambda*dnorm(x, mean=mean, sd=sd)}
        p<-ggplot(data.frame(x=x)) + 
            geom_histogram(aes(x=x,y=..density..),fill="#00000080",bins = 50) +
            stat_function(fun=sdnorm,
                          args=list(mean=mixmdl$mu[2],
                                    sd=mixmdl$sigma[2],
                                    lambda=mixmdl$lambda[2]),
                          colour = "#0000FF",size=2) +
            stat_function(fun=sdnorm,
                          args=list(mean=mixmdl$mu[1],
                                    sd=mixmdl$sigma[1],
                                    lambda=mixmdl$lambda[1]),
                          colour = "#FF0000",size=2)+
            # scale_colour_manual("Lgend title", values = c("#FF0000", "#0000FF"))+
            ggtitle(title)+
            xlab("") + 
            ylab('Density')+
            theme_light()
        return(p)
    }
    metrics<-function(actual, predicted){
        library(Metrics)
        output<-data.frame(rbind(
            c('MAE', mae(actual, predicted)),
            c('RMSE',rmse(actual, predicted)),
            c('SMAPE',smape(actual, predicted)),
            c('MDAE',mdae(actual, predicted)),
            c('SPEARMAN RHO',cor.test(actual,predicted,method="spearman")[[4]]),
            c('PEARSON R',cor.test(actual,predicted,method="pearson")[[4]]),
            c('R SQR',summary(lm(actual ~ predicted))[[8]])
        ))
        colnames(output)<-c('metric', 'value')
        output$value<-sapply(output$value, function(x) formatC(as.numeric(as.character(x)), digits = 2, format = "f"))
        return(output)
    }
    
    grid<-function(shrinkage_input,interaction_input, bagfraction_input, iteration_input){
        return(expand.grid(
            shrinkage = as.numeric(as.character(strsplit(shrinkage_input,",")[[1]])),
            interaction.depth = as.numeric(as.character(strsplit(interaction_input,",")[[1]])),
            n.minobsinnode = c(5),
            bag.fraction = as.numeric(as.character(strsplit(bagfraction_input,",")[[1]])), 
            iteration= as.numeric(as.character(strsplit(iteration_input,",")[[1]])),
            optimal_trees = 0,               # a place to dump results
            min_RMSE = 0 ))                  # a place to dump results
    }
    model_fit_grid_search<-function(dataset, hyper_grid){
        for(i in 1:nrow(hyper_grid)) {
            print(i)
            set.seed(123)
            # train model
            gbm.tune <- gbm(
                formula = label ~ .,
                distribution = "gaussian",
                data = dataset,
                n.trees = hyper_grid$iteration[i],
                interaction.depth = hyper_grid$interaction.depth[i],
                shrinkage = hyper_grid$shrinkage[i],
                n.minobsinnode = hyper_grid$n.minobsinnode[i],
                bag.fraction = hyper_grid$bag.fraction[i],
                train.fraction = .75,
                cv.folds = 3,
                n.cores = NULL, # will use all cores by default
                verbose = FALSE
            )
            
            # add min training error and trees to grid
            hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
            hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
        }
        
        
        top<-hyper_grid %>% 
            dplyr::arrange(min_RMSE) %>%
            head(1)
        
        return(gbm(
            formula = label ~ .,
            distribution = "gaussian",
            data = dataset,
            n.trees = top$iteration,
            interaction.depth = top$interaction.depth,
            shrinkage = top$shrinkage,
            n.minobsinnode = top$n.minobsinnode,
            bag.fraction = top$bag.fraction,
            train.fraction = .75,
            cv.folds = 3,
            n.cores = NULL, # will use all cores by default
            verbose = FALSE
        ))
    }
    pipeline <- eventReactive(input$go, {
        req(input$file1)
        predictors <- read_tsv(input$file1$datapath)
        
        input_dataframe<-attach_metadata_predictors(predictors, input$Assay, input$Antibiotic, input$Concentration)
        
        test_train<-test_train_split(input_dataframe,input$slider1)
        
        fitted_model<-model_fit_grid_search(test_train[[1]],grid(input$shrinkage,input$interaction, input$bagfraction, input$iteration))
        
        observeEvent(input$go, {enable("save")})
        observeEvent(req(input$file2), {enable("save_un")})
        
        return(list(fitted_model,test_train[[1]],test_train[[2]]))
    })
    
    output$table<- renderTable({
        results<-model_predict(pipeline()[[1]], pipeline()[[2]])
        mixmdl_pred <-normalmixEM(results$predict)
        mixmdl_test <-normalmixEM(results$test)
        
        labels_pred<-predict_label_bimodal(mixmdl_pred)
        labels_test<-predict_label_bimodal(mixmdl_test)
        
        return(data.frame(table(labels_test, labels_pred)))
    },align='c', caption="CONFUSION MATRIX", caption.placement = getOption("xtable.caption.placement", "top"))
    output$contents <- renderTable({
        return(pipeline())
    })
    output$plot <-renderPlot({
        library(mixtools)
        
        results<-model_predict(pipeline()[[1]], pipeline()[[2]])
        mixmdl =normalmixEM(results$predict)
        p_pred<-plot_bimodal(results$predict, mixmdl, 'Predicted')
        
        x<-results$test
        mixmdl=normalmixEM(x)
        p_test<-plot_bimodal(x, mixmdl, 'test')
        return(grid.arrange(
            p_pred ,
            p_test ,
            nrow = 1,
            top = "Fitted Bimodal Plots"
        ))
    })
    output$plot_performance<-renderPlot({return(gbm.perf(pipeline()[[1]], method = "OOB"))})
    output$importance <-renderPlot({
        importance<-summary(pipeline()[[1]])[summary(pipeline()[[1]])$rel.inf>0,]
        importance$var<-gsub('.match','',importance$var)
        
        importance<-data.frame(importance)
        positions<-importance$var
        p<-ggplot(data.frame(importance), aes(x=var, y=rel.inf))+
            geom_bar(stat = "identity")+ 
            scale_x_discrete(limits = positions)+
            xlab("") + 
            ylab('Feature Importance')+
            theme_light()+
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        return(p)
    })
    output$summary <-renderPlot({
        library(ggplot2)
        library(ggExtra)
        xgr<-model_predict(pipeline()[[1]], pipeline()[[3]])
        train <- ggplot(xgr, aes(x=test, y=predicted)) +
            geom_point() +
            geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)+ 
            coord_cartesian(ylim = c(min(xgr$predicted),max(xgr$predicted)), xlim = c(min(xgr$test), max(xgr$test)))+
            theme_light() 
        train_margin <- ggMarginal(train, color="purple", size=4)
        
        xgr<-model_predict(pipeline()[[1]], pipeline()[[2]])
        test <- ggplot(xgr, aes(x=test, y=predicted)) +
            geom_point() +
            geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)+ 
            coord_cartesian(ylim = c(min(xgr$predicted),max(xgr$predicted)), xlim = c(min(xgr$test), max(xgr$test)))+
            theme_light() 
        test_margin <- ggMarginal(test, color="purple", size=4)
        
        return(grid.arrange(
            train_margin ,
            test_margin ,
            nrow = 1,
            top = "Scatter Plot Test versus Predicted"
        ))
    })
    output$report_test<-renderTable({
        results<-model_predict(pipeline()[[1]], pipeline()[[2]])
        return(metrics(results$test, results$predicted))
    },align='c', caption="TEST", caption.placement = getOption("xtable.caption.placement", "top"))
    output$report_train<-renderTable({ 
        results<-model_predict(pipeline()[[1]], pipeline()[[3]])
        return(metrics(results$test, results$predicted))
    },align='c', caption="   TRAIN", caption.placement = getOption("xtable.caption.placement", "top"))
    
    observe({
        volumes <- c("UserFolder"="/")
        shinyFileSave(input, "save", roots=volumes, session=session)
        fileinfo <- parseSavePath(volumes, input$save)
        data <- model_predict(pipeline()[[1]], pipeline()[[2]])
        if (nrow(fileinfo) > 0) {
            write.csv(data, as.character(fileinfo$datapath))
        }
    })
    observe({
        req(input$file2)
        volumes <- c("UserFolder"="/")
        shinyFileSave(input, "save_un", roots=volumes, session=session)
        predictors <- read_tsv(input$file2$datapath)
        fileinfo <- parseSavePath(volumes, input$save_un)
        tuned_model=pipeline()[[1]]
        data<-data.frame(predict(tuned_model, n.trees = tuned_model$n.trees, predictors))
        colnames(data)<-'predict'
        if (nrow(fileinfo) > 0) {
            write.csv(data, as.character(fileinfo$datapath))
        }
    })
}

shinyApp(ui, server)
