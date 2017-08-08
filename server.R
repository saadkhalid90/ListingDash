## loading the required libraries
library(shiny)
library(sqldf)
library(rdrop2)
library(dplyr)
library(lubridate)

# Define server logic for listing dashboard
function(input, output) {
  dataIn <- reactive({
    ## saved application token to access dropbox through R 
    token <- readRDS("droptoken.rds")
    ##  accessing dropbox acount
    drop_acc(dtoken = token)
    
    ## reading in file from dropbox (export_'todays date'.csv)
    filename <- paste("export_", Sys.Date(), ".csv", sep = "")
    list_data <- drop_read_csv(paste("Listing Data/", filename, sep = ""), sep = ",", dtoken = token)
    comp_clusters <- drop_read_csv("Listing Data/clusters_completed.csv", sep = ",", dtoken = token)
    error_clusters <- drop_read_csv("Listing Data/clusters_error.csv", sep = ",", dtoken = token)
    
    comp_clusters <- comp_clusters[(!(comp_clusters$CLUSTER_NO %in% error_clusters$CLUSTER_NO)), ]
    
    ## removing test entries
    list_data <- subset(list_data, ENUMERATOR_CODE != 'consulting')
    
    ## standardizing date formats
    list_data$DATE_OF_ENUMERATION <- gsub("/", "-", list_data$DATE_OF_ENUMERATION)
    list_data$DATE_OF_ENUMERATION <- parse_date_time(x = list_data$DATE_OF_ENUMERATION, 
                                                     orders = c("%d-%m-%y", "%d-%m-%Y", "%d/%m/%Y"))
    
    ## converting POSIX.ct into date object for all dates
    list_data$DATE_OF_ENUMERATION <- as.Date(list_data$DATE_OF_ENUMERATION, 
                                             format = "%d-%m-%Y")
    
    
    ## query for summary at the cluster level
    subset_cluster <- sqldf('select CLUSTER_NO, ENUMERATOR, ENUMERATOR_CODE, DISTRICT, count(*) as NUM_UNIT, max(SRNO_OF_STRUCTURE) as NUM_STR, min(SRNO_OF_STRUCTURE) as START_STR, max(SRNO_OF_HH) as NUM_HH, max(SRNO_OF_ELIGIBLE_HH) as NUM_ELI_HH, min(DATE_OF_ENUMERATION) as START_DATE, max(DATE_OF_ENUMERATION) as END_DATE 
                            from list_data 
                            group by CLUSTER_NO
                            order by CLUSTER_NO')
    
    ## subsetting the cluster level summary for completed clusters
    comp_cluster_summ <- subset_cluster[subset_cluster$CLUSTER_NO %in% comp_clusters$CLUSTER_NO, ] %>%
                           select("CLUSTER_NO", "ENUMERATOR", "ENUMERATOR_CODE",
                                  "DISTRICT", "NUM_UNIT", "NUM_STR", "NUM_HH", "NUM_ELI_HH")
    
    error_cluster_summ <- subset_cluster[subset_cluster$CLUSTER_NO %in% error_clusters$CLUSTER_NO, ] %>%
                            select("CLUSTER_NO", "ENUMERATOR", "ENUMERATOR_CODE",
                                   "DISTRICT", "NUM_UNIT", "NUM_STR", "NUM_HH", "NUM_ELI_HH")
    
    
    ## changing the date to a viewable format and calculating number of days
    subset_cluster$START_DATE <- as.Date(subset_cluster$START_DATE, origin = "1970-01-01 UTC")
    subset_cluster$END_DATE <- as.Date(subset_cluster$END_DATE, origin = "1970-01-01 UTC")
    subset_cluster$DAYS <- difftime(subset_cluster$END_DATE ,subset_cluster$START_DATE , units = c("days")) + 1
    
    ## adding a status column to check cluster status
    subset_cluster$STATUS <- "Pending"
    subset_cluster$STATUS[subset_cluster$CLUSTER_NO %in% comp_clusters$CLUSTER_NO] <- "Complete"
    subset_cluster$STATUS[subset_cluster$CLUSTER_NO %in% error_clusters$CLUSTER_NO] <- "Review"
    
    
    ## query for getting district level summary
    subset_district <- sqldf('select DISTRICT, count(*) as CLUSTERS_OP, sum(NUM_STR) as NUM_STR, sum(NUM_HH) as NUM_HH, sum(NUM_ELI_HH) as NUM_ELI_HH   
                             from subset_cluster 
                             group by DISTRICT
                             order by DISTRICT')
    
    
    ## return the entire data(from dropbox) and district and cluster level summaries
    return(list(list_data = list_data,
                subset_district = subset_district,
                subset_cluster = subset_cluster,
                comp_cluster_summ = comp_cluster_summ,
                error_cluster_summ = error_cluster_summ
                ))
  })
  
  ## reactive function for creating datewise summaries
  dateData <- reactive({
    ## subset data date-wise
    list_data_date <- subset(dataIn()$list_data, DATE_OF_ENUMERATION == input$date_select)
    
    ## query for date-wise cluster level summary
    subset_cluster_date <- sqldf("select CLUSTER_NO, ENUMERATOR, ENUMERATOR_CODE, DISTRICT, 
                                 count(*) as NUM_UNIT, (max(SRNO_OF_STRUCTURE) - min(SRNO_OF_STRUCTURE) + 1) as NUM_STR, 
                                 min(SRNO_OF_STRUCTURE) as START_STR, max(SRNO_OF_STRUCTURE) as END_STR, 
                                 (max(SRNO_OF_HH) - min(SRNO_OF_HH) + 1) as NUM_HH, (max(SRNO_OF_ELIGIBLE_HH) - min(SRNO_OF_ELIGIBLE_HH) + 1) as NUM_ELI_HH 
                                 from list_data_date group by CLUSTER_NO 
                                 order by CLUSTER_NO")
    return(subset_cluster_date)
  })
  
  ## display data table of district level summary
  output$tableDist <- DT::renderDataTable(
    DT::datatable(dataIn()$subset_district, options = list(pageLength = 50))
  )
  
  ## display data table of cluster level summary
  output$tableClust <- DT::renderDataTable({
    DT_sc <- DT::datatable(dataIn()$subset_cluster, filter = 'top', options = list(pageLength = 25)) %>% DT::formatStyle(
      'STATUS',
      target = 'row',
      backgroundColor = DT::styleEqual(c('Complete', 'Review'), c('lightblue', 'pink'))
    )
  })
  
  ## display data table of datewise summary
  output$tabledateClust <- DT::renderDataTable(
    DT::datatable(dateData(), options = list(pageLength = 50))
  )
  
  ## data table for viewing detailed cluster level data
  output$tableDataClust <- DT::renderDataTable({
    CLUST_list_data <- subset(dataIn()$list_data %>% 
                                select(DATE_OF_ENUMERATION, CLUSTER_NO, DISTRICT, TEHSIL, ENUMERATOR, ENUMERATOR_CODE, 
                                       SRNO_OF_STRUCTURE, DU_NDU, SRNO_OF_HH, CHILD_0TO23_LIVING, CHILDREN_AGE023, 
                                       MOTHER_OF_CHILDREN_AGE023, SRNO_OF_ELIGIBLE_HH), 
                              CLUSTER_NO == input$selectCluster)
    
    ## select a subset of columns to display
    names(CLUST_list_data) <- c("Date", "ClusterNo", "District", "Tehsil", "Enumerator", "EnumCode", "StrNo", "DU_NDU",
                                "SR.HH", "ChildLiving", "EligChildren", "EligMothers", "SR.ELIG.HH")
    DT::datatable((CLUST_list_data), options = list(pageLength = 100))
  })
  
  ## get UI input for the cluster number, all unique cluster numbers within the dataset
  output$ClustSelect <- renderUI({
    avail_clusters <- as.numeric(levels(as.factor((dataIn()$list_data)$CLUSTER_NO)))
    avail_clusters_list <- as.list(avail_clusters)
    selectInput("selectCluster", label = h3("Select Cluster"), avail_clusters_list)
  })
  
  ## Set up data download options for district and cluster level summaries as well
  ## as detailed cluster level data
  
  output$DistDwnld <- downloadHandler(
    filename = function() {
      paste('district-summary-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(dataIn()$subset_district, con)
    }
  )
  
  output$ClustDwnld <- downloadHandler(
    filename = function() {
      paste('cluster-summary-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(dataIn()$subset_cluster, con)
    }
  )
  
  output$ClustDataDwnld <- downloadHandler(
    filename = function() {
      paste('cluster-no-', input$selectCluster, '-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(subset(dataIn()$list_data, CLUSTER_NO == input$selectCluster), con)
    }
  )
  
  output$CompClustDwnld <- downloadHandler(
    filename = function() {
      paste('comp_cluster_summary_', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(dataIn()$comp_cluster_summ, con)
    }
  )
  
  output$ErrorClustDwnld <- downloadHandler(
    filename = function() {
      paste('QAreview_cluster_summary_', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(dataIn()$error_cluster_summ, con)
    }
  )
}