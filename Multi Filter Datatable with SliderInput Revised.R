#################################################################################
#Loading required libraries
#################################################################################
library(shiny)
library(shinydashboard)  # for Dashboard
library(shinyWidgets)    # for sendSweetAlert function
library(shinyalert)      # for alert message very nice format
library(DT)              # for using %>% which works as a pipe in R code
library(shinyjs)         # for DT:: datatable output and render
library(dplyr)           # select functions are covered in the library it is used while selecting and deleting a row
library(ggplot2)
library(car)             # to generate QQ plot to test Normal distribution of data

if (length(sessionInfo()$otherPkgs$shinydashboardPlus)>0){
  detach("package:shinydashboardPlus", unload = TRUE)
}

#################################################################################
#style function for Action button default 50px and width 180px; you can change
#################################################################################
styleButtonBlue<- function(xheight="45px",xwidth="155px",xcolor='#4682B4'){
  paste("white-space: normal;
  text-align:center;
  color: #ffffff;
        background-color:",xcolor,";",
        "border-color: #ffffff;
        border-width:2px;
        height:",xheight,";
        width:",xwidth,";
        font-size: 13px;")
}


#max frequency plus 8% to ylim in histogram
fngetMaxFreq <-function(x){
  H <-hist(x, plot = FALSE)
  return(round(max(H$counts)*1.08,0))
}

#################################################################################
# header which is part of ui 
#################################################################################
header <- shinydashboardPlus::dashboardHeader(
  title = "Dynamic Multi Filter",
  titleWidth = '375px',
  tags$li(
    shinydashboardPlus::dropdownBlock(
      id = "mydropdown",
      title = "Action Menu Bar",
      icon = icon("sliders"),
      actionBttn(inputId = "mFileImport", label = "Upload Dataset..!"),
      actionBttn(inputId = "mFileModify", label = "Modify Dataset..!"),
      actionBttn(inputId = "mDownloaddtdfBtn", label = "Download CSV..!")
    ),
    class = "dropdown"
  )
)


sidebar <- shinydashboardPlus::dashboardSidebar(
  id = 'msidebarid',
  # Remove the sidebar toggle element
  tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")),
  minified = FALSE,  #this option will completely close the side bar  and expand the header text
  collapsed = TRUE,
  useShinyjs(),
  sidebarMenu()#sidebar menu
)



#################################################################################
# dashboard body and sidebar which is part of ui 
#################################################################################
body <- dashboardBody(
  useShinyalert(),
  shinyjs::useShinyjs(),
  column(
    width = 12,
    align = "center",
    box(
      id = "slidebarbox208",
      width = 12,
      height ='100px',
      align='center',
      title = NULL,
      status = "danger",
      solidHeader = TRUE,
      splitLayout(cellWidths = c('0%','34%','16%','34%','16%'),
                  tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                  selectInput(inputId = 'mNumVarFilter',
                              label = "Select Numeric Variable to Filter",
                              choices = NULL,selected = NULL,
                              multiple = TRUE
                  ), 
                  HTML(paste('<br>',actionButton(inputId = 'mselectAllNum',label = "Select ALL Numeric fields",style = styleButtonBlue(xheight = '40px',xwidth = '175px',xcolor = '#b8860b')))),
                  selectInput(inputId = 'mChrVarFilter',
                              label = "Select Character Variable to Filter",
                              choices = NULL,selected = NULL,
                              multiple = TRUE),
                  HTML(paste('<br>',actionButton(inputId = 'mselectAllChar',label = "Select ALL Character fields",style = styleButtonBlue(xheight = '40px',xwidth = '175px',xcolor = '#b8860b')))),
      )
    ),
    tags$head(
      tags$style(
        paste0("#slidebarbox206{color:black; font-size:10px; font-style:bold;overflow:auto;text-align: center;margin: 0px 0px 0px 0px;
                                            width: '100%';height: 425px;max-height: 425px; background: #ffffff;}")
      )
    ),
    box  (
      id = "slidebarbox206",
      width = 5,
      height = '425px',
      align ='center',
      HTML(paste('<h4><b>','Play with Slider & PickerInput & Filter Data','</b><h5>')),
      title = NULL,
      status = "warning",
      solidHeader = TRUE,
      collapsible = FALSE,
      awesomeCheckbox(inputId = "mRedesignSliders",label = "Impact ALL Filters",value = FALSE),
      uiOutput(outputId = "muimultisliderplay"),
      uiOutput(outputId = "multislidertext")
    ),#box closure slider input
    tags$head(
      tags$style(
        paste0("#mplotoptionboxID{color:white; font-size:10px; font-style:bold;overflow:auto;text-align: left;margin: 0px 0px 0px 0px;
                                            width: '100%';height: 425px;max-height: 425px; background: #000000;}")
      )
    ),
      box(
        id='mplotoptionboxID',
        width = 2,
        height = '425px',
        background = 'black',
        align = "left",
        HTML(paste('<b><h5>',"Plot Options",'</b><h5>')),
        prettyRadioButtons(
          inputId = "mplottype",
          label = "Select Plot Type",
          choices = c("Box Plot", "Hist & Box / Bar","QQ Plot", "Density Plot"),
          inline = FALSE,
          status  = "danger"
        ),
        
        tags$hr(),
        x <- uiOutput('radio_Btns')
    ),
    

    box(
      id = "slidebarbox207",
      width = 5,
      height = '425px',
      title =NULL,
      status = "warning",
      solidHeader = TRUE,
      tags$head(
        tags$style(
          paste0("#tblmultifilter{color:black; font-size:12px; font-style:bold;overflow:auto;text-align: justify; margin: 5px 5px 5px 5px;
                                            width: '100%';height: 375px;max-height: 375px; background: #ffffff;}")
        )
      ),
      tabsetPanel(
        tabPanel(
          title = "Show Plot",
          plotOutput('mmultiplot', height = '350px')
        ),
        tabPanel(
          title = "Show Table",
          DT::dataTableOutput('tblmultifilter', height = '350px')
        )
      )
    ), #box closure
   
      box(
        width = 12,
        height ='100%',
        title = 'Correlation Pair-Panel Plot (ONLY Numeric fields and filter NOT applied here)',
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
      box(
        width = 2,
        height ='500px',
        background = 'black',
        align = "left",
        x <- uiOutput('radio_BtnsCorrelation')
      ),
      column(
        width = 10,
        align = "center",
        plotOutput('mcorrelationpairpanel',height = 475,width = '100%')
      ) #box closure
      ) #column closre
    )#box closure
) # dashboardBody closure


ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body
)



#################################################################################
# server function starts here 
#################################################################################

server <- function(input, output, session) {
  ##this is to hide right side bar
  shinyjs::addCssClass(selector = "body", class = "sidebar-collapse")
  onevent("mouseenter", "sidebarCollapsed", shinyjs::removeCssClass(selector = "body", class = "sidebar-collapse"))
  onevent("mouseleave", "sidebarCollapsed", shinyjs::addCssClass(selector = "body", class = "sidebar-collapse"))
  inserted <- c()
  slidercolrange <- -2
  
  vmy <- reactiveValues(mydata=NULL,data_1=NULL)
  
  #################################################################################
  # File dataset upload code starts here
  #################################################################################
  
  observeEvent(input$mFileImport,{
    showModal(
      modalDialog(
        size = 's',
        column(
          width = 12,
          offset = 0,
          align = "center",
          HTML(paste('<b><h5>',"Upload Dataset",'</b><h6>')),
         # Input: Select a file ----
              fileInput("file",
                        label = "Select: csv, xls, xlsx",
                        multiple = FALSE,
                        accept = c("text/csv/txt/Excel",
                                   "text/comma-separated-values,text/plain/excel",
                                   ".csv",".xls",".xlsx")),
              
              column(
                width = 5,
                offset = 1,
                align = "left",
                fluidRow(
                  # Input: Checkbox if file has header ----
                  checkboxInput("header", "Header", TRUE),
                  
                  # Input: Select separator ----
                  radioButtons("sep", "Separator",
                               choices = c(Comma = ",",
                                           Semicolon = ";",
                                           Tab = "\t"),
                               selected = ",")
                )
              ),
              column(
                width = 5,
                offset = 0,
                align = "left",
                fluidRow(
                  br(),
                  br(),
                  # Input: Select quotes ----
                  radioButtons("quote", "Quote",
                               choices = c(None = "",
                                           "Double Qot." = '"',
                                           "Single Qot." = "'"),
                               selected = '"')
                )
              ),#column
             ), #column
            align = "center",
            HTML("click outside this box to exit"),
        easyClose = TRUE
      )
    )
  })
  
  observeEvent(input$file,{
    ext <- tools::file_ext(input$file$name)
    if (ext == "xls" | ext == 'xlsx'){
      vmy$mydata <- as.data.frame(readxl::read_excel(input$file$datapath))  # got from https://readxl.tidyverse.org/
    }
    else if (ext == "csv"){
      tryCatch({            #avoid rscript showing error initially before execution,Skipping error in for-loop where you got:https://stackoverflow.com/questions/14748557/skipping-error-in-for-loop
        
        vmy$mydata <- as.data.frame(read.csv(input$file$datapath,
                                             header = input$header,
                                             sep = input$sep,
                                             quote = input$quote)
        )
      },error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))})
    }else
      {
      shinyalert("Oops!", "valid files are only csv or excel", type = "error")
      return()
    }
    removeModal()
    mnumericcolname <-  names(dplyr::select_if(vmy$mydata,is.numeric)) 
    for (i in mnumericcolname){
      vmy$mydata[i] <- round(vmy$mydata[i],2)
    }
    vmy$mydata <- na.omit(vmy$mydata)
    vmy$mydata <- vmy$mydata[complete.cases(vmy$mydata), ]
    row.names(vmy$mydata) <- 1:nrow(vmy$mydata)
    
    fnupdatevarchoice()
    fncreatedftype()
    updateAwesomeCheckbox(session,inputId = 'mRedesignSliders',value = FALSE)
    disable("mRedesignSliders")
  })
  

  #################################################################################
  # File dataset upload code ENDS here
  #################################################################################

  fnupdatevarchoice <- function(){
    updateSelectInput(session,inputId = "mNumVarFilter",choices = names(dplyr::select_if(vmy$mydata,is.numeric)) )
    updateSelectInput(session,inputId = "mChrVarFilter",choices = names(dplyr::select_if(vmy$mydata,is.character)) )
  }
  
  
  observeEvent(input$mselectAllNum,{
    if (length(input$file)==0){
      shinyalert("Oops!", "Hi first browse and select dataset ...!", type = "error")
      return()
    }
    if (length(names(dplyr::select_if(vmy$mydata,is.numeric)))==0){
      shinyalert("Oops!", "There is no NUMERIC field in this dataset ...!", type = "error")
      return()
    }
    updateSelectInput(session,inputId = "mNumVarFilter",choices = names(dplyr::select_if(vmy$mydata,is.numeric)),selected = names(dplyr::select_if(vmy$mydata,is.numeric)) )
  })
  
  
  observeEvent(input$mselectAllChar,{
    if (length(input$file)==0){
      shinyalert("Oops!", "Hi first browse and select dataset ...!", type = "error")
      return()
    }
    if (length(names(dplyr::select_if(vmy$mydata,is.character)))==0){
      shinyalert("Oops!", "There is no CHARACTER field in this dataset ...!", type = "error")
      return()
    }
    updateSelectInput(session,inputId = "mChrVarFilter",choices = names(dplyr::select_if(vmy$mydata,is.character)),selected =  names(dplyr::select_if(vmy$mydata,is.character)) )
  })
  
  
  #################################################################################
  # File dataset MODIFY code starts here
  #################################################################################
  
  fncreatedftype <- function(){
    vmy$df_types <- data.frame("col_types" = unlist(lapply(vmy$mydata, typeof)))
    vmy$df_types$Var_name <- rownames(vmy$df_types)
    row.names(vmy$df_types) <- NULL
    vmy$df_types <- vmy$df_types %>% dplyr::select(-col_types, everything())
  }

  
  observeEvent(input$mFileModify,{
    if (length(input$file)==0){
      shinyalert("Oops!", "Hi first browse and select dataset ...!", type = "error")
      return()
    }
    
    showModal(
      modalDialog(
        size = 's',
        column(
          width = 12,
          offset = 0,
          align = "center",
          HTML(paste('<b><h5>',"Modify Dataset",'</b><h5>')),
          
              DT::dataTableOutput("dt",height = 300),
              tags$style(HTML('table.dataTable tr.selected td{background-color: pink !important;}')),
              useShinyjs(),
              extendShinyjs(text = paste0("shinyjs.resetDTClick = function() { Shiny.onInputChange('dt_cell_clicked', null); }"),functions = c('foo','bar')),
              textOutput("mselectedvariable"),
              br(),
              actionButton(inputId = 'mbtndelete',label = "Delete Selected Variable",style = styleButtonBlue(xheight = '45px',xwidth = '155px'))
        ), #column
        easyClose = TRUE
      )
    )
  })
  

  output$dt <- DT::renderDataTable({
    DT::datatable(vmy$df_types,
                  rownames = FALSE,
                  width = NULL,
                  height = NULL,
                  editable = FALSE,
                  selection = list(mode = "single", selected = NULL, target = 'row'),
                  fillContainer = getOption("DT.fillContainer", TRUE),
                  options = list(scrollY = '300px',dom = 't',ordering=FALSE, pageLength = -1,class="compact",
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': '#808080', 'color': '#fff'});",  
                                   "}")
                                 
                  ) 
                  
    )
    
  })
  
  
  output$mselectedvariable <-  renderText({
    if(length(input$dt_cell_clicked) != 0){
      clicked_list <- input$dt_cell_clicked
      HTML(paste("selected:",vmy$df_types[clicked_list$row,1],'\n',"Type:",vmy$df_types[clicked_list$row,2]))
      
    }
  })

  
  
  #################################################################################
  # File dataset DELETE VARIABLE code starts here
  #################################################################################
  
  ### delete selected column
  ### this is warning messge for deleting
  observeEvent(input$mbtndelete,{
    showModal(
      if(length(vmy$df_types[input$dt_cell_clicked$row,1])>0 ){
        modalDialog(
          title = "Warning",
          paste("Are you sure delete variable:",vmy$df_types[input$dt_cell_clicked$row,1] ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("ok", "Yes")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select the variable that you want to delete!" ),easyClose = TRUE
        )
      }
      
    )
  })
  
  
  ### If user say OK, then delete the selected rows
  observeEvent(input$ok, {
    temp <- dplyr::select(vmy$mydata,-c(vmy$df_types[input$dt_cell_clicked$row,1]))
    vmy$mydata <- temp
    removeModal()
    temp2 <- subset(vmy$df_types, Var_name!=vmy$df_types[input$dt_cell_clicked$row,1] )
    vmy$df_types <- temp2
    fnupdatevarchoice()
  })
  
  #################################################################################
  # File dataset DELETE VARIABLE code ENDS here
  #################################################################################
  
  output$tblmultifilter <- DT::renderDataTable({
    dtdftemp <- vmy$data_1()
    vmy$dtdf <- dtdftemp
    DT::datatable(vmy$dtdf,
                  class ='cell-border stripe compact white-space: nowrap', 
                  escape= FALSE,
                  rownames = TRUE,
                  editable = FALSE,
                  selection = list(mode = "single", selected = c(1), target = 'row'),
                  fillContainer = getOption("DT.fillContainer", TRUE),
                  options = list(scrollY = '400px', 
                    lengthMenu = list(c(15, 25, 50,-1), c('15', '25','50' ,'All')),
                    paging = TRUE,
                    lenthChange=TRUE,
                    searching = FALSE,
                    fixedColumns = FALSE,
                    autoWidth = FALSE,
                    ordering = FALSE
                  )
    ) 
  })
  
  my.styleallrows <- function(.) formatStyle(., columns=0, target= 'row',color = 'black', 
                                             backgroundColor = '#ffffed',
                                             fontWeight ='normal',lineHeight='75%')
  my.styleonecolumn <- function(.) formatStyle(., columns=c("var_name"), target= 'cell',color = 'black', 
                                               backgroundColor = '#ffffed',
                                               fontWeight ='bold',lineHeight='70%')
  
  observeEvent(input$mNumVarFilter,{
    disable("mRedesignSliders")
    updateAwesomeCheckbox(session,inputId = 'mRedesignSliders',value = FALSE)
    enable("mRedesignSliders")
  }) 
  
  observeEvent(input$mChrVarFilter,{
    disable("mRedesignSliders")
    updateAwesomeCheckbox(session,inputId = 'mRedesignSliders',value = FALSE)
    enable("mRedesignSliders")
  }) 
  
  
  #######- above multi table datatable end
  
  output$muimultisliderplay <- renderUI({
    tryCatch({
      if (length(input$mNumVarFilter) != 0){
        slider_options <- input$mNumVarFilter
      }
      else{
        return()
      }
      
      # First, create a list of sliders each with a different name
      sliders <- lapply(1:length(slider_options), function(i) {
        if (slidercolrange==12){
          slidercolrange <- 1
        }
        else{
          slidercolrange <- slidercolrange ++ 4
        }
        inputName1A <- slider_options[i]
        
        if (input$mRedesignSliders == TRUE){
          column(slidercolrange+4,sliderInput(inputId = inputName1A, label = inputName1A, 
                                              min=min(vmy$data_1()[,inputName1A],na.rm = TRUE), 
                                              max=max(vmy$data_1()[,inputName1A],na.rm = TRUE),
                                              value=c(min(vmy$data_1()[[inputName1A]],na.rm = TRUE),max(vmy$data_1()[[inputName1A]],na.rm = TRUE)),
                                              width = "500px"))
        }
        else{
          column(slidercolrange+4,sliderInput(inputId = inputName1A, label = inputName1A,
                                              min=min(vmy$mydata[,inputName1A],na.rm = TRUE),
                                              max=max(vmy$mydata[,inputName1A],na.rm = TRUE),
                                              value=c(min(vmy$mydata[[inputName1A]],na.rm = TRUE),max(vmy$mydata[[inputName1A]],na.rm = TRUE)),
                                              width = "500px"))
        }  
        
        
      })
      # Create a tagList of sliders (this is important)
      do.call(tagList, sliders)
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  })
  
  
  
  output$multislidertext <- renderUI({
    tryCatch({
      if (length(input$mChrVarFilter) != 0){
        slider_optionsTXT <- input$mChrVarFilter
      }
      else{
        return()
      }
      
      # First, create a list of sliders each with a different name
      sliders <- lapply(1:length(slider_optionsTXT), function(i) {
        if (slidercolrange==12){
          slidercolrange <- 1
        }
        else{
          slidercolrange <- slidercolrange ++ 4
        }
        inputName1ATXT <- slider_optionsTXT[i]
        
        if (input$mRedesignSliders == TRUE){
          mchoice <- as.list(unlist(t(distinct(vmy$data_1()[inputName1ATXT]))))
        }
        else{
          mchoice <- as.list(unlist(t(distinct(vmy$mydata[inputName1ATXT]))))
        }
        
        column(slidercolrange+4,pickerInput(inputId = inputName1ATXT, label = inputName1ATXT, choices =mchoice,selected =mchoice ,multiple = TRUE,
                                            options = list('actions-box' = TRUE), width = "175px")) 
        
      })
      # Create a tagList of sliders (this is important)
      do.call(tagList, sliders)
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  })
  
  
  vmy$data_1 <-reactive({
    tryCatch({ 
      data_ <- vmy$mydata
      slider_options <- slider_options <- input$mNumVarFilter
      # this is how you fetch the input variables from ui 
      for(i in slider_options) {
        
        xxtt<-as.double(eval(parse(text=paste0("input$",i))))
        data_ <- data_[data_[[i]] <= xxtt[2] &                       
                         data_[[i]] >= xxtt[1],]
        
      }
      
      slider_optionsTXT <-  input$mChrVarFilter 
      # this is how you fetch the input variables from ui component component character fields
      for(i in slider_optionsTXT) {
        
        xxttTXT<-eval(parse(text=paste0("input$",i)))
        data_ <- data_[data_[[i]] %in%  xxttTXT,]
      }
      data_
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    return(data_)
  })
  
  observeEvent(input$mDownloaddtdfBtn,{
    filename = function() {
      paste("filtered", Sys.Date(), ".csv", sep="")
    }
    content = function(file) {
      write.csv(data.frame(vmy$data_1()[input[["tblmultifilter_rows_all"]],]), file, row.names = FALSE)
    }
  })
  
  output$mDownloaddtdfBtn1<- downloadHandler(
    filename = function() {
      paste("filtered", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data.frame(vmy$data_1()[input[["tblmultifilter_rows_all"]],]), file, row.names = FALSE)
    }
  )
  
  output$mmultiplot <-renderPlot({
    if (length(input$radioreply)==0){
      return()
    }
    
    par(mfrow = c(1, 1))
    tryCatch({            
      
      switch(input$mplottype,
             "Box Plot" =  boxplot(x = vmy$data_1()[ ,input$radioreply],
                                   main = NULL,
                                   labels = TRUE,
                                   col = "#87CEFA",
                                   border = "black",
                                   # col = "orange",
                                   # border = "brown",
                                   notch = TRUE)+
               points(pch = 16,cex = 1.5,mean(vmy$data_1()[ ,input$radioreply]),col="red"),
             
             "Hist & Box / Bar"= 
               if (is.character(vmy$data_1()[,input$radioreply])  == TRUE){
                 # Outside bars
                 df<- data.frame(table(vmy$data_1()[,input$radioreply]))
                 ggplot(data=df, aes(x=Var1, y=Freq)) +
                   geom_bar(stat="identity", fill="#87CEFA")+
                   xlab(input$radioreply) +
                   ylab("Frequency") +
                   geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
                   theme_minimal()
               }else
               {
                 # # Layout to split the screen
                 
                 layout(mat = matrix(c(1,2),nrow=2,ncol = 1, byrow=TRUE),  height = c(8,3))
                 
                 # # boxplot(vmy$data_1()[,input$radioreply , horizontal=TRUE , ylim=c(-10,10), xaxt="n" , col=rgb(0.8,0.8,0,0.5) , frame=F)
                 par(mar=c(0, 3.1, 1.1, 2.1)) # #par("mar") (bottom, left, top, right) in lines
                 
                 hist(vmy$data_1()[,input$radioreply],
                      main = NULL,
                      labels = TRUE,
                      col = "#87CEFA",
                      border = "white",
                      breaks = 10,
                      ylim = c(0,fngetMaxFreq(vmy$data_1()[[input$radioreply]])),
                      xlab = paste("Bin of ",input$radioreply))+geom_point()+geom_smooth()+coord_cartesian()%>%
                   abline(v = mean(vmy$data_1()[,input$radioreply], na.rm = T),
                          col = "red",
                          lwd = 2)%>%
                   abline(v = median(vmy$data_1()[,input$radioreply], na.rm = T),
                          col = "black",
                          lwd = 2)
                 
                 # Draw the boxplot and the histogram
                 par(mar=c(0, 3.1, 3.1, 2.1)) # #par("mar") (bottom, left, top, right) in lines
                 
                 boxplot(vmy$data_1()[,input$radioreply],
                         main = NULL,
                         xlab = NULL,
                         ylab = NULL,
                         col = "#CDE2B8",
                         border = "black",
                         horizontal = TRUE,
                         notch = FALSE,
                         outline = TRUE,
                         axes = TRUE,
                         staplewex = 1
                 )+coord_cartesian()+
                   text(x=quantile(vmy$data_1()[,input$radioreply]),labels=quantile(vmy$data_1()[,input$radioreply]),y=1.25,cex = 0.90)
                 points(mean(vmy$data_1()[,input$radioreply]), col = "red", pch = 19, cex = 1.5)
               },
             
             "QQ Plot" = qqPlot(unlist(vmy$data_1()[input$radioreply])),   
             
             "Density Plot" =ggplot(vmy$data_1(), aes(x=vmy$data_1()[,input$radioreply])) +
               geom_density(fill = "#87CEFA")+xlab(input$radioreply)+
               geom_vline(data=vmy$data_1(), aes(xintercept = mean(vmy$data_1()[,input$radioreply])), colour='red') +
               geom_vline(data=vmy$data_1(), aes(xintercept = median(vmy$data_1()[,input$radioreply])), colour='black'),
             
      )}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  })
  
 # red line is mean and black is median
  
  
  output$radio_Btns <- renderUI({
    if (length(input$file)==0){
      return()
    }
    options <- colnames(vmy$mydata) # The options are dynamically generated on the server
    prettyRadioButtons(
      inputId = "radioreply",
      label = "Select variable",
      choices = options,
      inline  = FALSE,
      status  = "danger"
     )# radioGroupbtn closure
  }) # renderUI closure
  
  
  
#########################################################################
## Correlation table related 
#########################################################################  
  
  output$radio_BtnsCorrelation <- renderUI({
    if (length(input$file)==0){
      return()
    }
    
    options <- colnames(dplyr::select_if(vmy$mydata,is.numeric)) # The options are dynamically generated on the server
    prettyCheckboxGroup(
      inputId = "radioreplyCorrelation",
      label = 'Un-check to remove variables from Plot',
      choices = options,
      selected = options,
      inline  = FALSE,
      status  = "danger"
    )# prettyCheckboxGroup closure
  }) # renderUI closure
  
  

  output$mcorrelationpairpanel <- renderPlot({
    if (length(input$file)==0){
      return()
    }
    mcolnames <- colnames(vmy$mydata) %in% c(input$radioreplyCorrelation)

    library(psych)
    tryCatch({   
    pairs.panels(vmy$mydata[,mcolnames],
                 smooth = TRUE,      # If TRUE, draws loess smooths
                 scale = FALSE,      # If TRUE, scales the correlation text font
                 density = FALSE,    # If TRUE, adds density plots and histograms
                 ellipses = TRUE,    # If TRUE, draws ellipses
                 method = "pearson", # Correlation method (also "spearman" or "kendall")
                 pch = 21,           # pch symbol
                 lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
                 cor = TRUE,         # If TRUE, reports correlations
                 jiggle = FALSE,     # If TRUE, data points are jittered
                 factor = 2,         # Jittering factor
                 smoother = TRUE,    # If TRUE, then smooth.scatter the data points -- slow but pretty with lots of subjects
                 digits = 2,         # the number of digits to show
                 cex.cor = 0.8,       # If this is specified, this will change the size of the text in the correlations. 
                 hist.col = 4,       # Histograms color
                 stars = TRUE)       # If TRUE, adds significance level with stars
    },error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))})
    
  })
  
  
 
} #server closure
shinyApp(ui, server)


