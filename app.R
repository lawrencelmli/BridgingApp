library(tidyverse)
library(shiny)
library(shinythemes)
library(xtable)
library(knitr)
library(kableExtra)
library(shinyjs)
# library(lubridate)
# library(pagedown)


BrHighRisk <- readRDS("BrHighRisk.rda")
BrModRisk <- readRDS("BrModRisk.rda")
BrLowRisk <- readRDS("BrLowRisk.rda")
BrHighRiskRenal <- readRDS("BrHighRiskRenal.rda")
BrModRiskRenal <- readRDS("BrModRiskRenal.rda")

ui <- fluidPage(
  theme = shinytheme("cosmo"), useShinyjs(),
  
  titlePanel("Bridging of Warfarin Before Elective Surgery - version 0.1"),
  
  
  br(),
  
  
  p("The concept for the app was conceived at the Royal College of Anaesthetists #Hack Day in 2017."), 
  
  p("This app was written by Lawrence Li, the code for the original app was written by Danny JN Wong."),
  
  a(href="https://github.com/dannyjnwong/PreopDrugs", "Click here for the source code for the original app."),
  
  p("MIT License; Copyright (c) 2019 Lawrence LM Li."),
  
  p(strong("Disclaimer: This app is based on the bridging guideline for patients on Warfarin from East and North Hertfordshire NHS Trust.")),
  p(strong("It is intended to be used by a health professional in conjunction with the full guideline.")),
  a(href = "FullGuideline.pdf", "Please refer to the guideline - 'Managing Oral Antigoagulation for Invasive Procedures in Adult Patients on Warfarin'",
    target = "_blank"),

  
  hr(),
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  tabsetPanel(
    tabPanel("Bridging Warfarin", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 div(id = "resetBridge",
                     dateInput("OpDate", label = "Please select date of operation", value = Sys.Date()),
                     
                     numericInput("weight", "Patient's Weight in Kg", 
                                value = 70),
                     
                     numericInput("gfr", "Patient's eGFR (ml/min)", value = 60),
                     
                     selectInput("thromRisk", "What is the Thrombotic Risk",
                             c("", "High", "Moderate", "Low"),
                             selected = NULL),
                     
                     selectInput("bleedRisk", "What is the Surgical Bleeding Risk",
                             c("", "Very High", "High", "Low"),
                             selected = NULL)
                     ),
                 
                 helpText("Please refer to full guideline on how to determine Thrombotic and Sugical Bleeding Risks"),
                 
                 tags$a(href = "risks.jpg", "Quick reference to Thrombotic and Surgical Bleeding Risks criteria", target = "_blank"),
                 br(),
                 
                 actionButton("reset_bridge", "Reset"),
                 
                 width = 2),
               
               mainPanel(
                 wellPanel(
                  p("Please check that all the details are correct. 
                    The author does not take responsibility for any unintended consequences as a result of using this app."),
                  textInput("operation", "What is the planned procedure?", placeholder = "Enter planned operation here")
                 ),
                 
                 div(div(style="display: inline-block;vertical-align:top; ", p("Patient's Weight: ")),
                     div(style="display: inline-block;vertical-align:top; ", strong(textOutput(outputId = "brWeight"))),
                     div(style="display: inline-block;vertical-align:top; ", p(" Kg"))
                 ),
                 
                 div(div(style="display: inline-block;vertical-align:top; ", p("Patient's eGFR: ")),
                     div(style="display: inline-block;vertical-align:top; ", strong(textOutput(outputId = "brGFR"))),
                     div(style="display: inline-block;vertical-align:top; ", p(" ml/min"))
                 ),
                 
                 div(div(style="display: inline-block;vertical-align:top; ", p("Thrombotic Risk: ")),
                     div(style="display: inline-block;vertical-align:top; ", strong(textOutput(outputId = "clotRisk")))
                 ),
                 
                 div(div(style="display: inline-block;vertical-align:top; ", p("Surgical Bleeding Risk: ")),
                     div(style="display: inline-block;vertical-align:top; ", strong(textOutput(outputId = "blRisk"))),
                 ),
                 
                 div(div(style="display: inline-block;vertical-align:top; ", p("Planned Operation: ")),
                     div(style="display: inline-block;vertical-align:top; ", strong(textOutput(outputId = "opType"))),
                 ),
                 
                 br(),
                 
                 # tableOutput(outputId = "bridgeInst"), kable is better
                 
                 htmlOutput("bridgeKable"),
                 
                 helpText(textOutput("renal_text")),
                 
                 strong(textOutput("br_note")),
                 
                 textOutput("br_text1"),
                 
                 textOutput("br_text2"),
                 
                 textOutput("br_text3"),
                 
                 br(),
                 
                 hidden(
                    div(id = "download",
                      div(style="display: inline-block;vertical-align:middle;", downloadButton('downloadBridging', "Download as HTML")),
                      div(style="display: inline-block;vertical-align:middle; width: 5px;", HTML("<br>")),
                      div(style="display: inline-block;vertical-align:middle; ", helpText("Save as HTML and print from browser"))
                   )
                )
               )
             ))
  )
  
)

server <- function(input, output){
################# Bridging ################
  
  observe({
    shinyjs::toggle(id = "download", condition = input$thromRisk)
  })
  
  observeEvent(input$reset_bridge, {
    shinyjs::reset(id = "resetBridge")
  })
  
  output$brWeight <- renderText({
    input$weight
  })
  
  output$brGFR <- renderText({
    input$gfr
  })
  
  output$clotRisk <- renderText({
    input$thromRisk
  })
  
  output$blRisk <- renderText({
    input$bleedRisk
  })
  
  output$opType <- renderText({
    input$operation
  })
  
  bridgingTable <- reactive({
    if(input$thromRisk == "High" & input$gfr >= 30){
      bridge <- BrHighRisk
      
      # To sort out the Date and Weekday Columns
      opdate <- as.Date(input$OpDate)
      dates <- as.Date(c(opdate - 6, opdate - 5, opdate - 4, opdate -3, opdate - 2, opdate - 1, opdate, opdate + 1, opdate + 2))
      dates_chr <- format(as.Date(c(opdate - 6, opdate - 5, opdate - 4, opdate -3, opdate - 2, opdate - 1, opdate, opdate + 1, opdate + 2)), "%d/%m/%y")
      dates_chr <- as.character(dates_chr)
      bridge$Date <- dates_chr
      wdays <- weekdays(dates)
      bridge$Weekday <- wdays
      
      if(input$weight <= 46){
        bridge <- bridge %>% 
          select(Day, Date, Weekday, Warfarin, "INR Check" = INR, "Dalteparin Treatment" = TxDose_1,  "Dalteparin Prophylaxis*" = Prophylaxis_1)
      }else if(input$weight > 46 & input$weight < 57){
        if(input$weight < 50){
          bridge <- bridge %>%
            select(Day, Date, Weekday, Warfarin, "INR Check" = INR, "Dalteparin Treatment" = TxDose_2, "Dalteparin Prophylaxis*" = Prophylaxis_1)
        }else if(input$weight >= 50 & input$weight < 100){
          bridge <- bridge %>%
            select(Day, Date, Weekday, Warfarin, "INR Check" = INR, "Dalteparin Treatment" = TxDose_2, "Dalteparin Prophylaxis*" = Prophylaxis_2)
        }
      }else if(input$weight >= 57 & input$weight < 69){
        bridge <- bridge %>%
          select(Day, Date, Weekday, Warfarin, "INR Check" = INR, "Dalteparin Treatment" = TxDose_3, "Dalteparin Prophylaxis*" = Prophylaxis_2)
      }else if(input$weight >= 69  & input$weight < 83){
        bridge <- bridge %>%
          select(Day, Date, Weekday, Warfarin, "INR Check" = INR, "Dalteparin Treatment" = TxDose_4, "Dalteparin Prophylaxis*" = Prophylaxis_2)
      }else if(input$weight >= 83 & input$weight < 100){
        bridge <- bridge %>%
          select(Day, Date, Weekday, "Warfarin Dose" = Warfarin, "INR Check" = INR, "Dalteparin Treatment" = TxDose_5, "Dalteparin Prophylaxis*" = Prophylaxis_2)
      }else if(input$weight >= 100 & input$weight < 150){
        bridge <- bridge %>%
          select(Day, Date, Weekday, "Warfarin Dose" = Warfarin, "INR Check" = INR, "Dalteparin Treatment" = TxDose_5, "Dalteparin Prophylaxis*" = Prophylaxis_3)
      }else if(input$weight >= 150){
        bridge <- bridge %>%
          select(Day, Date, Weekday, "Warfarin Dose" = Warfarin, "INR Check" = INR, "Dalteparin Treatment" = TxDose_5, "Dalteparin Prophylaxis*" = Prophylaxis_4)
      }
      
    }else if(input$thromRisk == "Moderate" & input$gfr >= 30){
      bridge <- BrModRisk
      
      # To sort out the Date and Weekday Columns
      opdate <- as.Date(input$OpDate)
      dates <- as.Date(c(opdate - 6, opdate - 5, opdate - 4, opdate -3, opdate - 2, opdate - 1, opdate, opdate + 1, opdate + 2))
      dates_chr <- format(as.Date(c(opdate - 6, opdate - 5, opdate - 4, opdate -3, opdate - 2, opdate - 1, opdate, opdate + 1, opdate + 2)), "%d/%m/%y")
      dates_chr <- as.character(dates_chr)
      bridge$Date <- dates_chr
      wdays <- weekdays(dates)
      bridge$Weekday <- wdays
      
      if(input$weight < 50){
        bridge <- bridge %>%
          select(Day, Date, Weekday, "Warfarin Dose" = Warfarin, "INR Check" = INR, "Dalteparin Prophylaxis*" = Prophylaxis_1)
      }else if(input$weight >= 50 & input$weight < 100){
        bridge <- bridge %>%
          select(Day, Date, Weekday, "Warfarin Dose" = Warfarin, "INR Check" = INR, "Dalteparin Prophylaxis*" = Prophylaxis_2)
      }else if(input$weight >= 100 & input$weight < 150){
        bridge <- bridge %>%
          select(Day, Date, Weekday, "Warfarin Dose" = Warfarin, "INR Check" = INR, "Dalteparin Prophylaxis*" = Prophylaxis_3)
      }else if(input$weight >= 150){
        bridge <- bridge %>%
          select(Day, Date, Weekday, "Warfarin Dose" = Warfarin, "INR Check" = INR, "Dalteparin Prophylaxis*" = Prophylaxis_4)
      }
      
    }else if(input$thromRisk == "Low" & input$gfr >= 30){
      bridge <- BrLowRisk
      
      # To sort out the Date and Weekday Columns
      opdate <- as.Date(input$OpDate)
      dates <- as.Date(c(opdate - 6, opdate - 5, opdate - 4, opdate -3, opdate - 2, opdate - 1, opdate, opdate + 1, opdate + 2))
      dates_chr <- format(as.Date(c(opdate - 6, opdate - 5, opdate - 4, opdate -3, opdate - 2, opdate - 1, opdate, opdate + 1, opdate + 2)), "%d/%m/%y")
      dates_chr <- as.character(dates_chr)
      bridge$Date <- dates_chr
      wdays <- weekdays(dates)
      bridge$Weekday <- wdays
      
      bridge <- bridge %>%
        select(Day, Date, Weekday, "Warfarin Dose" = Warfarin)
      
    }else if(input$thromRisk == "Low" & input$gfr < 30){ # Same as raised GFR
      bridge <- BrLowRisk
      
      # To sort out the Date and Weekday Columns
      opdate <- as.Date(input$OpDate)
      dates <- as.Date(c(opdate - 6, opdate - 5, opdate - 4, opdate -3, opdate - 2, opdate - 1, opdate, opdate + 1, opdate + 2))
      dates_chr <- format(as.Date(c(opdate - 6, opdate - 5, opdate - 4, opdate -3, opdate - 2, opdate - 1, opdate, opdate + 1, opdate + 2)), "%d/%m/%y")
      dates_chr <- as.character(dates_chr)
      bridge$Date <- dates_chr
      wdays <- weekdays(dates)
      bridge$Weekday <- wdays
      
      bridge <- bridge %>%
        select(Day, Date, Weekday, "Warfarin Dose" = Warfarin)
      
    }else if(input$thromRisk == "High" & input$gfr < 30){
      bridge <- BrHighRiskRenal
      
      # To sort out the Date and Weekday Columns
      opdate <- as.Date(input$OpDate)
      dates <- as.Date(c(opdate - 6, opdate - 5, opdate - 4, opdate -3, opdate - 2, opdate - 1, opdate, opdate + 1, opdate + 2))
      dates_chr <- format(as.Date(c(opdate - 6, opdate - 5, opdate - 4, opdate -3, opdate - 2, opdate - 1, opdate, opdate + 1, opdate + 2)), "%d/%m/%y")
      dates_chr <- as.character(dates_chr)
      bridge$Date <- dates_chr
      wdays <- weekdays(dates)
      bridge$Weekday <- wdays
      
      if(input$weight < 48 & input$gfr >= 15 & input$gfr < 30){
        bridge <- bridge %>%
          select(Day, Date, Weekday, "Warfarin Dose" = Warfarin, "INR Check" = INR, "Dalteparin Treatment" = TxDose_1a, "Dalteparin Prophylaxis*" = Prophylaxis)
      }else if(input$weight < 48 & input$gfr < 15){
        bridge <- bridge %>%
          select(Day, Date, Weekday, "Warfarin Dose" = Warfarin, "INR Check" = INR, "Dalteparin Treatment" = TxDose_1a, "Dalteparin Prophylaxis*" = Prophylaxis)
      }else if(input$weight >= 48 & input$weight < 62 & input$gfr >= 15 & input$gfr < 30){
        bridge <- bridge %>%
          select(Day, Date, Weekday, "Warfarin Dose" = Warfarin, "INR Check" = INR, "Dalteparin Treatment" = TxDose_2a, "Dalteparin Prophylaxis*" = Prophylaxis)
      }else if(input$weight >= 48 & input$weight < 62 & input$gfr < 15){
        bridge <- bridge %>%
          select(Day, Date, Weekday, "Warfarin Dose" = Warfarin, "INR Check" = INR, "Dalteparin Treatment" = TxDose_2b, "Dalteparin Prophylaxis*" = Prophylaxis)
      }else if(input$weight >= 62 & input$weight < 80 & input$gfr >= 15 & input$gfr < 30){
        bridge <- bridge %>%
          select(Day, Date, Weekday, "Warfarin Dose" = Warfarin, "INR Check" = INR, "Dalteparin Treatment" = TxDose_3a, "Dalteparin Prophylaxis*" = Prophylaxis)
      }else if(input$weight >= 62 & input$weight < 80 & input$gfr < 15){
        bridge <- bridge %>%
          select(Day, Date, Weekday, "Warfarin Dose" = Warfarin, "INR Check" = INR, "Dalteparin Treatment" = TxDose_3b, "Dalteparin Prophylaxis*" = Prophylaxis)
      }else if(input$weight >= 80 & input$gfr >= 15 & input$gfr < 30){
        bridge <- bridge %>%
          select(Day, Date, Weekday, "Warfarin Dose" = Warfarin, "INR Check" = INR, "Dalteparin Treatment" = TxDose_4a, "Dalteparin Prophylaxis*" = Prophylaxis)
      }else if(input$weight >= 80 & input$gfr < 15){
        bridge <- bridge %>%
          select(Day, Date, Weekday, "Warfarin Dose" = Warfarin, "INR Check" = INR, "Dalteparin Treatment" = TxDose_4b, "Dalteparin Prophylaxis*" = Prophylaxis)
      }
      
    }else if(input$thromRisk == "Moderate" & input$gfr < 30){
      bridge <- BrModRiskRenal
      
      # To sort out the Date and Weekday Columns
      opdate <- as.Date(input$OpDate)
      dates <- as.Date(c(opdate - 6, opdate - 5, opdate - 4, opdate -3, opdate - 2, opdate - 1, opdate, opdate + 1, opdate + 2))
      dates_chr <- format(as.Date(c(opdate - 6, opdate - 5, opdate - 4, opdate -3, opdate - 2, opdate - 1, opdate, opdate + 1, opdate + 2)), "%d/%m/%y")
      dates_chr <- as.character(dates_chr)
      bridge$Date <- dates_chr
      wdays <- weekdays(dates)
      bridge$Weekday <- wdays
      
      bridge <- bridge %>%
        select(Day, Date, "Warfarin Dose" = Warfarin, "INR Check" = INR, "Dalteparin Prophylaxis*" = Prophylaxis)
      
    }
    
    return(bridge)
    
    
  })
  
  # output$bridgeInst <- renderTable({
  #   xtable(bridgingTable())
  # })
  # kable gives better formatting
  
  output$bridgeKable <- renderText({
    bridgingInst <- bridgingTable()
    
    kable(bridgingInst, "html", booktabs = T) %>% 
      kable_styling(full_width = TRUE, position = "center") %>% row_spec(0, bold = T) %>% row_spec(7, color = "black", background = "#80c3e8", bold = "T")
  })
  
  
  brNote <- reactive({
    if(input$thromRisk == "High" | input$thromRisk == "Moderate"){
      br_note <- ("Note:")
    }else{
      br_note <- ("")
    }
    
    return(br_note)
  })
  
  output$br_note <- renderText(brNote())
  
  brText1 <- reactive({
    if(input$thromRisk == "High" | input$thromRisk == "Moderate"){
      br_text1 <- ("Dalteparin (at least 6 hours post-surgery), but only if haemostasis secured and 
                  surgical bleeding risk falls in high or low group.")
    }else{
      br_text1 <- ("")
    }
    
    return(br_text1)
  })
  
  output$br_text1 <- renderText(brText1())
  
  
  brText2 <- reactive({
    if(input$thromRisk == "High" | input$thromRisk == "Moderate"){
      br_text2 <- ("For very high bleeding risk group: follow surgeon's instructions for anticoagulation post-operatively; in the absence of specific instructions, consider waiting 
                  till 24 hours post-operatively before starting LMWH.")
    }else{
      br_text2 <- ("")
    }
    
    return(br_text2)
  })
  
  output$br_text2 <- renderText(brText2())
  
  brText3 <- reactive({
    if(input$thromRisk == "High" | input$thromRisk == "Moderate"){
      br_text3 <- ("Restart warfarin (at usual maintenance dose) 24 hours post procedure.  Stop Dalteparin when INR in therapeutic range post-operatively")
    }else{
      br_text3 <- ("")
    }
    
    return(br_text3)
  })
  
  output$br_text3 <- renderText(brText3())
  
  renalText <- reactive({
    if(input$thromRisk == "High" | input$thromRisk == "Moderate"){
      renal_text <- ("*Consider using renal dose dalteparin (2500 Units OD) for prophylaxis if Serum Creatinine > 150 micromol/L") 
    }else{
      renal_text <- ("")
    }
    
    return(renal_text)
    
  })
  
  output$renal_text <- renderText(renalText())
  
  output$downloadBridging <- downloadHandler(
    filename = function() {("BridgingInstructions.html") # change this to pdf if pagedown is working
      # paste('my-report', sep = '.', switch(
      #   input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      # ))
    },
    
    content = function(file) {
      src <- normalizePath('reportBridgingHTML.Rmd')
      src2 <- normalizePath("printout.css")
      label <- normalizePath("pxLabel2.png")
      
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'reportBridgingHTML.Rmd', overwrite = TRUE)
      file.copy(src2, "printout.css", overwrite = TRUE)
      file.copy(label, "pxLabel2.png", overwrite = TRUE)
      
      
      library(rmarkdown)
      out <- render('reportBridgingHTML.Rmd', 
                    params = list(weight = input$weight, GFR = input$gfr, thromRisk = input$thromRisk, blRisk = input$bleedRisk, op = input$operation),
                    'html_document')
      # library(pagedown)
      # out <- pagedown::chrome_print(out, "BridgingInstructions.pdf", async = TRUE)
      file.rename(out, file)
    }
  )
}

shinyApp(ui = ui, server = server)
                