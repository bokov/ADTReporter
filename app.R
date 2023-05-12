library(shiny);
library(dplyr);
library(DT);
library(rio);
library(shinylogs);
library(shinyjs);

# https://www.r-bloggers.com/2012/07/validating-email-adresses-in-r/
isValidEmail <- function(xx) grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", c(as.character(xx),'')[1], ignore.case=TRUE);

jsCode <- '
shinyjs.getCookie = function(name,target) {
  var nameEQ = name + "=";
  var ca = document.cookie.split(";");
  for(var i = 0; i < ca.length; i++) {
    var c = ca[i];
    while (c.charAt(0) == " ") c = c.substring(1, c.length);
    if (c.indexOf(nameEQ) == 0) {
      var cookieValue = c.substring(nameEQ.length, c.length);
      Shiny.setInputValue(target, cookieValue);
      return cookieValue;
    }
  }
  return null;
}
'
source('default_settings.R');
if(file.exists('local_settings.R')) source('local_settings.R')

# ui ----
ui <- fluidPage(
  # custom css ----
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
  ),
  # enable cookies ----
  useShinyjs(),
  extendShinyjs(text = jsCode,functions='getCookie'),  # Define the JS function

  # enable tracking ----
  use_tracking(),
  
  # title ----
  titlePanel(apptitle),

  # dynamic debug button ----
  # Useful to put in any webapp: the button is displayed only if a file named 
  # debug exists in the same folder as the webapp script.
  if(file.exists("debug")) actionButton("debug", "debug"),
  if(file.exists("message.md")) includeMarkdown('message.md'),
  uiOutput('dynamic_userselfid'),
  #textInput('userselfid',userselfid_label),
  textOutput("emailValidationMsg"), hr(),
  span(textOutput('validEmail'),class='adhoc_ADT_hidden'),
  # Conditional panel ----
  conditionalPanel(condition = "output.validEmail == 'TRUE'",
                   fluidRow(
                     column(2,selectInput("event_type", "Event Type:",choices = event_types)),
                     column(2,selectInput("contract", "Contract:", choices = contracts)),
                     column(2,dateInput("date", "Date:")),
                     column(2,numericInput("count", "Count:",value=NULL)),
                     column(3,textInput("comment", "Comment:")),
                     column(1,disabled(actionButton("add", HTML(addbuttoncode), class = "btn-success")))
                   ),
                   DTOutput("data_table"),
                   actionButton("submit", HTML("<span class='glyphicon glyphicon-ok'></span> Submit"), class = "btn-success")
  ),
  # feedback ----
  actionButton("feedback", "Feedback"),
  hidden(div(id = "feedback_panel",
    tags$div(
      textAreaInput("feedback_text", 
                    label = NULL, 
                    placeholder = "If you encounter bugs or have problems, please describe what's going on here, because your report will automatically be accompanied by the state of the app at the time you submitted it and that will help with troubleshooting.",
                    width='100%', 
                    rows = 2),
      actionButton("submit_feedback", "Submit Feedback")
    )))
  
);

server <- function(input, output, session) {
  # On first load, generate or retrieve unique ID ----
  shinyjs::runjs("
        if (document.cookie.indexOf('uniqueid') == -1) {
          var uniqueid = Math.random().toString(36).substr(2);
          document.cookie = 'uniqueid=' + uniqueid + ';max-age=31536000';
        }
      ")
  shinyjs::runjs("shinyjs.getCookie('uniqueid','uniqueid')");  # Get the cookie
  shinyjs::runjs("shinyjs.getCookie('userselfid','last_userselfid')");  # Get the cookie
  
  # Email validation ----
  output$dynamic_userselfid <- renderUI(span(textInput('userselfid'
                                                  ,userselfid_label
                                                  ,value = coalesce(URLdecode(input$last_userselfid),'')),class='adhoc_ADT_inline'));
  rvemailvalid <- reactive(isValidEmail(input$userselfid));
  output$validEmail <- renderText(rvemailvalid());
  outputOptions(output, "validEmail", suspendWhenHidden = FALSE);
  output$emailValidationMsg <- renderText({if (!rvemailvalid()) email_validation_msg else '';});
  # The validEmail output field above is hidden, meaning it doesn't get 
  # evaluated by the conditional panel. The following line of code allows it to
  # function while continuing to be invisible to the user
  observe({
    req(input$userselfid);
    if(rvemailvalid()) shinyjs::runjs(sprintf("document.cookie = 'userselfid=' + encodeURIComponent('%s') + ';max-age=31536000';", input$userselfid))
  })
  
  # Log ----
  track_usage(storage_mode = store_rds(path = "logs/"))
  # init empty data ----
  data <- reactiveVal(data.frame(EventType = character(), Contract = character(), Date = character(), 
                                 Count = integer(), Comment = character(), 
                                 Delete = character(), stringsAsFactors = FALSE));
  observeEvent({input$event_type; input$contract; input$count;},{
    if(!(input$event_type==''||input$contract==''||is.na(input$count))){
      enable('add') } else disable('add')
  });

  # add row ----
  observeEvent(input$add, {
    new_entry <- data.frame(EventType = input$event_type, Contract = input$contract, Date = input$date, 
                            Count = input$count, Comment = input$comment, 
                            Delete = deletebuttoncode, stringsAsFactors = FALSE)
    data(rbind(data(), new_entry));
    updateTextInput(inputId='comment',value='');
    updateTextInput(inputId='count',value='');
    updateTextInput(inputId='event_type',value='');
    updateTextInput(inputId='contract',value='');
    shinyjs::runjs('$("#event_type-selectized").focus();');
  });

  # renderDT ----
  output$data_table <- renderDT({
    datatable(data(), selection = 'none', escape = FALSE, 
              options = list(pageLength = nrow(data()), dom = 't'), # Disable pagination
              rownames = FALSE # Hide rownames
    ) %>% 
      formatStyle(columns = c('EventType', 'Contract', 'Date', 'Count', 'Comment'), 
                  target = 'row')
  }, server = FALSE);
  
  # delete row ----
  observeEvent(input$data_table_cell_clicked, {
    req(cell <- input$data_table_cell_clicked);
    if (length(cell$value)>0 && cell$value == deletebuttoncode) {
      data(data()[-cell$row,])};
  });
  
  # submit ----
  observeEvent(input$submit, {
    result <- select(data(),-'Delete') %>% 
      mutate(userselfid=input$userselfid, uniqueid=input$uniqueid
             ,ip=c(session$clientData$ip,'')[1]
             ,event=session$clientData$url_search
             ,full_request=paste0(session$clientData$url_protocol
                                  , session$clientData$url_hostname
                                  , session$clientData$url_port
                                  , session$clientData$url_pathname
                                  , session$clientData$url_search));
    export(result,outputfile,append=file.exists(outputfile));
    print(result);
    
    # Clear the data
    data(data.frame(EventType = character(), Contract = character(), Date = character(), 
                    Count = integer(), Comment = character(), 
                    Delete = character(), stringsAsFactors = FALSE))
  });
  
  observeEvent(input$feedback,show('feedback_panel'));
  
  observeEvent(input$submit_feedback, {
    # Clear the text area and hide the feedback form
    updateTextAreaInput(session, "feedback_text", value = "")
    hide('feedback_panel')
  });
  
  # debug listener ----
  # If the debug button does exist and gets pressed, start browser() for 
  # debugging the webapp from the inside. 
  observe({req(input$debug); if(input$debug>0) browser()});
};

shinyApp(ui = ui, server = server)
