#' SNB butler
#' @description      SNB User Interfaces
#' @param user       local system and db user (passed to install_demo_SNB) .
#' @param demo       default to FALSE; if TRUE runs  `install_demo_SNB()` first.
#' @param in.browser default to FALSE. If true open the local interface in the default browser.
#' @author           MV
#' @export
#' @return           nothing
#' @examples  
#'\dontrun{       
#' butler(demo = TRUE)
#' }
butler <- function(user = 'testuser', in.browser = FALSE, HOST = 'scidb.mpio.orn.mpg.de') {

  options(host  = HOST )


    cat("", file = cclog() )



  # APP -------------------------------------------------
    app = shinyApp(ui_butler(),server_butler )
    if(in.browser) runApp(app, launch.browser =  TRUE) else
    runApp(app)


 }


#' @export
server_butler <- function(input, output, session) {

  HWID = harwareIDs()

  # init  -------------------------------
    autoInvSys    <- reactiveTimer(5000)
    autoInvSD     <- reactiveTimer(500)


  # System status ------------------------------
    output$systat <- renderTable({
      autoInvSys()
      system_status(input$date, input$pwd)
      } ,
      colnames = FALSE,
      sanitize.text.function = function(x) x
      )

      toastr_info('Before you proceed please review the settings. Is the repository marked as OK ?', preventDuplicates = TRUE, timeOut = 10000)


  # SD COPY   ------------------------------
  output$copy <- renderUI({

    autoInvSD()

    x = file_copy_status(download_date = input$date, boxid = HWID) %>%
    card_copy_status %$%
    msg %>%
    HTML

    })

    Copy <- eventReactive(input$button_copy, {

    # COPY
    file_copy_status(download_date = input$date, boxid = HWID) %>%
    sdcard_uploader

    # CLEAN
    file_copy_status(download_date = input$date, boxid = HWID) %>%
    scard_cleaner


    })

    output$copyOut <- renderUI({
    Copy()
    cc = readLines( cclog() )
    div(class = "col-sm-12 text-center; width: 200px ;word-break: break-all; white-space: normal;",
    HTML(paste('<hr>', length(cc), 'Cards copied:',paste(tail(cc, 10) , collapse = ',' ), '...' ))
    )

    })

  # SD FORMAT  ------------------------------
  output$format <- renderUI({
    autoInvSD()
    x = card_format_status()
    HTML(x$html)
    })

    Format <- eventReactive(input$button_format, {
    sdcard_format(password = input$pwd)
    })

    output$formatOut <- renderUI({
    x = Format()
    div(class = "col-sm-12 text-center; width: 200px ;word-break: break-all; white-space: normal;",
    HTML(paste('<hr>', x, 'Cards formated. Remove all cards now.') ) )
    })

    # SD PREPARE  ------------------------------
    output$prepare <- renderUI({
    autoInvSD()
    x = card_prepare_status()
    HTML(x$html)
    })

    Prepare <- eventReactive(input$button_prepare, {
    sdcard_prepare(ids = input$new_ids)
    })

    output$prepareOut <- renderUI({
    x = Prepare()
    div(class = "col-sm-12 text-center; width: 200px ;word-break: break-all; white-space: normal;",
    HTML(paste('<hr>', x, 'Cards prepared') ) )
    })




  }

#' @export
ui_butler = function() {
  ui <- dashboardPage(skin = 'green',
    dashboardHeader( title = HTML( 'Data butler'   ) ) ,

    dashboardSidebar(
      useToastr(),

      dateInput("date", "Pull date" , value = Sys.Date(),  max = Sys.Date()) ,

      HTML('<hr>'),

      div(class = "col-sm-12 text-center",
      conditionalPanel( condition = "input.sdcards == 'Copy' ",
      actionButton("button_copy", "Copy cards", icon("copy", lib = 'glyphicon'),
      style="font-size:25px;background-color: #DFC531;border-color: #2e6da4")
      )),

      div(class = "col-sm-12 text-center",
      conditionalPanel( condition = "input.sdcards == 'Prepare' ",
      actionButton("button_prepare", "Prepare cards", icon("paperclip", lib = 'font-awesome'),
      style="font-size:25px;background-color: #DFC531;border-color: #142B09"),

      div(style="font-size:25px",
      selectInput('new_ids', label = 'Box numbers', choices = 1:277, multiple = TRUE) )

      )),

      div(class = "col-sm-12 text-center",
      conditionalPanel( condition = "input.sdcards == 'Format' ",
      actionButton("button_format", "Format cards", icon("eraser", lib = 'font-awesome'),
      style="font-size:25px;background-color: #DFC531;border-color: #2e6da4"),

      div(style="font-size:25px", passwordInput("pwd", "Password" ) )

      ))
      ,

      HTML('<hr>'),
      div(class = "col-sm-12 text-center lead",uiOutput("copyOut", style="font-size:20px;color:white;") ),
      HTML('<hr>'),

      HTML('<hr>'),
      div(class = "col-sm-12 text-center lead",uiOutput("formatOut", style="font-size:20px;color:red;") ),
      HTML('<hr>'),

      HTML('<hr>'),
      div(class = "col-sm-12 text-center lead",uiOutput("prepareOut", style="font-size:20px;color:red;") ),
      HTML('<hr>'),



      options_footer()



      ),

    dashboardBody(

      fluidRow( column( width = 12,

      box(title = 'Settings', status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
      footer = HTML('<small class="text-muted">When not all Settings are OK proceed with caution.</small>'),
      tableOutput("systat")
      ),

      tabBox( width = 12, id = 'sdcards',
      selected = "Copy",
      tabPanel(title = HTML('<h4 class="text-danger ">Copy</h4>'),           value = "Copy",   uiOutput("copy") ),
      tabPanel(title = HTML('<h4 class="text-danger ">Prepare</h4>'),        value = "Prepare",uiOutput("prepare") ),
      tabPanel(title = HTML('<h4 class="text-danger ">Format</h4>'),         value = "Format", uiOutput("format") )
      )

      )

      ) )

    )}





