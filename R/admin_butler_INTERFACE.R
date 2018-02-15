
#' SNB butler
#' @description      SNB User Interfaces
#' @param user       local system and db user (passed to install_demo_SNB) .
#' @param demo       default to FALSE; if TRUE runs  `install_demo_SNB()` first.
#' @param in.browser default to FALSE. If true open the local interface in the default browser.
#' @author           MV
#' @return           nothing
#' @examples         butler(demo = TRUE, user = 'mihai')
butler <- function(demo = FALSE, user = 'testuser', in.browser = FALSE) {

   # INIT ----------------------------------------------
      sapply(c('sdb', 'SNB', 'shiny', 'shinydashboard', 'doParallel', 'data.table', 'magrittr', 'shinytoastr'),
        function(x) require(x , character.only = TRUE, quietly = TRUE) )

      cat("", file = cclog() )

      if(demo) install_demo_SNB(user, '127.0.0.1')

      con = dbcon(user =getOption('DB_user'), host = getOption('host'),   db = getOption('snbDB_v2') )
      on.exit(dbDisconnect(con))
      boxid <- dbq(con, 'select box, hwid from boxid order by box, datetime_ desc')[!duplicated(box)]

  # SERVER --------------------------------------------
     server <- function(input, output, session) {


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

        toastr_info('Before you proceed please review the settings. Is the repository marked as ✓ ?', preventDuplicates = TRUE, timeOut = 10000)
        toastr_warning('Carol, keep the network cable plugged in!', preventDuplicates = TRUE, timeOut = 10000)

     # SD COPY   ------------------------------
        output$copy <- renderUI({

          autoInvSD()

          x = file_copy_status(download_date = input$date, bid = boxid) %>%
          card_copy_status %$%
          msg %>%
          HTML

          })

          Copy <- eventReactive(input$button_copy, {

          # COPY
          file_copy_status(download_date = input$date, bid = boxid) %>%
          sdcard_uploader

          # CLEAN
          file_copy_status(download_date = input$date, bid = boxid) %>%
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

     # DB UPLOAD: ------------------------------
        Upload <- eventReactive(input$button_upload, {
          con = dbcon(user =  getOption("DB_user") , host = getOption("host") )
          on.exit(dbDisconnect(con))

          Y = year(input$date)

          scidb_snbUpdater(con, db = getOption("snbDB"),   p = getOption("path.to.raw"), ui = TRUE, demo = demo, y = Y)
          scidb_snbUpdater(con, db = getOption("snbDB_v2"), p = getOption("path.to.raw_v2"), ui = TRUE, demo = demo, y = Y)


          })

        output$upload <- renderUI({
          Upload()
          })

     # FIELD datetime-s ----------------------------------
        output$dates <- renderUI({
          HTML('
            <p> Field data can be entered using a web browser by anybody and on any computer.<br>
            Click now the button below or go to <mark>http://scidb.mpio.orn.mpg.de</mark> latter and follow the links there.
            Alternatively, run <code> butler(local = FALSE) </code> to open the interface on this computer.
             </p>
             <hr>
            <div class = "text-center"> <a href="http://scidb.mpio.orn.mpg.de:3838/DataEntry/DB/SNBatWESTERHOLZ/file_status/" target="_blank"
                class="btn btn-primary  btn-lg" role="button"> Data butler<br>[intranet] </a></div>

            ')
          })

     # EVENTS -------------------------------------------
        output$events <- renderUI({
        HTML('
          <p> Copy/paste the following code into an R console.</p><br>
              <code>require(sdb)<br>
              <code>require(SNB)<br>
              con = dbcon(user = getOption("DB_user"), host =  getOption("host") )<br>
              update_DATA_QUALITY(con)<br>
              upload_events(con)<br>
              closeCon(con)</code>')
          })


     }

  # UI -------------------------------------------------
    ui <- dashboardPage(skin = 'green',
      dashboardHeader(
        title = HTML( paste(icon("user-secret", lib = 'font-awesome'), 'Data butler'   ) )
          ) ,

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

           )),


        div(class = "col-sm-12 text-center",
          conditionalPanel( condition = "input.sdcards == 'Upload' ",
            actionButton("button_upload", "Upload", icon("database", lib = 'font-awesome'),
              style="font-size:25px;background-color: #DFC531;border-color: #000000")

          )),

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
            footer = HTML('<small class="text-muted">When not all Settings are ✓ proceed with caution.</small>'),
            tableOutput("systat")
          ),

         tabBox( width = 12, id = 'sdcards',
            selected = "Copy",
            tabPanel(title = HTML('<h4 class="text-danger ">Copy</h4>'),           value = "Copy",   uiOutput("copy") ),
            tabPanel(title = HTML('<h4 class="text-danger ">Prepare</h4>'),        value = "Prepare",uiOutput("prepare") ),
            tabPanel(title = HTML('<h4 class="text-danger ">Format</h4>'),         value = "Format", uiOutput("format") ),
            tabPanel(title = HTML('<h4 class="text-primary">DB upload</h4>'),      value = "Upload", uiOutput("upload") ),
            tabPanel(title = HTML('<h4 class="text-success">Field data</h4>'),     value = "Dates",  uiOutput("dates") ),
            tabPanel(title = HTML('<h4 class="text-primary">DB Events table</h4>'),value = "Events", uiOutput("events") )
            )

        )

     ) ))

  # APP -------------------------------------------------
    app = shinyApp(ui,server)
    if(in.browser) runApp(app, launch.browser =  TRUE) else
    runApp(app)


 }




