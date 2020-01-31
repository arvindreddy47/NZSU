library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)
library(markdown)
library(data.table)
library(plotly)
library(ggplot2)
library(RODBC)
library(flexdashboard)
library(plyr)
library(dplyr)
library(shinyBS)

# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                     tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                     textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                     passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                     br(),
                     div(
                         style = "text-align: center;",
                         actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                  padding: 10px 15px; width: 150px; cursor: pointer;
                                  font-size: 18px; font-weight: 600;"),
                         shinyjs::hidden(
                             div(id = "nomatch",
                                 tags$p("Oops! Incorrect username or password!",
                                        style = "color: red; font-weight: 600; 
                                  padding-top: 5px;font-size:16px;", 
                                        class = "text-center"))),
                         br()
                         #br(),
                         #tags$code("Username: myuser  Password: mypass"),
                         #br(),
                         #tags$code("Username: myuser1  Password: mypass1")
                     ))
)

credentials = data.frame(
    username_id = c("ARREDD","etlusr_dev"),
    passod   = sapply(c("ARREDD","pass123word"),password_store),
    permission  = c("basic", "advanced"), 
    stringsAsFactors = F
)

header <- dashboardHeader( title = "NETEZZA SERVER UTILITY", uiOutput("logoutbtn"),titleWidth = 300)
sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui<-dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
    
    login = FALSE
    USER <- reactiveValues(login = login)
    
    observe({ 
        if (USER$login == FALSE) {
            if (!is.null(input$login)) {
                if (input$login > 0) {
                    Username <- isolate(input$userName)
                    Password <- isolate(input$passwd)
                    if(length(which(credentials$username_id==Username))==1) { 
                        pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
                        pasverify <- password_verify(pasmatch, Password)
                        if(pasverify) {
                            USER$login <- TRUE
                            #print(Username)
                            #print(Password)
                        } else {
                            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
                            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
                        }
                    } else {
                        shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
                        shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
                    }
                } 
            }
        }    
    })
    
    output$logoutbtn <- renderUI({
        req(USER$login)
        tags$li(a(icon("fa fa-sign-out"), "Logout", 
                  href="javascript:window.location.reload(true)"),
                class = "dropdown", 
                style = "background-color: #eee !important; border: 0;
            font-weight: bold; margin:5px; padding: 10px;")
    })
    
    output$sidebarpanel <- renderUI({
        if (USER$login == TRUE ){ 
            sidebarMenu(
                sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                                  label = "Search..."),
                menuItem("HOME", tabName = "dashboard", icon = icon("home"),selected = TRUE),
                menuItem("VISUALS", tabName = "page3", icon = icon("bar-chart-o"),
                         menuSubItem("CHARTS", tabName = "subitem1", icon = icon("bar-chart-o") ),
                         menuSubItem("SCHEMA GRAPHS", tabName = "subitem2", icon = icon("bar-chart-o") )),
                menuItem("DATA TABLE", tabName = "extra", icon = icon("table")),
                selectInput("select", "Schema",
                            c("ATLAS_NA_WRK_DEV","ATLAS_NA_DM_LNDG_DEV1","ATLAS_NA_DM_LNDG_DEV"),selected = TRUE),
                selectInput("year", "SELECT YEAR",
                            c("2019","2018"),selected = TRUE)
                #actionButton("SCHEMA","SELECT SCHEMA",icon("clipboard-check")),
                #actionButton("SUB",label = "SUBB"),
                )
        }
    })
    
    output$body <- renderUI({
        if (USER$login == TRUE ) {
            tabItems(
                tabItem(tabName ="dashboard", #class = "active",
                        fluidRow(
                            box(# dataTableOutput("results"))
                                h3("Schema Selected:"),
                                textOutput("selectout"),
                                width = 3,height = 200, collapsible = TRUE,status = "warning"
                            ),
                            
                            box(textInput("TEXT",h3("NZ CONSOLE")),width = 9,height = 200, collapsible = TRUE,status = "primary",
                                #h6("statement executed!"),
                                textOutput("TEXTOUT"),
                                actionButton("Submit", "Exectute Query"))
                            
                        ),
                        fluidRow(box(h3("Total Count:"),
                            infoBox(title = "" ,value =  455 , icon = icon("list"),subtitle = "TABLES",color = "navy"),
                            width = 3,height = 200, collapsible = TRUE,status = "warning"),
                            box(h3("Percentage of Space Occupied:"),
                                gauge("45%",
                                      min = 0,
                                      max = 100,
                                      gaugeSectors(success = c(0, 50),
                                                   warning = c(50.1,70),
                                                   danger =  c(70.1,100),
                                                   colors = c("green", "yellow", "red"))),width = 9,
                                height = 200,collapsible = TRUE,status = "primary")
                        )
                        ,fluidRow(
                            box(
                                h3("Current User:"),
                                infoBox(title = "" ,value =  "ARREDD" , icon = icon("user"),width = 3,color = "green"),
                                width = 3,height = 200, collapsible = TRUE,status = "warning")
                                )) 
                ,
                tabItem(tabName ="subitem1",
                            plotlyOutput("scatterplot")
                                     #)
                        #))#,box(h5("Description"),textOutput("desc"))
                #)
                ),
                tabItem(tabName ="subitem2",
                          #plotlyOutput("scatterplot")
                    h3("HEY")
                    #,plotlyOutput("bar")
                ),
                tabItem(
                    tabName = "extra",
                    # fluidRow(#box(#h4("Interactive Data Table"),
                    #     dataTableOutput('carstable'),
                        fluidPage(
                            box(width=12,
                                h3(strong(paste("List of tables in",input$select)), style="text-align:center;color:#0000ff;font-size:150%"),
                                hr(),
                                helpText("You can select rows by clicking on them. After you select a couple of rows, you can drop the tables that you wish to by clicking the drop button at the bottom",
                                         style="text-align:center"),
                                br(),
                                column(12,DT::dataTableOutput('carstable'))),
                            br(),br(),br(),
                            box(width = 12,
                                column(12, h4(helpText("To drop tables start selecting & hit the button below",style="text-align:center")),hr()),
                                column(1,
                                       actionButton("compare", "DROP", style="text-align:center;color: #0000ff; font-size:120%"),
                                       bsTooltip("compare", "Shows an animation modal window on the selected tables.",
                                                 "right", options = list(container = "body")))
                                # column(2, 
                                #        actionButton("show_all", "Show All", style="text-align:center;color:#996600; font-size:120%")),
                                # bsTooltip("show_all", "Shows countries that were hidden",
                                #           "right", options = list(container = "body"))
                                
                            )
                        
                    ),br(),
                    bsModal("modalExample", strong("Selected Tables", style="color:#0000ff; font-size:120%"), "compare", size = "large",
                            h4("Table Name:"),
                            textOutput("SHINY"),
                            div(style="display: inline-block;vertical-align:top; width: 200px;", 
                                actionButton('Submit_drop',"Submit"),
                                bsTooltip("Submit_drop", "You can click this if you want to drop the tables for the selected item(s).",
                                          "right", options = list(container = "body"))
                            )
                    )
                )
            )
            
        }
        else {
            loginpage
        }
    })
    
    Connection<- reactive({
        Connection <- odbcConnect(ifelse(is.null(input$select),"ATLAS_NA_WRK_DEV",input$select), uid = input$userName, pwd = input$passwd, believeNRows= FALSE)
    
        })
    
    
    #observeEvent(input$SCHEMA, {
    output$selectout <-  renderText({
        input$select
        print(input$select)
    }) 
    #})
    output$TEXTOUT <-  renderText({
        paste("Statement Executed!",input$TEXT)
        
    })
    
    observeEvent(input$Submit,{
        #Connection_DROP<- odbcConnect(input$select, uid = input$userName, pwd = input$passwd, believeNRows= FALSE)
        sqlQuery(Connection(),input$TEXT)
    })
    
    output$scatterplot <- renderPlotly({
        Test_data<- sqlQuery(Connection(),"SELECT src.* FROM _v_table_storage_stat SRC
        JOIN _V_TABLE TGT ON SRC.OBJID=TGT.OBJID")
        ## SPACE/MONTH
        MONTH_DATA<-Test_data %>% filter(Test_data$USED_BYTES>0)%>%
            mutate(MONTH= format.Date(CREATEDATE,"%y%m")) %>% 
            filter( substr(paste(20,as.numeric(MONTH),sep = ''),start = 0,stop = 4) == input$year)%>%
            group_by(MONTH) %>%
            summarise( MONTH_USD_SPACE = sum(USED_BYTES/1024**3)) %>% data.frame()
        DUMMY_MONTHS = data.frame("MONTH"= c("01","02","03","04","05","06","07","08","09","10","11","12"),
                                  MONTH_USD_SPACE = 0 ,stringsAsFactors = FALSE)
        DUMMY_MONTHS$MONTH<- paste(substr(input$year,start = 3,stop = 4),DUMMY_MONTHS$MONTH ,sep = '')
        MONTH_DATA_SPC<-rbind(MONTH_DATA,DUMMY_MONTHS)
        MONTH_DATA_SPC<-arrange(MONTH_DATA_SPC[!duplicated(MONTH_DATA_SPC[c('MONTH')]),],MONTH)
        ## COUNT TBLS/MONTH
        MONTH_DATA_CNT<-Test_data%>% mutate(MONTH=format.Date(CREATEDATE,"%y%m")) %>%
            filter( substr(paste(20,as.numeric(MONTH),sep = ''),start = 0,stop = 4) == input$year)%>%
            group_by(MONTH) %>% count(MONTH) %>% data.frame()
        
        DUMMY_MONTHS_CNT = data.frame("MONTH"= c("01","02","03","04","05","06","07","08","09","10","11","12"),
                                      n = 0 ,stringsAsFactors = FALSE)
        DUMMY_MONTHS_CNT$MONTH<- paste(substr(input$year,start = 3,stop = 4),DUMMY_MONTHS_CNT$MONTH ,sep = '')
        MONTH_DATA_CNT_FNL<-rbind(MONTH_DATA_CNT,DUMMY_MONTHS_CNT)
        MONTH_DATA_CNT_FNL<-arrange(MONTH_DATA_CNT_FNL[!duplicated(MONTH_DATA_CNT_FNL[c('MONTH')]),],MONTH)
        ## plot
        x <- as.numeric(MONTH_DATA_SPC$MONTH)
        y_SPACE <- as.numeric(MONTH_DATA_SPC$MONTH_USD_SPACE)
        y_CNT <- as.numeric(MONTH_DATA_CNT_FNL$n)
        data <- data.frame(x, y_SPACE, y_CNT)
        
        xaxis <- list(title = "",
                      showline = TRUE,
                      showgrid = FALSE,
                      showticklabels = TRUE,
                      linecolor = 'rgb(204, 204, 204)',
                      linewidth = 2,
                      autotick = FALSE,
                      ticks = 'outside',
                      tickcolor = 'rgb(204, 204, 204)',
                      tickwidth = 2,
                      ticklen = 5,
                      tickfont = list(family = 'Arial',
                                      size = 12,
                                      color = 'rgb(82, 82, 82)'))
        
        yaxis <- list(title = "",
                      showgrid = FALSE,
                      zeroline = FALSE,
                      showline = FALSE,
                      showticklabels = FALSE)
        
        margin <- list(autoexpand = FALSE,
                       l = 100,
                       r = 100,
                       t = 110)
        
        # Build the annotations
        
        SPACE_1 <- list(
            xref = 'paper',
            yref = 'y',
            x = 0.05,
            y = y_SPACE[1],
            xanchor = 'right',
            yanchor = 'middle',
            text = ~paste(input$select, round(y_SPACE[1],2), 'GB'),
            font = list(family = 'Arial',
                        size = 7.5,
                        color = 'rgba(67,67,67,1)'),
            showarrow = FALSE)
        
        CNT_1 <- list(
            xref = 'paper',
            yref = 'y',
            x = 0.05,
            y = y_CNT[1],
            xanchor = 'right',
            yanchor = 'middle',
            text = ~paste('TBL COUNT ', y_CNT[1]),
            font = list(family = 'Arial',
                        size = 7.5,
                        color = 'rgba(49,130,189, 1)'),
            showarrow = FALSE)
        
        SPACE_2 <- list(
            xref = 'paper',
            x = 0.95,
            y = y_SPACE[12],
            xanchor = 'left',
            yanchor = 'middle',
            text = paste(input$select, round(y_SPACE[12],2), 'GB'),
            font = list(family = 'Arial',
                        size = 7.5,
                        color = 'rgba(67,67,67,1)'),
            showarrow = FALSE)
        
        CNT_2 <- list(
            xref = 'paper',
            x = 0.95,
            y = y_CNT[12],
            xanchor = 'left',
            yanchor = 'middle',
            text = paste('TBL CNT', y_CNT[12]),
            font = list(family = 'Arial',
                        size = 7.5,
                        color = 'rgba(67,67,67,1)'),
            showarrow = FALSE)
        
        plot_ly(data, x = ~x,width = 1000, height = 500) %>%
            add_trace(y = ~y_SPACE, type = 'scatter', mode = 'lines', line = list(color = 'rgba(0,0,0,0.7)', width = 3), name = 'Space')  %>%
            add_trace(y = ~y_CNT, type = 'scatter', mode = 'lines', line = list(color = 'rgba(49,130,189, 1)', width = 4), name = 'Count') %>%
            add_trace(x = ~c(x[1], x[12]), y = ~c(y_SPACE[1], y_SPACE[12]), type = 'scatter', mode = 'markers', marker = list(color = 'rgba(67,67,67,1)', size = 8), showlegend= FALSE) %>%
            add_trace(x = ~c(x[1], x[12]), y = ~c(y_CNT[1], y_CNT[12]), type = 'scatter', mode = 'markers', marker = list(color = 'rgba(49,130,189, 1)', size = 12),showlegend= FALSE) %>%
            layout(title = "SPACE & CNT VS MONTH", xaxis = xaxis, yaxis = yaxis, margin = margin,
                   autosize = FALSE,
                   showlegend = TRUE,
                   annotations = SPACE_1) %>%
            layout(annotations = CNT_1) %>%
            layout(annotations = SPACE_2) %>%
            layout(annotations = CNT_2)
    })
    
    db_data<- reactive({
        
        db_data<- data.frame(sqlQuery(Connection(),"SELECT *, ROUND(SUM(NVL(used_bytes,0)/pow(1024,3))OVER(PARTITION BY DATABASE ORDER BY DATABASE),2) AS TOTAL_SPACE_USED FROM (
	select DATABASE,SRC.TABLENAME,NVL(USED_BYTES,0) AS USED_BYTES,NVL(ROUND(NVL(used_bytes,0)/pow(1024,3),2),0) as used_SPACE
	--,sum(NVL(ALLOCATED_BYTES,0)/pow(1024,3)) as total_size
    from _v_table_storage_stat SRC
    JOIN _V_TABLE TGT ON SRC.OBJID=TGT.OBJID
    GROUP BY DATABASE,SRC.TABLENAME,USED_BYTES) X"))
        })
#     #Connection_schema <- reactive( odbcConnect(ifelse(is.null(input$select),"ATLAS_NA_WRK_DEV",input$select), uid = input$userName, pwd = input$passwd, believeNRows= FALSE))
#     Connection_schema <- odbcConnect("ATLAS_NA_WRK_DEV", uid = "ARREDD", pwd = "ARREDD", believeNRows= FALSE)
#     db_data<- sqlQuery(Connection_schema,"SELECT *, ROUND(SUM(NVL(used_bytes,0)/pow(1024,3))OVER(PARTITION BY DATABASE ORDER BY DATABASE),2) AS TOTAL_SPACE_USED FROM (
# 	select DATABASE,SRC.TABLENAME,NVL(USED_BYTES,0) AS USED_BYTES,NVL(ROUND(NVL(used_bytes,0)/pow(1024,3),2),0) as used_SPACE
# 	--,sum(NVL(ALLOCATED_BYTES,0)/pow(1024,3)) as total_size
#     from _v_table_storage_stat SRC
#     JOIN _V_TABLE TGT ON SRC.OBJID=TGT.OBJID
#     GROUP BY DATABASE,SRC.TABLENAME,USED_BYTES) X")
   # vals<-reactiveValues()
   #  vals$db_data<- db_data
        output$carstable <- renderDataTable({
        #Connection<- odbcConnect(input$select, uid = input$userName, pwd = input$passwd, believeNRows= FALSE)
#         db_data<- sqlQuery(Connection(),"SELECT *, ROUND(SUM(NVL(used_bytes,0)/pow(1024,3))OVER(PARTITION BY DATABASE ORDER BY DATABASE),2) AS TOTAL_SPACE_USED FROM (
# 	select DATABASE,SRC.TABLENAME,NVL(USED_BYTES,0) AS USED_BYTES,NVL(ROUND(NVL(used_bytes,0)/pow(1024,3),2),0) as used_SPACE
# 	--,sum(NVL(ALLOCATED_BYTES,0)/pow(1024,3)) as total_size
#     from _v_table_storage_stat SRC
#     JOIN _V_TABLE TGT ON SRC.OBJID=TGT.OBJID
#     GROUP BY DATABASE,SRC.TABLENAME,USED_BYTES) X")
        
        datatable(db_data(),
                  caption = "Space Occupied",
                  rownames = T,
                  filter = "top",
                  extensions = "Buttons",#selection = 'single',
                  options= list (dom = 'Bfrtip', buttons = list('copy','csv','pdf')), list(pageLength = 1000))
        
        
    })
    selected_tables = reactive({
        x = isolate(db_data())
        x = as.character(x[input$carstable_rows_selected,]$TABLENAME)
        
        
    })
    output$SHINY<-renderText({
        selected_tables()
    })
    observeEvent(input$Submit_drop,
                 {    XYZ<- data.frame(Tables = selected_tables())
                 XYZ<-paste("DROP TABLE"," ",XYZ$Tables,";",sep = "")
                 XYZ<- paste(unlist(XYZ),collapse = " ")
                 print(XYZ)
                     #print(paste("DROP TABLE"," ",selected_tables(),";",sep = ""))
                     #print(paste("DROP TABLE"," ",as.character(selected_tables()),";",sep = ""))
                     z<-sqlQuery(Connection(),XYZ)
                     print(z)
                 })
    
    
    
    selected_trends <- reactive({
        req(input$select)
        req(input$userName)
        x<-paste("Username accessed:",input$select)
        #Username<- input$userName
    })
    
    output$desc <- renderText({
        #paste(input$TEXT,input$userName,input$passwd,input$select,selected_trends())
        selected_trends()
        #paste("xxxxx:",selected_trends()) #,selected_trends()$passwd,selected_trends()$select)
        #paste(trend_text, "The index is set to 1.0 on January 1, 2004 and is calculated only for US search traffic.")
    })
    

    
    
}

runApp(list(ui = ui, server = server),launch.browser = TRUE)
