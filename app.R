library(shiny)
library(shinydashboard)
library(httr)
library(jsonlite)
library(shinyjs)

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
                     br(),
                     br(),
                     tags$code("Username: ASUS  Password: 88888888")
                     ))
                     )

credentials <- data.frame(
  username_id = c("ASUS"),
  passod   = sapply(c("88888888"), bcrypt::hashpw),
  permission  = c("basic"), 
  stringsAsFactors = F
)

header <- dashboardHeader( title = "Data center dashboard", uiOutput("logoutbtn"))
sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  
  login = T
  USER <- reactiveValues(login = login)
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- bcrypt::checkpw(Password, pasmatch)
            if(pasverify) {
              USER$login <- TRUE
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
        hr(),
        div(
        span("Information",style = "color:orange;"),
        a(textOutput("ethincome"), href="https://www.sparkpool.com/"),
        a(textOutput("ethrate"), href="https://www.coingecko.com/zh-tw/%E6%95%B8%E5%AD%97%E8%B2%A8%E5%B9%A3/%E4%BB%A5%E5%A4%AA%E5%B9%A3"),
        style = "text-align:center;"),  
        hr(), 
        menuItem("Summary", tabName = "summary", icon = icon("dashboard")),
        menuItem("Photo", tabName = "photo", icon = icon("th")),
        menuItem("Offline Cards", tabName = "drop", icon = icon("database")),
        selectInput("day", 
                    "Select Day:",
                    rev(substring(list.files("/data/kent/sparkpool/", pattern = "summary", full.names = F), 1, 8))
                    
        )  
   
      )
    }
  })
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      tabItems(
        # First tab content
        tabItem(tabName = "summary", class = "active",
                fluidRow(             
                  box(
                    h5("2019/7/10之後每天早上9:00更新"),
                    downloadButton("SummaryDownload", "Download Daily Report")                  
                  )
                ),
                fluidRow(                
                  box(width=12, div(style = 'overflow-x:scroll; font-size:50%', tableOutput("Summarytable")),
                      tags$head(tags$style("#Summarytable th:nth-of-type(2){background:#99FFFF;}
                                           #Summarytable th:nth-of-type(3){background:#FFB7DD;}
                                           #Summarytable th:nth-of-type(4){background:#99FF99;}
                                           #Summarytable th:nth-of-type(5){background:#CCBBFF;}
                                           #Summarytable th:nth-of-type(6){background:#00DDDD;}
                                           #Summarytable th:nth-of-type(7){background:#D2691E;}
                                           #Summarytable th:nth-of-type(8){background:#5f9ea0;}
                                           #Summarytable th:nth-of-type(9){background:#FFB8DD;}
                                           #Summarytable th:nth-of-type(10){background:#99FF99;}
                                           #Summarytable th:nth-of-type(11){background:#003C9D;}
                                           #Summarytabletable, th, td {border: 1px solid black;}
                                           ;"))
                      
                      
                      )              
                      ),
                fluidRow(  
                  box(width=6,
                    h5("五股TA1"), a("https://gpumine.org/worker/eth/0x5029ba38558b9E08011C5E19b1E2Db6e01bA895b", href="https://gpumine.org/worker/eth/0x5029ba38558b9E08011C5E19b1E2Db6e01bA895b", target="_blank"),                   
                    h5("平鎮TB1"), a("https://gpumine.org/worker/eth/0x2431b37Ca9f516aAd592a2aa4AcA4F4532B78Bff", href="https://gpumine.org/worker/eth/0x2431b37Ca9f516aAd592a2aa4AcA4F4532B78Bff", target="_blank"),
                    h5("大園TB2"), a("https://gpumine.org/worker/eth/0xcC21f39fD1961b7D1c3D16c103af2987aBA09816", href="https://gpumine.org/worker/eth/0xcC21f39fD1961b7D1c3D16c103af2987aBA09816", target="_blank"),                     
                    h5("中國CA1"), a("https://www.sparkpool.com/miner/d3933cFD67b25b68f1cc319db4eCEF7D8ad60692/data?currency=ETH", href="https://www.sparkpool.com/miner/d3933cFD67b25b68f1cc319db4eCEF7D8ad60692/data?currency=ETH", target="_blank"), 
                    h5("中國CA2"), a("https://www.sparkpool.com/miner/b8a6bfa5818950cfa1f867059b0511a864bec46d/data?currency=ETH", href="https://www.sparkpool.com/miner/b8a6bfa5818950cfa1f867059b0511a864bec46d/data?currency=ETH", target="_blank"),
                    h5("中國CC1"), a("https://www.sparkpool.com/miner/2A01720cA6890366125718a645dd8c3AbCc7F6e5/data?currency=ETH", href="https://www.sparkpool.com/miner/2A01720cA6890366125718a645dd8c3AbCc7F6e5/data?currency=ETH", target="_blank"),
                    h5("中國CC2"), a("https://www.sparkpool.com/miner/eBfAEc3C043Bb8098Fcf01d7c4A35faC6676eBE5/data?currency=ETH", href="https://www.sparkpool.com/miner/eBfAEc3C043Bb8098Fcf01d7c4A35faC6676eBE5/data?currency=ETH", target="_blank"), 
                    h5("中國CB1"), a("https://www.sparkpool.com/miner/a53f12a20fd687cff5e0b2c8191139629bc8d511/data?currency=ETH", href="https://www.sparkpool.com/miner/a53f12a20fd687cff5e0b2c8191139629bc8d511/data?currency=ETH", target="_blank"),
                    h5("中國CA3"), a("https://www.sparkpool.com/miner/09ed9806af2Db7a1B36a81A7D20B92736cBc649c/data?currency=ETH", href="https://www.sparkpool.com/miner/09ed9806af2Db7a1B36a81A7D20B92736cBc649c/data?currency=ETH", target="_blank"), 
                    h5("歐洲SA1"), a("https://ethermine.org/miners/0F3436BA7A16dab881f81a52F182419Bb8854EA5/dashboard", href="https://ethermine.org/miners/0F3436BA7A16dab881f81a52F182419Bb8854EA5/dashboard", target="_blank"),
                    h5("出金"), a("https://etherscan.io/address/", href= "https://etherscan.io/address/")               
                    
                  ),
                  box(
                    htmlOutput("frame")
                    
                  )
                )
                  ),      
        # Second tab content
        tabItem(tabName = "photo",
                fluidRow(
                  box(
                    h5("每天下午2:00更新", downloadButton("PhotoDownload", "Download Photo"))                
                  )
                ),
                fluidRow(
                  tabBox(width=12,
                         # The id lets us use input$tabset1 on the server to find the current tab
                         id = "tabset1", height = "1000px",
                         tabPanel("TA1_五股", htmlOutput("photo1")),
                         tabPanel("TB1_平鎮", htmlOutput("photo2")),
                         tabPanel("TB2_大園", htmlOutput("photo3"))
                  )
                )
        ),
        tabItem(tabName = "drop",
                box(
                  sliderInput("Cardslider", label = "Number of offline Cards", min = 1, max = 12, value = c(1, 12), ticks = F),
                  radioButtons("Cardradio", label = "",
                               choices = list("五股" = 64, "平鎮" = 774, "大園" = 2895), selected = 64),
                  downloadButton("CardDownload", "Download offline cards")
                )
        )
                  )
    }
    else {
      loginpage
    }
  })
  output$SummaryDownload <- downloadHandler(
    filename <- function() {
      paste0(input$day, "_Data_center_daily_report.xlsx")
    },    
    content <- function(file) {
      file.copy(list.files("/data/kent/sparkpool/", pattern = paste0(input$day, "_Data_center_daily_report.xlsx"), full.names = T)
                , file)
    }
  )
  output$Summarytable <- renderTable({
    data <- (readRDS(list.files("/data/kent/sparkpool/", pattern = paste0(input$day, "summary"), full.names = T)))
    data <- t(data)
    colnames(data)<- c("TA1_五股", "TB1_平鎮",  "CA2_中國四川康定(合資)", "CA1_中國四川康定(代管)", "CC1_內蒙烏海(合資)", "CB1_江西撫州", "CC2_內蒙鄂爾多斯", "CA3_中國四川康定(合資)", "SA1_歐洲瑞典", "TB2_大園")[seq_len(ncol(data))]
    rownames(data)<- c("總機台數量", "上線數量", "掉線數量", "在線率", "當日出金",             
                       "累積出金", "每台平均算力", "平均每台每天出金", "平均每算力每天出金",
                       "乙太價格", "掉線機台", "錢包餘額")
    data[-11,]
  }, hover =F, rownames = T, width = 1000)  
  output$photo1 <- renderUI({
    filename <- list.files("/data/kent/photo/", pattern =paste0(as.Date(input$day, "%Y%m%d"), c("五股", "Wugu"), collapse="|"))
    file.copy(paste0("/data/kent/photo/", filename), 
              paste0("/srv/shiny-server/Data_center_dashboard/www/", filename),
              overwrite = T)
    file.copy(paste0("/data/kent/photo/", filename), 
              paste0("/srv/shiny-server/Data_center_dashboard/www/temp.png"),
              overwrite = T)    
    img(src = filename, height = 600, width = 1000)
    #img(src = "temp.png", height = 600, width = 1000)
  })  
  output$photo2 <- renderUI({
    
    filename <- list.files("/data/kent/photo/", pattern =paste0(as.Date(input$day, "%Y%m%d"), c("平鎮", "Pingzhen"), collapse="|"))    
    file.copy(paste0("/data/kent/photo/", filename), 
              paste0("/srv/shiny-server/Data_center_dashboard/www/", filename),
              overwrite = T)
    file.copy(paste0("/data/kent/photo/", filename), 
              paste0("/srv/shiny-server/Data_center_dashboard/www/temp.png"),
              overwrite = T)    
    img(src = filename, height = 600, width = 1000)
    #img(src = "temp.png", height = 600, width = 1000)
  })
  output$photo3 <- renderUI({
    
    filename <- list.files("/data/kent/photo/", pattern =paste0(as.Date(input$day, "%Y%m%d"), c("大園", "Dayuan"), collapse="|"))    
    file.copy(paste0("/data/kent/photo/", filename), 
              paste0("/srv/shiny-server/Data_center_dashboard/www/", filename),
              overwrite = T)
    file.copy(paste0("/data/kent/photo/", filename), 
              paste0("/srv/shiny-server/Data_center_dashboard/www/temp.png"),
              overwrite = T)    
    img(src = filename, height = 600, width = 1000)
    #img(src = "temp.png", height = 600, width = 1000)
  })  
  output$PhotoDownload <- downloadHandler(
    filename <- function() {
      paste0(input$day, "_photo.png")
    },    
    content <- function(file) {
      file.copy("/srv/shiny-server/Data_center_dashboard/www/temp.png"
                , file)
    }
  )
  
  
  output$CardDownload <- downloadHandler(
    filename <- function() {
      paste0(switch(as.character(input$Cardradio), "64" = "TA1_", "774" = "TB1_", "2895" = "TB2_") 
             ,input$Cardslider[1], "_", input$Cardslider[2], "_offline_cards.pdf")
    },    
    content <- function(file) {
      
      myHttpheader <- c(
        'Accept'='application/json',
        'Accept-Encoding'='gzip, deflate, br',
        'Accept-Language'='en-US,en;q=0.9,zh-TW;q=0.8,zh;q=0.7',
        'Authorization'='Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJodHRwczpcL1wvMTAuMC4zLjEwNjo4MSIsImlhdCI6MTU1MDYzNTIwNiwiZXhwIjoxODY3NDY0MDA2LCJuYmYiOjE1NTA2MzUyMDYsImp0aSI6MjUyOTIsInN1YiI6MjUyOTIsInJtYiI6dHJ1ZX0.n61YJobsb8z2MebS3bxj4C8AwRczwcEmCguFvt44mwM',
        'Connection'='keep-alive',
        'Content-Type'='application/json',
        'Cookie'='AC_SESS=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6NjQzNzksImlhdCI6MTUzOTg0ODU0NywiZXhwIjoxNTcxMzg0NTQ3fQ.OOshvSz0OPlEz0JYb4VJ4iXe9F9_tWdVF7c0RBe3B1o; AC_SESS.sig=rzaNd5d_sS4escle7ESLkCK_8SXOAKYVPQHdJZA6VvE; isLogin=true; origin_uri=https://account.sparkpool.com/; origin_uri.sig=_ucRQaNLcqv2iM9u5iCcTr4uoHgf66e_fUs2PP37eQo; _ga=GA1.2.1996734754.1539849791; __zlcmid=owhizUv2yyj7OU; jwtt=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJodHRwOlwvXC8xMC4wLjMuMTA2OjgxIiwiaWF0IjoxNTQwMzYwOTAwLCJleHAiOjE4NTcxOTMzMDAsIm5iZiI6MTU0MDM2MDkwMCwianRpIjoyNDA4LCJzdWIiOjI0MDgsInJtYiI6dHJ1ZX0.09aWJQadWG8qv5uXc-yNTzznLwHBzMO8S-HcxSQCbD0',
        'Host'='os.sparkpool.com',
        'Referer'='https://os.sparkpool.com/farms/64',
        'User-Agent'='Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/69.0.3497.100 Safari/537.36)'
      )
      out3.data <- jsonlite::fromJSON(httr::content(httr::GET(paste0("https://os.sparkpool.com/api/v2/farms/", input$Cardradio,"/workers")
                                                              ,httr::add_headers(.headers = myHttpheader)), "text"))
      out.by.gpu <- cbind(out3.data$data$name, data.table::rbindlist(lapply(out3.data$data$miners_stats$hashrates,#data.frame 
                                                                            function(x) if(is.null(x)) data.frame(miner = NA) else data.frame(t(unlist(x)))), fill = T))
      offlinecard <- apply(subset(out.by.gpu, select= names(out.by.gpu)[grep("^bus_numbers\\d", names(out.by.gpu))]), 1, function(y){!(c(1, 2, 7, 8, 11, 12, 13, 14, 15, 16, 17, 18) %in% y)})
      offlinecard.location <- which(apply(offlinecard, 2, sum) >= input$Cardslider[1] & apply(offlinecard ,2,sum) <= input$Cardslider[2])
      MB.p <- png::readPNG('/home/kent/MB.png')
      pdf("/srv/shiny-server/Data_center_dashboard/www/temp.pdf")
      farm <- switch(as.character(input$Cardradio), "64" = "TA1", "774" = "TB1", "2895" = "TB2") 
      location <- read.csv(paste0("/home/kent/location_", farm, ".csv"), header =T)[,-1]
      data <- data.frame(
        x = rep(1:nrow(location), ncol(location)),
        y =rep(1:ncol(location), each = nrow(location)),
        z.temp  = unlist(location)
      )
      data$z.off <- as.numeric(data$z.temp) %in% na.omit(as.numeric(stringr::str_extract(out.by.gpu$V1[offlinecard.location], "\\d+$")))           
      op <-par(mar=c(0, 0, 0, 0))
      plot(c(0, ncol(location) + 1), c(0, nrow(location)), type = "n", axes = F, xlab ="", ylab = "")   
      points(data$y[data$z.off], nrow(location)- data$x[data$z.off], pch = 1, col = 2, cex = 1.8)
      text(data$y, nrow(location)- data$x, data$z.temp, col = data$z.off  + 1, cex = .48, srt = 20)
      par(op)
      plot(c(0, ncol(location) + 1), c(0, nrow(location)), type = "n", axes = F, xlab ="", ylab = "")   
      #gridExtra::grid.table(data.frame(rig = out.by.gpu$V1[offlinecard.location], card = apply(offlinecard, 2, sum)[offlinecard.location]), theme = gridExtra::ttheme_default(base_size = 10))
      gridExtra::grid.table(matrix(c(paste(out.by.gpu$V1[offlinecard.location], apply(offlinecard, 2, sum)[offlinecard.location], sep = ":"), rep("", 4 - length(offlinecard.location) %% 4)), ncol = 4), theme = gridExtra::ttheme_default(base_size = 8))
      
      for(j in offlinecard.location){
        plot(0:20, type = "n", xlab = out.by.gpu$V1[j], ylab = "", axes = F)
        rasterImage(MB.p , 0,0,20,20)
        points( c(6, 6, 11.1, 11.1, 11.1, 6, 6, 6, 6, 11.1, 11.1, 11.1), 
                c(2.1, 3.3, 4.5, 2.1, 6.9, 5.7, 4.5, 6.9, 9.3, 9.3, 5.7, 3.3),
                col = (!offlinecard[,j]) + 2, cex = 1.8, pch = c(15, 22)[offlinecard[,j]+1])
        text( c(6, 6, 11.1, 11.1, 11.1, 6, 6, 6, 6, 11.1, 11.1, 11.1), 
              c(2.1, 3.3, 4.5, 2.1, 6.9, 5.7, 4.5, 6.9, 9.3, 9.3, 5.7, 3.3),
              c(1, 2, 7, 8, "b", "c", "d", "e", "f", 10, 11, 12))
      }
      dev.off()
      file.copy("/srv/shiny-server/Data_center_dashboard/www/temp.pdf"
                , file)
    }
  )
  
  output$frame <- renderUI({
    tags$body(HTML('
                   <script src="https://widgets.coingecko.com/coingecko-coin-compare-chart-widget.js"></script>
                   <coingecko-coin-compare-chart-widget  coin-ids="bitcoin,ethereum,eos,ripple,litecoin" currency="usd" locale="en"></coingecko-coin-compare-chart-widget>'))
  }) 
  output$ethrate <- renderText({
    invalidateLater(10000)
    #ethurl <- "https://www.maicoin.com/api/prices/eth-usd"
    #myHttpheader <- c(
    #  'Accept'='*/*',
    #  'Referer'=' https://www.maicoin.com/zh-TW/charts/eth',
    #  'User-Agent'='Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/74.0.3729.169 Safari/537.36',
    #  'X-CSRF-Token'='GJ1CbgR2kz10TFRWjS+7KJ4Z0ljJ1IRTYCb+NsZKI+LWXaie8hMb2vMv+TRlyhkUWi15tTgv3zlKq4b5NK0jwQ==',
    #  'X-Requested-With'='XMLHttpRequest ')
    #ethrate <- as.numeric(substring(jsonlite::fromJSON(httr::content(httr::GET(ethurl                                                                               , httr::add_headers(.headers = myHttpheader)), "text"))$formatted_sell_price, 2))
    ethurl <- "https://www.coingecko.com/zh-tw/%E5%8C%AF%E7%8E%87%E5%9C%96/%E4%BB%A5%E5%A4%AA%E5%B9%A3/usd"
    ethpage <- xml2::read_html(ethurl)
    ethrate <- as.numeric(gsub("\\$","", rvest::html_text(rvest::html_nodes(ethpage, xpath = "//span[@class='no-wrap']"))[1]))
    
    paste0("乙太價格(coingecko):",ethrate)
  })
  output$ethincome <- renderText({
    invalidateLater(10000)
    sparkpool.income.day <- jsonlite::fromJSON(httr::content(httr::GET("https://www.sparkpool.com/v1/pool/stats?pool=SPARK_POOL_CN"), "text"))$data    
    ethincome <- sparkpool.income.day[sparkpool.income.day$currency=="ETH", "income"]
    paste0("ETH Income/Day(Sparkpool):",ethincome)
    })
  }

shinyApp(ui, server)



#rbindlist(lapply(rev(tail(list.files("/data/kent/sparkpool/", pattern = "summary", full.names = T), 2)), readRDS), fill =T)

