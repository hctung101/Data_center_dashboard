library(shiny)
library(shinydashboard)
library(httr)
library(xlsx)
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
                     tags$code("Username: ASUS  Password: 華碩")
                     ))
                     )

credentials <- data.frame(
  username_id = c("ASUS", "PM", "RD"),
  passod   = sapply(c("cj86gji4", "cj86gji4pm", "cj86gji4rd"), bcrypt::hashpw),
  permission  = c("basic"), 
  stringsAsFactors = F
)

header <- dashboardHeader( title = "Data center dashboard", uiOutput("logoutbtn"))
sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  
  login = F
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
        p(img(src = "eth.png", height = 15, width = 15, style="margin: 0px 5px 5px 5px;"), span("Information",style = "color:orange;")),
        a(textOutput("ethincome"), href="https://www.sparkpool.com/"),
        a(textOutput("ethrate"), href="https://www.coingecko.com/zh-tw/%E6%95%B8%E5%AD%97%E8%B2%A8%E5%B9%A3/%E4%BB%A5%E5%A4%AA%E5%B9%A3"),
        style = "text-align:center;"),  
        hr(), 
        menuItem("Summary", tabName = "summary", icon = icon("bar-chart")),
        menuItem("Photo", tabName = "photo", icon = icon("thermometer")),
        menuItem("Offline Cards", tabName = "drop", icon = icon("image")),
        menuItem("Wallet", tabName = "wallet", icon = icon("usd")),
        menuItem("ETH income/day(Sparkpool)", tabName = "Sparkpoolincome", icon = icon("money")),
        selectInput("day", 
                    "Select Day:",
                    rev(substring(list.files("/data/kent/sparkpool/", pattern = "summary", full.names = F), 1, 8))
                    
        ),
        div(
            checkboxInput("checkRepairPCS", HTML('<span style="color:blue;">維修台數(台/天)</span>'), value = F),
            htmlOutput("RepairPCS")
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
                                           #Summarytable th:nth-of-type(12){background:#77DDFF;}
                                           #Summarytable th:nth-of-type(13){background:#77DDFF;}
                                           #Summarytabletable, th, td {border: 1px solid black;}
                                           ;"))
                      
                      
                      )              
                      ),
                fluidRow(  
                  box(width=6,
                    h5("五股TA1"), a("https://gpumine.org/tw/workers/eth/0x5029ba38558b9E08011C5E19b1E2Db6e01bA895b", href="https://gpumine.org/tw/workers/eth/0x5029ba38558b9E08011C5E19b1E2Db6e01bA895b", target="_blank"),
                    h5("三峽TA2"), a("https://gpumine.org/tw/workers/eth/0x1c44d8679F577d15763c01552bA222eafC56b374", href="https://gpumine.org/tw/workers/eth/0x1c44d8679F577d15763c01552bA222eafC56b374", target="_blank"),  
                    h5("平鎮TB1"), a("https://gpumine.org/tw/workers/eth/0x2431b37Ca9f516aAd592a2aa4AcA4F4532B78Bff", href="https://gpumine.org/tw/workers/eth/0x2431b37Ca9f516aAd592a2aa4AcA4F4532B78Bff", target="_blank"),
                    h5("大園TB2"), a("https://gpumine.org/tw/workers/eth/0x8dac4436854aE2dF5fb08860394b08Ea689F12DE", href="https://gpumine.org/tw/workers/eth/0x8dac4436854aE2dF5fb08860394b08Ea689F12DE", target="_blank"),                     
                    h5("中國CA1"), a("https://www.sparkpool.com/miner/d3933cFD67b25b68f1cc319db4eCEF7D8ad60692/data?currency=ETH", href="https://www.sparkpool.com/miner/d3933cFD67b25b68f1cc319db4eCEF7D8ad60692/data?currency=ETH", target="_blank"), 
                    h5("中國CA2"), a("https://www.sparkpool.com/miner/b8a6bfa5818950cfa1f867059b0511a864bec46d/data?currency=ETH", href="https://www.sparkpool.com/miner/b8a6bfa5818950cfa1f867059b0511a864bec46d/data?currency=ETH", target="_blank"),
                    h5("中國CA3"), a("https://www.sparkpool.com/miner/09ed9806af2Db7a1B36a81A7D20B92736cBc649c/data?currency=ETH", href="https://www.sparkpool.com/miner/09ed9806af2Db7a1B36a81A7D20B92736cBc649c/data?currency=ETH", target="_blank"),
                    h5("中國CA4"), a("https://www.sparkpool.com/miner/df40e32c9a1e045abdf049b48007e8e08602d443/data?currency=ETH", href="https://www.sparkpool.com/miner/df40e32c9a1e045abdf049b48007e8e08602d443/data?currency=ETH", target="_blank"),
                    h5("中國CA5"), a("https://www.sparkpool.com/miner/4A8E5ABC10A232b17D8551c45E3603A0202555a1/data?currency=ETH", href="https://www.sparkpool.com/miner/4A8E5ABC10A232b17D8551c45E3603A0202555a1/data?currency=ETH", target="_blank"),
                    h5("中國CB1"), a("https://www.sparkpool.com/miner/a53f12a20fd687cff5e0b2c8191139629bc8d511/data?currency=ETH", href="https://www.sparkpool.com/miner/a53f12a20fd687cff5e0b2c8191139629bc8d511/data?currency=ETH", target="_blank"),
                    h5("中國CC1"), a("https://www.sparkpool.com/miner/2A01720cA6890366125718a645dd8c3AbCc7F6e5/data?currency=ETH", href="https://www.sparkpool.com/miner/2A01720cA6890366125718a645dd8c3AbCc7F6e5/data?currency=ETH", target="_blank"),
                    h5("中國CC2"), a("https://www.sparkpool.com/miner/eBfAEc3C043Bb8098Fcf01d7c4A35faC6676eBE5/data?currency=ETH", href="https://www.sparkpool.com/miner/eBfAEc3C043Bb8098Fcf01d7c4A35faC6676eBE5/data?currency=ETH", target="_blank"),                                         
                    h5("歐洲SA1"), a("https://ethermine.org/miners/0F3436BA7A16dab881f81a52F182419Bb8854EA5/dashboard", href="https://ethermine.org/miners/0F3436BA7A16dab881f81a52F182419Bb8854EA5/dashboard", target="_blank")                              
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
        # Third tab content
        tabItem(tabName = "drop",
                box(
                  sliderInput("Cardslider", label = "Number of offline Cards", min = 1, max = 12, value = c(1, 12), ticks = F),
                  radioButtons("Cardradio", label = "",
                               choices = list("五股" = 64, "平鎮" = 774, "大園" = 2895), selected = 64),
                  h5("現況更新", downloadButton("CardDownload", "Download offline cards"))
                )
        ),
        # Fourth tab content
        tabItem(tabName = "wallet",
                fluidRow(  
                box(width=10,
                    h5("現況更新", downloadButton("WalletDownload", "Wallet Report Download"))
                  ),
                box(width=10,
                    h5("五股TA1"), a("https://etherscan.io/address/0x5029ba38558b9E08011C5E19b1E2Db6e01bA895b", href="https://etherscan.io/address/0x5029ba38558b9E08011C5E19b1E2Db6e01bA895b", target="_blank"),  
                    h5("三峽TA2"), a("https://etherscan.io/address/0x1c44d8679F577d15763c01552bA222eafC56b374", href="https://etherscan.io/address/0x1c44d8679F577d15763c01552bA222eafC56b374", target="_blank"),  
                    h5("平鎮TB1"), a("https://etherscan.io/address/0x2431b37Ca9f516aAd592a2aa4AcA4F4532B78Bff", href="https://etherscan.io/address/0x2431b37Ca9f516aAd592a2aa4AcA4F4532B78Bff", target="_blank"),
                    h5("大園TB2"), a("https://etherscan.io/address/0x8dac4436854aE2dF5fb08860394b08Ea689F12DE", href="https://etherscan.io/address/0x8dac4436854aE2dF5fb08860394b08Ea689F12DE", target="_blank"),                     
                    h5("中國CA1"), a("https://etherscan.io/address/0xd3933cFD67b25b68f1cc319db4eCEF7D8ad60692", href="https://etherscan.io/address/0xd3933cFD67b25b68f1cc319db4eCEF7D8ad60692", target="_blank"), 
                    h5("中國CA2"), a("https://etherscan.io/address/0xb8a6bfa5818950cfa1f867059b0511a864bec46d", href="https://etherscan.io/address/0xb8a6bfa5818950cfa1f867059b0511a864bec46d", target="_blank"),
                    h5("中國CA3"), a("https://etherscan.io/address/0x09ed9806af2Db7a1B36a81A7D20B92736cBc649c", href="https://etherscan.io/address/0x09ed9806af2Db7a1B36a81A7D20B92736cBc649c", target="_blank"),
                    h5("中國CA4"), a("https://etherscan.io/address/0xdf40e32c9a1e045abdf049b48007e8e08602d443", href="https://etherscan.io/address/0xdf40e32c9a1e045abdf049b48007e8e08602d443", target="_blank"),
                    h5("中國CA5"), a("https://etherscan.io/address/0x4A8E5ABC10A232b17D8551c45E3603A0202555a1", href="https://etherscan.io/address/0x4A8E5ABC10A232b17D8551c45E3603A0202555a1", target="_blank"),
                    h5("中國CB1"), a("https://etherscan.io/address/0xa53f12a20fd687cff5e0b2c8191139629bc8d511", href="https://etherscan.io/address/0xa53f12a20fd687cff5e0b2c8191139629bc8d511", target="_blank"),
                    h5("中國CC1"), a("https://etherscan.io/address/0x2A01720cA6890366125718a645dd8c3AbCc7F6e5", href="https://etherscan.io/address/0x2A01720cA6890366125718a645dd8c3AbCc7F6e5", target="_blank"),
                    h5("中國CC2"), a("https://etherscan.io/address/0xeBfAEc3C043Bb8098Fcf01d7c4A35faC6676eBE5", href="https://etherscan.io/address/0xeBfAEc3C043Bb8098Fcf01d7c4A35faC6676eBE5", target="_blank"),                                         
                    h5("歐洲SA1"), a("https://etherscan.io/address/0x0F3436BA7A16dab881f81a52F182419Bb8854EA5", href="https://etherscan.io/address/0x0F3436BA7A16dab881f81a52F182419Bb8854EA5", target="_blank")                              
                )
                )
        ),
        # Fifth tab content
        tabItem(tabName = "Sparkpoolincome",
                fluidRow(  
                  box(width=10,                  
                    downloadButton("SparkpoolincomeDownload", "ETH income Report Download"),
                    radioButtons("Sparkpooltime", "Choose time to download ETH Income/Day(Sparkpool)", c("9:00(From 10/30)" = "clock9", "15:30(From 10/30) change to 15:40(From 01/05)" = "clock3", "9:00 & 15:40(From 01/05)" = "clock39","Raw from web" = "raw"))
                  ),             
                  box(width=10,
                      div(style = 'overflow-x:scroll; font-size:50%', htmlOutput("Sparkpoolincomeframe"))
                      )
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
    colnames(data)<- c("TA1_五股", "TB1_平鎮",  "CA2_中國四川康定(合資)", "CA1_中國四川康定(代管)", "CC1_內蒙烏海(合資)", "CB1_江西撫州", "CC2_內蒙鄂爾多斯", "CA3_中國四川康定(合資)", "SA1_歐洲瑞典", "TB2_大園", "CA4_中國四川康定(合資)", "CA5_中國四川康定(合資)", "TA2_三峽")[seq_len(ncol(data))]
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
      shiny::withProgress(message = 'Download data from os.sparkpool.com', style = "notification", value = 0.1, {
        
        myHttpheader <- c(
          'accept'='application/json',
          'accept-encoding'='gzip, deflate, br',
          'accept-language'='en-US,en;q=0.9,zh-TW;q=0.8,zh-CN;q=0.7,zh;q=0.6,ja;q=0.5',
          'authorization'='Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJodHRwOlwvXC8xMC4wLjMuMTA2OjgxIiwiaWF0IjoxNjA3OTM0ODk4LCJleHAiOjE5MjQ2NzcyOTgsIm5iZiI6MTYwNzkzNDg5OCwianRpIjozNTAyODUsInN1YiI6MzUwMjg1LCJybWIiOnRydWV9.7OhKCCYqZ2_OL3vquH3Pznj-vasPLbRkPpRZJlI1rIY',
          'content-type'='application/json',
          'cookie'='_ga=GA1.2.1271891077.1600824842; jwtt=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJodHRwOlwvXC8xMC4wLjMuMTA2OjgxIiwiaWF0IjoxNjA3OTM0ODk4LCJleHAiOjE5MjQ2NzcyOTgsIm5iZiI6MTYwNzkzNDg5OCwianRpIjozNTAyODUsInN1YiI6MzUwMjg1LCJybWIiOnRydWV9.7OhKCCYqZ2_OL3vquH3Pznj-vasPLbRkPpRZJlI1rIY; accordion-state-troubleshooting=true; timeRequest=1609383532260',
          'referer'='https://os.flintos.cn/farms/64/',
          'sec-fetch-dest'='empty',
          'sec-fetch-mode'='same-origin',
          'sec-fetch-site'='same-origin',
          'user-agent'='Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/87.0.4280.88 Safari/537.36',
          'x-api-version'='2.2')
      out3.data <- jsonlite::fromJSON(httr::content(httr::GET(paste0("https://os.flintos.cn/api/v2/farms/", input$Cardradio,"/workers2")
                                                              ,httr::add_headers(.headers = myHttpheader)), "text"))
      out.by.gpu <- cbind(out3.data$data$name, data.table::rbindlist(lapply(out3.data$data$miners_stats$hashrates,#data.frame 
                                                                            function(x) if(is.null(x)) data.frame(miner = NA) else data.frame(t(unlist(x)))), fill = T))
      offlinecard <- apply(subset(out.by.gpu, select= names(out.by.gpu)[grep("^bus_numbers\\d", names(out.by.gpu))]), 1, function(y){!(c(1, 2, 7, 8, 11, 12, 13, 14, 15, 16, 17, 18) %in% y)})
      offlinecard[, !grepl("-12", out.by.gpu$V1)] <- apply(subset(out.by.gpu, select= names(out.by.gpu)[grep("^bus_numbers\\d", names(out.by.gpu))]), 1, function(y){!(c(2, 3,  6, 7,  8, 9, 10,	11, NA, NA, NA, NA) %in% y)})[, !grepl("-12", out.by.gpu$V1)]
      
      
      offlinecard.location <- which(apply(offlinecard, 2, sum) >= input$Cardslider[1] & apply(offlinecard ,2,sum) <= input$Cardslider[2])
      

      MB.p <- list(png::readPNG('/home/kent/MB.png'), png::readPNG('/home/kent/MB2.png'))
      pdf("/srv/shiny-server/Data_center_dashboard/www/temp.pdf")
      farm <- switch(as.character(input$Cardradio), "64" = "TA1", "774" = "TB1", "2895" = "TB2") 
      location <- read.csv(paste0("/home/kent/location_", farm, ".csv"), header =T)[,-1]
      data <- data.frame(
        x = rep(1:nrow(location), ncol(location)),
        y =rep(1:ncol(location), each = nrow(location)),
        z.temp  = unlist(location)
      )
      data$z.off <- as.numeric(data$z.temp) %in% na.omit(as.numeric(stringr::str_extract(out.by.gpu$V1[offlinecard.location], "\\d+$")))           
      if(farm == "TB2"){
        data$z.off <- as.numeric(data$z.temp) %in% na.omit(as.numeric(stringr::str_extract(out.by.gpu$V1[offlinecard.location], "^\\d+")))                   
      }
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
        if(grepl("-12", out.by.gpu$V1[j])){
        rasterImage(MB.p[[1]] , 0,0,20,20)
        points( c(6, 6, 11.1, 11.1, 11.1, 6, 6, 6, 6, 11.1, 11.1, 11.1), 
                c(2.1, 3.3, 4.5, 2.1, 6.9, 5.7, 4.5, 6.9, 9.3, 9.3, 5.7, 3.3),
                col = (!offlinecard[,j]) + 2, cex = 1.8, pch = c(15, 22)[offlinecard[,j]+1])
        text( c(6, 6, 11.1, 11.1, 11.1, 6, 6, 6, 6, 11.1, 11.1, 11.1), 
              c(2.1, 3.3, 4.5, 2.1, 6.9, 5.7, 4.5, 6.9, 9.3, 9.3, 5.7, 3.3),
              c(1, 2, 7, 8, "b", "c", "d", "e", "f", 10, 11, 12))
        
        }else{
        rasterImage(MB.p[[2]] , 0,0,20,20)
        points( c(1, 3.1, 5.2, 7.3, 9.4, 11.5, 13.6, 15.7,25,25,25,25), 
                c(rep(12, 12)),
                col = (!offlinecard[,j]) + 2, cex = 1.8, pch = c(15, 22)[offlinecard[,j]+1])
        text( c(1, 3.1, 5.2, 7.3, 9.4, 11.5, 13.6, 15.7,25,25,25,25), 
              c(rep(12, 12)),
              c("b", "a", 9, 8, 7, 6, 3, 2, 10, 11, 12, NA, NA, NA, NA))
        }
      }
      dev.off()
      })
      file.copy("/srv/shiny-server/Data_center_dashboard/www/temp.pdf"
                , file)
    }
  )
  output$WalletDownload <- downloadHandler(
    filename <- function() {
      paste0(Sys.Date(), " wallet report.xlsx")
    },    
    content <- function(file) {
      shiny::withProgress(message = 'Download data from https://etherscan.io', style = "notification", value = 0.1, {
      set_config(use_proxy(url = "", port = 8787))
      wb <- xlsx::loadWorkbook("/home/kent/Data_center_weekly_report_template.xlsx")      
      Rigtemplatepath <- "/home/kent/Online Rig templatev11.xlsx"
      wb1 <- xlsx::loadWorkbook(Rigtemplatepath)
      Mine.name <- names(xlsx::getSheets(wb1))

      wallet <- c("0x5029ba38558b9E08011C5E19b1E2Db6e01bA895b", 
                  "0x2431b37Ca9f516aAd592a2aa4AcA4F4532B78Bff",          
                  "0xb8A6bfa5818950CfA1F867059b0511A864BeC46D",
                  "0xd3933cFD67b25b68f1cc319db4eCEF7D8ad60692",
                  "0x2A01720cA6890366125718a645dd8c3AbCc7F6e5",
                  "0xa53F12a20Fd687cfF5E0b2c8191139629BC8d511",
                  "0xeBfAEc3C043Bb8098Fcf01d7c4A35faC6676eBE5",
                  "0x09ed9806af2Db7a1B36a81A7D20B92736cBc649c",
                  "0x0F3436BA7A16dab881f81a52F182419Bb8854EA5",
                  "0x8dac4436854aE2dF5fb08860394b08Ea689F12DE",
                  "0xdf40e32c9a1e045abdf049b48007e8e08602d443",
                  "0x4A8E5ABC10A232b17D8551c45E3603A0202555a1",
                  "0x1c44d8679F577d15763c01552bA222eafC56b374")
      
      for(wallet.i in seq_len(length(wallet))){    
        ethwalleturl <- paste0("https://etherscan.io/address/", wallet[wallet.i])
      
        ethwalletpage <- xml2::read_html(httr::GET(ethwalleturl))
        #ethwallet <- rvest::html_text(rvest::html_nodes(ethwalletpage , xpath = "//div[@class='card-body']//div[@class='col-md-8']"))[1]
        ethtransactions <- rvest::html_table(ethwalletpage, fill = T)[[1]]
        names(ethtransactions) <- c("","Txn Hash", "Block",  "Date", "age", "From", "status", "To", "Value", "[Txn Fee]")
        ethtransactions <- ethtransactions[order(ethtransactions$"Date"),]
        
        
        #ethtransactions$"Txn Hash" <-  rvest::html_text(rvest::html_nodes(ethwalletpage , xpath = "//a[@class='hash-tag text-truncate']"))[seq(1,50,2)]
        #ethtransactions$"From" <-  rvest::html_text(rvest::html_nodes(ethwalletpage , xpath = "//a[@class='hash-tag text-truncate']"))[seq(2,50,2)]
        #ethtransactions$"To" <- rvest::html_text(rvest::html_nodes(ethwalletpage , xpath = "//span[@class='hash-tag text-truncate']"))
        #ethtransactions$"To"[ethtransactions$"status"=="OUT"] <-  ethtransactions$"From"[ethtransactions$"status"=="OUT"]
        #ethtransactions$"From"[ethtransactions$"status"=="OUT"] <- wallet[wallet.i]
        
        xlsx::addDataFrame( ethtransactions, sheet =  xlsx::getSheets(wb)[[Mine.name[wallet.i]]], col.names = T, row.names=F, startRow= 1, startColumn = 1)
        
      }
      
     
      filenamepath <- paste0("/srv/shiny-server/Data_center_dashboard/www/temp.xlsx")
      xlsx::saveWorkbook(wb, filenamepath)
      #xlsx::forceFormulaRefresh(filenamepath)
      })
      file.copy(filenamepath, file)
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
    ethurl <- "https://api.coingecko.com/api/v3/coins/markets?vs_currency=usd&ids=ethereum&order=market_cap_desc&per_page=100&page=1&sparkline=false"
    ethpage <- httr::content(GET(ethurl), "text")
    ethrate <-fromJSON(ethpage)$current_price
    paste0("乙太價格(coingecko):",ethrate)
  })
  output$ethincome <- renderText({
    invalidateLater(10000)
    sparkpool.income.day <- jsonlite::fromJSON(httr::content(httr::GET("https://www.sparkpool.com/v1/pool/stats?pool=SPARK_POOL_CN"), "text"))$data    
    ethincome <- sparkpool.income.day[sparkpool.income.day$currency=="ETH", "meanIncome24h"]
    paste0("ETH Income/Day(Sparkpool):",ethincome)
    })
  
  output$RepairPCS <- renderUI({
    #invalidateLater(10000)
    ethurl <- "https://api.coingecko.com/api/v3/coins/markets?vs_currency=usd&ids=ethereum&order=market_cap_desc&per_page=100&page=1&sparkline=false"
    ethpage <- httr::content(GET(ethurl), "text")
    ethrate <-fromJSON(ethpage)$current_price
    summarytable <- readRDS(data.table::last(list.files("/data/kent/sparkpool/", pattern = "summary", full.names = T), 1))
    Average.per.rig.payment <- as.numeric(levels(summarytable[,8]))[summarytable[,8]]
    repairpcs <- c(
      200 / (Average.per.rig.payment[1] * 30.5 * ethrate - 2.55 * 24 * 1.7),
      800 / (Average.per.rig.payment[2] * 30.5 * ethrate - 2.41 * 24 * 1.7),
      800 / (Average.per.rig.payment[2] * 30.5 * ethrate - 2.41 * 24 * 1.7))
    if(input$checkRepairPCS){
      myHttpheader <- c(
        'accept'='application/json',
        'accept-encoding'='gzip, deflate, br',
        'accept-language'='en-US,en;q=0.9,zh-TW;q=0.8,zh-CN;q=0.7,zh;q=0.6,ja;q=0.5',
        'authorization'='Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJodHRwOlwvXC8xMC4wLjMuMTA2OjgxIiwiaWF0IjoxNjA3OTM0ODk4LCJleHAiOjE5MjQ2NzcyOTgsIm5iZiI6MTYwNzkzNDg5OCwianRpIjozNTAyODUsInN1YiI6MzUwMjg1LCJybWIiOnRydWV9.7OhKCCYqZ2_OL3vquH3Pznj-vasPLbRkPpRZJlI1rIY',
        'content-type'='application/json',
        'cookie'='_ga=GA1.2.1271891077.1600824842; jwtt=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJodHRwOlwvXC8xMC4wLjMuMTA2OjgxIiwiaWF0IjoxNjA3OTM0ODk4LCJleHAiOjE5MjQ2NzcyOTgsIm5iZiI6MTYwNzkzNDg5OCwianRpIjozNTAyODUsInN1YiI6MzUwMjg1LCJybWIiOnRydWV9.7OhKCCYqZ2_OL3vquH3Pznj-vasPLbRkPpRZJlI1rIY; accordion-state-troubleshooting=true; timeRequest=1609383532260',
        'referer'='https://os.flintos.cn/farms/64/',
        'sec-fetch-dest'='empty',
        'sec-fetch-mode'='same-origin',
        'sec-fetch-site'='same-origin',
        'user-agent'='Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/87.0.4280.88 Safari/537.36',
        'x-api-version'='2.2')
      nworker <- unlist(lapply( list(64, 774, 2895), function(x)jsonlite::fromJSON(httr::content(httr::GET(paste0("https://os.flintos.cn/api/v2/farms/", x),
                                                                                                           httr::add_headers(.headers = myHttpheader)), "text"))$stats$rigs_online))
      inout <- (c(187, 705, 486)) - nworker >=  repairpcs 
    }else{
      inout <- c(F, F ,F)
    }
    paste0(c("五股","平鎮","大園"), ":", round(repairpcs, 2), collapse ="")
    tags$body(span(paste0("五股:", round(repairpcs, 2)[1]), style = c("color:white", "color:red")[inout + 1][1]),
              span(paste0("平鎮:", round(repairpcs, 2)[2]), style = c("color:white", "color:red")[inout + 1][2]),
              span(paste0("大園:", round(repairpcs, 2)[3]), style = c("color:white", "color:red")[inout + 1][3])
              )
  })
  output$SparkpoolincomeDownload <- downloadHandler(
    filename <- function() {
      paste0(Sys.Date(), " SparkpoolincomeDownload report.csv")
    },    
    content <- function(file) {
      shiny::withProgress(message = 'Download data from server', style = "notification", value = 0.1, {
         Sparkpoolincome <- switch(input$Sparkpooltime,                                   
                "clock9" = { Sparkpoolincometemp <- data.table::rbindlist(lapply(list.files("/data/kent/sparkpoolincomeday/", pattern = "_09" , full.names = T), read.csv))
                             Sparkpoolincometemp[Sparkpoolincometemp$currency=="ETH"]},  
                "clock3" = { Sparkpoolincometemp <- data.table::rbindlist(lapply(list.files("/data/kent/sparkpoolincomeday/", pattern = "_15" , full.names = T), read.csv))
                             Sparkpoolincometemp[Sparkpoolincometemp$currency=="ETH"]},  
                "clock39" = { Sparkpoolincometemp <- data.table::rbindlist(lapply(list.files("/data/kent/sparkpoolincomeday/", full.names = T), read.csv))
                              Sparkpoolincometemp[Sparkpoolincometemp$currency=="ETH"]},  
                "raw" = jsonlite::fromJSON(httr::content(httr::GET("https://www.sparkpool.com/v1/currency/statsHistory?currency=ETH&zoom=m"),"text"))$data)
     
         filenamepath <- paste0("/srv/shiny-server/Data_center_dashboard/www/temp.csv")
         write.csv(Sparkpoolincome, filenamepath, row.names = F)
      })
      
      file.copy(filenamepath, file)
    }
  )
  output$Sparkpoolincomeframe<- renderUI({
    
   tags$iframe(src="https://www.sparkpool.com/token/ETH#chart2", height=400, width=1000)

  })
}

shinyApp(ui, server)



#rbindlist(lapply(rev(tail(list.files("/data/kent/sparkpool/", pattern = "summary", full.names = T), 2)), readRDS), fill =T)

