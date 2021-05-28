options( java.parameters = "-Xmx6g" )
library(rvest)
library(httr)
library(XML)
library(stringr)
library(data.table)
library(xlsx)
library(jsonlite)
#library(RCurl)
#https://eth-tw.gpumine.org/
#https://eth-tw.gpumine.org/worker?addr=0xC85a5e01E02F8C82f345e2e83F583B1f47d87f02
options(scipen=999)
set_config(use_proxy(url = "", port = 8787))
#Setup----------------------------------------------------------
filename <- paste0(gsub("-| |:","",as.Date(Sys.time())), "_Data_center_daily_report.xlsx")
file.copy("/home/kent/gpuming_templatev14.xlsx", paste0("/data/kent/sparkpool/", filename))
filenamepath <- paste0("/data/kent/sparkpool/", filename)
wb <- loadWorkbook(filenamepath)

Rigtemplatepath <- "/home/kent/Online Rig templatev11.xlsx"
wb1 <- loadWorkbook(Rigtemplatepath)
Mine.name <- names(getSheets(wb1))
#read.xlsx(Rigtemplatepath, sheetName = Mine.name[wallet.i])$Rig
#na.omit(readColumns(getSheets(wb1)[[Mine.name[wallet.i]]], startColumn= 1, endColumn= 1, startRow= 1)$Rig)
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
            "0x4A8E5ABC10A232b17D8551c45E3603A0202555a1")
type.l <- c("TW", "TW", "CN", "CN", "CN", "CN", "CN", "CN", "EU", "TW", "CN", "CN")
workers.all.n.l <- c(248, 705, 15000, 4000, 8000, 25000, 12000, 3250, 1340, 426, 1800, 1100)
result.miner.l <- lapply( wallet, function(x){ if(which(x == wallet) %in% c(1,2,10)){GET(paste0("https://gpumine.org/api/miner?currency=ETH&address=", x))}else{ if(which(x == wallet) == 9){GET(paste0("https://api.ethermine.org/miner/", tolower(substring(x, 3)), "/dashboard"))}else{GET(paste0("https://www.sparkpool.com/v1/worker/list?miner=", tolower(substring(x, 3)), "&pool=SPARK_POOL_CN&currency=ETH"))}}})
result.minerInfo.l <- lapply( wallet, function(x) GET(paste0("https://www.sparkpool.com/v1/miner/stats?miner=", tolower(substring(x, 3)), "&pool=SPARK_POOL_CN&currency=ETH")))
result.bill.l <- lapply( wallet, function(x){ if(which(x == wallet) %in% c(1,2,10)){GET(paste0("https://gpumine.org/api/bill?currency=ETH&address=", x))}else{if(which(x == wallet) == 9){GET(paste0("https://api.ethermine.org/miner/", tolower(substring(x, 3)), "/dashboard/payouts"))}else{GET(paste0("https://www.sparkpool.com/v1/bill/list?miner=", tolower(substring(x, 3)), "&pool=SPARK_POOL_CN&currency=ETH&pageSize=35"))}}})
result.billInfo.l <- lapply( wallet, function(x) GET(paste0("https://www.sparkpool.com/v1/bill/stats?miner=", tolower(substring(x, 3)), "&pool=SPARK_POOL_CN&currency=ETH")))

print(paste0("result.miner.l ",Mine.name, ":",unlist(lapply(result.miner.l, status_code))))
print(paste0("result.minerInfo.l ",Mine.name, ":",unlist(lapply(result.minerInfo.l, status_code))))
print(paste0("result.bill.l ",Mine.name, ":",unlist(lapply(result.bill.l, status_code))))
print(paste0("result.billInfo.l ",Mine.name, ":",unlist(lapply(result.billInfo.l, status_code))))

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}
#Get ethrate & wallet & mail.txt setup------------------------------------------------
#ethurl <- "https://www.maicoin.com/api/prices/eth-usd"
#myHttpheader <- c(
#  'Accept'='*/*',
#  'Referer'=' https://www.maicoin.com/zh-TW/charts/eth',
#  'User-Agent'='Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/74.0.3729.169 Safari/537.36',
#  'X-CSRF-Token'='GJ1CbgR2kz10TFRWjS+7KJ4Z0ljJ1IRTYCb+NsZKI+LWXaie8hMb2vMv+TRlyhkUWi15tTgv3zlKq4b5NK0jwQ==',
#  'X-Requested-With'='XMLHttpRequest ')


#ethrate <- as.numeric(substring(fromJSON(content(GET(ethurl
#                                                     , add_headers(.headers = myHttpheader)), "text"))$formatted_sell_price, 2))
sparkpool.income.day <- fromJSON(content(GET("https://www.sparkpool.com/v1/pool/stats?pool=SPARK_POOL_CN"), "text"))$data
sparkpool.income.day$time <- Sys.time()
write.csv(sparkpool.income.day, paste0("/data/kent/sparkpoolincomeday/", format(sparkpool.income.day$time[1],"%Y-%m-%d_%H"), ".csv"))
ethurl <- "https://api.coingecko.com/api/v3/coins/markets?vs_currency=usd&ids=ethereum&order=market_cap_desc&per_page=100&page=1&sparkline=false"
ethpage <- httr::content(GET(ethurl), "text")
ethrate <-fromJSON(ethpage)$current_price
if(length(ethrate)==0 || is.na(ethrate)){
  ethurl <- "https://crypto.cnyes.com/ETH/24h"
  ethpage <- read_html(GET(ethurl))
  big_num <- as.numeric(html_text(html_nodes(ethpage, xpath = "//span[@class='jsx-143270965 big-num']")))
  small_num <- as.numeric(html_text(html_nodes(ethpage, xpath = "//span[@class='jsx-143270965 small-num']")))
  ethrate <- sum(c(big_num,small_num), na.rm = T)
}

summary.data <- NULL

#Create Content----------------------------------------------------------

for(wallet.i in seq_len(length(wallet))){
  print(wallet.i)
  result1 <- result.miner.l[[wallet.i]]
  result2 <- result.minerInfo.l[[wallet.i]]
  result3 <- result.bill.l[[wallet.i]]
  result4 <- result.billInfo.l[[wallet.i]]
  switch( type.l[wallet.i],
          #For TW Data Center----------------------------------------------------------
          TW = {
            out1.data <- fromJSON(content(result1, "text"))
            out1.data$workers$data <- out1.data$data$workers$list
            workers <- out1.data$data$workers$list
            out1.data$hashrate <- list(data = list(hashrate = out1.data$data$hashrate, meanHashrate24H = out1.data$data$hashrate24h ))
            out1.data$workers$data$meanHashrate24h <- out1.data$workers$data$hashrate24h
            #write.csv(workers, "/home/kent/rig.csv")
            out1.data$nworkers <- sum(workers$hashrate > 0)
            #if(wallet.i==6){
            saveRDS(out1.data, paste0("/data/kent/sparkpool/", gsub("-| |:", "", as.Date(Sys.time())), "all_", Mine.name[wallet.i], ".RDS" ), version =  2) 
            #RCurl::ftpUpload(paste0("D:/Job/VGA_Mining/sparkpool/", gsub("-| |:", "", as.Date(Sys.time())), "all_", Mine.name[wallet.i], ".RDS" ), paste0("sftp://172.16.98.53:22/data/kent/sparkpool/", gsub("-| |:", "", as.Date(Sys.time())), "all_", Mine.name[wallet.i], ".RDS" )
            #                 ,userpwd = "kent:a0088202")
            #}
            file.all <- tail( list.files("/data/kent/sparkpool", pattern = paste0("all_", Mine.name[wallet.i]), full.names = T), 30)
            n.file <- ifelse(length(file.all) > 30 , 30, length(file.all))
            rig.status <- rbindlist(lapply(as.list(1:n.file), function(x){
              online.rig.temp <- readRDS(file.all[x])$workers$data$rig[readRDS(file.all[x])$workers$data$hashrate > 0]
              if(length(online.rig.temp) != 0){
                
                
                data.frame(online.rig.temp ,
                           format(seq(as.Date(Sys.time())- n.file + 1, as.Date(Sys.time()),by="1 day"),"%m/%d")[x])
              }else{
                data.frame("XXX",
                           format(seq(as.Date(Sys.time())- n.file + 1, as.Date(Sys.time()),by="1 day"),"%m/%d")[x])}}))
            
            
            workers.all.name <- readColumns(getSheets(wb1)[[Mine.name[wallet.i]]], startColumn= 1, endColumn= 1, startRow= 1)$Rig
            rig.status.table <- as.data.frame.matrix(table(factor(unlist(rig.status[,1]), workers.all.name), unlist(rig.status[,2])))
            offlinedays <- rowSums(!rig.status.table)
            rig.status.table$status <- sapply(rig.status.table[, ncol(rig.status.table)], function(x)ifelse(x, "Mining", "Offline"))
            rig.status.table$offlinedays <- offlinedays
            
            #writeWorksheet( wb, data = rig.status.table, sheet = paste0("Rig Status_", Mine.name[wallet.i]), header = T , rownames = "Rig")
            addDataFrame(cbind(Rig = row.names(rig.status.table), rig.status.table), sheet = getSheets(wb)[[paste0("Rig Status_", Mine.name[wallet.i])]], row.names = F)
            
            
            #workers.all.n <- length(workers.all.name)
            workers.all.n <- workers.all.n.l[wallet.i]
            online.rig.n <- out1.data$nworkers
            online.perc <- online.rig.n / workers.all.n
            
            
            #TA1 TA2 code like TA1-104-12-0256/PANDA0016
            offline.rig.name <- paste(sort(str_extract(sort(workers.all.name[!workers.all.name %in% workers$rig[workers$hashrate > 0]]), "[[:digit:]]{4}$|^[[:digit:]]{4}")), collapse = "/")
            
            #Payments page-----------------------------------------------------------
            
            out3.data <- fromJSON(content(result3, "text"))
            
            payments <- out3.data$data$payments
            payments$time <- format(as.POSIXct(as.numeric(payments$time), origin = "1970-01-01"), "%m/%d")
            payments$paid <- as.numeric(payments$paid)
            payments <- payments[payments$paid > 0 & payments$time %in% format(seq(Sys.Date() - 30,Sys.Date() - 1,"day"), "%m/%d"), ]
            # For 1/7 pay two time----------------------------------------
            payments.temp.date <- payments$time[duplicated(payments$time)]
            payments.temp <- data.table(payments)[, sum(paid), by = time]
            payments <- payments[!duplicated(payments$time),]
            payments[payments$time %in% payments.temp.date, "paid"] <- payments.temp[time %in% payments.temp.date, V1]
            # For complment data(some day without mining no payments)
            payments2 <- data.table(payments)[, .(time = format(seq(Sys.Date()-30,Sys.Date()-1,"day"), "%m/%d"))]
            payments <- merge(payments2, payments, by ='time', all = TRUE)
            payments <- payments[ unlist(sapply(format(seq(Sys.Date() - 1, Sys.Date() - 30, "-1 day"), "%m/%d"), function(x){which(payments$time==x)})), ]
            #-----------------------------------------------------------
            total.payments <- data.frame(n1 = c("ETH(9:00) exchange rate", "TotalPaid", "US dollars"),
                                         n2 = c(ethrate,  out3.data$data$totalPaid, ethrate *  out3.data$data$totalPaid))
            addDataFrame( total.payments, sheet =  getSheets(wb)[[paste0("Payments_", Mine.name[wallet.i])]], col.names=F, row.names=F,
                          startRow= 1, startColumn = 1)
            addDataFrame( subset(payments, select= c("time", "paid", "tx_hash", "status"))[rev(seq_len(nrow(head(payments, 30)))),], sheet =  getSheets(wb)[[paste0("Payments_", Mine.name[wallet.i])]], col.names=F, row.names=F,
                          startRow = 6 + min(which( format(seq(Sys.Date()-30,Sys.Date()- 1,"day"), "%m/%d") %in% payments$time)) - 1, startColumn = 1)
            
            #writeWorksheet(wb, data = total.payments, header = F, sheet = paste0("Payments_", Mine.name[wallet.i]))
            #writeWorksheet( wb, data =  (subset(payments, select= c("time", "paid", "tx_hash", "status2"))[rev(seq_len(nrow(payments))),]), sheet = paste0("Payments_", Mine.name[wallet.i]), header = F,
            #                startCol = 1, startRow = 6 + (30 - nrow(payments)))
            #setForceFormulaRecalculation(wb, paste0("Payments_", Mine.name[wallet.i]), T)
            
            #total_rig-----------------------------------------------------------
            total.workers <- data.frame(n1 = c("Current_Hashrate", "Avg._Hashrate"),
                                        n2 = c(as.numeric(out1.data$hashrate$data$hashrate) / 1000000, as.numeric(out1.data$hashrate$data$meanHashrate24H) / 1000000))
            addDataFrame( total.workers, sheet =  getSheets(wb)[[paste0("total_rig_", Mine.name[wallet.i])]], col.names=F, row.names=F,
                          startRow= 1, startColumn = 1)
            
            #writeWorksheet(wb, data = total.workers, header = F, sheet = paste0("total_rig_", Mine.name[wallet.i]))
            
            total_rig <- data.frame(time  = format(as.Date(str_extract(file.all,"\\d{8}(?=all)"), "%Y%m%d"), "%m/%d"),
                                    Avg.Hashrate = sapply(file.all, function(x){as.numeric(readRDS(x)$hashrate$data$meanHashrate24H) / 1000000}),
                                    Online_Rig = sapply(file.all, function(x){readRDS(x)$nworkers }),
                                    Total.Rig = workers.all.n, stringsAsFactors = F)
            total_rig$Avg.Avg.Hashrate_Online_Rig <- sapply(file.all, function(x){mean(as.numeric(readRDS(x)$workers$data$meanHashrate24h[readRDS(x)$workers$data$hashrate > 0])/ 1000000)})            
            total_rig$Avg.Avg.Hashrate_Online_Rig[is.na(total_rig$Avg.Avg.Hashrate_Online_Rig)] <- 0
            total_rig$Avg.Avg.Hashrate_Total.Rig <- with(total_rig, Avg.Hashrate / Total.Rig)            
            
            addDataFrame( total_rig, sheet =  getSheets(wb)[[paste0("total_rig_", Mine.name[wallet.i])]], col.names=F, row.names=F,
                          startRow= 5 + (30 - nrow(total_rig)), startColumn = 1)
            #writeWorksheet(wb, data = total_rig , header = F , sheet = paste0("total_rig_", Mine.name[wallet.i]), startCol = 1, startRow = 5 + (30 - nrow(total_rig)))
            
            Average.hash.rate <- last(total_rig$Avg.Avg.Hashrate_Online_Rig, 1)
            switch(as.character(wallet.i),
                   "1" = {
                     Average.hash.rate.table <- as.data.table(readRDS(last(file.all, 1))$workers$data)[hashrate > 0, round(mean(as.numeric(meanHashrate24h))/ 1000000, 0), by = (rig %like% "104-12") * 1 + (rig %like% "106-12") *2]
                     Average.hash.rate <- paste0(Average.hash.rate.table[order(rig), V1], "MH/s" ,collapse = ";")
                   },
                   "2" = {
                     Average.hash.rate.table <- as.data.table(readRDS(last(file.all, 1))$workers$data)[hashrate > 0, round(mean(as.numeric(meanHashrate24h))/ 1000000, 0), by = (rig %like% "104-12") * 1 + (rig %like% "VEGA-8") *2]
                     Average.hash.rate <- paste0(Average.hash.rate.table[order(rig), V1], "MH/s" ,collapse = ";")
                   },
                   "10" = {
                     Average.hash.rate.table <- as.data.table(readRDS(last(file.all, 1))$workers$data)[hashrate > 0, round(mean(as.numeric(meanHashrate24h))/ 1000000, 0), by = (rig %like% "104-12") * 1 + (rig %like% "106-12") *2 + (rig %like% "106-8") * 3]
                     Average.hash.rate <- paste0(Average.hash.rate.table[order(rig), V1], "MH/s" ,collapse = ";")
                   }
            )
            Average.per.rig.payment <-   head(payments$paid, 1) /   online.rig.n 
            Average.per.hash.payment <- head(payments$paid, 1)  / as.numeric(out1.data$hashrate$data$meanHashrate24H) *1000000
            
            #setForceFormulaRecalculation(wb, paste0("total_rig_", Mine.name[wallet.i]), T)
            
            #Summary-----------------------------------------------------------
            ethwalleturl <- paste0("https://etherscan.io/address/", wallet[wallet.i])
            ethwalletpage <- read_html(GET(ethwalleturl))
            ethwallet <- html_text(html_nodes(ethwalletpage , xpath = "//div[@class='card-body']//div[@class='col-md-8']"))[1]
            ethtransactions <- html_table(ethwalletpage, fill = T)[[1]]
            names(ethtransactions) <- c("Q","Txn Hash", "Method", "Block",  "Date", "age", "From", "status", "To", "Value", "[Txn Fee]")
            paymentslast <- ifelse(any(ethtransactions$status == "IN" &  grepl(Sys.Date() - 1, ethtransactions$Date)), as.numeric(strsplit(ethtransactions[ethtransactions$status == "IN", ][1, "Value"], " ")[[1]][1]), NA)
            summary.data <- rbind(summary.data , c(workers.all.n, online.rig.n, workers.all.n - online.rig.n, percent(online.perc),  paymentslast, total.payments[2,2], Average.hash.rate, Average.per.rig.payment, Average.per.hash.payment, ethrate, offline.rig.name, ethwallet))
            
          },
          #For CN Data Center----------------------------------------------------------
          CN = { 
            out1.data <- fromJSON(content(result1, "text"))
            #For new web to fit old web
            out1.data$workers$data <- out1.data$data 
            out1.data$workers$data$rig <- out1.data$workers$data$worker
            workers <- out1.data$workers$data
            #   if(wallet.i == 6){    
            #     wondermoleHttpheader <- c(
            #       ':authority'='a.wondermole.com',
            #       ':method'='GET',
            #       ':path'='/v1/web/miner?mid=257&status=-1&page=1&limit=100',
            #       ':scheme'='https',
            #       'accept'='*/*',
            #       'accept-encoding'='gzip, deflate, br',
            #       'accept-language'='en-US,en;q=0.9,zh-TW;q=0.8,zh-CN;q=0.7,zh;q=0.6',
            #       'access-control-allow-methods'=' GET,POST,PATCH,PUT,OPTIONS',
            #       'access-control-allow-origin'='*',
            #       'client-type'='web',
            #       'cookie'='Hm_lvt_cdafb785e0ed9642aac8a4bedb70fa78=1563940225; Hm_lpvt_cdafb785e0ed9642aac8a4bedb70fa78=1563940225; access_token=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1aWQiOjEwNjksIm1hYyI6IiIsImV4cCI6MTU2Mzk5ODYxOCwianRpIjoiMTA2OSIsIm5iZiI6MTU2Mzk1NTQxOH0.h9RP-p_W6dg7oV36-v_ERqDXxtjISVtOLB7B8BtsVNM',
            #       'lang'=' zh',
            #       'referer'=' https://a.wondermole.com/',
            #       'user-agent'=' Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/75.0.3770.142 Safari/537.36'
            #     )
            #         
            #     workers.all.n <- fromJSON(content(GET("https://a.wondermole.com/v1/web/monitorinfo?mid=257",add_headers(.headers = wondermoleHttpheader)), "text"))$result$info$miner_num
            #     if(!is.null(workers.all.n)){
            #     workers <- rbindlist(lapply(as.list(seq_len(ceiling(workers.all.n/1000))), function(x){
            #       (fromJSON(content(
            #         GET(paste0("https://a.wondermole.com/v1/web/miner?mid=257&status=-1&page=", x,"&limit=1000")
            #             ,add_headers(.headers = wondermoleHttpheader)), "text"))$result$list)
            #       
            #     }))
            #     workers$rig <- workers$name
            #     workers$hashrate <- workers$hash * 1000000
            #     out1.data$workers$data <- workers  
            #     }
            #   }
            
            out1.data$hashrate$data <- fromJSON(content(result2, "text"))$data
            out1.data$hashrate$data$meanHashrate24H <- out1.data$hashrate$data$meanHashrate24h
            #write.csv(workers, "/home/kent/rig.csv")
            out1.data$nworkers <- sum(workers$hashrate > 0)
            #if(wallet.i==6){
            saveRDS(out1.data, paste0("/data/kent/sparkpool/", gsub("-| |:", "", as.Date(Sys.time())), "all_", Mine.name[wallet.i], ".RDS" ), version =  2) 
            #RCurl::ftpUpload(paste0("/data/kent/sparkpool/", gsub("-| |:", "", as.Date(Sys.time())), "all_", Mine.name[wallet.i], ".RDS" ), paste0("sftp://172.16.98.53:22/data/kent/sparkpool/", gsub("-| |:", "", as.Date(Sys.time())), "all_", Mine.name[wallet.i], ".RDS" )
            #                 ,userpwd = "kent:a0088202")
            #}
            file.all <- tail( list.files("/data/kent/sparkpool", pattern = paste0("all_", Mine.name[wallet.i]), full.names = T), 30)
            n.file <- ifelse(length(file.all) > 30 , 30, length(file.all))
            rig.status <- rbindlist(lapply(as.list(1:n.file), function(x){
              online.rig.temp <- readRDS(file.all[x])$workers$data$rig[readRDS(file.all[x])$workers$data$hashrate > 0]
              if(length(online.rig.temp) != 0){
                
                
                data.frame(online.rig.temp ,
                           format(seq(as.Date(Sys.time())- n.file + 1, as.Date(Sys.time()),by="1 day"),"%m/%d")[x])
              }else{
                data.frame("XXX",
                           format(seq(as.Date(Sys.time())- n.file + 1, as.Date(Sys.time()),by="1 day"),"%m/%d")[x])}}))
            
            
            workers.all.name <- readColumns(getSheets(wb1)[[Mine.name[wallet.i]]], startColumn= 1, endColumn= 1, startRow= 1)$Rig
            rig.status.table <- as.data.frame.matrix(table(factor(unlist(rig.status[,1]), workers.all.name), unlist(rig.status[,2])))
            offlinedays <- rowSums(!rig.status.table)
            rig.status.table$status <- sapply(rig.status.table[, ncol(rig.status.table)], function(x)ifelse(x, "Mining", "Offline"))
            rig.status.table$offlinedays <- offlinedays
            if(wallet.i <= 2 & ncol(rig.status.table) < 32){
              gpumine.temp <- read.xlsx(paste0("/data/kent/gpumine/", gsub("-| |:","",as.Date(Sys.time())), "_.xlsx"), 
                                        sheetName= paste0("Rig Status_", Mine.name[wallet.i]))
              names(gpumine.temp)[names(gpumine.temp) %like% "^X"] <- format(as.Date( names(gpumine.temp)[names(gpumine.temp)%like%"^X"], "X%m.%d"), "%m/%d")
              #write.xlsx(gpumine.temp, filenamepath, sheetName = paste0("Rig Status_", Mine.name[wallet.i]))
              addDataFrame(gpumine.temp, sheet = getSheets(wb)[[paste0("Rig Status_", Mine.name[wallet.i])]], row.names = F)
              
            }else{
              #writeWorksheet( wb, data = rig.status.table, sheet = paste0("Rig Status_", Mine.name[wallet.i]), header = T , rownames = "Rig")
              addDataFrame(cbind(Rig = row.names(rig.status.table), rig.status.table), sheet = getSheets(wb)[[paste0("Rig Status_", Mine.name[wallet.i])]], row.names = F)
            }
            
            #workers.all.n <- length(workers.all.name)
            workers.all.n <- workers.all.n.l[wallet.i]
            online.rig.n <- out1.data$nworkers
            online.perc <- online.rig.n / workers.all.n
            
            
            if(wallet.i == 6){
              offline.rig.name <- paste(sort(workers.all.name[!workers.all.name %in% workers$rig[workers$hashrate > 0]]), collapse = "/")
              
            }else{
              #CA2 code like CA2-406-05
              offline.rig.name <- paste(sort(str_extract(sort(workers.all.name[!workers.all.name %in% workers$rig[workers$hashrate > 0]]), "[[:digit:]]{2,}-[[:digit:]]{2,}$")), collapse = "/")
            }
            
            #Payments page-----------------------------------------------------------
            
            out3.data <- fromJSON(content(result3, "text"))
            
            payments <- out3.data$data
            payments$time <- format(as.POSIXct(payments$time, origin = "1970-01-01"), "%m/%d")
            payments$paid <- as.numeric(payments$amount)
            payments <- payments[payments$time %in% format(seq(Sys.Date()-30,Sys.Date()-1,"day"), "%m/%d"),]
            # For 1/7 pay two time----------------------------------------
            payments.temp.date <- payments$time[duplicated(payments$time)]
            payments.temp <- data.table(payments)[, sum(paid), by = time]
            payments <- payments[!duplicated(payments$time),]
            payments[payments$time %in% payments.temp.date, "paid"] <- payments.temp[time %in% payments.temp.date, V1]
            # For complment data(some day without mining no payments)
            payments2 <- data.table(payments)[, .(time = format(seq(Sys.Date()-30,Sys.Date()-1,"day"), "%m/%d"))]
            payments <- merge(payments2, payments, by = 'time', all = TRUE)
            payments$paid[is.na(payments$paid)] <- 0
            payments <- payments[ unlist(sapply(format(seq(Sys.Date()-1,Sys.Date()-30,"-1 day"), "%m/%d"), function(x){which(payments$time==x)})), ]
            #-----------------------------------------------------------
            total.payments <- data.frame(n1 = c("ETH(9:00) exchange rate", "TotalPaid", "US dollars"),
                                         n2 = c(ethrate, fromJSON(content(result4, "text"))$data$totalPaid, ethrate * fromJSON(content(result4, "text"))$data$totalPaid))
            
            addDataFrame( total.payments, sheet =  getSheets(wb)[[paste0("Payments_", Mine.name[wallet.i])]], col.names=F, row.names=F,
                          startRow= 1, startColumn = 1)
            addDataFrame( subset(payments, select= c("time", "paid", "transactionId", "status"))[rev(seq_len(nrow(head(payments, 30)))),], sheet =  getSheets(wb)[[paste0("Payments_", Mine.name[wallet.i])]], col.names=F, row.names=F,
                          startRow = 6 + min(which( format(seq(Sys.Date()-30,Sys.Date()- 1,"day"), "%m/%d") %in% payments$time)) - 1, startColumn = 1)
            
            #writeWorksheet(wb, data = total.payments, header = F, sheet = paste0("Payments_", Mine.name[wallet.i]))
            #writeWorksheet( wb, data =  (subset(payments, select= c("time", "paid", "tx_hash", "status2"))[rev(seq_len(nrow(payments))),]), sheet = paste0("Payments_", Mine.name[wallet.i]), header = F,
            #                startCol = 1, startRow = 6 + (30 - nrow(payments)))
            #setForceFormulaRecalculation(wb, paste0("Payments_", Mine.name[wallet.i]), T)
            
            #total_rig-----------------------------------------------------------
            total.workers <- data.frame(n1 = c("Current_Hashrate", "Avg._Hashrate"),
                                        n2 = c(as.numeric(out1.data$hashrate$data$hashrate) / 1000000, as.numeric(out1.data$hashrate$data$meanHashrate24H) / 1000000))
            addDataFrame( total.workers, sheet =  getSheets(wb)[[paste0("total_rig_", Mine.name[wallet.i])]], col.names=F, row.names=F,
                          startRow= 1, startColumn = 1)
            
            #writeWorksheet(wb, data = total.workers, header = F, sheet = paste0("total_rig_", Mine.name[wallet.i]))
            
            total_rig <- data.frame(time  = format(as.Date(str_extract(file.all,"\\d{8}(?=all)"), "%Y%m%d"), "%m/%d"),
                                    Avg.Hashrate = sapply(file.all, function(x){as.numeric(readRDS(x)$hashrate$data$meanHashrate24H) / 1000000}),
                                    Online_Rig = sapply(file.all, function(x){readRDS(x)$nworkers }),
                                    Total.Rig = workers.all.n, stringsAsFactors = F)
            total_rig$Avg.Avg.Hashrate_Online_Rig <- sapply(file.all, function(x){mean(as.numeric(readRDS(x)$workers$data$meanHashrate24h[readRDS(x)$workers$data$hashrate > 0])/ 1000000)})            
            total_rig$Avg.Avg.Hashrate_Online_Rig[is.na(total_rig$Avg.Avg.Hashrate_Online_Rig)] <- 0
            total_rig$Avg.Avg.Hashrate_Total.Rig <- with(total_rig, Avg.Hashrate / Total.Rig)            
            
            addDataFrame( total_rig, sheet =  getSheets(wb)[[paste0("total_rig_", Mine.name[wallet.i])]], col.names=F, row.names=F,
                          startRow= 5 + (30 - nrow(total_rig)), startColumn = 1)
            #writeWorksheet(wb, data = total_rig , header = F , sheet = paste0("total_rig_", Mine.name[wallet.i]), startCol = 1, startRow = 5 + (30 - nrow(total_rig)))
            if(wallet.i <= 2 & nrow(total_rig) < 30){
              gpumine.temp <- read.xlsx(paste0("/data/kent/gpumine/", gsub("-| |:","",as.Date(Sys.time())), "_.xlsx"), 
                                        sheetName= paste0("total_rig_", Mine.name[wallet.i]), startRow = 5, header=F)
              addDataFrame( head(gpumine.temp, 30 - nrow(total_rig)), sheet =  getSheets(wb)[[paste0("total_rig_", Mine.name[wallet.i])]], col.names=F, row.names=F,
                            startRow= 5)
              
              #gpumine.temp <- readWorksheet(loadWorkbook(paste0("/data/kent/gpumine/", gsub("-| |:","",as.Date(Sys.time())), "_.xlsx")), 
              #                              header = F , sheet = paste0("total_rig_", Mine.name[wallet.i]), startCol = 1, startRow = 5 )
              #writeWorksheet(wb, data = head(gpumine.temp, 30 - nrow(total_rig))  , header = F , sheet = paste0("total_rig_", Mine.name[wallet.i]), startCol = 1, startRow = 5 )
            }
            
            Average.hash.rate <- last(total_rig$Avg.Avg.Hashrate_Online_Rig, 1)
            switch(as.character(wallet.i),
                   "12" = {
                     Average.hash.rate.table <- as.data.table(readRDS(last(file.all, 1))$workers$data)[hashrate > 0, round(mean(as.numeric(meanHashrate24h))/ 1000000, 0), by = (rig %like% "SIX") * 1]
                     Average.hash.rate <- paste0(Average.hash.rate.table[order(rig), V1], "MH/s" ,collapse = ";")
                   })
            Average.per.rig.payment <-   head(payments$paid, 1) /   online.rig.n 
            Average.per.hash.payment <- head(payments$paid, 1)  / as.numeric(out1.data$hashrate$data$meanHashrate24H) *1000000
            
            #setForceFormulaRecalculation(wb, paste0("total_rig_", Mine.name[wallet.i]), T)
            
            #Summary-----------------------------------------------------------
            ethwalleturl <- paste0("https://etherscan.io/address/", wallet[wallet.i])
            ethwalletpage <- read_html(GET(ethwalleturl))
            ethwallet <- html_text(html_nodes(ethwalletpage , xpath = "//div[@class='card-body']//div[@class='col-md-8']"))[1]
            ethtransactions <- html_table(ethwalletpage, fill = T)[[1]]
            names(ethtransactions) <- c("Q","Txn Hash", "Method", "Block",	"Date", "age", "From", "status", "To", "Value", "[Txn Fee]")
            paymentslast <- ifelse(any(ethtransactions$status == "IN" &  grepl(Sys.Date() - 1, ethtransactions$Date)), as.numeric(strsplit(ethtransactions[ethtransactions$status == "IN", ][1, "Value"], " ")[[1]][1]), NA)
            if(wallet.i == 3){
            paymentslast <- ifelse(any(ethtransactions$status == "IN" &  grepl(Sys.Date() - 1, ethtransactions$Date)),  sum(as.numeric(gsub(" Ether", "", ethtransactions[ethtransactions$status == "IN"&  grepl(Sys.Date() - 1, ethtransactions$Date), ][, "Value"]))), NA)}
            summary.data <- rbind(summary.data , c(workers.all.n, online.rig.n, workers.all.n - online.rig.n, percent(online.perc),  paymentslast, total.payments[2,2], Average.hash.rate, Average.per.rig.payment, Average.per.hash.payment, ethrate, offline.rig.name, ethwallet))
          },
          #For EU Data Center----------------------------------------------------------
          EU = { 
          out1.data <- fromJSON(content(result1, "text"))
          out1.data$workers$data <- out1.data$data$workers
          workers <- out1.data$data$workers
          
          #data.frame(time=(as.POSIXct(out1.data$data$statistics$time, origin = "1970-01-01")),avg=rev(sapply(1:144,function(x)mean(tail(out1.data$data$statistics$currentHashrate,x)))),144:1)
          #mean(tail(out1.data$data$statistics$currentHashrate, 37)) 6hr
          out1.data$hashrate <- list(data = list(hashrate = out1.data$data$currentStatistics$currentHashrate, meanHashrate24H = mean(out1.data$data$statistics$currentHashrate)))
          
          out1.data$workers$data$meanHashrate24h <- mean(out1.data$workers$data$currentHashrate)
          #write.csv(workers, "C:/Users/kent_tung/Desktoprig.csv")
          out1.data$nworkers <- sum(workers$currentHashrate > 0)
          #if(wallet.i==6){
          saveRDS(out1.data, paste0("/data/kent/sparkpool/", gsub("-| |:", "", as.Date(Sys.time())), "all_", Mine.name[wallet.i], ".RDS" ), version =  2) 
          #RCurl::ftpUpload(paste0("/data/kent/sparkpool/", gsub("-| |:", "", as.Date(Sys.time())), "all_", Mine.name[wallet.i], ".RDS" ), paste0("sftp://172.16.98.53:22/data/kent/sparkpool/", gsub("-| |:", "", as.Date(Sys.time())), "all_", Mine.name[wallet.i], ".RDS" )
          #                 ,userpwd = "kent:a0088202")
          #}
          file.all <- tail( list.files("/data/kent/sparkpool", pattern = paste0("all_", Mine.name[wallet.i]), full.names = T), 30)
          n.file <- ifelse(length(file.all) > 30 , 30, length(file.all))
          rig.status <- rbindlist(lapply(as.list(1:n.file), function(x){
            online.rig.temp <- readRDS(file.all[x])$workers$data$worker[readRDS(file.all[x])$workers$data$currentHashrate > 0]
            if(length(online.rig.temp) != 0){
              
              
              data.frame(online.rig.temp ,
                         format(seq(as.Date(Sys.time())- n.file + 1, as.Date(Sys.time()),by="1 day"),"%m/%d")[x])
            }else{
              data.frame("XXX",
                         format(seq(as.Date(Sys.time())- n.file + 1, as.Date(Sys.time()),by="1 day"),"%m/%d")[x])}}))
          
          
          workers.all.name <- readColumns(getSheets(wb1)[[Mine.name[wallet.i]]], startColumn= 1, endColumn= 1, startRow= 1)$Rig
          rig.status.table <- as.data.frame.matrix(table(factor(unlist(rig.status[,1]), workers.all.name), unlist(rig.status[,2])))
          offlinedays <- rowSums(!rig.status.table)
          rig.status.table$status <- sapply(rig.status.table[, ncol(rig.status.table)], function(x)ifelse(x, "Mining", "Offline"))
          rig.status.table$offlinedays <- offlinedays
          
          #writeWorksheet( wb, data = rig.status.table, sheet = paste0("Rig Status_", Mine.name[wallet.i]), header = T , rownames = "Rig")
          addDataFrame(cbind(Rig = row.names(rig.status.table), rig.status.table), sheet = getSheets(wb)[[paste0("Rig Status_", Mine.name[wallet.i])]], row.names = F)
          
          
          #workers.all.n <- length(workers.all.name)
          workers.all.n <- workers.all.n.l[wallet.i]
          online.rig.n <- out1.data$nworkers
          online.perc <- online.rig.n / workers.all.n
          
          
          #TA1 TA2 code like TA1-104-12-0256/PANDA0016
          offline.rig.name <- paste(sort(str_extract(sort(workers.all.name[!workers.all.name %in% workers$worker[workers$currentHashrate > 0]]), "[[:digit:]]{4}$")), collapse = "/")
          
          #Payments page-----------------------------------------------------------
          
          out3.data <- fromJSON(content(result3, "text"))
          
          payments <- out3.data$data$payouts
          payments$time <- format(as.POSIXct(payments$paidOn, origin = "1970-01-01"), "%m/%d")
          payments$paid <- as.numeric(payments$amount) / 1000000000000000000
          out3.data$totalPaid <- sum(payments$paid )
          payments <- payments[payments$paid > 1 & payments$time %in% format(seq(Sys.Date() - 30,Sys.Date(),"day"), "%m/%d"), ]
          #payments$status <- "completed"
          # For 1/7 pay two time----------------------------------------
          payments.temp.date <- payments$time[duplicated(payments$time)]
          payments.temp <- data.table(payments)[, sum(paid), by = time]
          payments <- payments[!duplicated(payments$time),]
          payments[payments$time %in% payments.temp.date, "paid"] <- payments.temp[time %in% payments.temp.date, V1]
          # For complment data(some day without mining no payments)
          payments2 <- data.table(payments)[, .(time = format(seq(Sys.Date()-30,Sys.Date()-1,"day"), "%m/%d"))]
          payments <- merge(payments2, payments, by ='time', all = TRUE)
          payments <- payments[ unlist(sapply(format(seq(Sys.Date() , Sys.Date() - 30, "-1 day"), "%m/%d"), function(x){which(payments$time==x)})), ]
          #-----------------------------------------------------------
          total.payments <- data.frame(n1 = c("ETH(9:00) exchange rate", "TotalPaid", "US dollars"),
                                       n2 = c(ethrate,  out3.data$totalPaid, ethrate *  out3.data$totalPaid))
          addDataFrame( total.payments, sheet =  getSheets(wb)[[paste0("Payments_", Mine.name[wallet.i])]], col.names=F, row.names=F,
                        startRow= 1, startColumn = 1)
          #addDataFrame( subset(payments, select= c("time", "paid", "txHash", "status"))[rev(seq_len(nrow(payments))),], sheet =  getSheets(wb)[[paste0("Payments_", Mine.name[wallet.i])]], col.names=F, row.names=F,
          #              startRow = 6 + min(which( format(seq(Sys.Date() - 30,Sys.Date(),"day"), "%m/%d") %in% payments$time)) - 1, startColumn = 1)
          addDataFrame( subset(payments, select= c("time", "paid", "txHash"))[rev(seq_len(nrow(payments))),], sheet =  getSheets(wb)[[paste0("Payments_", Mine.name[wallet.i])]], col.names=F, row.names=F,
                        startRow = 6 + min(which( format(seq(Sys.Date() - 30,Sys.Date(),"day"), "%m/%d") %in% payments$time)) - 1, startColumn = 1)
          
          #writeWorksheet(wb, data = total.payments, header = F, sheet = paste0("Payments_", Mine.name[wallet.i]))
          #writeWorksheet( wb, data =  (subset(payments, select= c("time", "paid", "tx_hash", "status2"))[rev(seq_len(nrow(payments))),]), sheet = paste0("Payments_", Mine.name[wallet.i]), header = F,
          #                startCol = 1, startRow = 6 + (30 - nrow(payments)))
          #setForceFormulaRecalculation(wb, paste0("Payments_", Mine.name[wallet.i]), T)
          
          #total_rig-----------------------------------------------------------
          total.workers <- data.frame(n1 = c("Current_Hashrate", "Avg._Hashrate"),
                                      n2 = c(as.numeric(out1.data$hashrate$data$hashrate) / 1000000, as.numeric(out1.data$hashrate$data$meanHashrate24H) / 1000000))
          addDataFrame( total.workers, sheet =  getSheets(wb)[[paste0("total_rig_", Mine.name[wallet.i])]], col.names=F, row.names=F,
                        startRow= 1, startColumn = 1)
          
          #writeWorksheet(wb, data = total.workers, header = F, sheet = paste0("total_rig_", Mine.name[wallet.i]))
          
          total_rig <- data.frame(time  = format(as.Date(str_extract(file.all,"\\d{8}(?=all)"), "%Y%m%d"), "%m/%d"),
                                  Avg.Hashrate = sapply(file.all, function(x){as.numeric(readRDS(x)$hashrate$data$meanHashrate24H) / 1000000}),
                                  Online_Rig = sapply(file.all, function(x){readRDS(x)$nworkers }),
                                  Total.Rig = workers.all.n, stringsAsFactors = F)
          total_rig$Avg.Avg.Hashrate_Online_Rig <- sapply(file.all, function(x){mean(as.numeric(readRDS(x)$workers$data$currentHashrate[readRDS(x)$workers$data$currentHashrate > 0])/ 1000000)})            
          total_rig$Avg.Avg.Hashrate_Online_Rig[is.na(total_rig$Avg.Avg.Hashrate_Online_Rig)] <- 0
          total_rig$Avg.Avg.Hashrate_Total.Rig <- with(total_rig, Avg.Hashrate / Total.Rig)            
          
          addDataFrame( total_rig, sheet =  getSheets(wb)[[paste0("total_rig_", Mine.name[wallet.i])]], col.names=F, row.names=F,
                        startRow= 5 + (30 - nrow(total_rig)), startColumn = 1)
          #writeWorksheet(wb, data = total_rig , header = F , sheet = paste0("total_rig_", Mine.name[wallet.i]), startCol = 1, startRow = 5 + (30 - nrow(total_rig)))
          
          Average.hash.rate <- last(total_rig$Avg.Avg.Hashrate_Online_Rig, 1)
          Average.per.rig.payment <-   head(payments$paid, 1) /   online.rig.n 
          Average.per.hash.payment <- head(payments$paid, 1)  / as.numeric(out1.data$hashrate$data$meanHashrate24H) *1000000
          
          #setForceFormulaRecalculation(wb, paste0("total_rig_", Mine.name[wallet.i]), T)
          
          #Summary-----------------------------------------------------------
          ethwalleturl <- paste0("https://etherscan.io/address/", wallet[wallet.i])
          ethwalleturl <- paste0("https://etherscan.io/address/","0x21fe74a7185da3713e8bfaEDA276b5d98d226E06")
          ethwalletpage <- read_html(GET(ethwalleturl))
          ethwallet <- html_text(html_nodes(ethwalletpage , xpath = "//div[@class='card-body']//div[@class='col-md-8']"))[1]
          ethtransactions <- html_table(ethwalletpage, fill = T)[[1]]
          names(ethtransactions) <- c("Q","Txn Hash", "Block", "Method",	"Date", "age", "From", "status", "To", "Value", "[Txn Fee]")
          paymentslast <- ifelse(any(ethtransactions$status == "IN" &  grepl(Sys.Date() - 1, ethtransactions$Date)), as.numeric(strsplit(ethtransactions[ethtransactions$status == "IN", ][1, "Value"], " ")[[1]][1]), NA)
          summary.data <- rbind(summary.data , c(workers.all.n, online.rig.n, workers.all.n - online.rig.n, percent(online.perc),  paymentslast, total.payments[2,2], Average.hash.rate, Average.per.rig.payment, Average.per.hash.payment, ethrate, offline.rig.name, ethwallet))
          
          }
          
          )
  
 
}

saveRDS(as.data.frame.matrix(summary.data), paste0("/data/kent/sparkpool/", gsub("-| |:", "", as.Date(Sys.time())), "summary.RDS" ), version =  2) 
#RCurl::ftpUpload(paste0("/data/kent/sparkpool/", gsub("-| |:", "", as.Date(Sys.time())), "summary.RDS" ), paste0("sftp://172.16.98.53:22/data/kent/sparkpool/", gsub("-| |:", "", as.Date(Sys.time())), "summary.RDS" )
#                 ,userpwd = "kent:a0088202")

summary.data.today.last <- rbindlist(lapply(rev(tail(list.files("/data/kent/sparkpool/", pattern = "summary", full.names = T), 2)), readRDS), fill =T)
addDataFrame( t(summary.data.today.last), sheet =  getSheets(wb)[["Summary"]], col.names=T, row.names=F)

summary.data.today.last1540 <- rbindlist(lapply(rev(tail(list.files("/data/kent/sparkpool1540/", pattern = "summary", full.names = T), 2)), readRDS), fill =T)
addDataFrame( t(summary.data.today.last1540), sheet =  getSheets(wb)[["Summary"]], col.names=F, row.names=F, startRow = 14)

saveWorkbook(wb, filenamepath)
forceFormulaRefresh(filenamepath)

#for(wallet.i in seq_len(length(wallet))){
#RCurl::ftpUpload(paste0("/data/kent/sparkpool/", gsub("-| |:", "", as.Date(Sys.time())), "all_", Mine.name[wallet.i], ".RDS" ), paste0("sftp://172.16.98.53:22/data/kent/sparkpool/", gsub("-| |:", "", as.Date(Sys.time())), "all_", Mine.name[wallet.i], ".RDS" )
#                 ,userpwd = "kent:a0088202")
#}
#RCurl::ftpUpload(paste0("/data/kent/sparkpool/", gsub("-| |:", "", as.Date(Sys.time())), "summary.RDS" ), paste0("sftp://172.16.98.53:22/data/kent/sparkpool/", gsub("-| |:", "", as.Date(Sys.time())), "summary.RDS" )
#                 ,userpwd = "kent:a0088202")
#RCurl::ftpUpload(paste0("/data/kent/sparkpool/", filename), paste0("sftp://172.16.98.53:22/data/kent/sparkpool/", filename )
#                 , userpwd = "kent:a0088202")


#send mail here
summary.data.today.last <- summary.data.today.last[, lapply(.SD, as.character)]
summary.data.today.last[,V5:=  round(as.numeric(summary.data.today.last[,V5]), 2)]
summary.data.today.last[,V8:=  round(as.numeric(summary.data.today.last[,V8]), 7)]
summary.data.today.last[,V9:=  round(as.numeric(summary.data.today.last[,V9]), 7)]
summary.data.today.last[,V12:= str_extract(summary.data.today.last[,V12], ".+\\.\\d{2}")]
Average.hash.rate.tmp <- paste0(str_extract(summary.data.today.last[,V7],".+(?=\\.)"), "MH/s")
Average.hash.rate.tmp[Average.hash.rate.tmp%in%"NAMH/s"] <- summary.data.today.last[Average.hash.rate.tmp%in%"NAMH/s",V7]
summary.data.today.last[,V7:= Average.hash.rate.tmp]
#1540
summary.data.today.last1540 <- summary.data.today.last1540[, lapply(.SD, as.character)]
summary.data.today.last1540[,V5:=  round(as.numeric(summary.data.today.last1540[,V5]), 2)]
summary.data.today.last1540[,V8:=  round(as.numeric(summary.data.today.last1540[,V8]), 7)]
summary.data.today.last1540[,V9:=  round(as.numeric(summary.data.today.last1540[,V9]), 7)]
summary.data.today.last1540[,V12:= str_extract(summary.data.today.last1540[,V12], ".+\\.\\d{2}")]
Average.hash.rate.tmp <- paste0(str_extract(summary.data.today.last1540[,V7],".+(?=\\.)"), "MH/s")
Average.hash.rate.tmp[Average.hash.rate.tmp%in%"NAMH/s"] <- summary.data.today.last1540[Average.hash.rate.tmp%in%"NAMH/s",V7]
summary.data.today.last1540[,V7:= Average.hash.rate.tmp]
#sparkpool.income.day <- fromJSON(content(GET("https://www.sparkpool.com/v1/pool/stats?pool=SPARK_POOL_CN"), "text"))$data
mail.data <- data.frame(To = "Kent_Tung@asus.com",	
                        CC = "Sunny2_Wu@asus.com;Sam_Cheng@asus.com",	
                        Subject	= paste0("Data Center Report ", gsub("-| |:","",Sys.Date())),
                        Attachments	=  paste0("/data/kent/sparkpool/", gsub("-| |:","", Sys.Date()), "_Data_center_daily_report.xlsx"),
#                        Body = paste0("Update Daily Report as Attach: sparkpool income/Day:", sparkpool.income.day[sparkpool.income.day$currency=="ETH", "income"]),
                        stringsAsFactors = F)
#system(paste0('echo "', mail.data$Body, '" |mutt "', mail.data$To, '" -s "', mail.data$Subject, '" -a "', mail.data$Attachments, '" -c "', mail.data$CC, '"'))
#system(paste0('echo "', mail.data$Body, '" |mutt "', mail.data$To, '" -s "', mail.data$Subject, '" -a "', mail.data$Attachments, '"'))
rawmail <- readLines("/home/kent/email2.txt", encoding = "utf-8")
mail.body <- eval(parse(text = rawmail))
writeLines(mail.body, con = "/home/kent/emailout.html", useBytes=T)
system(paste0('mutt -e "set content_type="text/html"" "', mail.data$To, '" -s "', mail.data$Subject, '" -a "', mail.data$Attachments, '" -c "', mail.data$CC, '"< /home/kent/emailout.html'))


#result3 <- GET("https://api.ethermine.org/miner/21fe74a7185da3713e8bfaEDA276b5d98d226E06/dashboard")
#result3 <- GET("https://api.ethermine.org/miner/21fe74a7185da3713e8bfaEDA276b5d98d226E06/dashboard/payouts")
#out3.data <- fromJSON(content(result3, "text"))
#out3.data$data$workers 
#out3.data$data$payouts
