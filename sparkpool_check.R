options( java.parameters = "-Xmx6g" )
for(i in 1:10){
  if(14 == length(list.files("/data/kent/sparkpool/", pattern = gsub("-| |:","", Sys.Date())))){
    print("----------Check ok----------")
    break
  }else{
    print(paste0("----------Start ", i, " time----------"))
    try(source("/home/kent/sparkpoolsever.R"))
    
  } 
}
