#=====================================
# Libraries
#=====================================
install.packages('RSelenium')
install.packages('dplyr')
install.packages('rvest')
library(RSelenium)
library(dplyr)

#=====================================
# Web scrapping - Connecting to website
#=====================================
url = 'https://www.videocardbenchmark.net/high_end_gpus.html'
driver <- rsDriver(remoteServerAddr = "localhost",
                       browser=c("firefox"),
                       port=4446L)
remDr <- driver[["client"]]
remDr$navigate(url)
remDr$screenshot(display = TRUE)

#=====================================
# Web scrapping - DataFrame
#=====================================
data <- data.frame(cardname = character(1),
                         businterf= character(1),
                         maxmem= character(1),
                         coreclock= character(1),
                         memclock= character(1),
                         vidcat= character(1),
                         price= character(1),
                         avgg3dmark= character(1),
                         stringsAsFactors = FALSE)

#=====================================
# Web scrapping - collecting URL
#=====================================
#creating a loop to extract urls

a="/html/body/div[3]/div/div/div[3]/div[2]/div[3]/div/div[3]/ul/li["
b="]/a"

#=================================
# Collecting field values by 
# 1. handling missing columns using
# if-else structure and 
# 2.handling missing href using 
# Try-Catch block in the routine
#=================================

  for(i in 1:425)
    {
    i
  
    chartlist = remDr$findElement(using = "xpath",paste(c(a,i,b), collapse = "") )
    chartlist$clickElement()
    #==========  
    #name
    #==========
    tryCatch({
      suppressMessages({
    name<-remDr$findElement(using='xpath','//span[@class="cpuname"]')
    nametext=as.character(name$getElementText()[1])
      })
    },
    error = function(e) {
      'NA'
    } #This block handles missing hyperlinks
    )
    #============ 
    #businterface
    #============
    tryCatch({
      suppressMessages({
      bus<-remDr$findElement(using = 'xpath','//em[@class="left-desc-cpu"]/p[1]')
      bustext<-as.character(bus$getElementText()[1])
    
    if(trimws(unlist(strsplit(bustext, ":"))[[1]])=='Bus Interface'){
        bustext<-trimws(unlist(strsplit(bustext,':'))[[2]])
    }
    else {bustext = 'NA'} #This block handles missing hyperlinks
    })
    },
    error = function(e) {
      'NA'
    } #This block handles missing hyperlinks
    )
    
    #============ 
    #max meory
    #============ 
    tryCatch({
      suppressMessages({
        maxmem<-remDr$findElement(using='xpath','//em[@class="left-desc-cpu"]/p[2]')
        maxtext<-as.character(maxmem$getElementText()[1])
        if(trimws(unlist(strsplit(maxtext, ":"))[[1]])=='Max Memory Size'){
        maxtext<-trimws(unlist(strsplit(maxtext,' '))[[4]])
              }
        else {maxtext = 'NA'}
                      })
                      },
      error = function(e) {
        'NA'
      }
    )
    #============ 
    #coreclock
    #============ 
    tryCatch({
      suppressMessages({
    clock<-remDr$findElement(using = 'xpath','//em[@class="left-desc-cpu"]/p[3]')
    clocktext<-as.character(clock$getElementText()[1])
    if(trimws(unlist(strsplit(clocktext, ":"))[[1]])=='Core Clock(s)'){
      clocktext<-trimws(unlist(strsplit(clocktext,' '))[[3]])
    }
    else {clocktext = 'NA'}
      })
    },
    error = function(e) {
      'NA'
    }
    )
    
    #============ 
    #memclock
    #============ 
    tryCatch({
      suppressMessages({
    memclock<-remDr$findElement(using='xpath','//em[@class="left-desc-cpu"]/p[4]')
    memcltext<-as.character(memclock$getElementText()[1])
    if(trimws(unlist(strsplit(memcltext,':'))[[1]])=='Memory Clock(s)'){
      memcltext<-trimws(unlist(strsplit(memcltext,' '))[[3]])
    }
    else {memcltext = 'NA'}
    })
    },
    error = function(e) {
    'NA'
    }
    )
    #============ 
    #vidcat
    #============ 
    tryCatch({
      suppressMessages({
    vidcat<-remDr$findElement(using='xpath','//div[@class="desc-foot"]/p[1]')
    vidcattext<-as.character(vidcat$getElementText()[1])
    if(trimws(unlist(strsplit(vidcattext, ":"))[[1]])=='Videocard Category'){
      vidcattext<-trimws(unlist(strsplit(vidcattext,' '))[[3]])
    }
    else {vidcattext = 'NA'}
      })
    },
    error = function(e) {
      'NA'
    }
    )
    #============ 
    #price
    #============ 
    tryCatch({
      suppressMessages({
    price<-remDr$findElement(using='xpath','//div[@class="desc-foot"]/p[4]')
    pricetext<-as.character(price$getElementText()[1])
    if(trimws(unlist(strsplit(pricetext, ":"))[[1]])=='G3DMark/Price'){
      pricetext<-trimws(unlist(strsplit(pricetext,' '))[[2]])
    }
    else {pricetext = 'NA'}
      })
    },
    error = function(e) {
      'NA'
    }
    )
    #============ 
    #g3dmark
    #============ 
    g3dmark =remDr$findElement(using='xpath','/html/body/div[3]/div/div/div[3]/div[2]/center/div/div[1]/div[2]/span')
    g3text<-as.character(g3dmark$getElementText()[1])
  
    data<-rbind(data,c(nametext,bustext,memory_text,clocktext,memcltext,vidcattext,pricetext,g3text))
    
    #========================== 
    #goback to original page  
    #==========================
    chartlist$goBack()
  }
  
#==================================
# saving the dataframe
#==================================
write.csv(data,'data_videogames.csv')

