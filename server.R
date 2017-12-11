library(lubridate)
library(ggvis)
library(shiny)
library(XML)
library(scales)
library(leaflet)
library(htmltools)
library(jsonlite)

server<-function(input, output,session) {
options(scipen=8,digits=7)
  
scale_setting<-reactive({
ifelse(input$log,'log','linear')
})
it.scale_setting<-reactive({
ifelse(input$it.log,'log','linear')
})
  # Marine Offshore ---------------------------------------------------------

off.webpage<-'http://dnrp-apps2test/Marine-Portal-Stewardship/Sites?SiteType=2&pageSize=1000'
off.sites<-readHTMLTable(readLines(off.webpage),
                    stringsAsFactors = FALSE)[[1]]

colnames(off.sites)=c('Details','SiteID','SiteName','Locator','Latitude',
                                                      'Longitude','Shallow','SiteType','Area')
sites.off_names<-paste(off.sites$Locator,'-',off.sites$SiteName)
sites.off<-off.sites$Locator
names(sites.off)<-sites.off_names

observe({
  updateSelectInput(session,"off.site",
    choices=sort(sites.off)
  )
})

siteID<-reactive({off.sites$SiteID[input$off.site==off.sites$Locator]})
observe({
#getlistofDates
  # option1<-paste0('http://dnrp-apps2test/Marine-Portal-Stewardship/Samples?SiteID=2&',
#                 'MinDepth=0&MaxDepth=2&SampleTypes=WQ&pageSize=1000')
# option2<-paste0('http://dnrp-apps2test/Marine-Portal-Stewardship/WaterQuality?SiteID=2',
#             '&Parms=22&MinDepth=0&MaxDepth=2&pageSize=1000')
# system.time(readHTMLTable(readLines(option1),
#                     stringsAsFactors = FALSE))
#option1 is twice as fast
site_for_dates<-paste0('http://dnrp-apps2test/Marine-Portal-Stewardship/Samples?Sites=',siteID(),
            '&Parms=22&MinDepth=0&MaxDepth=2&pageSize=1000')

data.for.dates<-readHTMLTable(readLines(site_for_dates),
                    stringsAsFactors = FALSE)$sampleTable

# json.site<-paste0('http://dnrp-apps2test/Marine-Portal-Stewardship/ReviewServices/SampleParms',
#                   '?SiteType=1&RowLimit=10')
# data.for.dates<-fromJSON(readLines(json.site))
# data.for.dates$CollectDate<-gsub('/Date\\(','',data.for.dates$CollectDate)
# data.for.dates$CollectDate<-gsub(')/','',data.for.dates$CollectDate)
# data.for.dates$CollectDate<-as.Date(as.POSIXct(as.numeric(data.for.dates$CollectDate)/1000,origin="1970-01-01"))
if(is.data.frame(data.for.dates)){
  data.for.dates<-data.for.dates[grepl('WQ',data.for.dates[,1]),]
data.for.dates$Date<-as.Date(data.for.dates[,9],format='%m/%d/%Y')

  updateSelectInput(session,"date",
    choices=sort(unique(sort(data.for.dates$Date)),decreasing = T)
  )
}
#data.for.dates
})
data<-reactive({
#  for(j in unique(as.character(data.for.dates()$Date))){
    
# site<-paste0('http://dnrp-apps2test/Marine-Portal-Stewardship/WaterQuality?Sites=',siteID(),
#            #  '&MinDate=',input$date,'&MaxDate=',input$date,
#            '&MinDate=',j,'&MaxDate=',j,
#              '&pageSize=1000')
# data1<-readHTMLTable(readLines(site),
#                     stringsAsFactors = FALSE)$reviewWQTable

site.json<-paste0('http://dnrp-apps2test/Marine-Portal-Stewardship/ReviewServices/SampleParms?Site=',siteID(),
                        '&RowLimit=100000')

data<-fromJSON(readLines(site.json))

 data$CollectDate<-gsub('/Date\\(','',data$CollectDate)
 data$CollectDate<-gsub(')/','',data$CollectDate)
 data$CollectDate<-as.Date(as.POSIXct(as.numeric(data$CollectDate)/1000,origin="1970-01-01"))
# 
# 
# 
# if(is.null(data1))  data1<-data.frame(matrix(NA,1,36))
# colnames(data1)<-c('ReviewExpand','SampleID','GrabID','ProfileID','LabSampleNum','CollectDate','CollectTime',
#                         'Depth_m','Matrix','SiteType','Area','Locator','Site','Parameter','Value','OverrideValue',
#                         'Units','Quality','Qualifier','MDL','RDL','TextValue','SampleInfo','Released','Reviewed',
#                         'Reviewer','StewardNote','Reps','WorkNumber','Method','ListID','DateAnalyzed',
#                         'UpdateDate','OverrideReason','OverrideDate','OverriddenBy')
# if(j!=unique(data.for.dates()$Date)[1]) data<-rbind(data,data1) else data<-data1
# }
data$Value<-as.numeric(data$Value)
data$MDL<-as.numeric(data$Mdl)
data$RDL<-as.numeric(data$Rdl)
data$Depth<-as.numeric(data$Depth)
#data$CollectDate<-as.Date(data$CollectDate,format='%m/%d/%Y')
data$Year<-year(data$CollectDate)
data$Month<-month(data$CollectDate)
data$SampleID<-as.character(data$SampleId)
data$LabSampleNum<-as.character(data$LabSampleNum)
data$Parameter<-as.character(data$ParmDisplayName)
data[order(data$CollectDate,data$Parameter,data$Depth),]
})
#TEMP PLOT
vis.temp <- reactive({
    temp<-subset(data(),grepl('Temperature',Parameter)&Month==month(as.Date(input$date,format='%Y-%m-%d')))
    depths<-as.numeric(names(table(round(as.numeric(data()$Depth)))))
    if(length(depths)==0) depths=c(0,5)
    # Lables for axes
    xvar_name <- 'Temperature (°C)'
    yvar_name <- 'Depth (m)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),grepl('Temperature',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                      sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," °C", "<br>",
     round(sample$Depth,1)," m","<br>"
  )
  }

    temp %>% 
      ggvis(x = ~Value, y = ~Depth,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~Value, y= ~Depth,size := 75, size.hover := 200,fill:='red',stroke:='black',key := ~SampleID,
                  data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
      layer_paths(x= ~Value, y= ~Depth,stroke:='red',
                  data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
      #add_tooltip(point_tooltip, "click") %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("y", domain = c(0,depths[length(depths)]+1),reverse=T,clamp=T) %>%
      scale_numeric("x", domain = c(0,25)) %>%
      set_options(width = 325, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name)
  })

  vis.temp %>% bind_shiny("plot_temp")
vis.dens <- reactive({
    temp<-subset(data(),Parameter=='Density'&
                   Month==month(as.Date(input$date,format='%Y-%m-%d')))
 depths<-as.numeric(names(table(round(as.numeric(data()$Depth)))))
    if(length(depths)==0) depths=c(0,5)    # Lables for axes
    xvar_name <- 'Density (kg/m3)'
    yvar_name <- 'Depth (m)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),Parameter=='Density'&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," kg/m3", "<br>",
     round(sample$Depth,1)," m","<br>"
  )
  }

    temp %>% 
      ggvis(x = ~Value, y = ~Depth,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~Value, y= ~Depth,size := 75, size.hover := 200,fill:='red',stroke:='black',key := ~SampleID,
                  data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
      layer_paths(x= ~Value, y= ~Depth,stroke:='red',
                  data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("y", domain = c(0,depths[length(depths)]+1),reverse=T,clamp=T) %>%
      scale_numeric("x") %>%
      set_options(width = 325, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name)
  })

  vis.dens %>% bind_shiny("plot_dens")  
 
vis.do <- reactive({
    temp<-subset(data(),grepl('Oxygen, Field',Parameter)&!grepl('Saturation',Parameter)&
                   Month==month(as.Date(input$date,format='%Y-%m-%d')))
 depths<-as.numeric(names(table(round(as.numeric(data()$Depth)))))
    if(length(depths)==0) depths=c(0,5)    # Lables for axes
    xvar_name <- 'Dissolved Oxygen, Field (mg/L)'
    yvar_name <- 'Depth (m)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),grepl('Oxygen, Field',Parameter)&!grepl('Saturation',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," mg/L", "<br>",
     round(sample$Depth,1)," m","<br>"
  )
  }

    temp %>% 
      ggvis(x = ~Value, y = ~Depth,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~Value, y= ~Depth,size := 75, size.hover := 200,fill:='red',stroke:='black',key := ~SampleID,
                  data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
      layer_paths(x= ~Value, y= ~Depth,stroke:='red',
                  data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("y", domain = c(0,depths[length(depths)]+1),reverse=T,clamp=T) %>%
      scale_numeric("x", domain = c(0,15)) %>%
      set_options(width = 325, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name)
  })

  vis.do %>% bind_shiny("plot_do")

vis.do_lab <- reactive({
    temp<-subset(data(),Parameter=='Dissolved Oxygen'&
                   Month==month(as.Date(input$date,format='%Y-%m-%d')))
 depths<-as.numeric(names(table(round(as.numeric(data()$Depth)))))
    if(length(depths)==0) depths=c(0,5)    # Lables for axes
    xvar_name <- 'Dissolved Oxygen, Lab (mg/L)'
    yvar_name <- 'Depth (m)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),Parameter=='Dissolved Oxygen'&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," mg/L", "<br>",
     round(sample$Depth,1)," m","<br>"
  )
  }

    temp %>% 
      ggvis(x = ~Value, y = ~Depth,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~Value, y= ~Depth,size := 75, size.hover := 200,fill:='red',stroke:='black',key := ~SampleID,
                  data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
      layer_paths(x= ~Value, y= ~Depth,stroke:='red',
                  data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("y", domain = c(0,depths[length(depths)]+1),reverse=T,clamp=T) %>%
      scale_numeric("x", domain = c(0,15)) %>%
      set_options(width = 325, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name)
  })

  vis.do_lab %>% bind_shiny("plot_do_lab")  

vis.sal <- reactive({
    temp<-subset(data(),grepl('Salinity, Field',Parameter)&
                   Month==month(as.Date(input$date,format='%Y-%m-%d')))
 depths<-as.numeric(names(table(round(as.numeric(data()$Depth)))))
    if(length(depths)==0) depths=c(0,5)    # Lables for axes
    xvar_name <- 'Salinity, Field (mg/L)'
    yvar_name <- 'Depth (m)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),grepl('Salinity, Field',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," PPS", "<br>",
     round(sample$Depth,1)," m","<br>"
  )
  }

    temp %>% 
      ggvis(x = ~Value, y = ~Depth,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~Value, y= ~Depth,size := 75, size.hover := 200,fill:='red',stroke:='black',key := ~SampleID,
                  data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
      layer_paths(x= ~Value, y= ~Depth,stroke:='red',
                  data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("y", domain = c(0,depths[length(depths)]+1),reverse=T,clamp=T) %>%
      scale_numeric("x", domain = c(0,35)) %>%
      set_options(width = 325, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name)
  })

  vis.sal %>% bind_shiny("plot_sal")

vis.sal_lab <- reactive({
    temp<-subset(data(),Parameter=='Salinity'&
                   Month==month(as.Date(input$date,format='%Y-%m-%d')))
 depths<-as.numeric(names(table(round(as.numeric(data()$Depth)))))
    if(length(depths)==0) depths=c(0,5)    # Lables for axes
    xvar_name <- 'Salinity, Lab (PPS)'
    yvar_name <- 'Depth (m)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),Parameter=='Salinity'&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," PPS", "<br>",
     round(sample$Depth,1)," m","<br>"
  )
  }

    temp %>% 
      ggvis(x = ~Value, y = ~Depth,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~Value, y= ~Depth,size := 75, size.hover := 200,fill:='red',stroke:='black',key := ~SampleID,
                  data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
      layer_paths(x= ~Value, y= ~Depth,stroke:='red',
                  data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("y", domain = c(0,depths[length(depths)]+1),reverse=T,clamp=T) %>%
      scale_numeric("x", domain = c(0,35)) %>%
      set_options(width = 325, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name)
  })

  vis.sal_lab %>% bind_shiny("plot_sal_lab")    
      
vis.cond <- reactive({
    temp<-subset(data(),grepl('Salinity, Field',Parameter)&
                   Month==month(as.Date(input$date,format='%Y-%m-%d')))
 depths<-as.numeric(names(table(round(as.numeric(data()$Depth)))))
    if(length(depths)==0) depths=c(0,5)    # Lables for axes
    xvar_name <- 'Salinity (PSS)'
    yvar_name <- 'Depth (m)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),grepl('Salinity, Field',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," µS/cm", "<br>",
     round(sample$Depth,1)," m","<br>"
  )
  }

    temp %>% 
      ggvis(x = ~Value, y = ~Depth,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~Value, y= ~Depth,size := 75, size.hover := 200,fill:='red',stroke:='black',key := ~SampleID,
                  data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
      layer_paths(x= ~Value, y= ~Depth,stroke:='red',
                  data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("y", domain = c(0,depths[length(depths)]+1),reverse=T,clamp=T) %>%
           scale_numeric("x", domain = c(5,35)) %>%
      add_axis('x',title=xvar_name) %>%
           set_options(width = 325, height = 350) %>%
      add_axis("y", title = yvar_name)
  })

  vis.cond %>% bind_shiny("plot_cond")
  
vis.ph <- reactive({
    temp<-subset(data(),grepl('pH',Parameter)&
                   Month==month(as.Date(input$date,format='%Y-%m-%d')))
 depths<-as.numeric(names(table(round(as.numeric(data()$Depth)))))
    if(length(depths)==0) depths=c(0,5)    # Lables for axes
    xvar_name <- 'pH'
    yvar_name <- 'Depth (m)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),grepl('pH',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," units", "<br>",
     round(sample$Depth,1)," m","<br>"
  )
  }

    temp %>% 
      ggvis(x = ~Value, y = ~Depth,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~Value, y= ~Depth,size := 75, size.hover := 200,fill:='red',stroke:='black',key := ~SampleID,
                  data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
       layer_paths(x= ~Value, y= ~Depth,stroke:='red',
                   data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("y", domain = c(0,depths[length(depths)]+1),reverse=T,clamp=T) %>%
      scale_numeric("x", domain = c(5,12)) %>%
      set_options(width = 325, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name)
  })

  vis.ph %>% bind_shiny("plot_ph")

vis.orthop <- reactive({
    temp<-subset(data(),grepl('Orthophosphate',Parameter)&
                   Month==month(as.Date(input$date,format='%Y-%m-%d')))
 depths<-as.numeric(names(table(round(as.numeric(data()$Depth)))))
    if(length(depths)==0) depths=c(0,5)    # Lables for axes
    xvar_name <- 'Orthophosphate (mg/L)'
    yvar_name <- 'Depth (m)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),grepl('Orthophosphate',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," mg/L", "<br>",
     round(sample$Depth,1)," m","<br>"
  )
  }
{if(any(!is.na(temp$Value))) if(scale_setting()=='log') tick_nos<-10^(seq(floor(log10(min(temp$Value[temp$Value>0],
                                                                                          na.rm=T))),
                                               ceiling(log10(max(temp$Value,na.rm=T))),by=1)) else
                                                 tick_nos<-pretty(temp$Value)
else tick_nos<-1}
    temp %>% 
      ggvis(x = ~Value, y = ~Depth,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~Value, y= ~Depth,size := 75, size.hover := 200,fill:='red',stroke:='black',key := ~SampleID,
                  data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
#       layer_paths(x= ~Value, y= ~Depth,stroke:='red',
#                   data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("y", domain = c(0,depths[length(depths)]+1),reverse=T,clamp=T) %>%
      scale_numeric("x",trans=scale_setting(),domain=c(tick_nos[1],tick_nos[length(tick_nos)]),expand=0,nice=F) %>%
      set_options(width = 325, height = 350) %>%
      add_axis("x", title = xvar_name,format='.1r',values=tick_nos,
               properties=axis_props(
                 labels = list(angle = 45,fontSize = 10,align = "left",baseline = "middle",dx = 3)
               )) %>% 
      add_axis("y", title = yvar_name)
  })

  vis.orthop %>% bind_shiny("plot_ortho")
      
vis.totp <- reactive({
    temp<-subset(data(),grepl('Total Phosphorus',Parameter)&
                   Month==month(as.Date(input$date,format='%Y-%m-%d')))
 depths<-as.numeric(names(table(round(as.numeric(data()$Depth)))))
    if(length(depths)==0) depths=c(0,5)    # Lables for axes
    xvar_name <- 'Total Phosphorus (mg/L)'
    yvar_name <- 'Depth (m)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),grepl('Total Phosphorus',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," mg/L", "<br>",
     round(sample$Depth,1)," m","<br>"
  )
  }
{if(any(!is.na(temp$Value))) if(scale_setting()=='log') tick_nos<-10^(seq(floor(log10(min(temp$Value[temp$Value>0],
                                                                                          na.rm=T))),
                                               ceiling(log10(max(temp$Value,na.rm=T))),by=1)) else
                                                 tick_nos<-pretty(temp$Value)
else tick_nos<-1}
    temp %>% 
      ggvis(x = ~Value, y = ~Depth,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~Value, y= ~Depth,size := 75, size.hover := 200,fill:='red',stroke:='black',key := ~SampleID,
                  data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
      #layer_paths(x= ~Value, y= ~Depth,stroke:='red',
      #            data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("y", domain = c(0,depths[length(depths)]+1),reverse=T,clamp=T) %>%
      scale_numeric("x",trans=scale_setting(),domain=c(tick_nos[1],tick_nos[length(tick_nos)]),expand=0,nice=F) %>%
      set_options(width = 325, height = 350) %>%
      add_axis("x", title = xvar_name,format='.1r',values=tick_nos,
               properties=axis_props(
                 labels = list(angle = 45,fontSize = 10,align = "left",baseline = "middle",dx = 3)
               )) %>%
      add_axis("y", title = yvar_name)
  })

  vis.totp %>% bind_shiny("plot_totp")

vis.nitrate<- reactive({
    temp<-subset(data(),grepl('Nitrate',Parameter)&
                   Month==month(as.Date(input$date,format='%Y-%m-%d')))
 depths<-as.numeric(names(table(round(as.numeric(data()$Depth)))))
    if(length(depths)==0) depths=c(0,5)    # Lables for axes
    xvar_name <- 'Nitrate/Nitrite-N (mg/L)'
    yvar_name <- 'Depth (m)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),grepl('Nitrate',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," mg/L", "<br>",
     round(sample$Depth,1)," m","<br>"
  )
  }
{if(any(!is.na(temp$Value))) if(scale_setting()=='log') tick_nos<-10^(seq(floor(log10(min(temp$Value[temp$Value>0],
                                                                                          na.rm=T))),
                                               ceiling(log10(max(temp$Value,na.rm=T))),by=1)) else
                                                 tick_nos<-pretty(temp$Value)
else tick_nos<-1}
    temp %>% 
      ggvis(x = ~Value, y = ~Depth,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~Value, y= ~Depth,size := 75, size.hover := 200,fill:='red',stroke:='black',key := ~SampleID,
                  data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
#       layer_paths(x= ~Value, y= ~Depth,stroke:='red',
#                   data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("y", domain = c(0,depths[length(depths)]+1),reverse=T,clamp=T) %>%
          scale_numeric("x",trans=scale_setting(),domain=c(tick_nos[1],tick_nos[length(tick_nos)]),expand=0,nice=F) %>%
      set_options(width = 325, height = 350) %>%
      add_axis("x", title = xvar_name,format='.1r',values=tick_nos,
               properties=axis_props(
                 labels = list(angle = 45,fontSize = 10,align = "left",baseline = "middle",dx = 3)
               )) %>%
      add_axis("y", title = yvar_name)
  })

  vis.nitrate %>% bind_shiny("plot_nitrate")  
    
vis.ammonia<- reactive({
    temp<-subset(data(),grepl('Ammonia',Parameter)&
                   Month==month(as.Date(input$date,format='%Y-%m-%d')))
 depths<-as.numeric(names(table(round(as.numeric(data()$Depth)))))
    if(length(depths)==0) depths=c(0,5)    # Lables for axes
    xvar_name <- 'Ammonia-N (mg/L)'
    yvar_name <- 'Depth (m)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),grepl('Ammonia',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," mg/L", "<br>",
     round(sample$Depth,1)," m","<br>"
  )
  }
{if(any(!is.na(temp$Value))) if(scale_setting()=='log') tick_nos<-10^(seq(floor(log10(min(temp$Value[temp$Value>0],
                                                                                          na.rm=T))),
                                               ceiling(log10(max(temp$Value,na.rm=T))),by=1)) else
                                                 tick_nos<-pretty(temp$Value)
else tick_nos<-1}
    temp %>% 
      ggvis(x = ~Value, y = ~Depth,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~Value, y= ~Depth,size := 75, size.hover := 200,fill:='red',stroke:='black',key := ~SampleID,
                  data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
#       layer_paths(x= ~Value, y= ~Depth,stroke:='red',
#                   data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("y", domain = c(0,depths[length(depths)]+1),reverse=T,clamp=T) %>%
         scale_numeric("x",trans=scale_setting(),domain=c(tick_nos[1],tick_nos[length(tick_nos)]),expand=0,nice=F) %>%
      set_options(width = 325, height = 350) %>%
      add_axis("x", title = xvar_name,format='.1r',values=tick_nos,#subdivide=9,
               properties=axis_props(
                 labels = list(angle = 45,fontSize = 10,align = "left",baseline = "middle",dx = 3)
               )) %>%
      add_axis("y", title = yvar_name)
  })

  vis.ammonia %>% bind_shiny("plot_ammonia")    
  
vis.totn<- reactive({
    temp<-subset(data(),grepl('Total Nitrogen',Parameter)&
                   Month==month(as.Date(input$date,format='%Y-%m-%d')))
 depths<-as.numeric(names(table(round(as.numeric(data()$Depth)))))
    if(length(depths)==0) depths=c(0,5)    # Lables for axes
    xvar_name <- 'Total Nitrogen (mg/L)'
    yvar_name <- 'Depth (m)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),grepl('Total Nitrogen',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," mg/L", "<br>",
     round(sample$Depth,1)," m","<br>"
  )
  }
{if(any(!is.na(temp$Value))) if(scale_setting()=='log') tick_nos<-10^(seq(floor(log10(min(temp$Value[temp$Value>0],
                                                                                          na.rm=T))),
                                               ceiling(log10(max(temp$Value,na.rm=T))),by=1)) else
                                                 tick_nos<-pretty(temp$Value)
else tick_nos<-1}
    temp %>% 
      ggvis(x = ~Value, y = ~Depth,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~Value, y= ~Depth,size := 75, size.hover := 200,fill:='red',stroke:='black',key := ~SampleID,
                  data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
#       layer_paths(x= ~Value, y= ~Depth,stroke:='red',
#                   data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("y", domain = c(0,depths[length(depths)]+1),reverse=T,clamp=T) %>%
          scale_numeric("x",trans=scale_setting(),domain=c(tick_nos[1],tick_nos[length(tick_nos)]),expand=0,nice=F) %>%
      set_options(width = 325, height = 350) %>%
      add_axis("x", title = xvar_name,format='.1r',values=tick_nos,
               properties=axis_props(
                 labels = list(angle = 45,fontSize = 10,align = "left",baseline = "middle",dx = 3)
               )) %>%
      add_axis("y", title = yvar_name)
  })

  vis.totn %>% bind_shiny("plot_totn")   

  vis.tss<- reactive({
    temp<-subset(data(),grepl('Total Suspended Solids',Parameter)&
                   Month==month(as.Date(input$date,format='%Y-%m-%d')))
 depths<-as.numeric(names(table(round(as.numeric(data()$Depth)))))
    if(length(depths)==0) depths=c(0,5)    # Lables for axes
    xvar_name <- 'Total Suspended Solids (mg/L)'
    yvar_name <- 'Depth (m)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),grepl('Total Suspended Solids',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," mg/L", "<br>",
     round(sample$Depth,1)," m","<br>"
  )
  }
{if(any(!is.na(temp$Value))) if(scale_setting()=='log') tick_nos<-10^(seq(floor(log10(min(temp$Value[temp$Value>0],
                                                                                          na.rm=T))),
                                               ceiling(log10(max(temp$Value,na.rm=T))),by=1)) else
                                                 tick_nos<-pretty(temp$Value)
else tick_nos<-1}
    temp %>% 
      ggvis(x = ~Value, y = ~Depth,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~Value, y= ~Depth,size := 75, size.hover := 200,fill:='red',stroke:='black',key := ~SampleID,
                  data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
#       layer_paths(x= ~Value, y= ~Depth,stroke:='red',
#                   data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("y", domain = c(0,depths[length(depths)]+1),reverse=T,clamp=T) %>%
          scale_numeric("x",trans=scale_setting(),domain=c(tick_nos[1],tick_nos[length(tick_nos)]),expand=0,nice=F) %>%
      set_options(width = 325, height = 350) %>%
      add_axis("x", title = xvar_name,format='.1r',values=tick_nos,
               properties=axis_props(
                 labels = list(angle = 45,fontSize = 10,align = "left",baseline = "middle",dx = 3)
               )) %>%
      add_axis("y", title = yvar_name)
  })

  vis.tss %>% bind_shiny("plot_tss")   

  vis.entero_profile<- reactive({
    temp<-subset(data(),grepl('Enterococcus',Parameter)&
                   Month==month(as.Date(input$date,format='%Y-%m-%d')))
 depths<-as.numeric(names(table(round(as.numeric(data()$Depth)))))
    if(length(depths)==0) depths=c(0,5)    # Lables for axes
    xvar_name <- 'Enterococcus (CFU/100 mL)'
    yvar_name <- 'Depth (m)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),grepl('Enterococcus',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," CFU/100 mL", "<br>",
     round(sample$Depth,1)," m","<br>"
  )
  }
{if(any(!is.na(temp$Value))) if(scale_setting()=='log') tick_nos<-10^(seq(floor(log10(min(temp$Value[temp$Value>0],
                                                                                          na.rm=T))),
                                               ceiling(log10(max(temp$Value,na.rm=T))),by=1)) else
                                                 tick_nos<-pretty(temp$Value)
else tick_nos<-1}
    temp %>% 
      ggvis(x = ~Value, y = ~Depth,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~Value, y= ~Depth,size := 75, size.hover := 200,fill:='red',stroke:='black',key := ~SampleID,
                  data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
#       layer_paths(x= ~Value, y= ~Depth,stroke:='red',
#                   data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("y", domain = c(0,depths[length(depths)]+1),reverse=T,clamp=T) %>%
          scale_numeric("x",trans=scale_setting(),domain=c(tick_nos[1],tick_nos[length(tick_nos)]),expand=0,nice=F) %>%
      set_options(width = 325, height = 350) %>%
      add_axis("x", title = xvar_name,format='.1r',values=tick_nos,
               properties=axis_props(
                 labels = list(angle = 45,fontSize = 10,align = "left",baseline = "middle",dx = 3)
               )) %>%
      add_axis("y", title = yvar_name)
  })

  vis.entero_profile %>% bind_shiny("plot_entero_profile")     

  vis.fecal_profile<- reactive({
    temp<-subset(data(),grepl('Fecal Coliform',Parameter)&
                   Month==month(as.Date(input$date,format='%Y-%m-%d')))
 depths<-as.numeric(names(table(round(as.numeric(data()$Depth)))))
    if(length(depths)==0) depths=c(0,5)    # Lables for axes
    xvar_name <- 'Fecal Coliform (CFU/100 mL)'
    yvar_name <- 'Depth (m)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),grepl('Fecal Coliform',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," CFU/100 mL", "<br>",
     round(sample$Depth,1)," m","<br>"
  )
  }
{if(any(!is.na(temp$Value))) if(scale_setting()=='log') tick_nos<-10^(seq(floor(log10(min(temp$Value[temp$Value>0],
                                                                                          na.rm=T))),
                                               ceiling(log10(max(temp$Value,na.rm=T))),by=1)) else
                                                 tick_nos<-pretty(temp$Value)
else tick_nos<-1}
    temp %>% 
      ggvis(x = ~Value, y = ~Depth,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~Value, y= ~Depth,size := 75, size.hover := 200,fill:='red',stroke:='black',key := ~SampleID,
                  data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
#       layer_paths(x= ~Value, y= ~Depth,stroke:='red',
#                   data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("y", domain = c(0,depths[length(depths)]+1),reverse=T,clamp=T) %>%
          scale_numeric("x",trans=scale_setting(),domain=c(tick_nos[1],tick_nos[length(tick_nos)]),expand=0,nice=F) %>%
      set_options(width = 325, height = 350) %>%
      add_axis("x", title = xvar_name,format='.1r',values=tick_nos,
               properties=axis_props(
                 labels = list(angle = 45,fontSize = 10,align = "left",baseline = "middle",dx = 3)
               )) %>%
      add_axis("y", title = yvar_name)
  })

  vis.fecal_profile %>% bind_shiny("plot_fecal_profile")
  
  vis.coli_profile<- reactive({
    temp<-subset(data(),grepl('E. coli',Parameter)&
                   Month==month(as.Date(input$date,format='%Y-%m-%d')))
 depths<-as.numeric(names(table(round(as.numeric(data()$Depth)))))
    if(length(depths)==0) depths=c(0,5)    # Lables for axes
    xvar_name <- 'E. coli (CFU/100 mL)'
    yvar_name <- 'Depth (m)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),grepl('E. coli',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," CFU/100 mL", "<br>",
     round(sample$Depth,1)," m","<br>"
  )
  }
{if(any(!is.na(temp$Value))) if(scale_setting()=='log') tick_nos<-10^(seq(floor(log10(min(temp$Value[temp$Value>0],
                                                                                          na.rm=T))),
                                               ceiling(log10(max(temp$Value,na.rm=T))),by=1)) else
                                                 tick_nos<-pretty(temp$Value)
else tick_nos<-1}
    temp %>% 
      ggvis(x = ~Value, y = ~Depth,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~Value, y= ~Depth,size := 75, size.hover := 200,fill:='red',stroke:='black',key := ~SampleID,
                  data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
#       layer_paths(x= ~Value, y= ~Depth,stroke:='red',
#                   data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("y", domain = c(0,depths[length(depths)]+1),reverse=T,clamp=T) %>%
          scale_numeric("x",trans=scale_setting(),domain=c(tick_nos[1],tick_nos[length(tick_nos)]),expand=0,nice=F) %>%
      set_options(width = 325, height = 350) %>%
      add_axis("x", title = xvar_name,format='.1r',values=tick_nos,
               properties=axis_props(
                 labels = list(angle = 45,fontSize = 10,align = "left",baseline = "middle",dx = 3)
               )) %>%
      add_axis("y", title = yvar_name)
  })

  vis.coli_profile %>% bind_shiny("plot_coli_profile")   
    
vis.chla<- reactive({
    temp<-subset(data(),grepl('Pheophytin a',Parameter)&!is.na(Depth)&
                   Month==month(as.Date(input$date,format='%Y-%m-%d')))
 depths<-as.numeric(names(table(round(as.numeric(data()$Depth)))))
    if(length(depths)==0) depths=c(0,5)    # Lables for axes
    xvar_name <- 'Pheophytin a (µg/L)'
    yvar_name <- 'Depth (m)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),grepl('Pheophytin a',Parameter)&!is.na(Depth)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," µg/L", "<br>",
     round(sample$Depth,1)," m","<br>"
  )
  }

    temp %>% 
      ggvis(x = ~Value, y = ~Depth,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~Value, y= ~Depth,size := 75, size.hover := 200,fill:='red',stroke:='black',key := ~SampleID,
                  data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
#       layer_paths(x= ~Value, y= ~Depth,stroke:='red',
#                   data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("y", domain = c(0,depths[length(depths)]+1),reverse=T,clamp=T) %>%
      scale_numeric("x") %>%
       set_options(width = 325, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name)
  })

  vis.chla %>% bind_shiny("plot_chla")      
      
vis.chla<- reactive({
    temp<-subset(data(),grepl('Chlorophyll a',Parameter)&!is.na(Depth)&
                   Month==month(as.Date(input$date,format='%Y-%m-%d')))
 depths<-as.numeric(names(table(round(as.numeric(data()$Depth)))))
    if(length(depths)==0) depths=c(0,5)    # Lables for axes
    xvar_name <- 'Chlorophyll a, Lab (µg/L)'
    yvar_name <- 'Depth (m)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),grepl('Chlorophyll a',Parameter)&!is.na(Depth)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," µg/L", "<br>",
     round(sample$Depth,1)," m","<br>"
  )
  }

    temp %>% 
      ggvis(x = ~Value, y = ~Depth,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~Value, y= ~Depth,size := 75, size.hover := 200,fill:='red',stroke:='black',key := ~SampleID,
                  data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
#       layer_paths(x= ~Value, y= ~Depth,stroke:='red',
#                   data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("y", domain = c(0,depths[length(depths)]+1),reverse=T,clamp=T) %>%
      scale_numeric("x") %>%
       set_options(width = 325, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name)
  })

  vis.chla %>% bind_shiny("plot_chla")      
  
vis.chla_field<- reactive({
    temp<-subset(data(),grepl('Chlorophyll, Field',Parameter)&!is.na(Depth)&
                   Month==month(as.Date(input$date,format='%Y-%m-%d')))
 depths<-as.numeric(names(table(round(as.numeric(data()$Depth)))))
    if(length(depths)==0) depths=c(0,5)    # Lables for axes
    xvar_name <- 'Chlorophyll a, Field (µg/L)'
    yvar_name <- 'Depth (m)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),grepl('Chlorophyll, Field',Parameter)&!is.na(Depth)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," µg/L", "<br>",
     round(sample$Depth,1)," m","<br>"
  )
  }

    temp %>% 
      ggvis(x = ~Value, y = ~Depth,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~Value, y= ~Depth,size := 75, size.hover := 200,fill:='red',stroke:='black',key := ~SampleID,
                  data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
#       layer_paths(x= ~Value, y= ~Depth,stroke:='red',
#                   data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("y", domain = c(0,depths[length(depths)]+1),reverse=T,clamp=T) %>%
      scale_numeric("x") %>%
       set_options(width = 325, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name)
  })

  vis.chla_field %>% bind_shiny("plot_chla_field")   
vis.par<- reactive({
    temp<-subset(data(),Parameter=='Light Intensity (PAR)'&!is.na(Depth)&
                   Month==month(as.Date(input$date,format='%Y-%m-%d')))
 depths<-as.numeric(names(table(round(as.numeric(data()$Depth)))))
    if(length(depths)==0) depths=c(0,5)    # Lables for axes
    xvar_name <- 'Light Intensity (umol/sm2)'
    yvar_name <- 'Depth (m)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),Parameter=='Light Intensity (PAR)'&!is.na(Depth)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," µmol/sm2", "<br>",
     round(sample$Depth,1)," m","<br>"
  )
  }

    temp %>% 
      ggvis(x = ~Value, y = ~Depth,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~Value, y= ~Depth,size := 75, size.hover := 200,fill:='red',stroke:='black',key := ~SampleID,
                  data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
#       layer_paths(x= ~Value, y= ~Depth,stroke:='red',
#                   data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("y", domain = c(0,depths[length(depths)]+1),reverse=T,clamp=T) %>%
      scale_numeric("x") %>%
       set_options(width = 325, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name)
  })

  vis.par %>% bind_shiny("plot_par")

vis.surface_par<- reactive({
    temp<-subset(data(),Parameter=='Surface Light Intensity (PAR)'&!is.na(Depth)&
                   Month==month(as.Date(input$date,format='%Y-%m-%d')))
 depths<-as.numeric(names(table(round(as.numeric(data()$Depth)))))
    if(length(depths)==0) depths=c(0,5)    # Lables for axes
    xvar_name <- 'Surface Light Intensity (umol/sm2)'
    yvar_name <- 'Depth (m)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),Parameter=='Surface Light Intensity (PAR)'&!is.na(Depth)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," µmol/sm2", "<br>",
     round(sample$Depth,1)," m","<br>"
  )
  }

    temp %>% 
      ggvis(x = ~Value, y = ~Depth,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~Value, y= ~Depth,size := 75, size.hover := 200,fill:='red',stroke:='black',key := ~SampleID,
                  data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
#       layer_paths(x= ~Value, y= ~Depth,stroke:='red',
#                   data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("y", domain = c(0,depths[length(depths)]+1),reverse=T,clamp=T) %>%
      scale_numeric("x") %>%
       set_options(width = 325, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name)
  })

  vis.surface_par %>% bind_shiny("plot_surface_par")  
    
vis.light_trans<- reactive({
    temp<-subset(data(),grepl('Light Transmissivity',Parameter)&!is.na(Depth)&
                   Month==month(as.Date(input$date,format='%Y-%m-%d')))
 depths<-as.numeric(names(table(round(as.numeric(data()$Depth)))))
    if(length(depths)==0) depths=c(0,5)    # Lables for axes
    xvar_name <- 'Light Transmissivity (%)'
    yvar_name <- 'Depth (m)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),grepl('Light Transmissivity',Parameter)&!is.na(Depth)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," %", "<br>",
     round(sample$Depth,1)," m","<br>"
  )
  }

    temp %>% 
      ggvis(x = ~Value, y = ~Depth,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~Value, y= ~Depth,size := 75, size.hover := 200,fill:='red',stroke:='black',key := ~SampleID,
                  data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
#       layer_paths(x= ~Value, y= ~Depth,stroke:='red',
#                   data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("y", domain = c(0,depths[length(depths)]+1),reverse=T,clamp=T) %>%
      scale_numeric("x",domain=c(0,100)) %>%
       set_options(width = 325, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name)
  })

  vis.light_trans %>% bind_shiny("plot_light_trans")    
    
vis.secchi<- reactive({
    temp<-subset(data(),grepl('Secchi',Parameter))
    # Lables for axes
    xvar_name <- 'Day of Year'
    yvar_name <- 'Secchi Depth (m)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),grepl('Secchi',Parameter)&SampleID==x$SampleID)
    paste0("<b>", sample$SampleNum, "</b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," m"
  )
  }

    temp %>% 
      ggvis(x = ~yday(CollectDate), y = ~Value,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~yday(CollectDate), y= ~Value,size := 75, size.hover := 200,fill:='red',stroke:='black',
                   key := ~SampleID,
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      layer_paths(x= ~yday(CollectDate), y= ~Value,stroke:='red',
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("y", domain = c(0,10),reverse=T,clamp=T) %>%
      scale_numeric("x") %>%
      set_options(width = 650, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name)
  })

  vis.secchi %>% bind_shiny("plot_secchi")   

vis.sil<- reactive({
    temp<-subset(data(),grepl('Silica',Parameter)&
                   Month==month(as.Date(input$date,format='%Y-%m-%d')))
 depths<-as.numeric(names(table(round(as.numeric(data()$Depth)))))
    if(length(depths)==0) depths=c(0,5)    # Lables for axes    # Lables for axes
    xvar_name <- 'Silica (mg/L)'
    yvar_name <- 'Depth (m)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),grepl('Silica',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," mg/L", "<br>",
     round(sample$Depth,1)," m","<br>"
  )
  }

    temp %>% 
      ggvis(x = ~Value, y = ~Depth,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~Value, y= ~Depth,size := 75, size.hover := 200,fill:='red',stroke:='black',key := ~SampleID,
                  data=subset(temp,CollectDate==as.Date(input$date,format='%Y-%m-%d'))) %>%
#       layer_paths(x= ~Value, y= ~Depth,stroke:='red',
#                   data=subset(temp,Date==as.Date(input$date,format='%Y-%m-%d'))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("y", domain = c(0,depths[length(depths)]+1),reverse=T,clamp=T) %>%
      scale_numeric("x") %>%
      set_options(width = 325, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name)
  })

  vis.sil %>% bind_shiny("plot_sil")      
  # Marine Offshore - Depth Over Time ---------------------------------------

vis.temp_depth<- reactive({
    temp<-subset(data(),grepl('Temperature',Parameter)&Depth>input$depth[1]&Depth<input$depth[2])
    # Lables for axes
    xvar_name <- 'Day of Year'
    yvar_name <- 'Temperature (°C)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),grepl('Temperature',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," °C"
  )
  }
    temp %>%
      ggvis(x = ~yday(CollectDate), y = ~Value,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~yday(CollectDate), y= ~Value,size := 75, size.hover := 200,fill:='red',stroke:='black',
                   key := ~SampleID,
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      layer_paths(x= ~yday(CollectDate), y= ~Value,stroke:='red',
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("x") %>%
      set_options(width = 650, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
      scale_numeric("y") %>%
      add_axis("y", title = yvar_name)
  })

  vis.temp_depth %>% bind_shiny("plot_temp_depth")

vis.do_depth<- reactive({
    temp<-subset(data(),grepl('Dissolved Oxygen, Field',Parameter)&Depth>input$depth[1]&Depth<input$depth[2])
    # Lables for axes
    xvar_name <- 'Day of Year'
    yvar_name <- 'Dissolved Oxygen, Field (mg/L)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),grepl('Dissolved Oxygen, Field',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," mg/L"
  )
  }
    temp %>%
      ggvis(x = ~yday(CollectDate), y = ~Value,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~yday(CollectDate), y= ~Value,size := 75, size.hover := 200,fill:='red',stroke:='black',
                   key := ~SampleID,
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      layer_paths(x= ~yday(CollectDate), y= ~Value,stroke:='red',
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("x") %>%
      set_options(width = 650, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
      scale_numeric("y") %>%
      add_axis("y", title = yvar_name)
  })

  vis.do_depth %>% bind_shiny("plot_do_depth")  

vis.do_lab_depth<- reactive({
    temp<-subset(data(),Parameter=='Dissolved Oxygen'&Depth>input$depth[1]&Depth<input$depth[2])
    # Lables for axes
    xvar_name <- 'Day of Year'
    yvar_name <- 'Dissolved Oxygen, Lab (mg/L)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),Parameter=='Dissolved Oxygen'&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," mg/L"
  )
  }
    temp %>%
      ggvis(x = ~yday(CollectDate), y = ~Value,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~yday(CollectDate), y= ~Value,size := 75, size.hover := 200,fill:='red',stroke:='black',
                   key := ~SampleID,
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      layer_paths(x= ~yday(CollectDate), y= ~Value,stroke:='red',
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("x") %>%
      set_options(width = 650, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
      scale_numeric("y") %>%
      add_axis("y", title = yvar_name)
  })

  vis.do_lab_depth %>% bind_shiny("plot_do_lab_depth")    

vis.salinity_lab_depth<- reactive({
    temp<-subset(data(),Parameter=='Salinity'&Depth>input$depth[1]&Depth<input$depth[2])
    # Lables for axes
    xvar_name <- 'Day of Year'
    yvar_name <- 'Salinity, Lab (PPS)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),Parameter=='Salinity'&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," mg/L"
  )
  }
    temp %>%
      ggvis(x = ~yday(CollectDate), y = ~Value,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~yday(CollectDate), y= ~Value,size := 75, size.hover := 200,fill:='red',stroke:='black',
                   key := ~SampleID,
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      layer_paths(x= ~yday(CollectDate), y= ~Value,stroke:='red',
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("x") %>%
      set_options(width = 650, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
      scale_numeric("y") %>%
      add_axis("y", title = yvar_name)
  })

  vis.salinity_lab_depth %>% bind_shiny("plot_salinity_lab_depth")
  
vis.salinity_depth<- reactive({
    temp<-subset(data(),Parameter=='Salinity, Field'&Depth>input$depth[1]&Depth<input$depth[2])
    # Lables for axes
    xvar_name <- 'Day of Year'
    yvar_name <- 'Salinity, Field (PPS)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),Parameter=='Salinity, Field'&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," mg/L"
  )
  }
    temp %>%
      ggvis(x = ~yday(CollectDate), y = ~Value,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~yday(CollectDate), y= ~Value,size := 75, size.hover := 200,fill:='red',stroke:='black',
                   key := ~SampleID,
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      layer_paths(x= ~yday(CollectDate), y= ~Value,stroke:='red',
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("x") %>%
      set_options(width = 650, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
      scale_numeric("y") %>%
      add_axis("y", title = yvar_name)
  })

  vis.salinity_depth %>% bind_shiny("plot_salinity_depth")     
      
vis.fecal_depth<- reactive({
    temp<-subset(data(),grepl('Fecal Coliform',Parameter)&Depth>input$depth[1]&Depth<input$depth[2])
    # Lables for axes
    xvar_name <- 'Day of Year'
    yvar_name <- 'Fecal Coliform (CFU/100 mL)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),grepl('Fecal Coliform',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," CFU/100 mL"
  )
  }
{if(any(!is.na(temp$Value))) if(scale_setting()=='log') tick_nos<-10^(seq(floor(log10(min(temp$Value[temp$Value>0],
                                                                                          na.rm=T))),
                                               ceiling(log10(max(temp$Value,na.rm=T))),by=1)) else
                                                 tick_nos<-pretty(temp$Value)
else tick_nos<-1}
temp$Value[temp$Value==0|is.na(temp$Value)]<-.5
    temp %>%
      ggvis(x = ~yday(CollectDate), y = ~Value,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~yday(CollectDate), y= ~Value,size := 75, size.hover := 200,fill:='red',stroke:='black',
                   key := ~SampleID,
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      layer_paths(x= ~yday(CollectDate), y= ~Value,stroke:='red',
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("x") %>%
      set_options(width = 650, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
      scale_numeric("y",trans=scale_setting(),domain=c(tick_nos[1],tick_nos[length(tick_nos)]),expand=0,nice=F) %>%
      add_axis("y", title = yvar_name,format='#,r',values=tick_nos)
  })

  vis.fecal_depth %>% bind_shiny("plot_fecal_depth")

vis.entero_depth<- reactive({
    temp<-subset(data(),grepl('Enterococcus',Parameter)&Depth>input$depth[1]&Depth<input$depth[2])
    # Lables for axes
    xvar_name <- 'Day of Year'
    yvar_name <- 'Enterococcus (CFU/100 mL)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),grepl('Enterococcus',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," CFU/100 mL"
  )
  }
{if(any(!is.na(temp$Value))) if(scale_setting()=='log') tick_nos<-10^(seq(floor(log10(min(temp$Value[temp$Value>0],
                                                                                          na.rm=T))),
                                               ceiling(log10(max(temp$Value,na.rm=T))),by=1)) else
                                                 tick_nos<-pretty(temp$Value)
else tick_nos<-1}
temp$Value[temp$Value==0|is.na(temp$Value)]<-.5
    temp %>%
      ggvis(x = ~yday(CollectDate), y = ~Value,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~yday(CollectDate), y= ~Value,size := 75, size.hover := 200,fill:='red',stroke:='black',
                   key := ~SampleID,
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      layer_paths(x= ~yday(CollectDate), y= ~Value,stroke:='red',
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("x") %>%
      set_options(width = 650, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
         scale_numeric("y",trans=scale_setting(),domain=c(tick_nos[1],tick_nos[length(tick_nos)]),expand=0,nice=F) %>%
      add_axis("y", title = yvar_name,format='#,r',values=tick_nos)
  })

  vis.entero_depth %>% bind_shiny("plot_entero_depth")
  
vis.coli_depth<- reactive({
    temp<-subset(data(),grepl('E. coli',Parameter)&Depth>input$depth[1]&Depth<input$depth[2])
    # Lables for axes
    xvar_name <- 'Day of Year'
    yvar_name <- 'E. coli (CFU/100 mL)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),grepl('E. coli',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," CFU/100 mL"
  )
  }
{if(any(!is.na(temp$Value))) if(scale_setting()=='log') tick_nos<-10^(seq(floor(log10(min(temp$Value[temp$Value>0],
                                                                                          na.rm=T))),
                                               ceiling(log10(max(temp$Value,na.rm=T))),by=1)) else
                                                 tick_nos<-pretty(temp$Value)
else tick_nos<-1}
temp$Value[temp$Value==0|is.na(temp$Value)]<-.5
    temp %>%
      ggvis(x = ~yday(CollectDate), y = ~Value,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~yday(CollectDate), y= ~Value,size := 75, size.hover := 200,fill:='red',stroke:='black',
                   key := ~SampleID,
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      layer_paths(x= ~yday(CollectDate), y= ~Value,stroke:='red',
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("x") %>%
      set_options(width = 650, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
         scale_numeric("y",trans=scale_setting(),domain=c(tick_nos[1],tick_nos[length(tick_nos)]),expand=0,nice=F) %>%
      add_axis("y", title = yvar_name,format='#,r',values=tick_nos)
  })

  vis.coli_depth %>% bind_shiny("plot_coli_depth")  

vis.ammonia_depth<- reactive({
    temp<-subset(data(),grepl('Ammonia',Parameter)&Depth>input$depth[1]&Depth<input$depth[2])
    # Lables for axes
    xvar_name <- 'Day of Year'
    yvar_name <- 'Ammonia (mg/L)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),grepl('Ammonia',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," mg/L"
  )
  }
{if(any(!is.na(temp$Value))) if(scale_setting()=='log') tick_nos<-10^(seq(floor(log10(min(temp$Value[temp$Value>0],
                                                                                          na.rm=T))),
                                               ceiling(log10(max(temp$Value,na.rm=T))),by=1)) else
                                                 tick_nos<-pretty(temp$Value)
else tick_nos<-1}
    temp %>%
      ggvis(x = ~yday(CollectDate), y = ~Value,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~yday(CollectDate), y= ~Value,size := 75, size.hover := 200,fill:='red',stroke:='black',
                   key := ~SampleID,
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      layer_paths(x= ~yday(CollectDate), y= ~Value,stroke:='red',
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("x") %>%
      set_options(width = 650, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
         scale_numeric("y",trans=scale_setting(),domain=c(tick_nos[1],tick_nos[length(tick_nos)]),expand=0,nice=F) %>%
      add_axis("y", title = yvar_name,format='#,r',values=tick_nos)
  })

  vis.ammonia_depth %>% bind_shiny("plot_ammonia_depth")    

vis.nitrate_depth<- reactive({
    temp<-subset(data(),grepl('Nitrate',Parameter)&Depth>input$depth[1]&Depth<input$depth[2])
    # Lables for axes
    xvar_name <- 'Day of Year'
    yvar_name <- 'Nitrate+Nitrite (mg/L)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),grepl('Nitrate',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," mg/L"
  )
  }
{if(any(!is.na(temp$Value))) if(scale_setting()=='log') tick_nos<-10^(seq(floor(log10(min(temp$Value[temp$Value>0],
                                                                                          na.rm=T))),
                                               ceiling(log10(max(temp$Value,na.rm=T))),by=1)) else
                                                 tick_nos<-pretty(temp$Value)
else tick_nos<-1}
    temp %>%
      ggvis(x = ~yday(CollectDate), y = ~Value,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~yday(CollectDate), y= ~Value,size := 75, size.hover := 200,fill:='red',stroke:='black',
                   key := ~SampleID,
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      layer_paths(x= ~yday(CollectDate), y= ~Value,stroke:='red',
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("x") %>%
      set_options(width = 650, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
         scale_numeric("y",trans=scale_setting(),domain=c(tick_nos[1],tick_nos[length(tick_nos)]),expand=0,nice=F) %>%
      add_axis("y", title = yvar_name,format='#,r',values=tick_nos)
  })

  vis.nitrate_depth %>% bind_shiny("plot_nitrate_depth")    

vis.ortho_depth<- reactive({
    temp<-subset(data(),grepl('Orthophosphate',Parameter)&Depth>input$depth[1]&Depth<input$depth[2])
    # Lables for axes
    xvar_name <- 'Day of Year'
    yvar_name <- 'Orthophosphate (mg/L)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),grepl('Orthophosphate',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," mg/L"
  )
  }
{if(any(!is.na(temp$Value))) if(scale_setting()=='log') tick_nos<-10^(seq(floor(log10(min(temp$Value[temp$Value>0],
                                                                                          na.rm=T))),
                                               ceiling(log10(max(temp$Value,na.rm=T))),by=1)) else
                                                 tick_nos<-pretty(temp$Value)
else tick_nos<-1}
    temp %>%
      ggvis(x = ~yday(CollectDate), y = ~Value,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~yday(CollectDate), y= ~Value,size := 75, size.hover := 200,fill:='red',stroke:='black',
                   key := ~SampleID,
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      layer_paths(x= ~yday(CollectDate), y= ~Value,stroke:='red',
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("x") %>%
      set_options(width = 650, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
         scale_numeric("y",trans=scale_setting(),domain=c(tick_nos[1],tick_nos[length(tick_nos)]),expand=0,nice=F) %>%
      add_axis("y", title = yvar_name,format='#,r',values=tick_nos)
  })

  vis.ortho_depth %>% bind_shiny("plot_ortho_depth")      

vis.tss_depth<- reactive({
    temp<-subset(data(),grepl('Total Suspended Solids',Parameter)&Depth>input$depth[1]&Depth<input$depth[2])
    # Lables for axes
    xvar_name <- 'Day of Year'
    yvar_name <- 'TSS (mg/L)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),grepl('Total Suspended Solids',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," mg/L"
  )
  }
{if(any(!is.na(temp$Value))) if(scale_setting()=='log') tick_nos<-10^(seq(floor(log10(min(temp$Value[temp$Value>0],
                                                                                          na.rm=T))),
                                               ceiling(log10(max(temp$Value,na.rm=T))),by=1)) else
                                                 tick_nos<-pretty(temp$Value)
else tick_nos<-1}
    temp %>%
      ggvis(x = ~yday(CollectDate), y = ~Value,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~yday(CollectDate), y= ~Value,size := 75, size.hover := 200,fill:='red',stroke:='black',
                   key := ~SampleID,
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      layer_paths(x= ~yday(CollectDate), y= ~Value,stroke:='red',
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("x") %>%
      set_options(width = 650, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
         scale_numeric("y",trans=scale_setting(),domain=c(tick_nos[1],tick_nos[length(tick_nos)]),expand=0,nice=F) %>%
      add_axis("y", title = yvar_name,format='#,r',values=tick_nos)
  })

  vis.tss_depth %>% bind_shiny("plot_tss_depth")
  
vis.sil_depth<- reactive({
    temp<-subset(data(),grepl('Silica',Parameter)&Depth>input$depth[1]&Depth<input$depth[2])
    # Lables for axes
    xvar_name <- 'Day of Year'
    yvar_name <- 'Silica (mg/L)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),grepl('Silica',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," mg/L"
  )
  }
{if(any(!is.na(temp$Value))) if(scale_setting()=='log') tick_nos<-10^(seq(floor(log10(min(temp$Value[temp$Value>0],
                                                                                          na.rm=T))),
                                               ceiling(log10(max(temp$Value,na.rm=T))),by=1)) else
                                                 tick_nos<-pretty(temp$Value)
else tick_nos<-1}
    temp %>%
      ggvis(x = ~yday(CollectDate), y = ~Value,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~yday(CollectDate), y= ~Value,size := 75, size.hover := 200,fill:='red',stroke:='black',
                   key := ~SampleID,
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      layer_paths(x= ~yday(CollectDate), y= ~Value,stroke:='red',
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("x") %>%
      set_options(width = 650, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
         scale_numeric("y",trans=scale_setting(),domain=c(tick_nos[1],tick_nos[length(tick_nos)]),expand=0,nice=F) %>%
      add_axis("y", title = yvar_name,format='#,r',values=tick_nos)
  })

  vis.sil_depth %>% bind_shiny("plot_sil_depth")  
  
vis.chla_depth<- reactive({
    temp<-subset(data(),Parameter=='Chlorophyll a'&Depth>input$depth[1]&Depth<input$depth[2])
    # Lables for axes
    xvar_name <- 'Day of Year'
    yvar_name <- 'Chlorophyll a (µg/L)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),Parameter=='Chlorophyll a'&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," mg/L"
  )
  }
{if(any(!is.na(temp$Value))) if(scale_setting()=='log') tick_nos<-10^(seq(floor(log10(min(temp$Value[temp$Value>0],
                                                                                          na.rm=T))),
                                               ceiling(log10(max(temp$Value,na.rm=T))),by=1)) else
                                                 tick_nos<-pretty(temp$Value)
else tick_nos<-1}
    temp %>%
      ggvis(x = ~yday(CollectDate), y = ~Value,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~yday(CollectDate), y= ~Value,size := 75, size.hover := 200,fill:='red',stroke:='black',
                   key := ~SampleID,
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      layer_paths(x= ~yday(CollectDate), y= ~Value,stroke:='red',
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("x") %>%
      set_options(width = 650, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
         scale_numeric("y",trans=scale_setting(),domain=c(tick_nos[1],tick_nos[length(tick_nos)]),expand=0,nice=F) %>%
      add_axis("y", title = yvar_name,format='#,r',values=tick_nos)
  })

  vis.chla_depth %>% bind_shiny("plot_chla_depth")
  
vis.pheo_depth<- reactive({
    temp<-subset(data(),Parameter=='Pheophytin a'&Depth>input$depth[1]&Depth<input$depth[2])
    # Lables for axes
    xvar_name <- 'Day of Year'
    yvar_name <- 'Pheophytin a (µg/L)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),Parameter=='Pheophytin a'&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," µg/L"
  )
  }
{if(any(!is.na(temp$Value))) if(scale_setting()=='log') tick_nos<-10^(seq(floor(log10(min(temp$Value[temp$Value>0],
                                                                                          na.rm=T))),
                                               ceiling(log10(max(temp$Value,na.rm=T))),by=1)) else
                                                 tick_nos<-pretty(temp$Value)
else tick_nos<-1}
    temp %>%
      ggvis(x = ~yday(CollectDate), y = ~Value,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~yday(CollectDate), y= ~Value,size := 75, size.hover := 200,fill:='red',stroke:='black',
                   key := ~SampleID,
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      layer_paths(x= ~yday(CollectDate), y= ~Value,stroke:='red',
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("x") %>%
      set_options(width = 650, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
         scale_numeric("y",trans=scale_setting(),domain=c(tick_nos[1],tick_nos[length(tick_nos)]),expand=0,nice=F) %>%
      add_axis("y", title = yvar_name,format='#,r',values=tick_nos)
  })

  vis.pheo_depth %>% bind_shiny("plot_pheo_depth")  
  
vis.chla_field_depth<- reactive({
    temp<-subset(data(),Parameter=='Chlorophyll, Field'&Depth>input$depth[1]&Depth<input$depth[2])
    # Lables for axes
    xvar_name <- 'Day of Year'
    yvar_name <- 'Chlorophyll a, Field (µg/L)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),Parameter=='Chlorophyll, Field'&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," mg/L"
  )
  }
{if(any(!is.na(temp$Value))) if(scale_setting()=='log') tick_nos<-10^(seq(floor(log10(min(temp$Value[temp$Value>0],
                                                                                          na.rm=T))),
                                               ceiling(log10(max(temp$Value,na.rm=T))),by=1)) else
                                                 tick_nos<-pretty(temp$Value)
else tick_nos<-1}
    temp %>%
      ggvis(x = ~yday(CollectDate), y = ~Value,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~yday(CollectDate), y= ~Value,size := 75, size.hover := 200,fill:='red',stroke:='black',
                   key := ~SampleID,
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      layer_paths(x= ~yday(CollectDate), y= ~Value,stroke:='red',
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("x") %>%
      set_options(width = 650, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
         scale_numeric("y",trans=scale_setting(),domain=c(tick_nos[1],tick_nos[length(tick_nos)]),expand=0,nice=F) %>%
      add_axis("y", title = yvar_name,format='#,r',values=tick_nos)
  })

  vis.chla_field_depth %>% bind_shiny("plot_chla_field_depth")   
  
vis.light_depth<- reactive({
    temp<-subset(data(),Parameter=='Light Transmissivity'&Depth>input$depth[1]&Depth<input$depth[2])
    # Lables for axes
    xvar_name <- 'Day of Year'
    yvar_name <- 'Light Transmissivity (%)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),Parameter=='Light Transmissivity'&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," %"
  )
  }
    temp %>%
      ggvis(x = ~yday(CollectDate), y = ~Value,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~yday(CollectDate), y= ~Value,size := 75, size.hover := 200,fill:='red',stroke:='black',
                   key := ~SampleID,
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      layer_paths(x= ~yday(CollectDate), y= ~Value,stroke:='red',
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("x") %>%
      set_options(width = 650, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
      scale_numeric("y") %>%
      add_axis("y", title = yvar_name)
  })

  vis.light_depth %>% bind_shiny("plot_light_depth")
  
  
vis.par_depth<- reactive({
    temp<-subset(data(),Parameter=='Light Intensity (PAR)'&Depth>input$depth[1]&Depth<input$depth[2])
    # Lables for axes
    xvar_name <- 'Day of Year'
    yvar_name <- 'Light Intensity (umol/sm2)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(data(),Parameter=='Light Intensity (PAR)'&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," umol/sm2"
  )
  }
    temp %>%
      ggvis(x = ~yday(CollectDate), y = ~Value,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~yday(CollectDate), y= ~Value,size := 75, size.hover := 200,fill:='red',stroke:='black',
                   key := ~SampleID,
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      layer_paths(x= ~yday(CollectDate), y= ~Value,stroke:='red',
                  data=subset(temp,CollectDate<=as.Date(input$date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$date,format='%Y-%m-%d')))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("x") %>%
      set_options(width = 650, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
      scale_numeric("y") %>%
      add_axis("y", title = yvar_name)
  })

  vis.par_depth %>% bind_shiny("plot_par_depth")   
          
  # Marine Intertidal ---------------------------------------------------------
 
  
it.webpage<-'http://dnrp-apps2test/Marine-Portal-Stewardship/Sites?SiteType=1&pageSize=1000'
it.sites<-readHTMLTable(readLines(it.webpage),
                    stringsAsFactors = FALSE)[[1]]

colnames(it.sites)=c('Details','SiteID','SiteName','Locator','Latitude',
                                                      'Longitude','Shallow','SiteType','Area')
sites.it_names<-paste(it.sites$Locator,'-',it.sites$SiteName)
sites.it<-it.sites$Locator
names(sites.it)<-sites.it_names

observe({
  updateSelectInput(session,"it.site",
    choices=sort(sites.it)
  )
})

it.siteID<-reactive({it.sites$SiteID[input$it.site==it.sites$Locator]})
observe({
#getlistofDates
it.site_for_dates<-paste0('http://dnrp-apps2test/Marine-Portal-Stewardship/Samples?Sites=',it.siteID(),
            '&Parms=22&pageSize=1000')
it.data.for.dates<-readHTMLTable(readLines(it.site_for_dates),
                    stringsAsFactors = FALSE)$sampleTable

if(is.data.frame(it.data.for.dates)){
  it.data.for.dates<-it.data.for.dates[grepl('WQ',it.data.for.dates[,1]),]
it.data.for.dates$Date<-as.Date(it.data.for.dates[,9],format='%m/%d/%Y')

  updateSelectInput(session,"it.date",
    choices=sort(unique(sort(it.data.for.dates$Date)),decreasing = T)
  )
}
})
it.data<-reactive({
# site<-paste0('http://dnrp-apps2test/Marine-Portal-Stewardship/WaterQuality?Sites=',it.siteID(),
#              
#              '&pageSize=1000')
# data<-readHTMLTable(readLines(site),
#                     stringsAsFactors = FALSE)$reviewWQTable
# if(is.null(data))  data<-data.frame(matrix(NA,1,36))
# colnames(data)<-c('ReviewExpand','SampleID','GrabID','ProfileID','LabSampleNum','CollectDate','CollectTime',
#                         'Depth_m','Matrix','SiteType','Area','Locator','Site','Parameter','Value','OverrideValue',
#                         'Units','Quality','Qualifier','MDL','RDL','TextValue','SampleInfo','Released','Reviewed',
#                         'Reviewer','StewardNote','Reps','WorkNumber','Method','ListID','DateAnalyzed',
#                         'UpdateDate','OverrideReason','OverrideDate','OverriddenBy')
site.json<-paste0('http://dnrp-apps2test/Marine-Portal-Stewardship/ReviewServices/SampleParms?Site=',it.siteID(),
                         '&RowLimit=100000')

data<-fromJSON(readLines(site.json))
data$CollectDate<-gsub('/Date\\(','',data$CollectDate)
data$CollectDate<-gsub(')/','',data$CollectDate)
data$CollectDate<-as.Date(as.POSIXct(as.numeric(data$CollectDate)/1000,origin="1970-01-01"))

data$Value<-as.numeric(data$Value)
data$MDL<-as.numeric(data$Mdl)
data$RDL<-as.numeric(data$Rdl)
data$Depth<-as.numeric(data$Depth)
#data$CollectDate<-as.Date(data$CollectDate,format='%m/%d/%Y')
data$Year<-year(data$CollectDate)
data$Month<-month(data$CollectDate)
data$SampleID<-as.character(data$SampleId)
data$LabSampleNum<-as.character(data$LabSampleNum)
data$Parameter<-as.character(data$ParmDisplayName)
data[order(data$CollectDate,data$Parameter),]
})

it.vis.fecal<- reactive({
    temp<-subset(it.data(),grepl('Fecal Coliform',Parameter))
    # Lables for axes
    xvar_name <- 'Day of Year'
    yvar_name <- 'Fecal Coliform (CFU/100 mL)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(it.data(),grepl('Fecal Coliform',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," CFU/100 mL"
  )
  }
{if(any(!is.na(temp$Value))) if(it.scale_setting()=='log') tick_nos<-10^(seq(floor(log10(min(temp$Value[temp$Value>0],
                                                                                          na.rm=T))),
                                               ceiling(log10(max(temp$Value,na.rm=T))),by=1)) else
                                                 tick_nos<-pretty(temp$Value)
else tick_nos<-1}
temp$Value[temp$Value==0|is.na(temp$Value)]<-.5
    temp %>% 
      ggvis(x = ~yday(CollectDate), y = ~Value,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~yday(CollectDate), y= ~Value,size := 75, size.hover := 200,fill:='red',stroke:='black',
                   key := ~SampleID,
                  data=subset(temp,CollectDate<=as.Date(input$it.date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$it.date,format='%Y-%m-%d')))) %>%
      layer_paths(x= ~yday(CollectDate), y= ~Value,stroke:='red',
                  data=subset(temp,CollectDate<=as.Date(input$it.date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$it.date,format='%Y-%m-%d')))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("x") %>%
      set_options(width = 650, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
      scale_numeric("y",trans=it.scale_setting(),domain=c(tick_nos[1],tick_nos[length(tick_nos)]),expand=0,nice=F) %>%
      add_axis("y", title = yvar_name,format='#,r',values=tick_nos)
  })

  it.vis.fecal %>% bind_shiny("plot_fecal_it")   

it.vis.entero<- reactive({
    temp<-subset(it.data(),grepl('Enterococcus',Parameter))
    # Lables for axes
    xvar_name <- 'Day of Year'
    yvar_name <- 'Enterococcus (CFU/100 mL)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(it.data(),grepl('Enterococcus',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," CFU/100 mL"
  )
  }
{if(any(!is.na(temp$Value))) if(it.scale_setting()=='log') tick_nos<-10^(seq(floor(log10(min(temp$Value[temp$Value>0],
                                                                                          na.rm=T))),
                                               ceiling(log10(max(temp$Value,na.rm=T))),by=1)) else
                                                 tick_nos<-pretty(temp$Value)
else tick_nos<-1}
temp$Value[temp$Value==0|is.na(temp$Value)]<-.5
    temp %>% 
      ggvis(x = ~yday(CollectDate), y = ~Value,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~yday(CollectDate), y= ~Value,size := 75, size.hover := 200,fill:='red',stroke:='black',
                   key := ~SampleID,
                  data=subset(temp,CollectDate<=as.Date(input$it.date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$it.date,format='%Y-%m-%d')))) %>%
      layer_paths(x= ~yday(CollectDate), y= ~Value,stroke:='red',
                  data=subset(temp,CollectDate<=as.Date(input$it.date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$it.date,format='%Y-%m-%d')))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("x") %>%
      set_options(width = 650, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
      scale_numeric("y",trans=it.scale_setting(),domain=c(tick_nos[1],tick_nos[length(tick_nos)]),expand=0,nice=F) %>%
      add_axis("y", title = yvar_name,format='#,r',values=tick_nos)
  })

  it.vis.entero %>% bind_shiny("plot_entero_it")     

it.vis.coli<- reactive({
    temp<-subset(it.data(),grepl('E. coli',Parameter))
    # Lables for axes
    xvar_name <- 'Day of Year'
    yvar_name <- 'E. coli (CFU/100 mL)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(it.data(),grepl('E. coli',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," CFU/100 mL"
  )
  }
{if(any(!is.na(temp$Value))) if(it.scale_setting()=='log') tick_nos<-10^(seq(floor(log10(min(temp$Value[temp$Value>0],
                                                                                          na.rm=T))),
                                               ceiling(log10(max(temp$Value,na.rm=T))),by=1)) else
                                                 tick_nos<-pretty(temp$Value)
else tick_nos<-1}
temp$Value[temp$Value==0|is.na(temp$Value)]<-.5
    temp %>% 
      ggvis(x = ~yday(CollectDate), y = ~Value,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~yday(CollectDate), y= ~Value,size := 75, size.hover := 200,fill:='red',stroke:='black',
                   key := ~SampleID,
                  data=subset(temp,CollectDate<=as.Date(input$it.date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$it.date,format='%Y-%m-%d')))) %>%
      layer_paths(x= ~yday(CollectDate), y= ~Value,stroke:='red',
                  data=subset(temp,CollectDate<=as.Date(input$it.date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$it.date,format='%Y-%m-%d')))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("x") %>%
      set_options(width = 650, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
      scale_numeric("y",trans=it.scale_setting(),domain=c(tick_nos[1],tick_nos[length(tick_nos)]),expand=0,nice=F) %>%
      add_axis("y", title = yvar_name,format='#,r',values=tick_nos)
  })

  it.vis.coli %>% bind_shiny("plot_coli_it")     
    
it.vis.ortho<- reactive({
    temp<-subset(it.data(),grepl('Orthophosphate',Parameter))
    # Lables for axes
    xvar_name <- 'Day of Year'
    yvar_name <- 'Orthophosphate (mg/L)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(it.data(),grepl('Orthophosphate',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," mg/L"
  )
  }
{if(any(!is.na(temp$Value))) if(it.scale_setting()=='log') tick_nos<-10^(seq(floor(log10(min(temp$Value[temp$Value>0],
                                                                                          na.rm=T))),
                                               ceiling(log10(max(temp$Value,na.rm=T))),by=1)) else
                                                 tick_nos<-pretty(temp$Value)
else tick_nos<-1}
temp %>% 
      ggvis(x = ~yday(CollectDate), y = ~Value,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~yday(CollectDate), y= ~Value,size := 75, size.hover := 200,fill:='red',stroke:='black',
                   key := ~SampleID,
                  data=subset(temp,CollectDate<=as.Date(input$it.date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$it.date,format='%Y-%m-%d')))) %>%
      layer_paths(x= ~yday(CollectDate), y= ~Value,stroke:='red',
                  data=subset(temp,CollectDate<=as.Date(input$it.date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$it.date,format='%Y-%m-%d')))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("x") %>%
      set_options(width = 650, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
      scale_numeric("y",trans=it.scale_setting(),domain=c(tick_nos[1],tick_nos[length(tick_nos)]),expand=0,nice=F) %>%
      add_axis("y", title = yvar_name,format='#,r',values=tick_nos)
  })

  it.vis.ortho %>% bind_shiny("plot_ortho_it")     

  it.vis.ammonia<- reactive({
    temp<-subset(it.data(),grepl('Ammonia',Parameter))
    # Lables for axes
    xvar_name <- 'Day of Year'
    yvar_name <- 'Ammonia (mg/L)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(it.data(),grepl('Ammonia',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," mg/L"
  )
  }
{if(any(!is.na(temp$Value))) if(it.scale_setting()=='log') tick_nos<-10^(seq(floor(log10(min(temp$Value[temp$Value>0],
                                                                                          na.rm=T))),
                                               ceiling(log10(max(temp$Value,na.rm=T))),by=1)) else
                                                 tick_nos<-pretty(temp$Value)
else tick_nos<-1}
temp %>% 
      ggvis(x = ~yday(CollectDate), y = ~Value,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~yday(CollectDate), y= ~Value,size := 75, size.hover := 200,fill:='red',stroke:='black',
                   key := ~SampleID,
                  data=subset(temp,CollectDate<=as.Date(input$it.date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$it.date,format='%Y-%m-%d')))) %>%
      layer_paths(x= ~yday(CollectDate), y= ~Value,stroke:='red',
                  data=subset(temp,CollectDate<=as.Date(input$it.date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$it.date,format='%Y-%m-%d')))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("x") %>%
      set_options(width = 650, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
      scale_numeric("y",trans=it.scale_setting(),domain=c(tick_nos[1],tick_nos[length(tick_nos)]),expand=0,nice=F) %>%
      add_axis("y", title = yvar_name,format='#,r',values=tick_nos)
  })

  it.vis.ammonia %>% bind_shiny("plot_ammonia_it")     

it.vis.nitrate<- reactive({
    temp<-subset(it.data(),grepl('Nitrate',Parameter))
    # Lables for axes
    xvar_name <- 'Day of Year'
    yvar_name <- 'Nitrate+Nitrite (mg/L)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(it.data(),grepl('Nitrate',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," mg/L"
  )
  }
{if(any(!is.na(temp$Value))) if(it.scale_setting()=='log') tick_nos<-10^(seq(floor(log10(min(temp$Value[temp$Value>0],
                                                                                          na.rm=T))),
                                               ceiling(log10(max(temp$Value,na.rm=T))),by=1)) else
                                                 tick_nos<-pretty(temp$Value)
else tick_nos<-1}
temp %>% 
      ggvis(x = ~yday(CollectDate), y = ~Value,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~yday(CollectDate), y= ~Value,size := 75, size.hover := 200,fill:='red',stroke:='black',
                   key := ~SampleID,
                  data=subset(temp,CollectDate<=as.Date(input$it.date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$it.date,format='%Y-%m-%d')))) %>%
      layer_paths(x= ~yday(CollectDate), y= ~Value,stroke:='red',
                  data=subset(temp,CollectDate<=as.Date(input$it.date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$it.date,format='%Y-%m-%d')))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("x") %>%
      set_options(width = 650, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
      scale_numeric("y",trans=it.scale_setting(),domain=c(tick_nos[1],tick_nos[length(tick_nos)]),expand=0,nice=F) %>%
      add_axis("y", title = yvar_name,format='#,r',values=tick_nos)
  })

  it.vis.nitrate %>% bind_shiny("plot_nitrate_it")     
         
it.vis.temp<- reactive({
    temp<-subset(it.data(),grepl('Temperature',Parameter))
    # Lables for axes
    xvar_name <- 'Day of Year'
    yvar_name <- 'Temperature (°C)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(it.data(),grepl('Temperature',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," °C"
  )
  }

temp %>% 
      ggvis(x = ~yday(CollectDate), y = ~Value,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~yday(CollectDate), y= ~Value,size := 75, size.hover := 200,fill:='red',stroke:='black',
                   key := ~SampleID,
                  data=subset(temp,CollectDate<=as.Date(input$it.date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$it.date,format='%Y-%m-%d')))) %>%
      layer_paths(x= ~yday(CollectDate), y= ~Value,stroke:='red',
                  data=subset(temp,CollectDate<=as.Date(input$it.date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$it.date,format='%Y-%m-%d')))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("x") %>%
      set_options(width = 650, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
      scale_numeric("y") %>%
      add_axis("y", title = yvar_name)
  })

  it.vis.temp %>% bind_shiny("plot_temp_it")  

it.vis.salinity<- reactive({
    temp<-subset(it.data(),grepl('Salinity',Parameter))
    # Lables for axes
    xvar_name <- 'Day of Year'
    yvar_name <- 'Salinity (PPS)'

point_tooltip <- function(x) {
    if (is.null(x)) return(NULL)

    # Pick out the movie with this ID
       sample <- subset(it.data(),grepl('Salinity',Parameter)&SampleID==x$SampleID)
    paste0("<b>", paste0('<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         sample$LabSampleNum),'">',sample$LabSampleNum, "</a></b><br>",
      as.Date(sample$CollectDate,'%Y-%m-%d'), "<br>",
      sample$Value," °C"
  )
  }

temp %>% 
      ggvis(x = ~yday(CollectDate), y = ~Value,key := ~SampleID) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      layer_points(x= ~yday(CollectDate), y= ~Value,size := 75, size.hover := 200,fill:='red',stroke:='black',
                   key := ~SampleID,
                  data=subset(temp,CollectDate<=as.Date(input$it.date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$it.date,format='%Y-%m-%d')))) %>%
      layer_paths(x= ~yday(CollectDate), y= ~Value,stroke:='red',
                  data=subset(temp,CollectDate<=as.Date(input$it.date,format='%Y-%m-%d')&
                                year(CollectDate)==year(as.Date(input$it.date,format='%Y-%m-%d')))) %>%
      add_tooltip(point_tooltip, "click") %>%
      scale_numeric("x") %>%
      set_options(width = 650, height = 350) %>%
      add_axis("x", title = xvar_name) %>%
      scale_numeric("y") %>%
      add_axis("y", title = yvar_name)
  })

  it.vis.salinity %>% bind_shiny("plot_salinity_it")    
    

# Marine Map by Depth -----------------------------------------------------
output$comparison_map <- renderLeaflet({
leaflet() %>%
    # addTiles(
    #     urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
    #     attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    #   ) %>%
    addProviderTiles('Esri.WorldGrayCanvas') %>%
           #  addTiles(urlTemplate =
           #      'http://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}', 
           # attribution = '') %>%
    #addTiles() %>%
    setView(lat=47.58, lng=-122.1,zoom=9)
})
# observe({  
# site_for_map_dates_off<-paste0('http://dnrp-apps2test/Marine-Portal-Stewardship/WaterQuality?Parms=22&MinDepth=0.00&',
#                                'MaxDepth=2.00&SiteType=2&pageSize=1000')
# data.for.map_dates_off<-readHTMLTable(readLines(site_for_dates),
#                     stringsAsFactors = FALSE)$sampleTable
# site_for_map_dates_it<-paste0('http://dnrp-apps2test/Marine-Portal-Stewardship/WaterQuality?Parms=22&',
#                                'SiteType=1&pageSize=1000')
# data.for.map_dates_it<-readHTMLTable(readLines(site_for_dates),
#                     stringsAsFactors = FALSE)$sampleTable
# 
# 
# dates_for_map<-as.Date(c(data.for.map_dates_off[,9],data.for.map_dates_it[,9]),format='%m/%d/%Y')
# 
#   updateSelectInput(session,"date_map",
#     choices=sort(unique(sort(dates_for_map)),decreasing = T)
#   )
# }) 
map_data<-reactive({

# site<-paste0('http://dnrp-apps2test/Marine-Portal-Stewardship/WaterQuality?MinDate=',format(input$date_map[1]),
#                '&MaxDate=',format(input$date_map[2]),'&Parms=',input$parm_comp,'&SiteType=2&MinDepth=',
#              input$depth_map[1],
#              '&MaxDepth=',input$depth_map[2],'&pageSize=1000')
# data<-readHTMLTable(readLines(site),
#                     stringsAsFactors = FALSE)$reviewWQTable
site<-paste0('http://dnrp-apps2test/Marine-Portal-Stewardship/ReviewServices/SampleParms?',
      'MinDate=',format(input$date_map[1]),'&MaxDate=',format(input$date_map[2]),
      '&MinDepth=',input$depth_map[1],'&MaxDepth=',input$depth_map[2],
      '&Parm=',input$parm_comp,'&SiteType=2&RowLimit=10000')
data<-fromJSON(readLines(site))
  
if(input$depth_map[1]<2){
# site_it<-paste0('http://dnrp-apps2test/Marine-Portal-Stewardship/WaterQuality?MinDate=',format(input$date_map[1]),
#                '&MaxDate=',format(input$date_map[2]),'&Parms=',input$parm_comp,'&SiteType=1&pageSize=1000')
# data_it<-readHTMLTable(readLines(site_it),
#                     stringsAsFactors = FALSE)$reviewWQTable
site_it<-paste0('http://dnrp-apps2test/Marine-Portal-Stewardship/ReviewServices/SampleParms?',
      'MinDate=',format(input$date_map[1]),'&MaxDate=',format(input$date_map[2]),
      #'&MinDepth=',input$depth_map[1],'&MaxDepth=',input$depth_map[2],
      '&Parm=',input$parm_comp,'&SiteType=1&RowLimit=10000')
data_it<-fromJSON(readLines(site_it))  
 data<-rbind(data,data_it)
  }
#   
if(!is.data.frame(data))  {data<-data.frame(matrix(NA,1,19))
colnames(data)<-c("SampleId", "ParmId", "ParmDisplayName", "Value", "OverrideValue", 
"Units", "Mdl", "Rdl", "QualityId", "QfrCode", "TextValue", "CollectDate", 
"LabSampleNum", "Depth", "SampleParmsReleased", "IsMarine", "Locator", 
"SiteId", "SiteTypeId")
}
data$CollectDate<-gsub('/Date\\(','',data$CollectDate)
data$CollectDate<-gsub(')/','',data$CollectDate)
data$CollectDate<-as.Date(as.POSIXct(as.numeric(data$CollectDate)/1000,origin="1970-01-01"))
data$Value<-as.numeric(data$Value)
data$MDL<-as.numeric(data$Mdl)
data$RDL<-as.numeric(data$Rdl)
data$Depth<-as.numeric(data$Depth)
#data$CollectDate<-as.Date(data$CollectDate,format='%m/%d/%Y')
data$Year<-year(data$CollectDate)
data$Month<-month(data$CollectDate)
data$SampleID<-as.character(data$SampleId)
data$LabSampleNum<-as.character(data$LabSampleNum)
data$Parameter<-as.character(data$ParmDisplayName)
data$Value<-with(data,ifelse(is.na(Value),MDL,Value))

data<-merge(data,rbind(off.sites,it.sites),by='Locator')
data$Latitude<-as.numeric(data$Latitude)
data$Longitude<-as.numeric(data$Longitude)
data[order(data$CollectDate,data$Parameter),]
})
observe({
    temp<-map_data()
    if(nrow(temp)>0&!all(is.na(temp$Value))){
    output$MAPDATA<-renderText('')
    domain<-c(min(temp$Value,na.rm=T)*.9,max(temp$Value,na.rm=T)*1.1)
    pal<-colorNumeric(c('blue','yellow','red'),domain=domain)
    temp$size<-rescale(temp$Value,to=c(3.5,12))
    leafletProxy('comparison_map',data=temp) %>% clearMarkers() %>% clearControls() %>%
      addCircleMarkers(lng=~Longitude,lat=~Latitude,color=~pal(Value),radius=~size,stroke=F,fillOpacity = .9,
             popup=~paste0('<b>',SiteName,'</b><br>',CollectDate,'<br>',Value,' ', temp$Units[1],'<br>',
                           "Depth: ",Depth,' m<br>',#Qualifier,'<br>',
                           '<a href="http://dnrp-apps2test/Marine-Portal-Stewardship/Sample/Edit/?lsn=',
                         LabSampleNum,'">',LabSampleNum, "</a></b>")) %>%
      addLegend("bottomright", pal = pal, values = domain[1]:domain[2],
      title = names(input$parm_comp),
      labFormat = labelFormat(suffix = temp$Units[1]),
      opacity = 1
  )
    }else {output$MAPDATA<-renderText('NO DATA OR DETECTS IN DATE RANGE')
    leafletProxy('comparison_map') %>% clearMarkers() %>% clearControls() 
    }
  })  
}




