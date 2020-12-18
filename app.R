library(shiny)

if(!require(data.table))
  install.packages("data.table")
library(data.table)

if(!require(dplyr))
  install.packages("dplyr")
library(dplyr)

if(!require(tidyr))
  install.packages("tidyr")
library(tidyr)

if(!require(stringi))
  install.packages("stringi")
library(stringi)

if(!require(ggplot2))
  install.packages("ggplot2")
library(ggplot2)

if(!require(ggraph))
  install.packages("ggraph")
library(ggraph)

if(!require(tidygraph))
  install.packages("tidygraph")
library(tidygraph)

if(!require(rtweet))
  install.packages("rtweet")
library(rtweet)

if(!require(leaflet))
  install.packages("leaflet")
library(leaflet)

if(!require(leafletCN))
  install.packages("leafletCN")
library(leafletCN)

if(!require(maptools))
  install.packages("maptools")
library(maptools)

if(!require(sf))
  install.packages("sf")
library(sf)

if(!require(rsconnect))
  install.packages("rsconnect")
library(rsconnect)

load("Tweet_HR_R02_data4.RData")

pal <- colorNumeric(palette="Blues", domain=c(-10,max(AME_D$Rank)))
pal2 <- colorNumeric(palette="Blues", domain=c(-10,max(AME_DH$Rank)))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Tweet"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    fluidRow(
      column(1,selectInput("Month",
                           label = "Month",
                           choices = unique(7),
                           selected = 7),offset = 1),
      column(1,selectInput("Day",
                           label = "Day",
                           choices = c(unique(1:10),"Auto"),
                           selected = 6)),
      column(1,selectInput("Hour",
                           label = "Hour",
                           choices = c("-",unique(0:23),"Auto"),
                           selected = "16")),
      column(1,selectInput("TDFK",
                           label = "Prefectures",
                           choices = TDFK$Jname,
                           selected = TDFK$Jname[40]))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      column(6,leafletOutput("map",height="400px"),
             plotOutput("plot2",height="400px")),
      column(6,plotOutput("plot",height="800px")),
      width = 12
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  refreshPlot <- reactiveTimer(intervalMs = 5000)
  vals0 <- reactiveValues(counter = 1)
  vals <- reactiveValues(counter = 0)
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = "http://cyberjapandata.gsi.go.jp/xyz/pale/{z}/{x}/{y}.png") %>%
      setView(lng=137.6850225,lat=33.58333,zoom=6) %>%
      addPolygons(data=jpn, fill=F, weight = 1, opacity = 1, color="black")
  })
  
  output$plot <- renderPlot({
    m=input$Month
    d=input$Day
    h=input$Hour
    
    if(input$Day=="Auto"|input$Hour=="Auto"){
      refreshPlot()
      if(isolate(vals$counter)>=24){
        vals$counter <- 0
        if(input$Day=="Auto")
          vals0$counter <- isolate(vals0$counter) + 1
      }
      if(isolate(vals0$counter)>10)
        vals0$counter <- 1
      
      if(input$Day=="Auto")
        d=isolate(vals0$counter)
      if(input$Hour=="Auto")
        h=isolate(vals$counter)
      
      # vals$counter <- isolate(vals$counter) + 1
      # if(input$Day=="Auto"&input$Hour!="Auto")
      #   vals0$counter <- isolate(vals0$counter) + 1
    }
    
    if(h!="-"){
      TFS <-
        TFSSS %>%
        filter(Month==m,Day==d,Hour==h)
      
      rk=100
      cln=10
      
      TFS0 <-
        TFS %>%
        distinct(word1,word2,freq,rate,Ranks,rate1,rate2,word) %>%
        
        arrange(Ranks,desc(rate1),desc(rate2)) %>%
        # filter(freq>1) %>%
        filter(Ranks<=rk) %>%
        arrange(desc(rate1),desc(rate2)) %>%
        # filter(rate1>=0.5|rate2>=0.5) %>%
        # filter(!(Ranks==max(Ranks)&rate1<0.1)) %>%
        mutate(num=1:n()) %>%
        filter(num<=100)
      
      TFSS <-
        TFS %>%
        filter(word %in% TFS0$word)
      
      k=length(unique(TFSS$Tweet))
      
      TFS1 <-
        rbind(TFSS %>%
                select(word=word1,Tweet,N)) %>%
        rbind(TFSS %>%
                select(word=word2,Tweet,N)) %>%
        distinct(word,Tweet,.keep_all = T) %>%
        spread(word,N,fill=0)
      
      TFS2 <-
        TFS1 %>%
        select(-Tweet) %>%
        distinct() %>%
        t()
      
      TFS2_d <- dist(TFS2)
      TFS2_hc <- hclust(TFS2_d, method = "ward.D2") 
      # plot(TFS2_hc)
      
      TFS2_cl <- cutree(TFS2_hc, k=min(k,cln)) %>%
        data.frame() %>%
        rename(Cluster=".") %>%
        add_rownames("word")
      
      TFS3 <-
        TFS %>%
        filter(word %in% TFS0$word) %>%
        # filter((rate1>=rt)|(rate2>=rt)) %>%
        arrange(JTime,Tweet) %>%
        # filter(word1!=word2) %>%
        inner_join(TFS2_cl,by=c("word1"="word")) %>%
        inner_join(TFS2_cl,by=c("word2"="word")) %>%
        mutate(Stime=as.POSIXct(paste(Year,Month,Day,Hour),format="%Y %m %d %H"))
      
      k=length(unique(TFS3$Tweet))
      
      TFS00 <-
        TFS3 %>%
        distinct(word1,word2,freq,rate,Ranks,rate1,rate2,word) %>%
        arrange(Ranks)
      
      TFS3_cl <-
        data.frame() %>%
        rbind(TFS3 %>%
                select(word=word1,Freq=Freq.x,Cluster=Cluster.x)) %>%
        rbind(TFS3 %>%
                select(word=word2,Freq=Freq.y,Cluster=Cluster.y)) %>%
        distinct() %>%
        mutate(Cluster=factor(Cluster))
      
      g <- as_tbl_graph(TFS00, directed = T) %>%
        left_join(TFS3_cl,by=c("name"="word")) %>%
        left_join(ED,by=c("name"="Unicode")) %>%
        mutate(name=ifelse(!is.na(code),code,name))
      
      G <- data.frame(g)
      MN=max(G$Freq)
      keta=nchar(MN)-1
      br<- seq(10^keta,floor(MN/10^keta)*10^keta,10^keta)
      if(length(br)==1){
        keta=keta-1
        br<- seq(10^keta,floor(MN/10^keta)*10^keta,10^keta)
      }
      #####    
      p<-
        g %>%
        ggraph(layout ="nicely") +
        # geom_edge_link(aes(alpha = rate1,width = rate),color="royalblue", #
        #                arrow = arrow(length = unit(5,'mm')), end_cap = circle(10,'mm'),force_flip = F) +
        geom_edge_link(aes(width = rate, alpha = rate1),color="royalblue", #
                       arrow = arrow(length = unit(3,'mm')), end_cap = circle(5,'mm'),force_flip = F) +
        geom_node_point(aes(col = Cluster, size = Freq)) +
        geom_node_text(aes(label = name), repel = F, size=7.5) +
        ggtitle(paste0("",min(TFS3$JTime),"~",max(TFS3$JTime),"\n",nrow(TFS00),"rules")) +
        theme_graph(title_size = 30) +
        scale_edge_alpha(range = c(0,1)) +
        scale_size_continuous(range = c(5,30),breaks = c(min(br),max(br))) + #,breaks = seq(0,floor(mn/keta)*keta,by = keta)
        theme( legend.text =  element_text(size = 20),
               legend.title = element_text(face = "bold", size = 20, hjust = 0)) +
        guides(alpha = guide_legend(title = "LEFT", title.position = "left")) +
        guides(colour = guide_legend(order=1 , override.aes = list(size=10)),
               size   = guide_legend(order=2),
               edge_width = F,
               edge_alpha = F) +
        scale_colour_manual(values = ggColorHue(max(TFS3$Cluster.x,TFS3$Cluster.y)),drop=F,breaks=unique(sort(G$Cluster)))
      
      
      plot(p)
    }
    
  })
  
  observe({
    m=input$Month
    d=input$Day
    h=input$Hour
    
    # m=7
    # d=6
    # h="-"
    # h=1
    
    if(h=="-"){
      RainS <-
        AME_D %>%
        filter(Month==m,Day==d) %>%
        mutate(cl=pal(Rank))
    }
    
    if(input$Day=="Auto"|input$Hour=="Auto"){
      refreshPlot()
      
      if(isolate(vals$counter)>=24){
        vals$counter <- 0
        if(input$Day=="Auto")
          vals0$counter <- isolate(vals0$counter) + 1
      }
      if(isolate(vals0$counter)>10)
        vals0$counter <- 1
      
      
      if(input$Day=="Auto")
        d=isolate(vals0$counter)
      if(input$Hour=="Auto")
        h=isolate(vals$counter)
      
      
      vals$counter <- isolate(vals$counter) + 1
      if(input$Day=="Auto"&input$Hour!="Auto")
        vals0$counter <- isolate(vals0$counter) + 1
    }
    
    if(h!="-"){
    RainS <-
      AME_DH %>%
      filter(Month==m,Day==d,Hour==h) %>%
      mutate(cl=pal2(Rank))
    }
    
    if(max(RainS$cRank)==0)
      RainS <-
      RainS %>%
      mutate(cRank=ifelse(Rain==max(Rain),floor(max(Rain)),0))
    
    leafletProxy("map") %>%
      clearControls() %>%
      removeShape(layerId=paste0("X",1:100000)) %>%
      addCircles(data=RainS,lng=~lng,lat=~lat, color=~cl, stroke=FALSE,fillOpacity = 1,radius = 10000,
                 label = ~paste0(Jname," ",Area," ",Rain,"mm"), labelOptions = list(textsize="20px"),
                 layerId=paste0("X",1:nrow(RainS))) %>%
      addLegend(data=RainS %>% distinct(cRank,.keep_all = T), position='topright', color=~cl,labels = ~cRank,
                title = paste0(m,"/",d," ",h,":00","<br>","(mm)"))
  })
  
  output$plot2 <- renderPlot({
    tdfk=input$TDFK
    
    tfk=TDFK[TDFK$Jname==tdfk]$jn
    tf=TDFK[TDFK$Jname==tdfk]$Name
    
    RainS <-
      AME_DH %>%
      left_join(TDFK %>% select(Jname,Name,jn)) %>%
      filter(Jname %in% tdfk) %>%
      group_by(Year,Month,Day,Hour) %>%
      summarise(m=max(Rain)) %>%
      right_join(MDH) %>%
      complete(Year,Month,Day,Hour,fill=list(m=0)) %>%
      mutate(Time=as.POSIXct(paste(Year,Month,Day,Hour),format="%Y %m %d %H"))
    
    b=35
    TDSC <-
      TDS2 %>%
      ungroup() %>%
      left_join(TDFK %>% select(Jname,Name,jn),by=c("Tdfk"="jn")) %>%
      filter(Jname %in% tdfk) %>%
      complete(Year,Month,Day,Hour,fill=list(n=0)) %>%
      left_join(RainS) %>%
      mutate(m1=m*10) %>%
      mutate(m2=m*b)
    
    l=c(0,max(100,max(TDSC$n),max(TDSC$m),3600))
    
    TDSC %>%
      filter(Day<10) %>%
      ggplot(aes(x=Time,y=n)) +
      geom_area(fill=ggColorHue(1)) +
      geom_text(data = TDSC %>% filter(n==max(n)),
                aes(y=n+100,label=format(Time,"%H:00")),size=5) +
      geom_line(aes(y=m2),size=1,col="blue") +
      labs(x="",y="Count",fill="") +
      scale_y_continuous(breaks = seq(0,1000000,500),expand = c(0, 0),limits=l,
                         sec.axis = sec_axis(~ . / b, name = "Rain (mm)",seq(0,1000,5))) + #
      scale_x_datetime(date_breaks = "1 day",date_minor_breaks = "1 hour",date_labels = "%d") +
      ggtitle(paste(tf,"Tweet count & Precipitation")) +
      theme(text = element_text(size=30)) +
      theme(axis.text.y = element_text(size=20)) +
      theme(axis.title.x = element_blank()) +
      theme(legend.position = "")
  })
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)







