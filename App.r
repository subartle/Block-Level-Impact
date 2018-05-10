library(shiny)
library(shinythemes)
library(httpuv)
library(leaflet)
library(tigris)
library(acs)
library(maptools)
library(base)
library(dygraphs)
library(png)
library(plotly)
library(rgeos)
library(tidyverse)
library(rgdal)
library(ggthemes)
library(DT)
library(ggplot2)
library(corrplot)

Dat.ABGroups <-
  read.csv("https://raw.githubusercontent.com/subartle/Block-Level-Impact/master/Block_Level_Data.csv")

Dat.Tran <- read.csv("https://raw.githubusercontent.com/subartle/Block-Level-Impact/master/Transposed.csv", header = T, check.names=F)

Dat.Hist <- read.csv("https://raw.githubusercontent.com/subartle/Block-Level-Impact/master/Histogram.csv", header = T, check.names=F)

Dat.Hist2 <- read.csv("https://raw.githubusercontent.com/subartle/Block-Level-Impact/master/Histogram2.csv", header = T, check.names=F)

Dat.Corr <- read.csv("https://raw.githubusercontent.com/subartle/Block-Level-Impact/master/Completed_0413208.csv", header = T, check.names=F)

Dat.legend <- read.csv("https://raw.githubusercontent.com/subartle/Block-Level-Impact/master/Legend.csv", header = T, check.names=F)

#get rid of fat columns
Dat.Hist$Label <- as.character(Dat.Hist$Label)
Dat.Hist$Count <- as.numeric(as.character(Dat.Hist$Count))
Dat.Hist <- na.omit(Dat.Hist)

#Clean up Dat.Hist
Dat.Hist.Joiner <- Dat.Hist[c(1:51),c(3:4)]
Dat.Hist.Joiner$GEOID <- as.character(Dat.Hist.Joiner$GEOID)

Dat.Hist <- merge(Dat.Hist2, Dat.Hist.Joiner, by.x = "CBG", by.y = "CBG", all.x = TRUE)
Dat.Hist$GEOID <- Dat.Hist$GEOID.y

#get rid of fat columns
Dat.Hist$Label <- as.character(Dat.Hist$Label)
Dat.Hist$Count <- as.numeric(as.character(Dat.Hist$Count))

#set rowlables for Dat.Tran and get rid of 1st column
rownames(Dat.Tran) <- Dat.Tran$`Census Block`
Dat.Tran <- Dat.Tran[,c(2:52)]

#rownames(Dat.Corr) <- Dat.Corr$`Row Labels`
Dat.Corrt <- Dat.Corr[,c(3,4,5,6,7,9,10,13,14,16)]
colnames(Dat.Corrt) <- c("AS 2017", "Change AS", "Per. Change AS", 
                         "OCV 1 yr+", "OCV", "Avg Days to Close", 
                         "Condemnations", "Per. Single Fam", "Per. Rental", 
                         "Per. City Res. Owned")
Dat.Corrt <- na.omit(Dat.Corrt)
Mt <- cor(Dat.Corrt)

#download block groups into R
LancasterCounty.BlockGroups <- block_groups(state = 42, county = 071, cb = TRUE)
Lancaster.BlockGroups <- LancasterCounty.BlockGroups[LancasterCounty.BlockGroups$GEOID == "420710147003" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710147001" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710118052" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710014004" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710014003" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710014002" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710014001" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710012004" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710012003" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710012002" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710012001" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710001001" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710001002" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710001003" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710002001" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710002002" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710003001" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710003002" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710003003" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710003004" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710004001" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710004002" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710004003" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710004004" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710005001" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710005002" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710005003" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710005004" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710006001" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710006002" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710006003" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710006004" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710007001" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710007002" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710007003" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710008001" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710008002" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710008003" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710008004" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710009001" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710009002" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710009003" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710009004" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710010002" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710010001" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710010003" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710010004" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710011001" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710011002" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710147002" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710011003" ,]


Dat.ABGroups$GEOID <- as.character(Dat.ABGroups$GEOID1)

Dat.ABGroups$PercentChange <- Dat.ABGroups$Actual_Change/Dat.ABGroups$AS_2007
Dat.ABGroups$Lat2 <- Dat.ABGroups$Lat*-1

LancasterBGs <- merge(Lancaster.BlockGroups, Dat.ABGroups, by.x = "GEOID", by.y = "GEOID", all.x = TRUE)

ui <- fluidPage(theme = shinytheme("flatly"),
                mainPanel(
                  h3("Measuring Impact"),
                  h4("Block Profiles"),
                  selectInput('inputblockprofiles', 'Which census blocks would you like to see?', 
                              choices = colnames(Dat.Tran), selectize=FALSE),
                  fixedRow(column(7, DT::dataTableOutput("blockprofiles")),
                           column(5, leafletOutput("referencemap", height = "700", width = "400"))),
                  fixedRow(h4("Variable Profile"),
                           selectInput('histograminput', 'Which variable would you like to see?',
                                       choices = unique(Dat.Hist$Label), width = "500", 
                                       selectize = FALSE)),
                  fixedRow(column(5, plotOutput("histogram")),
                           column(7, leafletOutput("mymap"))),
                  h4("Correlations"),
                  h5("How to read:"),
                  h5("The closer the number is to 1, the stronger the correlation between the 
                     two variables. On that same note - the closer the number is to 0, the weaker the correlation 
                     (if it's too weak, you should ignore). Some of the correlations should be ignored, as the 
                     two variables relationships are obvious (i.e. Percent Change in Average Sale Amount and 
                     Total $ Amount Change in Average Sale have a strong positive correlation becuase they are essentially the same variable). 
                     If you need help understanding the labels or where the data came from, see the legend below. 
                     Also - be wary of correlations! They don't necessarily mean that one variable is causing 
                     the other. Use these correlations as an indicator of areas for further research!!!"),
                  fixedRow(column(4, selectInput('corrselect', "Choose your visualization:", 
                                                 choices = c("circle", "number"), selectize = FALSE)),
                           column(8, plotOutput("corr"))),
                  fixedRow(
                    h4("Legend")),
                  fixedRow(DT::dataTableOutput("legend"))))


server <- function(input, output, session) {
  
  output$referencemap <- renderLeaflet({
    
    Experiment <- Dat.ABGroups[Dat.ABGroups$CensusBlockGroup == input$inputblockprofiles,]
    
    leaflet(LancasterBGs) %>%
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
      setView(Experiment$Lat, Experiment$Lon, zoom = 16) %>%
      addPolygons(weight = 4,
                  opacity = 1,
                  color = "black",
                  dashArray = "2",
                  fillOpacity = .1) %>%
      addLabelOnlyMarkers(lng = Dat.ABGroups$Lat, 
                          lat = Dat.ABGroups$Lon, 
                          label = as.character(Dat.ABGroups$CensusBlockGroup),
                          labelOptions = labelOptions(noHide = T, textOnly = TRUE,
                                                      style = list("color" = "firebrick",
                                                                   "font-size" = "12px")))
    
  })
  
  output$histogram <- renderPlot({
    Dat.Hist.Filt <- Dat.Hist[Dat.Hist$Label == input$histograminput,]
    
    p <- ggplot(Dat.Hist.Filt, aes(Dat.Hist.Filt$Count)) +
      stat_density(aes(group = Dat.Hist.Filt$Label, color = "darkblue"),
                   position ="identity",geom="line") +
      geom_density(color="black", fill = "darkcyan", alpha = 0.4) +
      geom_vline(aes(xintercept=mean(Dat.Hist.Filt$Count)),
                 color="darkcyan", linetype="dashed", size=1) + 
      theme(panel.background = element_rect(fill = '#ffffff'), legend.position="none") +
      scale_x_continuous(name = paste("Citywide Distribution of ", unique(Dat.Hist.Filt$Label)),
                         breaks=seq(min(Dat.Hist.Filt$Count, na.rm=TRUE),
                                    max(Dat.Hist.Filt$Count, na.rm=TRUE),
                                    max(Dat.Hist.Filt$Count, na.rm=TRUE)/5)) +
      labs(y = NULL)
    
    print(p)
  })
  
  output$mymap <- renderLeaflet({
    Dat.Hist3 <-Dat.Hist[Dat.Hist$Label == input$histograminput,]
    LancasterBGs2 <- merge(Lancaster.BlockGroups, Dat.Hist3, by.x = "GEOID", by.y = "GEOID", all.x = TRUE)
    
    qpal <- colorBin("YlGnBu", domain = LancasterBGs2$Count, bins = 4)
    
    leaflet(LancasterBGs2) %>%
      addProviderTiles(providers$Stamen.Toner) %>%
      setView(-76.28978, 40.04199, zoom = 13) %>%
      addPolygons(fillColor = ~qpal(Count),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.6,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "white",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE)) %>% 
      addLegend(pal = qpal, values = ~Count, opacity = 0.7, title = unique(LancasterBGs2$Label),
                position = "bottomright")
    
    
  })
  
  output$blockprofiles <- DT::renderDataTable({
    DT::datatable(Dat.Tran[,input$inputblockprofiles, drop = FALSE], options = list(lengthMenu = c(4,10,16), pageLength = 16))
  })
  
  output$corr <- renderPlot({
    corrplot(Mt, method= input$corrselect, type = "upper", tl.col = "black", tl.srt = 45)
  })
  
  output$legend <- DT::renderDataTable({
    DT::datatable(Dat.legend, options = list(lengthMenu = c(5, 10), pageLength = 5))
  })
  
}


shinyApp(ui, server)
