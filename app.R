#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

# setwd("D:/dvproject/Financial-Customer-Complaints")

# load packages

library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(ggmap)
library(dplyr)
library(tidyverse)
library(tigris)
library(scales)
library(maps)
library(mapproj)
library(ggthemes)
library(statebins)
library(lubridate)
library(zoo)
library(plotly)
library(png)
library(tidytext)
library(tm)
library(wordcloud)
library(quanteda)
library(randomcoloR)
library(hrbrthemes)


Sys.setlocale(locale="English")

# load the rds data written previously

dat <- read_rds("cfpb_1y.rds")
dat$yearmon = as.yearmon(dat$date)

# load up tdm data
dfComplaints_tdm = read.csv('dfComplaints_tdm.csv')

# load up data for sentiment
dfComplaints = read.csv('dfComplaints.csv')

# load up data for pyramid plot
dfSampleTop20 = read.csv('dfSampleTop20.csv')

# 1. Prepare to merge the state map and electoral data
cfpb_state <- dat %>%
  dplyr::rename(state.abb = State) %>%
  group_by(state.abb) %>% 
  summarise(complaints = n()) 

us.states <- map_data("state")
us.states <- as_data_frame(us.states)
us.states <- dplyr::rename(us.states, state = region)
us.states$subregion = NULL
us.states$state <- str_to_title(us.states$state)
#us.states

# Add State Abbreviations and Centers

statenames <- as_data_frame(
  cbind(state=state.name, state.abb = state.abb, 
        state.center.x = state.center$x, 
        state.center.y = state.center$y))
statenames <- statenames %>% mutate_each_(funs(as.numeric), 
                                          vars=c("state.center.x","state.center.y"))
us.states <- left_join(us.states, statenames)
#str(us.states)

# 2. Merge the data
cfpb.merged <- left_join(cfpb_state, us.states, by='state.abb')
cfpb.merged <- cfpb.merged %>%
  filter(!is.na(state))

# Define the UI
# Use a pretty theme

ui <- navbarPage("Financial Consumer Complaints",
                 theme = shinytheme('paper'),
                 
                 tabPanel('Home',
                          plotOutput(outputId = "png", width = "20%" , height = "140px"),
                          
                          h1("Financial Consumer Complaints"),
                          
                          plotOutput(outputId = "homepage", width = "100%" , height = "300px"),
                          
                          shiny::HTML("<h5>This is an interactive tool to help you explore the financial
                            customer complaints data from 2019 to 2020. Please feel free to navigate to any of the tabs above to explore visualizations broken down by geographical location, text content, and complaint type.</h5>"),
                          
                          shiny::HTML("<h5>The Consumer Financial Protection Bureau is a U.S. government
                            agency that makes sure banks, lenders, and other financial companies
                            treat you fairly. Consumer Complaint Database is a collection of
                            complaints about consumer financial products and services that we
                            sent to companies for response.</h5>")
                          ),
                 
                 tabPanel('Geographical Overview',
                          tags$hr(),
                          h4("Geographic Region"),
                          HTML("<h5>Consumers from all 50 states and the District of Columbia submitted
                            complaints to the Bureau. To understand state and regional trends, we
                            analyzes the geographic distribution of complaints. Map 1 shows that,
                            from 2019 to 2020, the Bureau received more complaints from consumers in
                            California than anywhere else in the United States, followed by consumers
                            in Florida, Texas, and Georgia. Consumers in North Dakota submitted the
                            fewest complaints of any state.</h5>"),
                          
                          plotOutput("map"),
                          
                          plotOutput("bin_total"),
                          
                          plotOutput("bin_product"),
                          
                          plotOutput("bin_issue")
                 ),
                 
                 tabPanel('Text Overview',
                          h4("Comparison of different words across products"),
                          plotOutput('comparisoncloud'),
                          tags$hr(),
                          h4("Difference in sentiment scores across products"),
                          HTML('<h5>There are clearly two categories of sentiment. The descriptions for credit reporting services and debt collection have visibly lower sentiment scores than the other categories.</h5>'),
                          plotlyOutput('sentiment'),
                          tags$hr(),
                          h4('Pyramid Plot of most common words'),
                          HTML('<h5>The pyramid plot below shows the differences in word frequency in the complaints among the two groups, determined by their sentiment scores.</h5>'),
                          plotlyOutput('top20')
                 ),
                 
                 tabPanel('Complaints Data',
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("product",
                                          "Products:", choices=unique(dat$Product), selected=unique(dat$Product)[1]),
                              radioButtons("response", "Timely response",
                                           choices =c("Yes", "No"),selected = "Yes"),
                              downloadButton(outputId = "download_data",
                                             label = "Download data")
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Complaint by date",  plotOutput("distPlot")),
                                tabPanel("Complaint by states", plotlyOutput("barPlot")),
                                tabPanel("Complaint by types", plotlyOutput("barPlot2")),
                                tabPanel("Complaint by company", plotlyOutput("goemPlot"))
                              )
                            )
                          )
                 )
                 
)
# Define server

server <- function(input, output) {
  
  output$png <- renderPlot({
    pic = readPNG('logo.png')
    plot.new()
    grid::grid.raster(pic)
    
  })
  
  output$homepage <- renderPlot({
    pic = readPNG('homepage.png')
    plot.new()
    grid::grid.raster(pic)
    
  })
  
  output$distPlot <- renderPlot({
    df <- dat %>% filter(Product==input$product)
    df <- df[df$Timely.response.==input$response,]
    df %>% group_by(yearmon) %>% summarise(count=n()) %>%
      ggplot(aes(x=yearmon, y=count)) + geom_line(color="blue") +
      labs(x="Month Received", y="The number of complaints") +
      geom_point() + geom_vline(xintercept = as.yearmon("2020-3-21"), col="red")
  })
  
  output$barPlot <- renderPlotly({
    df <- dat %>% filter(Product==input$product)
    df <- df[df$`Timely.response.`==input$response,]
    p <- df %>% filter(nchar(State)==2) %>% group_by(State) %>% summarise(count=n()) %>%
      ggplot(aes(x=reorder(State, count), y=count, fill=State)) + geom_bar(stat="identity") + coord_flip()+
      labs(x="States", y="The number of complaints") 
    ggplotly(p, height=800)
    
  })
  
  output$barPlot2 <- renderPlotly({
    df <- dat %>% filter(Product==input$product)
    df <- df[df$`Timely.response.`==input$response,]
    c <- df  %>% group_by(Sub.product) %>% summarise(count=n()) %>%
      ggplot(aes(x=reorder(Sub.product,count), y=count, fill=count)) + geom_bar(stat="identity")+
      labs(x="Sub product", y="The number of complaints") + coord_flip()
    ggplotly(c, height=800) 
  })
  
  output$goemPlot <- renderPlotly({
    df <- dat %>% filter(Product==input$product)
    df <- df[df$`Timely.response.`==input$response,]
    d <- df  %>% group_by(Company) %>% summarise(count=n()) %>%head(5)%>%
      ggplot(aes(x=reorder(Company,count), y=count, fill=count)) + geom_bar(stat="identity")+
      labs(x="Company", y="The number of complaints") + coord_flip()
    ggplotly(d, height=800) 
  })
  
  output$comparisoncloud <- renderPlot({
    dfComplaints_tdm = dfComplaints_tdm %>% column_to_rownames(., var = "X")
    mypal = distinctColorPalette(9)
    comparison.cloud(dfComplaints_tdm, colors=mypal,
                     random.order=FALSE, scale=c(3, 0.2), 
                     max.words=500, title.size=1,
                     title.colors=mypal)
  })
  
  output$sentiment <- renderPlotly({
    sent.plot = dfComplaints %>% group_by(my, Product) %>%
      summarise(mean_sentiment = mean(sentiment)) %>%
      ggplot(data=.)+
      geom_line(aes(x=as.Date(as.yearmon(my)), y=mean_sentiment, color=Product))+
      labs(x='Time', y='Mean Sentiment')+
      theme_ipsum()
    ggplotly(sent.plot, height=450)
  })
  
  output$top20 <- renderPlotly({
    py = ggplot(dfSampleTop20, aes(x = reorder(term, frequency), fill = category))+
      geom_bar(data=dfSampleTop20 %>% filter(category=='low'), 
               aes(y=frequency), stat='identity')+
      geom_bar(data=dfSampleTop20 %>% filter(category=='high'), 
               aes(y=-frequency), stat='identity')+
      scale_fill_brewer(palette = "Set1")+
      labs(x='Term', y='Frequency',
           title="Top 20 Most Frequent Words in Complaints",
           subtitle= 'by Sentiment Category')+
      scale_y_continuous(labels=abs)+
      coord_flip()+
      theme_ipsum()
    ggplotly(py, height=450)
  })

  output$map <- renderPlot({
    ggplot(cfpb.merged, 
           aes(x = long, y = lat, group=group)) + 
      geom_polygon(aes(fill = complaints), color="white") +  
      geom_text(data=cfpb.merged, inherit.aes = FALSE,   
                aes(label=complaints,   
                    x=state.center.x, y=state.center.y),  
                colour="white", size=4) + 
      theme_map() + 
      coord_map(projection = "mercator") +
      ggtitle("Customer Complaints by State") +
      theme(title = element_text(size=10))
  })
  
  output$bin_total <- renderPlot({
    cfpb_state <- dat %>%
      filter(State %in% c(unique(statenames$state.abb))) %>%
      dplyr::rename(state.abb = State) %>%
      group_by(state.abb) %>% 
      summarise(complaints = n()) 
    
    cfpb.bin <- group_by(cfpb_state, state.abb) %>% slice(1)
    cfpb.bin$state <- cfpb.bin$state.abb
    cfpb.bin$value <- cfpb.bin$complaints
    
    statebins(cfpb.bin, state_col="state.abb", value_col="complaints",
              palette="RdBu", round=TRUE) +
      theme_statebins(legend_position="right") +
      ggtitle("Total Customer Complaints by State") 
  })
  
  output$bin_product <- renderPlot({
    cfpb_state2 <- dat %>%
      filter(State %in% c(unique(statenames$state.abb))) %>%
      filter(Product=="Credit reporting, credit repair services, or other personal consumer reports") %>%
      dplyr::rename(state.abb = State) %>%
      group_by(state.abb) %>% 
      summarise(complaints = n()) 
    
    cfpb.bin <- group_by(cfpb_state2, state.abb) %>% slice(1)
    cfpb.bin$state <- cfpb.bin$state.abb
    cfpb.bin$value <- cfpb.bin$complaints
    
    statebins(cfpb.bin, state_col="state.abb", value_col="complaints",
              palette="RdBu", round=TRUE) +
      theme_statebins(legend_position="right") +
      ggtitle("Product: Credit reporting, credit repair services, or other personal consumer reports")
  })
  
  output$bin_issue <- renderPlot({
    cfpb_state3 <- dat %>%
      filter(State %in% c(unique(statenames$state.abb))) %>%
      filter(Issue=="Incorrect information on your report") %>%
      dplyr::rename(state.abb = State) %>%
      group_by(state.abb) %>% 
      summarise(complaints = n()) 
    
    cfpb.bin <- group_by(cfpb_state3, state.abb) %>% slice(1)
    cfpb.bin$state <- cfpb.bin$state.abb
    cfpb.bin$value <- cfpb.bin$complaints
    
    statebins(cfpb.bin, state_col="state.abb", value_col="complaints",
              palette="RdBu", round=TRUE) +
      theme_statebins(legend_position="right") +
      ggtitle("Issue: Incorrect information on your report")
  })
  
  # render download data
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("financial-customer-complaints.csv")
    },
    content = function(file) {
      write.csv(dat, file)
    }
  )
    
  }

# Run the application 
shinyApp(ui = ui, server = server)