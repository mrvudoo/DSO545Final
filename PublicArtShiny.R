library(shiny)
library(dplyr)
library(ggplot2)

# read data
public_art_data = read.csv('PublicArt.csv')

zipcodes = public_art_data %>% 
  filter(!is.na(project.ZIP)) %>% 
  distinct(project.ZIP) %>% 
  arrange(project.ZIP)

medianrent = read.csv("MedianRental.csv")

medianhomeprice = read.csv('ca_medHome.csv')

AllCrime = read.csv("AllCrime.csv") %>%
  filter(ZIP %in% zipcodes$project.ZIP)

AllCrimeOtherYears = read.csv("AllCrimeOtherYears.csv") %>%
  filter(ZIP %in% zipcodes$project.ZIP)

FilteredData=rbind(AllCrime, AllCrimeOtherYears)

ui <- fluidPage(
  
  titlePanel("Public Art Program KPI Measures"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("artprogram",
                  "Project Name:",
                  choices = unique(public_art_data$PROJECT.NAME)
      ),
      
      actionButton("go","Find KPI"),
      
      tableOutput('tblProject')
      
    ),
    
    
    mainPanel(
      
      plotOutput("MedianHomePrice"),
      plotOutput("MedianRentPrice"),
      plotOutput("CrimeCount"),
      plotOutput("CrimePercentage")
    )
  )
)

server <- function(input, output) {
  
  projects <- eventReactive(input$go,{
    projects <- public_art_data %>% 
      filter(PROJECT.NAME == input$artprogram)
    
    projects <- public_art_data %>%
      filter(project.ZIP == projects$project.ZIP) %>%
      select(PROJECT.NAME, FULL.1..amount, END.DATE, project.ZIP)
  }) 
  
  output$tblProject <- renderTable(projects())
                             
  zip <- eventReactive(input$go,{
    zip <- as.character(projects()[1,4])
  })
  
  zipx <- eventReactive(input$go,{
    zipx <- paste("X", as.character(projects()[1,4]), sep = "")
  })
  
  
  output$MedianHomePrice <- renderPlot({
    ggplot(medianhomeprice) + 
      geom_line(aes_string(x="RegionName",y=zipx())) +
      xlab('Year') +
      ylab('Dollars (in thousands)') +
      scale_x_continuous(breaks=seq(1996,2016,1)) +
      ggtitle(paste('Median Home Prices for Zip Code', zip())) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$MedianRentPrice <- renderPlot({
    medianrent %>% 
      ggplot(aes_string(x='RegionName',y=zipx())) + 
      geom_line() +
      ylab("Zillow Rental Index") +
      xlab("Years") +
      ggtitle(paste("Zillow Rental Index by Years For", zip())) +
      theme(plot.title = element_text(hjust = .5)) +
      scale_x_continuous(breaks = seq(2010,2017,1))
  })
  
  output$CrimeCount <- renderPlot({
    FilteredData %>%
      filter(ZIP == zip()) %>%
      ggplot(aes(x=INCIDENT_YEAR)) +
      geom_bar() +
      xlab("Year") +
      ylab("Number of Incidents") +
      ggtitle("Crime Incidents in Los Angeles Over Time") + 
      theme(plot.title=element_text(hjust=0.5)) +
      scale_x_continuous(breaks = seq(2005,2016,1))
  })
  
  crimedata <- eventReactive(input$go,{
    crimedata = FilteredData %>%
      group_by(INCIDENT_YEAR, ZIP) %>%
      summarise(cnt = n())
    
    crimedata = crimedata %>%
      group_by(INCIDENT_YEAR) %>%
      mutate(year_sum = sum(cnt)) %>%
      mutate(year_percent = (cnt/year_sum)*100) %>%
      filter(ZIP == zip())
  })
  
  output$CrimePercentage <- renderPlot({
    crimedata() %>%
      ggplot(aes(x=INCIDENT_YEAR, y = year_percent))+
      geom_line() +
      xlab("Year") +
      ylab("Percentage of Crime in Los Angeles") +
      ggtitle("Percentage of Crime in Los Angeles Over Time") + 
      theme(plot.title=element_text(hjust=0.5)) +
      scale_x_continuous(breaks = seq(2005,2016,1))
  })
}

shinyApp(ui = ui, server = server)