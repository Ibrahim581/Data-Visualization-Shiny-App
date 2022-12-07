#

#
library(colourpicker)
library(shiny)
library(tidyverse)
library(ggplot2)
power.consumption <- read_csv("household_power_consumption.csv")
power.consumption.clean <- filter(power.consumption, power.consumption$Global_active_power !="?" )
power.consumption.2 <- power.consumption.clean %>%
  mutate(Voltage_quality = case_when(Voltage >= 240 ~ "high",
                                     Voltage < 240 ~ "low"))
the.quality <- function(x){
  if (as.numeric(x) < 4.97) return("Below Average")
  else return("Above Average" )
}

power.consumption.2$Global_intensity_Quality = sapply(power.consumption.2$Global_intensity, the.quality)
power.consumption.3 <- power.consumption.2 %>%
  mutate(Total_Sub_metering = as.numeric(Sub_metering_1) + as.numeric(Sub_metering_2) + as.numeric(Sub_metering_3))

# Define UI 
ui <- fluidPage(
  
  # Application title
  titlePanel("Household Electricity Consumption"),
  
  # Sidebar  
  sidebarLayout(
    sidebarPanel(
      
      colourInput("selectcolor", label = "Color"),
      
      selectInput("selectgen", label = h3("Choose a Month"), 
                  choices=list("January"=1, "February"=2, "March" = 3, "April" = 4, "May" = 5, "June" = 6), 
                  selected = 1),
     
      selectInput("selectvar", label = h3("Choose a Variable"), 
                  choices=list("Voltage"=1, "Global Intensity"=2, "Total Sub-metering" = 3, "Voltage Quality" = 4, "Quality of Global Intensity" = 5), 
                  selected = 1),
      
     
      
     
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
      
      checkboxInput("checkbox1", label="Display Mean", value=FALSE),
      
      
      
    #main panel
    ), 
    mainPanel(
      plotOutput("distPlot"),
      hr(),
      p('Mean:'),
      fluidRow(column(5, verbatimTextOutput("mean"))),
    )
  )
)

# Define server logic 
server <- function(input, output) {
  output$distPlot <- renderPlot({
    if(input$selectgen == 1) {months.use = power.consumption.3[power.consumption.3$index <= 44638,]}
    if(input$selectgen == 2) {months.use = power.consumption.3[power.consumption.3$index >44639 & power.consumption.3$index <84959,]}
    if(input$selectgen == 3) {months.use = power.consumption.3[power.consumption.3$index >84960 & power.consumption.3$index <129599,]}
    if(input$selectgen == 4) {months.use = power.consumption.3[power.consumption.3$index >129600 & power.consumption.3$index <172799,]}
    if(input$selectgen == 5) {months.use = power.consumption.3[power.consumption.3$index >172800 & power.consumption.3$index <217439,]}
    if(input$selectgen == 6) {months.use = power.consumption.3[power.consumption.3$index >21740 & power.consumption.3$index <260639,]}
    
    if(input$selectvar == 1){
      hist(as.numeric(months.use$Voltage), breaks = input$bins, main='Voltage Distribution',xlab='Voltage',col = input$selectcolor, border = 'darkgrey')
    }
    
    if(input$selectvar == 2){
      hist(as.numeric(months.use$Global_intensity), breaks = input$bins, main='Distribution of the Global Intensity',xlab='Global Intensity',col = input$selectcolor, border = 'darkgrey')
      
    }
    if(input$selectvar == 3){
      hist(as.numeric(months.use$Total_Sub_metering), breaks = input$bins, main='Distribution of the Total Sub-metering',xlab='Total Sub-metering',col = input$selectcolor, border = 'darkgrey')
      
    }
    else if(input$selectvar == 4){
      ggplot(data = months.use, aes(x = Voltage_quality, fill = Voltage_quality)) +
        geom_bar(color = input$selectcolor) +
        labs(title = "Distribution of the Voltage Quality", x = "Voltage Quality", y = "Amount", fill = "Legend") 
    }
    else if(input$selectvar == 5){
      ggplot(data = months.use, aes(x = Global_intensity_Quality, fill = Global_intensity_Quality)) +
        geom_bar(color = input$selectcolor) +
        labs(title = "Distribution of the Quality of Global Intensity", x = "Quality of Global Intensity", y = "Amount", fill = "Legend") 
    }
  })
  
  
  
  #Display mean if selected
  output$mean <- renderPrint({ 
    if(input$checkbox1 == TRUE & input$selectvar == 1){
      mean(as.numeric(power.consumption.3$Voltage), na.rm=TRUE)}
    else if(input$checkbox1 == TRUE & input$selectvar == 2) {
      mean(as.numeric(power.consumption.3$Global_intensity), na.rm=TRUE)}
    else if(input$checkbox1 == TRUE & input$selectvar == 3) {
      mean(as.numeric(power.consumption.3$Total_Sub_metering), na.rm=TRUE)}
    else if(input$checkbox1 == TRUE & input$selectvar == 4) {
      "Error! Variable is categorical"}
    else if(input$checkbox1 == TRUE & input$selectvar == 5) {
      "Error! Variable is categorical"}
  })
  


}
# Run the application 
shinyApp(ui = ui, server = server)
