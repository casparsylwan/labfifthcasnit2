#' One of those shiny
#'
#' @param input One parameter
#' @param output Another parameter+-
#'
#'
#' @import shiny
#' @import readr
#' @import dplyr
#' @import lubridate
#' @import tidyr
#' @import httr
#'
#' @export server
#' @export ui
#' @export riks_api
#' @export appshiny
#'

#Conecting to an api reading in date and transform it timeseries.

riks_api<- function(){

riks_url<-GET("https://www.riksbank.se/en-gb/statistics/search-interest--exchange-rates/?c=cAverage&f=Day&from=03%2f01%2f1999&g2-SECBREPOEFF=on&g97-EUDP3MEUR=on&g97-EUDP3MJPY=on&g97-EUDP3MUSD=on&s=Dot&to=27%2f09%2f2018&export=csv")
status_code(riks_url)
repo_day<-content(riks_url,"text",encoding = "UTF-8")
repo_day<-read_delim(repo_day,delim =  ";")
repo_day<-separate(repo_day,"Period",c("d","m","y"),convert = T)
repo_day<-unite(repo_day,"date",c("y","m","d"),sep="-")
repo_day<-mutate(repo_day,date = ymd(date))}






repo_day <-riks_api()

ui<- fluidPage(titlePanel("Repo"),
               sidebarLayout(

                 # Select type of trend to plot
                 selectInput(inputId = "Series",label = strong("Level of intrest"),
                             choices = unique(repo_day$Series),
                             selected = "Value"),

                 # Select date range to be plotted

                 dateRangeInput("date",strong("Date range"),start = "2000-01-03",end = "2018-09-27",
                                min= "1999-01-03", max= "2018-09-27")),

               mainPanel(
                 plotOutput(outputId = "lineplot", height = "300px"),
                 tags$a(href = "https://www.riksbank.se/en-gb/statistics/search-interest--exchange-rates/", "source :Riksbanken")
               ))



server<- function(input, output){

  # Subset data
  selected_trends <- reactive({

    req(input$date)
    repo_day %>%
      filter(
        Series == input$Series,
        date > as.POSIXct(input$date[1]) & date < as.POSIXct(input$date[2]))
  })

  output$lineplot <- renderPlot({

    par(mar=c(4,4,1,1))
    plot(x = selected_trends()$date, selected_trends()$Value, type = "l",
         xlab="Date", ylab= "Repo")






  })



}

appshiny  <- shinyApp(ui=ui, server=server)





