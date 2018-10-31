#' One of those shiny
#'
#' @field us_rate character.
#' @field eu_rate character.
#' @field nor_rate character.
#' @field uk_rate character.
#' @field jp_rate character.
#' @field base character.
#' @field from character.
#' @field to character.
#' @field adres character.
#' @field repo_day data.frame.
#' @field test data.frame.
#' @field start numeric.
#'
#' @param rate1 Us maturity 3 month
#' @param rate2 Japanese maturity 3 month
#' @param rate3 Greate britan maturity 3 month
#' @param rate4 Eurocountries maturity 3 month
#' @param rate5 Norwegian maturity 3 month
#'
#' @import shiny
#'
#' @importFrom readr read_delim
#' @importFrom dplyr mutate
#' @importFrom lubridate ymd
#' @import tidyr
#' @importFrom httr GET
#' @importFrom httr content
#' @import methods
#' @import testthat
#'
#'
#' @exportClass shiny_api
#' @export shiny_api
#' @importFrom methods new


shiny_api <- setRefClass("shiny_api",
                         fields = list(us_rate="character" ,eu_rate="character" ,nor_rate= "character", uk_rate= "character",
                                        jp_rate="character", base="character", from="character", to="character", adres="character",
                                        repo_day="data.frame", test="data.frame", start="numeric"),
                         methods = list(
                           riks_api = function(us="a", eu="a", nor="a", uk="a", jp=""){
                             "Method Centralbank API conects to central bank of sweden to get the data."

                             base<<-"https://www.riksbank.se/en-gb/statistics/search-interest--exchange-rates/?c=cAverage&s=Dot"

                             from<<-"&from=03%2f01%2f1999"
                             today<-Sys.Date()
                             today<-as.character(today)
                             today<-strsplit(today,"-")
                             today<-unlist(today)
                             to<<-sprintf("&to=%s%%%s%%%s",today[3],today[2],today[1])

                             if(us!="") {us_rate <<- "&g97-EUDP3MUSD=on"}
                             if(eu!="") {eu_rate <<- "&g97-EUDP3MJPY=on"}
                             if(nor!=""){nor_rate<<- "&g97-EUDP3MGBP=on"}
                             if(uk!="") {uk_rate <<- "&g97-EUDP3MEUR=on"}
                             if(jp!="") {jp_rate <<- "&g97-EUDP3MNOK=on"}

                             adres<<-paste0(base ,to, us_rate, eu_rate, uk_rate, jp_rate, nor_rate, from, "&export=csv")

                             riks_url<-httr::GET(adres)
                             #print(status_code(riks_url))

                             repo_days<- httr::content(riks_url,"text",encoding = "UTF-8")
                             repo_days<- readr::read_delim(repo_days,delim =  ";")
                             repo_days<- tidyr::separate(repo_days,"Period",c("d","m","y"),convert = T)
                             repo_days<- tidyr::unite(repo_days,"date",c("y","m","d"),sep="-")
                             repo_day<<- dplyr::mutate(repo_days, date = lubridate::ymd(date))
                              },
                           shiny_app = function(){
                             " Shiny_app starts the shiny and illustrates the data."

                            ui<-shiny::fluidPage(titlePanel("Repo"),
                                            sidebarLayout(

                                              # Select type of trend to plot
                                              selectInput(inputId = "Series",label = strong("Level of intrest"),
                                                          choices = unique(repo_day$Series),
                                                          selected = "Value"),

                                              # Select date range to be plotted

                                              dateRangeInput("date",strong("Date range"),start = "2000-01-03",end = "2018-10-31",
                                                             min= "1999-01-03", max= "2018-10-31")),

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

                             shinyApp(ui=ui, server=server)
                           },
                           initialize = function(start="Welcome"){
                             "Starts the api and initilize the values."
                             riks_api(); shiny_app()}
                         ))






