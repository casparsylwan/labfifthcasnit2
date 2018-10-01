#'
#'
#'
#'
#'
#' @export riks_api
#'


riks_api <- function(){

  riks_url<-GET("https://www.riksbank.se/en-gb/statistics/search-interest--exchange-rates/?c=cAverage&f=Day&from=03%2f01%2f1999&g2-SECBREPOEFF=on&g97-EUDP3MEUR=on&g97-EUDP3MJPY=on&g97-EUDP3MUSD=on&s=Dot&to=27%2f09%2f2018&export=csv")
  status_code(riks_url)
  repo_day<-content(riks_url,"text",encoding = "UTF-8")
  repo_day<-read_delim(repo_day,delim =  ";")
  repo_day<-separate(repo_day,"Period",c("d","m","y"),convert = T)
  repo_day<-unite(repo_day,"date",c("y","m","d"),sep="-")
  repo_day<-mutate(repo_day,date = ymd(date))
}
