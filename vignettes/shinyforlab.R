## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- fig.show='hold'----------------------------------------------------
library(ggplot2)
library(labfifthcasnit)
library(dplyr)
library(httr)
library(readr)
library(lubridate)
norway<-riks_api()
ggplot(norway, aes(x = date, y = Value)) + 
        geom_point()

## ---- fig.show='hold'----------------------------------------------------

us<-riks_api("a",rate5 = "")
ggplot(us, aes(x = date, y = Value)) + 
        geom_point()

## ---- fig.show='hold'----------------------------------------------------
jap<-riks_api(rate2 = "a",rate5 = "")
ggplot(jap, aes(x = date, y = Value)) + 
        geom_point()

## ---- fig.show='hold'----------------------------------------------------
eur1<-riks_api(rate3 = "a",rate5 = "")
ggplot(eur1, aes(x = date, y = Value)) + 
        geom_point()

