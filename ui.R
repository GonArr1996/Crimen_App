library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(DT)
library(randomForest)
library(shinythemes)

url_Crimen <- "https://archivo.datos.cdmx.gob.mx/fiscalia-general-de-justicia/carpetas-de-investigacion-fgj-de-la-ciudad-de-mexico/carpetas_completa_julio_2021.csv"
url_victimas <- "https://archivo.datos.cdmx.gob.mx/fiscalia-general-de-justicia/victimas-en-carpetas-de-investigacion-fgj/victimas_completa_julio_2021.csv"

Crimen_file <- "carpetas_completa_julio_2021.csv"
victimas_file <- "victimas_completa_julio_2021.csv"

if (!file.exists(Crimen_file)) {
       download.file(url_Crimen, Crimen_file, mode = "wb")
}

if (!file.exists(victimas_file)) {
       download.file(url_victimas, victimas_file, mode = "wb")
}

Crimen <- read.csv("carpetas_completa_julio_2021.csv", stringsAsFactors = F)
Victimas <- read.csv("victimas_completa_julio_2021.csv", stringsAsFactors = F)

Crimen$fecha_hechos <- as.Date(Crimen$fecha_hechos, tryFormats = c("%Y-%m-%d",
                                                                   "%Y/%m/%d"))

#Primero creamos una nueva base de datos donde ya estan agrupadas las observaciones por dia con group_by():
CrimenXdia <- Crimen %>% group_by(fecha_hechos)
#Despues sumamos en esa base de datos los casos de defuncion por cada grupo, es decir por cada dia:
CrimenXdia <- CrimenXdia %>% summarize(CrimenXDia = length(fecha_hechos))
CrimenXdia
#Voy a acotar de 1990-2020 que es lo mas significativo:
Crimen9020 <- CrimenXdia %>%
       filter(fecha_hechos >= as.Date("1990-01-01", format = "%Y-%m-%d"))

Crimen$delito <- factor(Crimen$delito) # Converti en factor
length(levels(Crimen$delito)) # Estoy viendo cuantos tipos de crimenes hay

CrimenXdelito <- Crimen %>%
       group_by(delito)

CrimenXdelito <- CrimenXdelito %>%
       summarize(CXD = length(delito))

Top10 <- Victimas %>% group_by(Delito) %>%
       summarize(DXD = length(Delito)) %>%
       arrange(desc(DXD)) %>%
       slice(1:10)              

# Aquí debes agregar el código que procesa y crea las gráficas a partir de tus archivos CSV.
Victimas$FechaHecho <- as.Date(Victimas$FechaHecho, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))


shinyUI(
       fluidPage(
              titlePanel("Análisis de crimen"),
              theme = shinythemes::shinytheme('simplex'),
              sidebarLayout(
                     sidebarPanel(
                            sliderInput("edadInput",
                                        "Edad:",
                                        min = 0,
                                        max = 100,
                                        value = 25),
                            radioButtons("sexoInput",
                                         "Sexo:",
                                         choices = list("Femenino" = "Femenino", "Masculino" = "Masculino"),
                                         selected = "Femenino"),
                            selectInput("alcaldiaInput",
                                        "Alcaldía:",
                                        choices = unique(Victimas$AlcaldiaHechos),
                                        selected = unique(Victimas$AlcaldiaHechos)[1]),
                            actionButton("calcularProb", "Calcular Probabilidad")
                     ),
                     mainPanel(
                            tabsetPanel(
                                   tabPanel("Gráficas",
                                            tabsetPanel(
                                                   tabPanel("Top 10", plotlyOutput("VT10"), DTOutput("VT10_table")),
                                                   tabPanel("Edad y Sexo", plotlyOutput("edadsexo"), DTOutput("edadsexo_table")),
                                                   tabPanel("Alcaldía", plotlyOutput("VTALC"), DTOutput("VTALC_table"))
                                            )),
                                   tabPanel("Probabilidades", 
                                            fluidRow(
                                                   column(6, verbatimTextOutput("probabilidad")),
                                                   column(6, plotOutput("barPlotProb"))
                                            )
                                   )
                            )
                     )
              )
       )
)
