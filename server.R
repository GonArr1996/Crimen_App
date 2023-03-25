
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(DT)
library(randomForest)
library(shinyMobile)


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
# Gráfica de VT10
# En la sección donde se crean las gráficas, agrega un nuevo filtro utilizando la alcaldía seleccionada en input$alcaldiaInput
VictimasTop10 <- Victimas %>%
       filter(Delito %in% Top10$Delito) %>%
       filter(AlcaldiaHechos == input$alcaldiaInput)

VictimasTop10 <- Victimas %>%
       filter(Delito == c(Top10$Delito))

Fem <- VictimasTop10[VictimasTop10$Sexo == "Femenino",]
Fem <- Fem %>%
       filter(Delito == c("VIOLENCIA FAMILIAR", "AMENAZAS", "ROBO DE OBJETOS"))

VictimasXgenero <- VictimasTop10 %>%
       filter(Delito == c("VIOLENCIA FAMILIAR", "AMENAZAS", "ROBO DE OBJETOS"))

CrimenXAlcadia <- Victimas %>%
       group_by(AlcaldiaHechos) %>%
       summarize(DXA = length(Delito)) %>% 
       arrange(desc(DXA)) %>% 
       slice(1:5)

CrimXAlc <- VictimasTop10 %>%
       filter(AlcaldiaHechos == c("IZTAPALAPA", "CUAUHTEMOC", "GUSTAVO A MADERO", "BENITO JUAREZ")) %>%
       filter(Delito == c("VIOLENCIA FAMILIAR", "AMENAZAS", "ROBO DE OBJETOS"))

VictimasTop10 <- Victimas %>%
       filter(Delito %in% Top10$Delito)

VT10 <- ggplot(VictimasTop10, aes(x = Delito, fill = Sexo)) +
       geom_bar(position = "stack") +
       facet_wrap(Delito~., scale = "free")

VT10 <- VT10 +         
       xlab("Crimen") +
       ylab("Crimen por dia") +
       ggtitle("Top 10 Crimenes por genero") +
       theme(axis.title.x = element_text(color = "black", size = 5),
             axis.title.y = element_text(color = "black", size = 6),
             axis.text.x = element_text(size = 5), 
             axis.text.y = element_text(size = 5),
             legend.title = element_text(size = 5),
             legend.text = element_text(size = 4),
             legend.position = "top",
             strip.text.x = element_text(size = 5, face = "bold"),
             plot.title = element_text(color = "darkblue", size = 17, family = "Courier"),
             plot.margin = unit(c(15, 5, 5, 5), "mm"))


# Gráfica de edadsexo
VictimasXgenero <- VictimasTop10 %>%
       filter(Delito %in% c("VIOLENCIA FAMILIAR", "AMENAZAS", "ROBO DE OBJETOS"))

edadsexo <- ggplot(VictimasXgenero, aes(x = Delito, y = Edad)) +
       geom_boxplot(alpha = 0.8) +
       geom_jitter(aes(color = Sexo), alpha = 0.1) +
       facet_wrap(Sexo ~ .)

# Gráfica de VTLAC
CrimXAlc <- VictimasTop10 %>%
       filter(AlcaldiaHechos %in% c("IZTAPALAPA", "CUAUHTEMOC", "GUSTAVO A MADERO", "BENITO JUAREZ")) %>%
       filter(Delito %in% c("VIOLENCIA FAMILIAR", "AMENAZAS", "ROBO DE OBJETOS"))

VTALC <- ggplot(CrimXAlc, aes(x = Delito, fill = Sexo)) +
       geom_bar(position = "dodge") +
       facet_grid(AlcaldiaHechos ~ Delito, scale = "free")


shinyServer(function(input, output) {
              # ... (El resto de las funciones renderPlotly y renderDataTable sigue aquí)
              
              # Agrega un observer para actualizar las gráficas cuando se cambia la alcaldía
              observeEvent(input$alcaldiaInput, {
                     output$VT10 <- renderPlotly({
                            p <- ggplotly(VT10)
                            p
                     })
                     
                     output$VT10_table <- renderDataTable({
                            DT::datatable(VictimasTop10, options = list(lengthChange = FALSE, pageLength = 10))
                     })
                     
                     output$edadsexo <- renderPlotly({
                            p <- ggplotly(edadsexo)
                            p
                     })
                     
                     output$edadsexo_table <- renderDataTable({
                            DT::datatable(VictimasXgenero, options = list(lengthChange = FALSE, pageLength = 10))
                     })
                     
                     output$VTALC <- renderPlotly({
                            p <- ggplotly(VTALC)
                            p
                     })
                     
       
       output$barPlotProb <- renderPlot({
              req(input$calcularProb)
              
              edad <- as.numeric(input$edadInput)
              sexo <- input$sexoInput
              
              prob_victima <- exp(0.05 * edad - 0.8) / (1 + exp(0.05 * edad - 0.8))
              prob_criminal <- exp(0.1 * edad - 1.5) / (1 + exp(0.1 * edad - 1.5))
              
              if (sexo == "Femenino") {
                     prob_victima <- prob_victima * 0.9
                     prob_criminal <- prob_criminal * 0.1
              }
              
              data_prob <- data.frame(
                     Categoria = c("Víctima", "Criminal"),
                     Probabilidad = c(prob_victima, prob_criminal)
              )
              
              bar_plot <- ggplot(data_prob, aes(x = Categoria, y = Probabilidad * 100, fill = Categoria)) +
                     geom_bar(stat = "identity", width = 0.5) +
                     geom_text(aes(label = sprintf("%.1f%%", Probabilidad * 100)), vjust = -0.5, size = 4) +
                     coord_cartesian(ylim = c(0, 100)) +
                     labs(title = "Probabilidades en función de la Edad y el Sexo",
                          x = "Categoría",
                          y = "Probabilidad (%)") +
                     theme_minimal() +
                     theme(legend.position = "none")
              
              return(bar_plot)
       })
       
})

