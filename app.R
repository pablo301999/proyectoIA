
#Rooms
#Distance
#Bedroom2
#Bathroom
#Car
#Landsize
#BuildingArea
#YearBuilt
#Propertycount


library(shiny)

library (rpart)
library (rpart.plot)
library (caret)
library (dplyr)
library(readr)      # Para leer datos
library(ggplot2)    # Para grafica mas vistosas
library(reshape)    # Para renombrar columnas




ui<-fluidPage(
    
    fluidRow(
        
        column(3, wellPanel(
            numericInput("rooms", label = h3("habitaciones"), value = 1),
            numericInput("distance", label = h3("Distance"), value = 1),
            numericInput("bedroom2", label = h3("Bedroom2"), value = 1)
            
        )),
        
        column(3, wellPanel(
            numericInput("bathroom", label = h3("Bathroom"), value = 1),
            numericInput("car", label = h3("Car"), value = 1),
            numericInput("landsize", label = h3("Landsize"), value = 1)
        )),
        
        
        column(3, wellPanel(
             numericInput("buildingarea", label = h3("BuildingArea"), value = 1),
             numericInput("yearbuilt", label = h3("YearBuilt"), value = 1),
             numericInput("propertycount", label = h3("Propertycount"), value = 1)
        )),
        
        
        
        column(3,
               tags$p("Prediccion del precio para los datos ingresados:"),
               verbatimTextOutput("prediccion")
    
       )
        
    ),
    
    mainPanel()
    
)



server<-function(input,output)
    
    {
    
    output$prediccion <- renderText({
        
        datos_casas <- read.csv("C:/Users/Roberto Trujillo/OneDrive/9no ciclo/Inteligencia artificial/proyecto/ProyectoFinal_iA/melb_data.txt")
        
        datos.Num <- select (datos_casas, Price, Rooms, Distance, Bedroom2, Bathroom, Car, Landsize, BuildingArea, YearBuilt, Propertycount) 
        
        mediana.BA <- median(datos.Num$BuildingArea, na.rm = TRUE)
        mediana.YB <- median(datos.Num$YearBuilt, na.rm = TRUE)    
        mediana.C <- median(datos.Num$Car, na.rm = TRUE)   
        #LIMPIEZA DE DATOS
        datos.Num<- datos.Num %>%
            mutate (BuildingArea = ifelse(is.na(BuildingArea), mediana.BA, BuildingArea))
        
        datos.Num <- datos.Num %>%
            mutate (YearBuilt = ifelse(is.na(YearBuilt), mediana.YB, YearBuilt)) 
        
        datos.Num <- datos.Num %>%
            mutate (Car = ifelse(is.na(Car), mediana.C, Car)) 
        
        
        
        set.seed(2020) #datos de entrenamiento
        entrena <- createDataPartition(datos.Num$Price, p=0.7, list = FALSE)
        head(entrena)
        
        head(datos.Num[-entrena,])
        nrow(datos.Num[-entrena,])
        
        
        datos.Entrena <- datos.Num[entrena,] 
        head(datos.Entrena)
        
        datos.Valida <- datos.Num[-entrena,]
        head(datos.Valida)
        
        
        set.seed(2020)
        arbol <- rpart(formula = Price  ~ ., data = datos.Entrena)
        arbol

        A<-input$rooms
        B<-input$distance
        C<-input$bedroom2
        D<-input$bathroom
        E<-input$car
        f<-input$landsize
        G<-input$buildingarea
        H<-input$yearbuilt
        I<-input$propertycount
        
        
        
        nuevo  <- data.frame(cbind(c(0),c(A), c(B), c(C), c(D),c(E), c(f), c(G), c(H), c(I)))
        
        colnames(nuevo) <- c("Price", "Rooms", "Distance", "Bedroom2", "Bathroom", "Car", "Landsize", "BuildingArea", "YearBuilt", "Propertycount")
        
       
        prediccion.price <- predict(arbol, newdata = datos.Valida)
        prediccion.price <- predict(arbol, newdata = nuevo
        )
        
        datos.Valida[1,]
        prediccion.price[1]
        
        
    })
    
}
shinyApp(ui=ui,server = server)
