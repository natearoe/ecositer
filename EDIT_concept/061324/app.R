library(shiny)
library(shinyMatrix)
library(shinyjs)
library(DT)

matrix1.input <- function(x){
  matrixInput(x,
              value = matrix(c(0.2), 4, 1, dimnames = list(c("A","B","C","D"),NULL)),
              rows = list(extend = FALSE,  names = TRUE),
              cols = list(extend = FALSE, names = FALSE, editableNames = FALSE),
              class = "numeric")}

matrix2.input <- function(x,y,z){
  matrixInput(x,
              value = matrix(c(y,z),1,2,dimnames=list(NULL,c("Y","Z"))),
              rows = list(extend = TRUE,  names = FALSE),
              cols = list(extend = FALSE, names = TRUE, editableNames = FALSE),
              class = "numeric")}

matrix.validate <- function(x,y){
  a <- y
  a[,1][a[,1]>x] <- x
  b <- diff(a[,1,drop=FALSE])
  b[b<=0] <- NA
  b <- c(1,b)
  a <- cbind(a,b)
  a <- na.omit(a)
  a <- a[,-c(3),drop=FALSE]
  return(a)}

vector.base <- function(x,y){
  a <- rep(y,x)
  b <- seq(1:x)
  c <- data.frame(x = b, y = a)
  return(c)}

vector.multi <- function(x,y,z){
  a <- rep(NA, x)
  a[y] <- z
  a[seq_len(min(y)-1)] <- a[min(y)]
  if(max(y) < x){a[seq(max(y)+1, x, 1)] <- 0}
  a <- approx(seq_along(a)[!is.na(a)],a[!is.na(a)],seq_along(a))$y
  b <- seq(1:x)
  c <- data.frame(x = b, z = a)
  return(c)}

vector.multiFinal <- function(x,y){vector.multi(x,matrix.validate(x,y)[,1],matrix.validate(x,y)[,2])}

button  <- function(x,y){actionButton(x,y,)}

matrix.link <- function(x,y){
  observeEvent(input$periods|input$base_input,{
    updateMatrixInput(session,x,value=matrix(c(input$periods,y),1,2,dimnames=list(NULL, c("y","z"))))})}

ui <-
  pageWithSidebar(
    headerPanel("Model"),
    sidebarPanel(uiOutput("Panels")),
    mainPanel(
      tabsetPanel(
        tabPanel("Dynamic", value=2,
                 helpText("Select output:"),
                 actionButton('showVectorPlotBtn','Vector plots',style="width:90px;font-size:80%"),
                 actionButton('showVectorValueBtn','Vector values',style="width:90px;font-size:80%"),
                 uiOutput("vectorTable")
        ),
        tabPanel("Data", value=3,
                 conditionalPanel(condition="input.choice==2"),
                 conditionalPanel(condition="input.choice==3")),
        tabPanel("Plot", value=4, plotOutput("plot")),
        id = "tabselected")
    ) # close main panel
  ) # close page with sidebar

server <- function(input,output,session)({
  periods              <-  reactive(input$periods)
  base_input           <-  reactive(input$base_input)
  vector_input         <-  reactive(input$vector_input)
  rv                   <-  reactiveValues()

  output$Panels <- renderUI({
    tagList(
      conditionalPanel(
        condition="input.tabselected==2",
        numericInput('begin.bal','',value=100000,step=1000,width = '100%'),
        sliderInput('periods','',min=1,max=120,value=60),
        matrix1.input("base_input"),
        useShinyjs(),
        actionButton('showVectorBtn','Show',style="width:8vw;margin-bottom:10px"),
        actionButton('hideVectorBtn','Hide',style="width:8vw;margin-bottom:10px"),
        actionButton('resetVectorBtn','Reset',style="width:8vw;margin-bottom:10px"),
        hidden(uiOutput("Vectors"))),
      conditionalPanel(
        condition="input.tabselected==3"),
      conditionalPanel(
        condition="input.tabselected==4")
    ) # close tagList
  }) # close renderUI

  renderUI({matrix.link("vector_input",input$base_input[1,1])})

  output$Vectors <- renderUI({
    input$resetVectorBtn
    tagList(matrix2.input("vector_input",input$periods,input$base_input[1,1]))
  }) # close render UI

  observeEvent(input$showVectorBtn,{shinyjs::show("Vectors")})
  observeEvent(input$hideVectorBtn,{shinyjs::hide("Vectors")})

  output$graph1 <- renderPlot(
    plot(if(input$showVectorBtn == 0) vector.base(periods(),input$base_input[1,1])
         else vector.multiFinal(periods(),matrix.validate(periods(),vector_input()))))

  output$table1 <- renderDT({
    if(input$showVectorBtn == 0) vector.base(periods(),input$base_input[1,1])
    else vector.multiFinal(periods(),matrix.validate(periods(),vector_input()))
  })

  observeEvent(input$showVectorPlotBtn,{rv$showme <- plotOutput("graph1")},ignoreNULL = FALSE)
  observeEvent(input$showVectorValueBtn,{rv$showme <- DTOutput("table1")})

  output$vectorTable <- renderUI({rv$showme})

}) # close server

shinyApp(ui, server)
