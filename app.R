library(shiny)

vg_cal <- function(x) {
  VGx <- data.frame()
  for (i in 1:nrow(x)) {
    ng <- x[i, 3]; t2c <- x[i, 2]; igv <- x[i, 4]
    ngc <- ng/sqrt((t2c+273.15)/288.15)
    if (t2c < 23.8) { vg <- igv + 0.862*ngc - 112.470 }
    if (t2c >= 23.8 & t2c <= 37.7) { vg <- igv + (0.862*ngc - 112.5) + (1.005*ngc - 71.408)*(t2c - 23.889)/13.9 }
    if (t2c > 37.7) { vg <- igv + 1.439*ngc - 150.898 }
    if(vg > 9.999) { vg <- NA }
    if(vg < -9.999) { vg <- NA }
    VGx <- rbind(VGx, vg)
  }
  VGx <- VGx[complete.cases(VGx), ]
  return(VGx)
}

# Define server logic for random distribution application
server <- shinyServer(function(input, output) {
  
  output$plot1 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    x_raw <- read.csv(inFile$datapath, header=TRUE, sep=",", quote='"')
    x <- ts(vg_cal(x_raw))
    xd <- diff(x)
    par(mar=c(4, 5, 5, 1))
    par(mfrow=c(2, 1))
    plot(x, xlab = "Time", 
            ylab = "VG Value",
            ylim = c(-5, 5),
            main = "Time Series of VG", col="grey")
    plot(xd, xlab = "Time",
             ylab = "Value of diff(VG)",
             ylim = c(-5, 5),
             main = "First Order Difference of VG", col="blue")
  })
  
  output$plot2 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    x_raw <- read.csv(inFile$datapath, header=TRUE, sep=",", quote='"')
    x <- ts(vg_cal(x_raw))
    xd <- diff(x)
    par(mar=c(4, 5, 5, 1))
    par(mfrow=c(2, 1))
    acf(xd, ylim=c(-0.2, 1), xlab="lag", ylab="ACF", main="ACF of diff(VG)")
    pacf(xd, ylim=c(-0.2, 0.1), xlab="lag", ylab="PACF", main="Partial ACF of diff(VG)")
  })

  output$contents <- renderPrint({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    t1 <- Sys.time()
    x_raw <- read.csv(inFile$datapath, header=TRUE, sep=",", quote='"')
    x <- ts(vg_cal(x_raw))
    xd <- diff(x)
    arimaFit <- arima(xd, order = c(12,0,4), optim.method = "Nelder-Mead")
    tab_coef <- data.frame(); rownames(tab_coef[1,]) <- "coef_calculated" 
    for (k in 1:16) {tab_coef[1, k] <- arimaFit$coef[k]}
    Coef_training <- c(0.240, 5.348, 5.967, 9.755, -2.721, 4.616, 5.575, -0.447, 
                       2.163, 2.471, 13.484, 0.792, 11.931, 5.831, -2.100, 4.201, -5.785)
    p <- as.numeric(Coef_training[1])
    for (i in 1:16) {
      p <- p + as.numeric(Coef_training[i + 1]) * as.numeric(tab_coef["coef_calculated", i])
    }
    t2 <- Sys.time() - t1
    if ((p <- format(p, digits = 2)) > 0.5) {
      cat("WARNING: COMPRESSOR STALL IN NEXT FLIGHT", "( p_hat =", p, ")")} else {
        cat("CLEAR: NORMAL CAUTION APPLY IN NEXT FLIGHT", "( p_hat =", p, ")")
      }
  })
  
})


# Define UI for random distribution application 
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Predict Compressor Stall Fault"),
  p(withMathJax("$$X_t (\\text{Variable Geometry } ) = f (T2C, NG, IGV)$$"),
  p(withMathJax("$$X_t = \\delta + AR_1X_{t-1} + AR_2X_{t-2} + ... + AR_pX_{t-p} + A_t + MA_1A_{t-1} 
                + MA_2A_{t-2} + ... + MA_qA_{t-q}$$")),
  p(withMathJax("$$\\text{PLEASE DOWNLOAD example.csv from https://github.com/AlbertShuxiangLi/developingdataproducts/tree/gh-pages/data 
    (use 'save link as ...')}$$")),
  
    sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose An Example CSV File',
                      accept=c('text/csv', 
                               'text/comma-separated-values,text/plain', 
                               '.csv')),
            tags$hr()
          ),
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel(withMathJax("$$X_t \\text{ and diff}(X_t) \\text{ Plots}$$"), 
                           plotOutput("plot1")), 
                  tabPanel(withMathJax("$$\\text{ACF and PACF Plots for diff}(X_t)$$"), 
                           plotOutput("plot2")), 
                  tabPanel(withMathJax("$$\\text{ARIMA-LRM Prediction (May Take > 2 Minutes)}$$"), 
                           textOutput('contents'))
          )
        )
      )
    )
  ))

shinyApp(ui = ui, server = server)