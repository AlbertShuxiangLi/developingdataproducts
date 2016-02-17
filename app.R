library(shiny)

# Define the formula to calculate the VG values from T2C, NG and IGV
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

# Define server logic for prediction application
server <- shinyServer(function(input, output) {

# Download example.csv
  
# Plot time series of VG values; Plot time series of diff(VG)  
  output$plot1 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    x_raw <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
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

# Plot ACF of diff(VG) for MA explortory; Plot PACF of diff(VG) for AR explortory  
  output$plot2 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    x_raw <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    x <- ts(vg_cal(x_raw))
    xd <- diff(x)
    par(mar=c(4, 5, 5, 1))
    par(mfrow=c(2, 1))
    acf(xd, ylim=c(-0.2, 1), xlab="lag", ylab="ACF", main="ACF of diff(VG)")
    pacf(xd, ylim=c(-0.2, 0.1), xlab="lag", ylab="PACF", main="Partial ACF of diff(VG)")
  })

# Display the prediction result: CLEAR (p < .5) or WARNING (p > .5)
  output$contents <- renderPrint({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    t1 <- Sys.time()
    x_raw <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
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
      cat("[ X ] WARNING: COMPRESSOR STALL IN NEXT FLIGHT", 
          "( p_hat =", p, ")")} else {
      cat("[ X ] CLEAR: NORMAL CAUTION APPLY IN NEXT FLIGHT", 
          "( p_hat =", p, ")")
      }
  })
  
})


# Define UI for prediction application 
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Predict Aircraft Turbofan Engine Compressor Stall Fault"),
  p(withMathJax("$$X_t (\\text{Variable Geometry } ) = f (T2C, NG, IGV)$$"),
  p(withMathJax("$$X_t = \\delta + AR_1X_{t-1} + AR_2X_{t-2} + ... + AR_pX_{t-p} + A_t + MA_1A_{t-1} 
                + MA_2A_{t-2} + ... + MA_qA_{t-q}$$")),
  p(withMathJax("$$p_{engineCompressorStall}=\\beta_0+\\displaystyle\\sum_{i=1}^{p}\\beta_i*AR_i 
                + \\displaystyle\\sum_{j=1}^{q}\\beta_{j+p}*MA_j + \\epsilon$$")),
  p(withMathJax("$$\\text{CLICK 'Download example.csv File' at Left Down Corner To Download example.csv}$$")),
  p(withMathJax("$$\\text{OR go to https://github.com/AlbertShuxiangLi/developingdataproducts/blob/gh-pages/data/example.zip
    then click 'Raw' button, and unzip locally)}$$")),
  
    sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose An Example CSV File to Upload for Analysis and Prediction',
                      accept=c('text/csv', 
                               'text/comma-separated-values,text/plain', 
                               '.csv')),
            tags$hr(),
            checkboxInput('header', 'Header (Must be Checked for Example)', TRUE),
            radioButtons('sep', 'Separator (Comma Must Be Selected for Example)',
                         c(Comma =',',
                         Semicolon=';',
                          Tab='\t'),
                          ','),
            radioButtons('quote', 'Quote',
                         c(None='',
                         'Double Quote'='"',
                          'Single Quote'="'"),
                          '"'),
            tags$hr(), tags$hr(),
            tags$a("------------------ github ------------------", 
                   href="https://github.com/AlbertShuxiangLi/developingdataproducts", 
                   target="_blank"),
            tags$hr(), tags$hr(),
            tags$a(href = 'example.csv', class = "btn", icon("download"), 'Download example.csv File.')      
            ),
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