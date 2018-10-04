require(shiny)
require(lubridate)
require(formattable)


library(shiny)
library(lubridate)
library(formattable)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Bond Yields"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Specify the number of observations to view ----
      numericInput("price", "Price:", 97.3),
      numericInput("par", "Principal Value:", 10000000000),
      numericInput("coupon", "Yearly coupon rate:", 9),
      dateRangeInput("date", "Time to Maturity",start= "2007-09-28",end="2018-09-15"),
      
      # Include clarifying text ----
      helpText("Asumsi 1 bulan=30 hari"),
      
      # Input: actionButton() to defer the rendering of output ----
      # until the user explicitly clicks the button (rather than
      # doing it immediately when inputs change). This is useful if
      # the computations required to render output are inordinately
      # time-consuming.
      actionButton('submit','Submit')
      
    ),
    
    #Main panel for displaying outputs ----
    mainPanel(

      h4("Yield"),
      textOutput("text"),

      # Output: Header + table of distribution ----
      h4("Tabel Amortisasi"),
      tableOutput("tabel_amortisasi")
    )
    
  )
)


# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  r_data <- reactive({
    
    req(input$submit)
    
    isolate({
      date1=input$date[1]
      date2=input$date[2]
      r=input$coupon
      Fv=input$par
      P=input$price*Fv/100
    })
    
    #function-------
    elapsed_months <- function(end_date, start_date) {
      ed <- as.POSIXlt(end_date)
      sd <- as.POSIXlt(start_date)
      12 * (ed$year - sd$year) + (ed$mon - sd$mon)
    }
    
    fun = function (Fv,P,k1,k2,r,n,i) {P*(i/1200)*(1+k1*i/(30*1200))-
        Fv*(r/1200)*(1+k1*i/(30*1200))-
        Fv*(i-r)/1200/(1+k2*i/(30*1200))/(1+i/(1200))^(n-2)}
    
    
    n=elapsed_months(date2,date1)+1
    k1=30-lubridate::day(date1)
    k2=lubridate::day(date2)
    if (k2>30) k2=30
    
    yield=uniroot(fun,Fv=Fv,P=P,k1=k1,k2=k2,n=n,r=r, c(1,90), tol=10e-10)$root
    
    #tabel amortisasi
    tabel=data.frame( NO_URUT=NA,
                      TAHUN=NA,
                      BULAN=NA,
                      HARI=NA,
                      INTEREST_KUPON=NA,
                      EFECTIVE_INTEREST=NA,
                      AMORTISASI_DISCOUNTAKHIR=NA,
                      NILAI_AMORTISASI=Fv-P,
                      NOMINAL_AMORTISASI=P,
                      HARGA_WAJAR=NA)
    
    for(i in c(1:n)){
      if (i ==1){
        tabel[i+1,4]=30-day(date1)
      }else if (i==n){
        if (day(date2)<30){
          tabel[i+1,4]=day(date2)
        } else {
          tabel[i+1,4]=30
        }
      } else{
        tabel[i+1,4]=30
      }
      
      tabel[i+1,1]=i
      tabel[i+1,2]=year(date1)+(month(date1)+i-2)%/%12
      if ((month(date1)+i-1)%%12==0) {
        tabel[i+1,3]=12
      }else {
        tabel[i+1,3]=(month(date1)+i-1)%%12 
      }
      tabel[i+1,5]=Fv*(r/12/30/100)*tabel[i+1,4]
      tabel[i+1,6]=tabel[i,9]*(yield/12/30/100)*tabel[i+1,4]
      tabel[i+1,7]=tabel[i+1,6]-tabel[i+1,5]
      tabel[i+1,8]=tabel[i,8]-tabel[i+1,7]
      tabel[i+1,9]=tabel[i+1,7]+tabel[i,9]
      tabel[i+1,10]=round(tabel[i+1,9]/Fv*100,2)
    }
    
    # ubah ke dalam format currency
    for (i in c(5:9)){
      tabel[,i]=formatC(tabel[,i], big.mark=',', format = 'f',digits = 2)
    }
    
    for (i in c(1:4)){
      tabel[,i]=as.integer(tabel[,i])
    }
    
    result=list(yield=yield, tabel=tabel)
    
    return(result)
  })
  
  output$text <- renderText({
    paste0("", r_data()$yield)
  })
  
  output$tabel_amortisasi <- renderTable({
    r_data()$tabel
  })
}

# Create Shiny app ----
shinyApp(ui, server)
