library(shiny)
library(tidyverse)

# define plotting function
score_p <- function(m1, m2, fl, iq, pl, rs, at){
  
  raw <- c(m1, m2, fl, iq, pl, rs, at)
  assignment <- c("Midterm 1","Midterm 2","Final Exam","InQuizitive","Peer Labs", "Research","Attendance")
  weight <- c(.15, .15, .15, .15, .15, .1, .15)
  score <- raw*weight
  df <- data.frame(assignment,raw, weight, score)
  total <- sum(df$score)
  
  tg <- ggplot(df,aes(1,1)) + 
    coord_cartesian(ylim = c(.5,1.5)) +
    annotate("text", x= 1, y=1.25, size =15, label = paste("Total Grade")) + 
    annotate("text",x=1,y=1, size =20,label = paste0(total,"%")) +
    theme_void()
  
  return(tg)
  
}

weight_p <- function(m1, m2, fl, iq, pl, rs, at){
  
  raw <- c(m1, m2, fl, iq, pl, rs, at)
  assignment <- c("Midterm 1","Midterm 2","Final Exam","InQuizitive","Peer Labs", "Research","Attendance")
  weight <- c(.15, .15, .15, .15, .15, .1, .15)
  score <- raw*weight
  df <- data.frame(assignment,raw, weight, score)
  total <- sum(df$score)
  
  df3 <- df %>% select(assignment,score) 
  df3 <- rbind(df3,c("lost points",(100-total)))
  df3$score <- as.numeric(df3$score)
  df3$assignment <- factor(df3$assignment, levels = rev(df3$assignment))

  return(pie(x = df3$score, labels = df3$assignment, 
               col = c("#F0F9E8", "#CCEBC5", "#A8DDB5", "skyblue", "#4EB3D3", "#2B8CBE", "#08589E","red"),
               main = 'weighted points'))
  
}

# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("Enter Percentage"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("m1",
                  "Midterm 1:",
                  min = 0,  max = 100, value = 80),
      sliderInput("m2",
                  "Midterm 2:",
                  min = 0,  max = 100, value = 80),
      sliderInput("fl",
                  "Final Exam:",
                  min = 0,  max = 100, value = 80),
      sliderInput("iq",
                  "InQuizitive:",
                  min = 0,  max = 100, value = 80),
      sliderInput("pl",
                  "Peer labs:",
                  min = 0,  max = 100, value = 80),
      sliderInput("rs",
                  "Research:",
                  min = 0,  max = 100, value = 80),
      sliderInput("at",
                  "Attendance:",
                  min = 0,  max = 100, value = 80),

      hr(),
      helpText("")
      , width = 3),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("totalPlot", height = 200, width = 600 ),
      plotOutput("piePlot", height = 600, width = 600 )  
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$totalPlot <- renderPlot({
    score_p(input$m1, input$m2, input$fl, input$iq, input$pl, input$rs, input$at)})
    
    output$piePlot <- renderPlot({
      weight_p(input$m1, input$m2, input$fl, input$iq, input$pl, input$rs, input$at)})
    

}

# Run the application 
shinyApp(ui = ui, server = server)
