#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(magrittr)
library(purrr)


## PARAMETERS
set.seed(13)
map_param <- list(
  "map" = list(
    "x_size" = 10,
    "y_size" = 10
  ),
  "peaks" = list(
    n_peaks = 4,
    size = 3
  ),
  "wind" = c(-1,1)  ## Winds blow NW (-1,1) to SW (-1,-1)
)

player_param <- list(
  "movement" = .5,
  "vision" = 1.5,

)


## generate xy axes based on parameters
map_param$map$x_seq = 1: map_param$map$x_size
map_param$map$y_seq = 1: map_param$map$y_size

## generate background sea

bkgrd <- data.frame(
  "x" = c(-1.5,1.5,1.5,-1.5)*map_param$map$x_size,
  "y" = c(1.5,1.5,-1.5,-1.5)*map_param$map$y_size
)




## generate mt peak coordinates

map_param$peaks$coords <- data.frame(
  
  "x" = runif(n = map_param$peaks$n_peaks,
              min = map_param$map$x_seq[1],
              max = map_param$map$x_size),
  
  "y" = runif(n = map_param$peaks$n_peaks,
                   min = map_param$map$y_seq[1],
                   max = map_param$map$y_size)
)

## Fill peaks

#### using the "errosion process", generate a cluster around the peak POINT in order to define a peak polygon.

#### peaks use the smallest sd, forest uses a moderate size sd and beaches will use a large SD, but limited geography

## Erode mts


ee <- apply(map_param$peaks$coords,1,function(x){
  data.frame(
    "x" = rnorm(20,x[1],.5),
    "y" = rnorm(20,x[2],1)
  )
})

ee1 <- NULL
for(rr in 1:length(ee)){
  ee1 <- rbind(ee1, ee[[rr]])
}

ee_poly <- vector(mode = "list",length=length(ee))

for(ll in 1:length(ee)){
  ee_poly[[ll]] <- ee[[ll]][chull(ee[[ll]]),]
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("exploreR"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          ## CONTROLS
          
          #### Movement
          
          fluidRow(actionButton("move_up","Up")),
          fluidRow(actionButton("move_left","Left"),
                   actionButton("move_down","Down"),
                   actionButton("move_right","Right")
          )
        ),
      
          
        mainPanel(
          
          ## MAIN MAP
           plotOutput("main_map")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  player <- reactiveValues(
    "cur_x" = map_param$peaks$coords$x[1],
    "cur_y" = map_param$peaks$coords$y[1],
    
  )
  
  mask <- reactiveValues(
    "mask" = bkgrd
  )
  ## MOVEMENT CONTROLS
  observeEvent(input$move_up,{
    player$cur_y <- player$cur_y + player_param$movement})
  
  observeEvent(input$move_down,{
    player$cur_y <- player$cur_y - player_param$movement})
  
  observeEvent(input$move_left,{
    player$cur_y <- player$cur_x - player_param$movement})
  
  observeEvent(input$move_right,{
    player$cur_y <- player$cur_x + player_param$movement})
  
  

    output$main_map <- renderPlot({
      
      ## basemap
      par("mar" = c(.1,.1,.1,.1))
      plot(map_param$map$x_seq, 
           map_param$map$y_seq,
           xlab ="",
           ylab = "",
           xaxt = "n",
           yaxt = "n",
           type = "n")
      
      
      polygon(bkgrd,col = hsv(.6,.6,.8))
      
      ## plot islands
      for(pp in seq_along(ee_poly)){
        polygon(ee_poly[[pp]], col = hsv(.35,.2,.8))
      }
      
      ## add mountains
      points(map_param$peaks$coords, pch = 16, cex = map_param$peaks$size, col = hsv(.1,.8,.2))
      
      ## add character
      
      points(player$cur_x, player$cur_y, pch = 8, col = "red", cex = 2)
      
       
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
