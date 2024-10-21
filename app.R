library(shiny)
library(shinyauthr)
library(shiny)
library(sortable)
library(dplyr)
library(googlesheets4)
library(shinyjs)

gs4_auth(cache = ".secrets", email = "andrew.dau21@gmail.com")


# dataframe that holds usernames, passwords and other user data
user_base <- tibble::tibble(
  user = c("andrew", "amanda"),
  password = sapply(c("ajd4205h", "Lucy2024!"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)

ui <- fluidPage(
  # logout button
  #div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  
  # login section
  shinyauthr::loginUI(id = "login"),
  
   # Plot to show user info after login
 # plotOutput("distPlot")
  #uiOutput("mainbucket
 column(width = 4),
 column(width = 4,
  uiOutput("mainsection"),
 # textInput("name", "Your Name:", placeholder = "Enter your name"),
 # textInput("email", "Email Address:", placeholder = "Enter your email"),
 # selectInput("category", "Category:", choices = c("Option 1", "Option 2", "Option 3")),
 # textAreaInput("comments", "Comments:", rows = 5),
 # actionButton("submit", "Submit"),
  
  uiOutput("save")),
 column(width=4)
 
 #verbatimTextOutput('summary')
)

server <- function(input, output, session) {
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # Logout to hide
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  
  output$mainsection <- renderUI({
    
    # Show only when authenticated
    req(credentials()$user_auth)
    tagList(
    textInput("item", "Item:", placeholder = "Enter item"),
    selectInput("location", "Location:", choices = sort(c("Hill","MegaDeck","UnderDeck","Deck","Barn","Garden","Front Yard","SheShed","Driveway"))),
    textAreaInput("details", "Details:", rows = 5),
    #actionButton("submit", "Submit")
    )
    
  })
  
 
  output$mainbucket <- renderUI({
    req(credentials()$user_auth)
    
    bucket_list(
      header = "Drag Items into the Ranking Bucket.  Top = Rank 1",
      group_name = "bucket_list_group",
      orientation = "horizontal",
      add_rank_list(
        text = "Drag from here",
        labels = list(
          "Evaluation of MACEscrape vs NASS lists vs RDDscrape for hemp",
          "Non-FSA List Frame Coverage Project",
          "Off-farm grain stocks outreach program",
          "CDL-modernization (platform agnostic)",
          "FSA as a frame (CDL acreage estiamtion modernization)",
          "Create and disseminate Ag Census",
          "Propensity, Impact, and Estimation (PIE) research",
          "Translating climate indicators into decisions Winter Wheat Pilot",
          "Land Values Model",
          "Census Estimation Research",
          "Data driven edit limit research"
          
        ),
        input_id = "rank_list_1"
      ),
      add_rank_list(
        text = "Ranking Bucket",
        labels = NULL,
        input_id = "rank_list_2"
      )
    )
  })
 
  output$save <- renderUI({
    req(credentials()$user_auth)
    actionButton('saveit',"Submit")
  }) 
  
  observeEvent(input$saveit, {
    #abc <<- input$rank_list_1
    if(length(input$rank_list_1) > 0)
    {
      showModal(modalDialog(
        title="Try Again",
        "You haven't ranked all the items!"
      ))
      
    }
    else{
    #sheet_id <- "https://docs.google.com/spreadsheets/d/1nRe1ZF-8HDV7HFprffNzKuC9iEvO7fD8r8xO5Rn9v3k/"
    sheet_id <- "https://docs.google.com/spreadsheets/d/1_uB8qh9F6qB59IAD67Hks3XMtTQviLr3432sfuKVvPI/"

    
    data <- as.data.frame(input$item)
    data$i <- input$location
    data$category <- input$details
    data$user <- credentials()$info$user
    data$date <- Sys.time()
    print(credentials()$user)
    #data$user <- credentials()$
    sheet_append(sheet_id, data)
    shinyjs::hide('saveit')
    showModal(modalDialog(
      title="Thank You",
      "Dismiss to Enter Another Item"
    ))
    
    }
   
  })
  output$summary <- renderText({
    ls(env=session$request)
  })

}

shinyApp(ui = ui, server = server)