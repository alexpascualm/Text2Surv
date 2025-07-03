### Login ###

#### PASSWORD server code ---------------------------------------------------- 
# reactive value containing user's authentication status
user_input <- reactiveValues(authenticated = FALSE, valid_credentials = FALSE, 
                             user_attempts = 5, status = "", hospital = "")

output$authorization <- reactive({
  if (user_input$authenticated) { return(1) }
  else { return(0) }
})

outputOptions(output, 'authorization', suspendWhenHidden = FALSE)

# authenticate user by:
#   1. checking whether their user name and password are in the credentials 
#       data frame and on the same row (credentials are valid)
#   2. if credentials are valid, retrieve their lockout status from the data frame
#   3. if user has failed login too many times and is not currently locked out, 
#       change locked out status to TRUE in credentials DF and save DF to file
#   4. if user is not authenticated, determine whether the user name or the password 
#       is bad (username precedent over pw) or he is locked out. set status value for
#       error message code below
observeEvent(input$login_button, {
  credentials <- readRDS("./credentials/credentials.rds")
  
  row_username <- which(credentials$user == input$user_name)
  row_password <- which(credentials$pw == digest(input$password)) # digest() makes md5 hash of password
  
  # if user name row and password name row are same, credentials are valid
  #   and retrieve locked out status
  if (length(row_username) == 1) {
    user_input$user_attempts <- credentials$attemps[row_username]
    
    if(length(row_password) >= 1 &&  # more than one user may have same pw
       (row_username %in% row_password)) {
      user_input$valid_credentials <- TRUE
    } else {
      user_input$valid_credentials <- FALSE
    }
  }
  
  # if a user has valid credentials and is not locked out, he is authenticated      
  if (user_input$valid_credentials == TRUE && user_input$user_attempts > 0) {
    user_input$authenticated <- TRUE
  } else {
    user_input$authenticated <- FALSE
  }
  
  # if user is not authenticated, set log in status variable for error messages below
  if (user_input$authenticated == FALSE) {
    if (user_input$user_attempts <= 0) {
      user_input$status <- "locked_out"
    } else if (length(row_username) > 1) {
      user_input$status <- "credentials_data_error"  
    } else if (input$user_name == "" || length(row_username) == 0) {
      user_input$status <- "bad_user"
    } else if (input$password == "" || length(row_password) == 0) {
      user_input$user_attempts <- user_input$user_attempts - 1
      # Save new attempts
      credentials$attemps[row_username] <- user_input$user_attempts
      saveRDS(credentials, "./credentials/credentials.rds")
      
      if (user_input$user_attempts == 0) {
        user_input$status <- "bad_password_locked"
      } else {
        user_input$status <- "bad_password"
      }
    }
  } else {
    # Restart number of attempts for the user
    credentials$attemps[row_username] <- 5
    saveRDS(credentials, "./credentials/credentials.rds")
    user_input$status <- "correct"
    user_input$hospital <- credentials$hosp[row_username]
    load_RData_df()
  }
})   

# password entry UI componenets:
#   username and password text fields, login button
output$uiLogin <- renderUI({
  wellPanel(
    shiny::tags$h2("Acceso a la aplicación", class = "text-center", style = "padding-top: 0;"),
    br(),
    shiny::textInput("user_name", shiny::tagList(shiny::icon("user"), "Usuario:")),
    shiny::passwordInput("password", shiny::tagList(shiny::icon("unlock-alt"), "Contraseña:")),
    shiny::div(
      style = "text-align: center;",
      actionButton("login_button", label=HTML("<b>Acceder</b>"), class = "btn-primary")
    )
  )
})

# red error message if bad credentials
output$pass <- renderUI({
  if (user_input$status == "locked_out") {
    h5(strong("Cuenta bloqueada por sobrepasar los intentos permitidos.\r\n
                Contacte con el administrador", style = "color:red"), align = "center")
  } else if (user_input$status == "credentials_data_error") {    
    h5(strong("Error con las credenciales.\r\nContacte con el administrador", 
              style = "color:red"), align = "center")
  } else if (user_input$status == "bad_user") {
    h5(strong("Usuario no registrado", style = "color:red"), align = "center")
  } else if (user_input$status == "bad_password") {
    h5(strong("Contraseña incorrecta", style = "color:red"), align = "center")
  } else if (user_input$status == "bad_password_locked") {
    h5(strong("Contraseña incorrecta."), style = "color:red", align = "center")
    h5(strong("Ha sobrepasado los intentos permitidos"), style = "color:red", align = "center")
  } else {
    ""
  }
})  
### Fin LOGIN ###