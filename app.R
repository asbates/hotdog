
library(shiny)
library(keras)


ui <- fluidPage(

  titlePanel("Hotdog? Not Hotdog?", windowTitle = "Hotdog"),

  sidebarLayout(

    sidebarPanel(

      fileInput(
        "imageinput",
        "Choose an image",
        accept = c('image/png', 'image/jpeg')

      ),

      actionButton(
        "gobutton",
        "Go!"
      )

    ),

    mainPanel(

      htmlOutput("textresult")

    )

  )


)


server <- function(input, output){

  model <- load_model_hdf5("2019-04-26-vgg16-fine-tune.h5")

  make_pred <- eventReactive(
    input$gobutton,
    {
      req(input$imageinput)

      img <- image_load(input$imageinput$datapath, target_size = c(150, 150))
      img_array <- image_to_array(img)
      img_array <- array_reshape(img_array, c(1, 150, 150, 3))
      img_array <- img_array / 255

      # 0 is hot dog, 1 is not hot dog
      pred <- predict_classes(model, img_array)
      pred[1, 1]
    }
  )

  output$textresult <- renderText({

    pred_numeric <- make_pred()
    if (pred_numeric){
      paste0(
        "<h1 style=\"font-size:60px;\"><span style=\"color:red\">",
        "Not Hotdog!",
        "</span></h1>"
        )
    } else {
      paste0(
        "<h1 style=\"font-size:60px;\"><span style=\"color:green\">",
        "Hotdog!",
        "</span></h1>"
        )
    }

  })

}


shiny::shinyApp(ui = ui, server = server)


