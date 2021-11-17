load_packages <- function() {
    library(base64enc)
    library(ggplot2)
    library(shiny)
    library(BiocManager)
    options(repos = BiocManager::repositories())
    library(EBImage)
}
load_packages()

# Max file upload size of 1000 MB
options(shiny.maxRequestSize=1000000*1024^2)
options(shiny.fullstacktrace=TRUE, shiny.deepstacktrace=TRUE)

rm(list=ls())

ui_function <- function(){ fluidPage(

    titlePanel(title=div(img(src=base64enc::dataURI(file="binary_fission_image.png", mime="image/png"),width=70,height=70),"Interactive Image Viewer"),windowTitle = "Interactive Image Viewer"),

    sidebarLayout(position = "left",
                  sidebarPanel = sidebarPanel(
                    fileInput(inputId = "image_file",
                                label = "Choose Image File from your computer.",
                                multiple = FALSE,
                                accept = c(".tiff",".tif")),
                    fileInput("file1", "Choose Coordinates CSV File from your computer.",
                              multiple = FALSE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                    radioButtons(inputId = "coord_file_format",
                                 label = "Change Format of Coordinate File Reader Table",
                                 choices = c("Y","N"),selected = "N"),

                    conditionalPanel(condition = "input.coord_file_format == 'Y'",
                    checkboxInput("header", "Header", TRUE),

                    radioButtons("sep", "Separator",
                                 choices = c(Comma = ",",
                                             Semicolon = ";",
                                             Tab = "\t",
                                             Space = " "),
                                 selected = " "),

                    radioButtons("quote", "Quote",
                                 choices = c(None = "",
                                             "Double Quote" = '"',
                                             "Single Quote" = "'"),
                                 selected = '"'),

                    radioButtons("disp", "Display",
                                 choices = c(Head = "head",
                                             All = "all"),
                                 selected = "all"),
                    textInput(inputId = "image_dim",
                              label = "Specify Image Dimensions X by X",
                              value="512",
                              placeholder="512")),
                    textInput("X_coord",label = "Choose an X Coordinate to Label"),
                    textInput("Y_coord",label = "Choose a Y Coordinate to Label")),
                    mainPanel = mainPanel(
                        #plotOutput("uploaded_image"),
                        plotOutput("coords_plot"),
                        tableOutput("contents"))
                    ))}


server <- shinyServer(function(input, output) {

    up_img <- reactive({ req(input$image_file) ; EBImage::readImage(file=input$image_file$datapath) })

    output$uploaded_image <- renderPlot({
        plot(up_img(),all=TRUE)
    })

    output$contents <- renderTable({

        req(input$file1)

        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)

        if(input$disp == "head") {
            return(head(df[order(df$Area,decreasing = TRUE),]))
        }
        else {
            return(df[order(df$Area,decreasing = TRUE),])
        }

    })


    output$coords_plot <- renderPlot({
        req(input$file1)

        discover_sep <- function(){
            req(input$sep)
            sep <- if (input$sep == ", ") {","}
            sep <- if (input$sep == "; ") {";"}
            sep <- if (input$sep == "\t") {"\t"}
            sep <- if (input$sep == " ") {" "}
        }
        chosen_sep <- discover_sep()
        if (!is.null(chosen_sep)){
          coords <- read.csv(file=input$file1$datapath,header=TRUE,sep=chosen_sep)
          coords <- data.frame(coords)
          plot(up_img(),all=TRUE)
          img_dim <- as.numeric(input$image_dim)
          ggplot2::xlim(c(1,img_dim))
          ggplot2::ylim(ylim=c(img_dim,1))
          points(x=coords$X,y=coords$Y,col="red")
          points(x=as.numeric(input$X_coord),y=as.numeric(input$Y_coord),col="blue")
        }
    })

})

shinyApp(ui = ui_function , server)
