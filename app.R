load_packages <- function() {
    library(fs)
    library(openxlsx)
    library(base64enc)
    library(ggplot2)
    library(shiny)
    library(magrittr)
    library(raster)
    library(dplyr)
    library(BiocManager)
    options(repos = BiocManager::repositories())
    library(EBImage)
    library(stringr)
    library(dynamicTreeCut)
    library(pdftools)
}
load_packages()

# Max file upload size of 100 MB
options(shiny.maxRequestSize=100*1024^2)
options(shiny.fullstacktrace=TRUE, shiny.deepstacktrace=TRUE)

rm(list=ls())

ui_function <- function(){ fluidPage(

    titlePanel(title=div(img(src=base64enc::dataURI(file="binary_fission_image.png", mime="image/png"),width=70,height=70),"Doublet Counter"),windowTitle = "Doublet Counter"),

    tags$a(href="https://github.com/Juan-CN/doublet-counter/","Macro for Generating Results.csv file --- Instructions for its Use --- and Program Source Code","\n"),

    sidebarLayout(position = "left",
                  sidebarPanel = sidebarPanel(
                      radioButtons(inputId = "real_data_synth_data_generation",
                                   label = "Analyze Real Data or Generate Synthetic Data",
                                   choices = c("Real", "Synth"),
                                   selected = "Synth"),
                      conditionalPanel(condition = "input.real_data_synth_data_generation == 'Real'",

                                       radioButtons(inputId = "image_summary_plot_button",
                                                    label = "1. Print Image Summary Plot(s)",
                                                    choices = c("y","n"),
                                                    selected = "n"),
                                       conditionalPanel(condition = "input.image_summary_plot_button == 'y'",
                                                        fileInput(inputId = "images_file_input",
                                                                  label = "Choose Image File(s) from your computer.",
                                                                  multiple = TRUE,
                                                                  accept = c(".tiff",".tif"))),
                                       textInput(inputId = "thres_value",
                                                 label = "2. Set Distance Threshold value",
                                                 value = 4.0,
                                                 placeholder = "number"),
                                       textInput(inputId = "area_value",
                                                 label = "3. Set Doublet Dot Area Threshold value",
                                                 value = 7.0,
                                                 placeholder = "number"),
                                       fileInput(inputId = "results_csv_file",
                                                 label = "4. Choose Results.csv File from your computer.",
                                                 multiple = FALSE,
                                                 accept = c(".csv")),
                                       radioButtons(inputId = "organize_or_not",
                                                    label = "Organize Analysis files for download",
                                                    choices = c("y","n"),
                                                    selected = "n"),
                                       conditionalPanel(condition = "input.organize_or_not == 'y'",
                                                        textInput(inputId = "organize_analysis_files_conditions",
                                                                  label = "Conditions by which to Organize Analysis files",
                                                                  value="",
                                                                  placeholder="word1, word2, word3, etc"),
                                                        actionButton(inputId = "organize_files_button",label = "Organize Files")),
                                       downloadButton(outputId = "analysis_files_download_button",
                                                      label = "5. Download Analysis Files")),

                      #cat(file = stderr(), "UI created", "\n"),
                      conditionalPanel(condition = "input.real_data_synth_data_generation == 'Synth'",
                                       textInput(inputId = "dot_number_sequence",
                                                 label = "Dot Number Sequence: Beginning, End, Step_Size",
                                                 value="25, 500, 25",
                                                 placeholder="25, 500, 25"),
                                       textInput(inputId = "number_of_images",
                                                 label = "How Many Images to Create per Set in Sequence?",
                                                 value="100",
                                                 placeholder="100"),
                                       textInput(inputId = "image_dim",
                                                 label = "Create Images of Dimensions X by X",
                                                 value="512",
                                                 placeholder="512"),
                                       textInput(inputId = "dot_sizes",
                                                 label = "Dot Sizes Ex: 1-25, 1-2 etc",
                                                 value="1, 25",
                                                 placeholder="1, 25"),
                                       radioButtons(inputId = "custom_prob_file",
                                                    label = "Use Default Dot Size Probability Distribution or Custom Probability File (Not Yet!)",
                                                    choices = c("d","c"),
                                                    selected = "d"),
                                       conditionalPanel(condition = "input.custom_prob_file == 'c'",
                                                        fileInput(inputId = "prob_dist_file_input",
                                                                  label = "Choose Dot Size Probability Distribution File (.csv) from your Computer.",
                                                                  multiple = FALSE,
                                                                  accept = c(".csv"))),
                                       radioButtons(inputId = "cell_type",
                                                    label = "Choose Cell Type to Emulate in Synthetic Images",
                                                    choices = c("Nurse","Oocyte","Other"),
                                                    selected = "Nurse"),
                                       conditionalPanel(condition = "input.cell_type == 'Other'",
                                                        textInput(inputId = "custom_margin_value",
                                                                  label = "Custom Margin Value: Defines Area of Canvas for Other Cell Type",
                                                                  value="250",
                                                                  placeholder="250")),
                                       actionButton(inputId = "create_synthetic_images_button",label = "Create Synthetic Images"),
                                       downloadButton(outputId = "synthetic_files_download_button",
                                                      label = "Download Synthetic Image Files"),
                                       radioButtons(inputId = "image_summary_plot_button_2",
                                                    label = "Print Image Summary Plot(s)",
                                                    choices = c("y","n"),
                                                    selected = "n"),
                                       conditionalPanel(condition = "input.image_summary_plot_button_2 == 'y'",
                                                        fileInput(inputId = "images_file_input_2",
                                                                  label = "Choose Image File(s) from your computer.",
                                                                  multiple = TRUE,
                                                                  accept = c(".tiff"))),
                                       textInput(inputId = "thres_value_2",
                                                 label = "Set Distance Threshold value",
                                                 value = 4.0,
                                                 placeholder = "number"),
                                       textInput(inputId = "area_value_2",
                                                 label = "Set Doublet Dot Area Threshold value",
                                                 value = 7.0,
                                                 placeholder = "number"),
                                       fileInput(inputId = "results_csv_file_2",
                                                 label = "Choose Results.csv File you Created from Downloaded Synthetic Images.",
                                                 multiple = FALSE,
                                                 accept = c(".csv")),
                                       downloadButton(outputId = "analysis_files_download_button_2",
                                                      label = "Download Analysis Files")),
                      cat(file=stderr(),"UI Created","\n"),



                  ), mainPanel = mainPanel(tableOutput(outputId = "text"))



    )


)}

server <- shinyServer(function(input, output) {
    observe({
        if(input$real_data_synth_data_generation == 'Real'){
            cat(file=stderr(),"Real Code Block","\n")
            # Show Binary Fission Image next to Title
            output$binary_fission_image<-renderImage({
                list(src = "./binary_fission_image.png")}, deleteFile = FALSE)

            # Remove old Analysis files
            start_fresh <- function(){
                unlink(x = list.files(pattern = ".csv"))
                unlink(x = list.files(pattern = ".xlsx"))
                unlink(x = list.files(pattern = ".pdf"))
                unlink(x = list.files(pattern = ".eps"))
                unlink(x = list.files(pattern = ".svg"))
                unlink(x = list.dirs(path = "./packrat"), recursive=TRUE)
                fs::dir_delete(list.dirs(recursive=FALSE)[list.dirs(recursive=FALSE) != "./rsconnect"])
            }
            start_fresh()

            # Show first three lines of Results.csv File to User
            output$contents <- renderTable(striped = TRUE, bordered = TRUE, rownames = FALSE, colnames = TRUE, {
                req(input$results_csv_file)
                if (is.null(input$results_csv_file) == FALSE) {
                    data <- head(read.csv2(file = input$results_csv_file$datapath,
                                           sep=",",
                                           dec=".",
                                           header=TRUE))[1:3,];
                    req(input$results_csv_file$datapath);
                    req(data)
                }
            })

            # Show names of uploaded Image Files to User
            #output$uploaded_image_files <- renderTable(striped = TRUE, bordered = TRUE, rownames = FALSE,colnames = TRUE,{
            #     if (is.null(input$images_file_input) == FALSE) {
            #         req(input$images_file_input$name);
            #         Images<-input$images_file_input$name;
            #         data.frame(Images)
            #     }
            # })
            output$uploaded_image_files <- renderDataTable(expr={
                if (is.null(input$images_file_input) == FALSE) {
                    req(input$images_file_input$name);
                    Images<-input$images_file_input$name;
                    data.frame(Images)
                }
            },options = list(pageLength = 5, info = FALSE))

            # Carry out the Analysis
            output$text<-renderTable({
                if (is.null(input$results_csv_file) == FALSE) {
                    withProgress(message = "Obtaining coordinates from Results.csv", min = 0, max = 1, style = "old", value = 0,
                                 expr = {
                                     req(input$results_csv_file);

                                     store_Results_input_on_disk_and_read_into_mem <- function() {
                                         file.copy(from = input$results_csv_file$datapath,to="Results.csv", overwrite = TRUE);
                                         Results<<-read.table(file = "Results.csv", header = TRUE, sep = ",", dec = ".");
                                         openxlsx::write.xlsx(x=Results,file="Results.xlsx",col.names = TRUE,row.names = FALSE)
                                     };
                                     store_Results_input_on_disk_and_read_into_mem();

                                     extract_coordinates_from_Results_to_files<-function() {
                                         #Results<<-Results[,c("X","Y","Filename")]
                                         Results<<-Results[,c("Area","X","Y","Filename")]
                                         file_names<<-unique(Results$Filename)
                                         files<<-paste0(sub(file_names,
                                                            pattern = ".tif",
                                                            replacement = ""), "_coords")
                                         n<-NULL
                                         for (n in 1:length(file_names)){
                                             results_filtered<-dplyr::filter(Results,
                                                                             Results$Filename == file_names[n]);
                                             write.table(x = results_filtered,
                                                         file = paste0(files[n],".csv"),
                                                         row.names = FALSE)
                                         }
                                     };
                                     extract_coordinates_from_Results_to_files();

                                     cat(file=stderr(),"Coordinates extracted","\n");

                                     count_doublets<-function() {
                                         req(input$image_summary_plot_button);
                                         get_files <- function() {
                                             directory<-getwd();
                                             files<-list.files(path = directory, pattern = ".csv");
                                             files<-files[files != "Results.csv"];
                                             files<<-sort(files);
                                             return(files)
                                         }
                                         get_files() %>% invisible()
                                         cat(file=stderr(),"Files listed","\n")
                                         set_variables <- function() {
                                             x<<-NULL
                                             i<<-NULL
                                             thres<<-as.numeric(input$thres_value)
                                             area_val<<-as.numeric(input$area_value)
                                             number1<<-NULL
                                             number2<<-NULL
                                             number3<<-NULL
                                             number4<<-NULL
                                             number5<<-NULL
                                             doublets<<-NULL
                                             dots<<-NULL
                                             data_table<<-NULL
                                             img_file_name<<-NULL
                                             num_neighbors<<-NULL
                                             image_summary_plot<<-input$image_summary_plot_button
                                             hc.c <<- NULL
                                             member_dynam.c <<- NULL
                                             member_dynam_table <<-NULL
                                         }
                                         set_variables()
                                         cat(file=stderr(),"Variables set","\n")

                                         for (x in seq_along(files)){
                                             cat(file=stderr(),"Analysis in progress","\n");
                                             print(x);
                                             print(files[x]) #cat(file=stderr(),print(file[x]))
                                             incProgress(amount=(1/(x*1000)),message="Calculating doublets...");
                                             data_coords<<-read.table(file = files[x], header = TRUE, dec = ".");
                                             #data_coords<<-data_coords[,c("X","Y")];
                                             data_coords<<-data_coords[,c("Area","X","Y")];
                                             coord_mat2<<-data_coords[c("X","Y")]
                                             #
                                             possible_multiple_coords<<-data_coords[c("X","Y")][which(data_coords$Area > area_val),]
                                             #num_area_multiples <<- nrow(possible_multiple_coords)
                                             # Doublet dots in line below
                                             data_coords<<-data_coords[c("X","Y")][which(data_coords$Area<=area_val),]

                                             if (all(is.na(data_coords)) == TRUE) {
                                                 empty_data_coords <- function(){
                                                     dots<<-dim(coord_mat2)[1];
                                                     num_neighbors<<-0;
                                                     doublets<<-0;
                                                     #
                                                     num_area_multiples <<- nrow(possible_multiple_coords);
                                                     num_dist_multiples <<- 0

                                                     #
                                                     number1<<-append(x=number1,values=dots);
                                                     number2<<-append(x=number2,values=num_neighbors);
                                                     number3<<-append(x=number3,values=doublets);
                                                     number4<<-append(x=number4,values=num_area_multiples);
                                                     number5<<-append(x=number5,values=num_dist_multiples);
                                                     return(c(number1,number2,number3,number4,number5))
                                                 }
                                                 empty_data_coords()
                                             } else {

                                                 coord_mat<<-as.matrix(round(data_coords, digits=2));
                                                 num_points<<-nrow(data_coords);
                                                 dist_p1_p2<<-round(pointDistance(p1=coord_mat[1:num_points,],lonlat=FALSE,allpairs=TRUE),digits=2);
                                                 neighbors<<-which(dist_p1_p2 <= thres & dist_p1_p2 != 0,arr.ind=TRUE);
                                                 cat(file=stderr(),"Neighbors","\n");

                                                 if (all(is.na(neighbors)) == "TRUE") {
                                                     assign_values <- function() {
                                                         dots<<-dim(coord_mat2)[1];
                                                         num_neighbors<<-0;
                                                         doublets<<-0;
                                                         #
                                                         num_area_multiples <<- nrow(possible_multiple_coords);
                                                         num_dist_multiples <<- 0
                                                         #
                                                         number1<<-append(x=number1,values=dots);
                                                         number2<<-append(x=number2,values=num_neighbors);
                                                         number3<<-append(x=number3,values=doublets);
                                                         #
                                                         number4<<-append(x=number4,values=num_area_multiples);
                                                         number5<<-append(x=number5,values=num_dist_multiples)
                                                         #
                                                         return(c(number1,number2,number3,number4,number5))
                                                     }
                                                     assign_values()
                                                 } else {
                                                     return_values <- function() {

                                                         neighbor_coords<<-unique(coord_mat[neighbors,]);
                                                         cat(file=stderr(),"Neighbor_Coords created","\n");
                                                         neighbor_distances<<-pointDistance(p1=neighbor_coords,lonlat=FALSE);
                                                         neighbor_distances[neighbor_distances == 0]<-NA ;
                                                         sum_dist_neigh_coords<<-apply(neighbor_distances<=thres,1,sum,na.rm=TRUE);
                                                         neighbor_coords2<<-neighbor_coords[which(sum_dist_neigh_coords==1),];
                                                         cat(file=stderr(),"Neighbor_Coords2, just before multiple mitigation","\n");
                                                         neighbor_distances2<<-pointDistance(p1=neighbor_coords2,lonlat=FALSE);
                                                         if (length(neighbor_distances2) == 1) {
                                                             assign_values2 <- function() {
                                                                 dots<<-dim(coord_mat2)[1];
                                                                 num_neighbors<<-0;
                                                                 doublets<<-0;
                                                                 num_area_multiples <<- nrow(possible_multiple_coords);
                                                                 #
                                                                 num_dist_multiples <<- 0
                                                                 #
                                                                 number1<<-append(x=number1,values=dots);
                                                                 number2<<-append(x=number2,values=num_neighbors);
                                                                 number3<<-append(x=number3,values=doublets);
                                                                 number4<<-append(x=number4,values=num_area_multiples)
                                                                 #
                                                                 number5<<-append(x=number5,values=num_dist_multiples)
                                                                 #
                                                                 cat(file=stderr(),"Neighbor distances = 0 block","\n")
                                                                 return(c(number1,number2,number3,number4,number5))
                                                             }
                                                             assign_values2()
                                                         } else {
                                                             neighbor_distances2[neighbor_distances2 == 0]<-NA ;
                                                             indices_original_points<<-which(sum_dist_neigh_coords==1);
                                                             indices_new_points<<-which(apply(neighbor_distances2<=thres,1,sum,na.rm=TRUE)==0);
                                                             if (all(is.na(indices_new_points)) == "TRUE") {} else {
                                                                 neighbor_coords2[indices_new_points,]<-NA ;
                                                                 cat(file=stderr(),"Neighbor_Coords2 , NA Introducuction","\n");
                                                                 na_removal_indices<-which(is.na(neighbor_coords2[,1]) == "TRUE");
                                                                 cat(file=stderr(),"Neighbor_Coords2 , NA Removal","\n");
                                                                 neighbor_coords2<-neighbor_coords2[-na_removal_indices,];
                                                                 cat(file=stderr(),"Neighbor_Coords2 truncated to true size","\n");
                                                             }

                                                             if (all(is.na(neighbor_coords2)) == "TRUE") {
                                                                 assign_values2 <- function() {
                                                                     dots<<-dim(coord_mat2)[1];
                                                                     num_neighbors<<-dim(neighbor_coords)[1];
                                                                     cat(file=stderr(),"Neighbor_Coords2 is Missing Block","\n");
                                                                     doublets<<-0;
                                                                     num_area_multiples <<- nrow(possible_multiple_coords);

                                                                     #
                                                                     neighbors_not_doublets <<- dplyr::anti_join(data.frame(neighbor_coords),data.frame(neighbor_coords2))



                                                                     if (all(is.na(neighbors_not_doublets)) == TRUE | (as.numeric(nrow(neighbors_not_doublets)) == 1)){

                                                                         assign_values5 <- function(){

                                                                             num_dist_multiples <<- 0
                                                                             number5<<-append(x=number5,values=num_dist_multiples)
                                                                             return(number5)
                                                                         }
                                                                         assign_values5()

                                                                     } else {

                                                                         #if (as.numeric(nrow(neighbors_not_doublets)) > 1) {
                                                                         assign_values6 <- function(){
                                                                             cat(file=stderr(),"Not Or Block","\n")
                                                                             custom_distance <- dist(x=neighbors_not_doublets)
                                                                             hc.c <- hclust(custom_distance)
                                                                             # member_dynam.c <- dynamicTreeCut::cutreeDynamic(dendro = hc.c,
                                                                             #                                                  minClusterSize = 3,
                                                                             #                                                  method = "hybrid",
                                                                             #                                                  distM = as.matrix(custom_distance),
                                                                             #                                                  deepSplit = 4)
                                                                             member_dynam.c <- dynamicTreeCut::cutreeDynamic(dendro = hc.c,
                                                                                                                             minClusterSize = 2,
                                                                                                                             method = "hybrid",
                                                                                                                             distM = as.matrix(custom_distance),
                                                                                                                             deepSplit = 3)

                                                                             num_dist_multiples <<- length(as.vector(table(member_dynam.c))) # of clusters
                                                                             number5 <<- append(x=number5,values=num_dist_multiples)
                                                                         }
                                                                         assign_values6()
                                                                     }


                                                                     #

                                                                     number1<<-append(x=number1,values=dots);
                                                                     number2<<-append(x=number2,values=num_neighbors);
                                                                     number3<<-append(x=number3,values=doublets);
                                                                     number4<<-append(x=number4,values=num_area_multiples)
                                                                     #number5<<-append(x=number5,values=num_dist_multiples)
                                                                     return(c(number1,number2,number3,number4,number5))
                                                                 }
                                                                 assign_values2()
                                                             } else {
                                                                 assign_values3 <- function() {
                                                                     neighbor_coords2<<-neighbor_coords2[1:dim(neighbor_coords2)[1],];
                                                                     write.table(x=neighbor_coords2,file=paste0("Neighb_Coords_2_",sub(x=files[x],pattern="_coords.csv",replacement=""),".csv"))
                                                                     openxlsx::write.xlsx(x=neighbor_coords2,file=paste0("Neighb_Coords_2_",sub(x=files[x],pattern="_coords.csv",replacement=""),".xlsx"),col.names=TRUE,row.names=FALSE)
                                                                     cat(file=stderr(),"Neighbor_Coords2, Dim","\n");
                                                                     neighbor_distances2<<-pointDistance(p1=neighbor_coords2,lonlat=FALSE);
                                                                     neighbor_distances2[neighbor_distances2 == 0]<-NA ;
                                                                     dots<<-dim(coord_mat2)[1] ;
                                                                     num_neighbors<-dim(neighbor_coords2)[1] ;
                                                                     doublets<<-(sum(apply(neighbor_distances2<=thres,1,sum,na.rm=TRUE),na.rm=TRUE)/2);
                                                                     doublets<<-round(doublets,digits=0);
                                                                     num_area_multiples <<- nrow(possible_multiple_coords);

                                                                     #
                                                                     neighbors_not_doublets <<- dplyr::anti_join(data.frame(neighbor_coords),data.frame(neighbor_coords2))
                                                                     if (all(is.na(neighbors_not_doublets)) == "TRUE" | as.numeric(nrow(neighbors_not_doublets)) == 1){
                                                                         assign_values5 <- function(){
                                                                             num_dist_multiples <<- 0
                                                                             number5<<-append(x=number5,values=num_dist_multiples)
                                                                             return(number5)
                                                                         }
                                                                         assign_values5()
                                                                     } else {
                                                                         custom_distance <<- dist(x=neighbors_not_doublets)
                                                                         hc.c <<- hclust(custom_distance)
                                                                         # member_dynam.c <- dynamicTreeCut::cutreeDynamic(dendro = hc.c,
                                                                         #                                                 minClusterSize = 3,
                                                                         #                                                 method = "hybrid",
                                                                         #                                                 distM = as.matrix(custom_distance),
                                                                         #                                                 deepSplit = 4)
                                                                         member_dynam.c <<- dynamicTreeCut::cutreeDynamic(dendro = hc.c,
                                                                                                                          minClusterSize = 2,
                                                                                                                          method = "hybrid",
                                                                                                                          distM = as.matrix(custom_distance),
                                                                                                                          deepSplit = 3)

                                                                         if (all(member_dynam.c) == 0) {
                                                                             member_dynam.c <<- rep(x=1,times=length(member_dynam.c))
                                                                         }

                                                                         num_dist_multiples <<- length(as.vector(table(member_dynam.c))) # of clusters
                                                                         number5 <<- append(x=number5,values=num_dist_multiples)
                                                                     }


                                                                     if (input$image_summary_plot_button=="y"){
                                                                         image_files<-sub(sub(x=files,pattern="_coords",replacement=".tif"),pattern=".csv",replacement="")
                                                                         image_files<-sort(image_files)
                                                                         combined_name_path<-transform(input$images_file_input,newcol=paste(name,datapath))
                                                                         combined_name_path<-sort(combined_name_path$newcol)
                                                                         combined_name_path<-as.character(combined_name_path)
                                                                         sep_list<-strsplit(x=combined_name_path,split=" ")
                                                                         a<-NULL
                                                                         names<-vector()
                                                                         paths<-vector()
                                                                         for (a in 1:length(sep_list)){
                                                                             img_name<<-sep_list[[a]][1]
                                                                             names<-append(x=names,values=img_name)
                                                                             img_path<<-sep_list[[a]][2]
                                                                             paths<-append(x=paths,values=img_path)
                                                                         }
                                                                         img_file_name<-names[x]
                                                                         img_file_path<-paths[x]

                                                                         actual_img<-EBImage::readImage(img_file_path)

                                                                         # Doublets Plot PDF
                                                                         # pdf(file=paste0("Doublets_Plotted_",sub(img_file_name,pattern=".tif",replacement=""),".pdf"))
                                                                         # par(oma=c(0,0,5,0),mar=c(5,5,5,5),mfrow=c(1,1))
                                                                         # plot(actual_img)
                                                                         # ggplot2::xlim(c(1,dim(actual_img)[1]))
                                                                         # ggplot2::ylim(ylim=c(dim(actual_img)[1],1))
                                                                         # mtext(side=3,outer=TRUE,text=paste(dots,"","Dots"," ",num_neighbors,"","Neighbors"," ",doublets,"","Doublets (Red)","\n",img_file_name))
                                                                         # points(neighbor_coords2,col="red",pch=20,cex=0.3,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         # par(bg="black")
                                                                         # plot(neighbor_coords2,bg="black",col="red",pch=1,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         # dev.off()

                                                                         # Doublets Plot Postscript (EPS)
                                                                         postscript(file=paste0("Doublets_Plotted_",sub(img_file_name,pattern=".tif",replacement=""),".eps"),horizontal = FALSE)
                                                                         par(oma=c(0,0,5,0),mar=c(5,5,5,5),mfrow=c(1,1))
                                                                         plot(actual_img)
                                                                         ggplot2::xlim(c(1,dim(actual_img)[1]))
                                                                         ggplot2::ylim(ylim=c(dim(actual_img)[1],1))
                                                                         mtext(side=3,outer=TRUE,text=paste(dots,"","Dots"," ",num_neighbors,"","Neighbors"," ",doublets,"","Doublets (Red)","\n",img_file_name))
                                                                         points(neighbor_coords2,col="red",pch=20,cex=0.3,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         #par(bg="black")
                                                                         #plot(neighbor_coords2,bg="black",col="red",pch=1,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         dev.off()

                                                                         ## Area and Dist Multiples Plot PDF
                                                                         #pdf(file=paste0("Area_Dist_Multiples_Plotted_",sub(img_file_name,pattern=".tif",replacement=""),".pdf"))
                                                                         pdf(file=paste0("Large_Blobs_Dist_Multiples_Plotted_",sub(img_file_name,pattern=".tif",replacement=""),".pdf"))
                                                                         par(oma=c(0,0,5,0),mar=c(5,5,5,5),mfrow=c(1,1))
                                                                         plot(actual_img)
                                                                         ggplot2::xlim(c(1,dim(actual_img)[1]))
                                                                         ggplot2::ylim(ylim=c(dim(actual_img)[1],1))
                                                                         mtext(side=3,outer=TRUE,text=paste(dots,"","Dots"," ",num_neighbors,"","Neighbors"," ",num_area_multiples,"","Large Blobs (Green)"," ",num_dist_multiples,"","Dist_Multiples (Multi-Color)","\n",img_file_name))
                                                                         points(possible_multiple_coords,col="green",pch=20,cex=0.3,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         #
                                                                         #points(neighbors_not_doublets,col="yellow",pch=20,cex=0.3,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         #color_vec <- RColorBrewer::brewer.pal(n = 12, name = "Set3")
                                                                         color_vec <- c(RColorBrewer::brewer.pal(n = 12, name = "Paired"),RColorBrewer::brewer.pal(name="PiYG",n=4)[1:2])
                                                                         points(neighbors_not_doublets,col=color_vec[member_dynam.c],pch=20,cex=0.3,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         #
                                                                         par(bg="black")
                                                                         plot(possible_multiple_coords,bg="black",col="green",pch=20,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         points(neighbors_not_doublets,col=color_vec[member_dynam.c],pch=1,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         dev.off()

                                                                         # Area and Dist Multiples Plot Postscript (EPS)

                                                                         #postscript(file=paste0("Area_Dist_Multiples_Plotted_",sub(img_file_name,pattern=".tif",replacement=""),".eps"),horizontal = FALSE)
                                                                         postscript(file=paste0("Large_Blobs_Dist_Multiples_Plotted_",sub(img_file_name,pattern=".tif",replacement=""),".eps"),horizontal = FALSE)
                                                                         par(oma=c(0,0,5,0),mar=c(5,5,5,5),mfrow=c(1,1))
                                                                         plot(actual_img)
                                                                         ggplot2::xlim(c(1,dim(actual_img)[1]))
                                                                         ggplot2::ylim(ylim=c(dim(actual_img)[1],1))
                                                                         mtext(side=3,outer=TRUE,text=paste(dots,"","Dots"," ",num_neighbors,"","Neighbors"," ",num_area_multiples,"","Large Blobs (Green)"," ",num_dist_multiples,"","Dist_Multiples (Multi-Color)","\n",img_file_name))
                                                                         points(possible_multiple_coords,col="green",pch=20,cex=0.3,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         #
                                                                         #points(neighbors_not_doublets,col="yellow",pch=20,cex=0.3,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         #color_vec <- RColorBrewer::brewer.pal(n = 12, name = "Set3")
                                                                         color_vec <- c(RColorBrewer::brewer.pal(n = 12, name = "Paired"),RColorBrewer::brewer.pal(name="PiYG",n=4)[1:2])
                                                                         points(neighbors_not_doublets,col=color_vec[member_dynam.c],pch=20,cex=0.3,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         #
                                                                         #par(bg="black")
                                                                         #plot(possible_multiple_coords,bg="black",col="green",pch=20,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         #points(neighbors_not_doublets,col=color_vec[member_dynam.c],pch=1,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         dev.off()

                                                                         # Doublets and Area Multiples Plot
                                                                         # pdf(file=paste0("Doublets_and_Area_Dist_Multiples_Plotted_",sub(img_file_name,pattern=".tif",replacement=""),".pdf"))
                                                                         # par(oma=c(0,0,5,0),mar=c(5,5,5,5),mfrow=c(1,1))
                                                                         # plot(actual_img)
                                                                         # ggplot2::xlim(c(1,dim(actual_img)[1]))
                                                                         # ggplot2::ylim(ylim=c(dim(actual_img)[1],1))
                                                                         # mtext(side=3,outer=TRUE,text=paste(dots,"","Dots"," ",num_neighbors,"","Neighbors"," ",doublets,"","Doublets (Red)"," ",num_area_multiples,"","Area_Multiples (Green)"," ",num_dist_multiples,"","Dist_Multiples (Yellow)","\n",img_file_name))
                                                                         # points(neighbor_coords2,col="red",pch=20,cex=0.3,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         # points(possible_multiple_coords,col="green",pch=20,cex=0.3,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         # points(neighbors_not_doublets,col="yellow",pch=20,cex=0.3,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         # dev.off()
                                                                     };

                                                                     number1<<-append(x=number1,values=dots);
                                                                     number2<<-append(x=number2,values=num_neighbors);
                                                                     number3<<-append(x=number3,values=doublets);
                                                                     #
                                                                     number4<<-append(x=number4,values=num_area_multiples)
                                                                     #
                                                                     return(c(neighbor_coords,neighbor_distances,sum_dist_neigh_coords,neighbor_coords2,neighbor_distances2,indices_original_points,indices_new_points,dots,num_neighbors,doublets,num_area_multiples,number1,number2,number3,number4,number5))

                                                                 }
                                                                 assign_values3()

                                                             }
                                                             return(c(neighbor_coords,neighbor_distances,sum_dist_neigh_coords,neighbor_coords2,neighbor_distances2,indices_original_points,indices_new_points,dots,num_neighbors,doublets,num_area_multiples,number1,number2,number3,number4,number5))

                                                         };
                                                     }
                                                     return_values()

                                                 };

                                             };
                                         };
                                         cat(file=stderr(),"Doublet Count Summary CSV file about to be created","\n")
                                         data_table<-data.frame()
                                         data_table<-data.frame(files,number1,number2,number3,number4,number5)
                                         names(data_table)<-c("File","Dots","Neighbors","Doublets","Large_Blobs","Dist_Multiples")
                                         write.csv2(x=data_table,file=paste0("Doublet_Count_Summary",".csv"))
                                         openxlsx::write.xlsx(x=data_table,file="Doublet_Count_Summary.xlsx",col.names = TRUE,row.names = FALSE)
                                         doublet_count_file<-read.csv2(file="Doublet_Count_Summary.csv",header=TRUE)
                                         sum_Dots<-sum(doublet_count_file$Dots)
                                         sum_Neighbors<-sum(doublet_count_file$Neighbors)
                                         sum_Doublets<-sum(doublet_count_file$Doublets)
                                         #
                                         #sum_Area_Multiples<-sum(doublet_count_file$Area_Multiples)
                                         sum_Area_Multiples<-sum(doublet_count_file$Large_Blobs)
                                         sum_Dist_Multiples<-sum(doublet_count_file$Dist_Multiples)

                                         sums<-c(sum_Dots,sum_Neighbors,sum_Doublets,sum_Area_Multiples,sum_Dist_Multiples)
                                         sums<-matrix(sums,ncol=5)
                                         colnames(sums)<-c("Sum_of_Dots","Sum_of_Neighbors","Sum_of_Doublets","Sum_of_Area_Multiples","Sum_of_Dist_Multiples")
                                         #
                                         write.table(x=sums,file="Totals.csv",row.names=FALSE,col.names=TRUE)
                                         openxlsx::write.xlsx(x=sums,file="Totals.xlsx",col.names = TRUE,row.names = FALSE)

                                     };
                                     count_doublets();
                                     cat(file=stderr(),"Doublets counted","\n");
                                 }
                    )
                }
            })

            # Organize Files
            observeEvent(eventExpr = input$organize_files_button,{
                withProgress(message = 'Organizing Files', value = 0, style = "old", {
                    conditions<-input$organize_analysis_files_conditions
                    list_of_conditions<-strsplit(x=conditions,split=", ")[[1]]
                    doublet_count_summary_file<-read.csv2(file="Doublet_Count_Summary.csv")
                    cat(file=stderr(),"Received keywords","\n")
                    l<-NULL
                    z<-NULL

                    for (l in seq_along(list_of_conditions)){
                        # Create dirs from condition keywords
                        folder_name<-paste0(list_of_conditions[l],"_folder")
                        dir.create(path=paste0("./",folder_name))
                        cat(file=stderr(),"Created condition directory","\n")
                        # Create Doublet Count Summaries for each condition
                        sub_file_dcsf<-doublet_count_summary_file[which(grepl(list_of_conditions[l],doublet_count_summary_file$File, fixed = TRUE) == TRUE),]
                        sub_file_dcsf<-sub_file_dcsf[,c("File","Dots","Neighbors","Doublets")]
                        write.table(x=sub_file_dcsf,file=paste0("DCSF_",list_of_conditions[l],".csv"),row.names = FALSE,col.names = TRUE)
                        # Create Totals for each condition
                        dcsf_file<-read.table(file=paste0("DCSF_",list_of_conditions[l],".csv"),header=TRUE)
                        sum_Dots<-sum(dcsf_file$Dots)
                        sum_Neighbors<-sum(dcsf_file$Neighbors)
                        sum_Doublets<-sum(dcsf_file$Doublets)
                        sums<-c(sum_Dots,sum_Neighbors,sum_Doublets)
                        sums<-matrix(sums,ncol=3)
                        colnames(sums)<-c("Sum of Dots","Sum of Neighbors","Sum of Doublets")
                        write.table(x=sums,file=paste0("Totals_","DCSF_",list_of_conditions[l],".csv"),row.names=FALSE,col.names=TRUE)
                        openxlsx::write.xlsx(x=sums,file=paste0("Totals_","DCSF_",list_of_conditions[l],".xlsx"),col.names = TRUE,row.names = FALSE)
                        # Produce a Total for each condition nurse # and oocyte # from DCSF file
                        # Calculate ratio = sum(doublets) / sum(dots)
                        #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                        #found_files <- stringr::str_extract_all(dcsf_file$File,pattern=paste0(list_of_conditions[l],"_._","|",list_of_conditions[l],"_.._"))

                        found_files <- stringr::str_extract_all(dcsf_file$File,pattern=paste0(list_of_conditions[l],"_._","|",list_of_conditions[l],"_.._","|",list_of_conditions[l],".","|",list_of_conditions[l],".."))

                        found_unique_files<-unique(found_files)
                        z<-NULL
                        r<-NULL
                        for (z in seq_along(found_unique_files)){
                            #print(z)
                            #print(found_unique_files[[z]])
                            rows_for_file_z<-which(stringr::str_detect(string=dcsf_file$File,pattern=found_unique_files[[z]]) == TRUE)
                            file_z_contents<-dcsf_file[rows_for_file_z,]

                            write.table(x=file_z_contents,file=paste0(found_unique_files[[z]],"DCSF.csv"),row.names=FALSE,col.names=TRUE)
                            # Only the Sums in Totals file
                            sums_of_z <- c(sum(file_z_contents$Dots),sum(file_z_contents$Neighbors),sum(file_z_contents$Doublets))
                            sums_of_z<- as.numeric(sums_of_z)
                            sums_of_z<-matrix(sums_of_z,ncol=3)
                            colnames(sums_of_z)<-c("Sum of Dots","Sum of Neighbors","Sum of Doublets")
                            write.table(x=sums_of_z,file=paste0(found_unique_files[[z]],"Totals.csv"),row.names=FALSE,col.names=TRUE)
                            # Only the Ratio in Doublet Index file
                            ratio <- (sum(file_z_contents$Doublets)/sum(file_z_contents$Dots))
                            ratio <- round(ratio,digits = 3)
                            ratio <- sums_of_z<-matrix(ratio,ncol=1)
                            colnames(ratio) <- "Doublet_Index"
                            write.table(x=ratio,file=paste0(found_unique_files[[z]],"Doublet_Index.csv"),row.names=FALSE,col.names=TRUE)

                            # List all Doublet Index files for nurse# or oocyte# for condition[l] for Doublet Index Summary File
                            #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                            #list_of_doublet_index_file_names <- list.files(pattern=list_of_conditions[l])[grep(x=list.files(pattern=list_of_conditions[l]),pattern="_Doublet_Index.csv")]
                            list_of_doublet_index_file_names <- list.files(pattern=list_of_conditions[l])[grep(x=list.files(pattern=list_of_conditions[l]),pattern="Doublet_Index.csv")]
                            #condition_doublet_index_files_data_frame <- data.frame(matrix(nrow=1*10^6,ncol=2))
                            condition_doublet_index_files_data_frame <- data.frame(rep(x=NA,1*10^6),rep(x=NA,1*10^6),stringsAsFactors = FALSE)
                            colnames(condition_doublet_index_files_data_frame) <- c("File","Doublet_Index")
                            condition_doublet_index_files_data_frame$File[1:length(list_of_doublet_index_file_names)] <- list_of_doublet_index_file_names

                            for (r in seq_along(list_of_doublet_index_file_names)){
                                #print(r)
                                fraction<-read.table(file=list_of_doublet_index_file_names[r],header=TRUE,sep=",",dec=".")

                                condition_doublet_index_files_data_frame$Doublet_Index[r] <- as.numeric(fraction)
                            }
                            condition_doublet_index_files_data_frame <- na.exclude(condition_doublet_index_files_data_frame)
                            condition_doublet_index_files_data_frame$Doublet_Index <- unlist(condition_doublet_index_files_data_frame$Doublet_Index)
                            write.table(x=condition_doublet_index_files_data_frame,file=paste0("Doublet_Indices_",list_of_conditions[l],".csv"),row.names = FALSE)



                            # List all Total files for nurse# or oocyte# for condition[l] for Titer (Dot_Total) Column

                            list_of_total_file_names <- list.files(pattern=list_of_conditions[l])[grep(x=list.files(pattern=list_of_conditions[l]),pattern="Totals.csv")]
                            #condition_total_files_data_frame <- data.frame(matrix(nrow=1*10^6,ncol=2))
                            condition_total_files_data_frame <- data.frame(rep(x=NA,1*10^6),rep(x=NA,1*10^6),stringsAsFactors = FALSE)
                            colnames(condition_total_files_data_frame) <- c("File","Titer")
                            condition_total_files_data_frame$File[1:length(list_of_total_file_names)] <- list_of_total_file_names
                            #condition_doublet_index_files_data_frame <- na.exclude(condition_doublet_index_files_data_frame)
                            for (r in seq_along(list_of_total_file_names)){

                                fraction<-read.table(file=list_of_total_file_names[r],header=TRUE,dec=".")

                                condition_total_files_data_frame$Titer[r] <- as.numeric(fraction$Sum.of.Dots)
                            }
                            condition_total_files_data_frame <- na.exclude(condition_total_files_data_frame)
                            condition_total_files_data_frame$Titer <- unlist(condition_total_files_data_frame$Titer)

                            write.table(x=condition_total_files_data_frame,file=paste0("Titers_",list_of_conditions[l],".csv"),row.names = FALSE)


                        }
                        ####
                        # Create Titer Summary File
                        t<-NULL
                        all_total_files <- list.files(pattern=paste0("Titers_"))

                        file_t_contents <- list()
                        for (t in seq_along(all_total_files)){
                            #print(t)
                            file_t_contents[[t]] <- read.table(file=all_total_files[t],header=TRUE,dec=".")

                        }}

                    write.table(x=data.table::rbindlist(l=file_t_contents),file="Titers_Summary_File.csv",row.names = FALSE)

                    # Create Doublet_Indices Summary File
                    d<-NULL
                    all_doublet_indices_files <- list.files(pattern=paste0("Doublet_Indices_"))

                    file_d_contents <- list()
                    for (d in seq_along(all_doublet_indices_files)){
                        #print(d)
                        file_d_contents[[d]] <- read.table(file=all_doublet_indices_files[d],header=TRUE,dec=".")

                    }

                    write.table(x=data.table::rbindlist(l=file_d_contents),file="Doublet_Indices_Summary_File.csv",row.names = FALSE)


                    # Create Data Analysis Pipeline Input Files from Doublet_Indices and Titers Summary Files

                    doublet_indices<-read.table("Doublet_Indices_Summary_File.csv",header=TRUE,dec=".",stringsAsFactors = FALSE)
                    titers <- read.table("Titers_Summary_File.csv",header=TRUE,dec=".",stringsAsFactors = FALSE)
                    u <- NULL
                    data_input_file_doublet_indices <- data.frame(matrix(data=rep(x=NA,1*10^6),ncol = length(list_of_conditions)),stringsAsFactors = FALSE)
                    data_input_file_titers <- data.frame(matrix(data=rep(x=NA,1*10^6),ncol = length(list_of_conditions)),stringsAsFactors = FALSE)
                    names(data_input_file_doublet_indices) <- list_of_conditions
                    names(data_input_file_titers) <- list_of_conditions
                    for (u in seq_along(list_of_conditions)){
                        #print(u)
                        #print(paste("Doublet_Index",u))
                        rows <- which(doublet_indices$File %>% grepl(pattern=list_of_conditions[u]) == TRUE)
                        # #      print(rows)
                        doublet_indices$File[rows] <- list_of_conditions[u]
                        # #      print("Conditions assigned")
                        data_input_file_doublet_indices[1:length(doublet_indices$Doublet_Index[which((dplyr::select(.data = doublet_indices,"File") == list_of_conditions[u]) ==TRUE)]),list_of_conditions[u]] <- doublet_indices$Doublet_Index[which((dplyr::select(.data = doublet_indices,"File") == list_of_conditions[u]) ==TRUE)]
                        # #      print("Data Input File Modified")
                    }
                    # # #
                    u <- NULL
                    for (u in seq_along(list_of_conditions)){
                        # #      #print(u)
                        # #      print(paste("Titer",u))
                        rows <- which(titers$File %>% grepl(pattern=list_of_conditions[u]) == TRUE)
                        # # print(rows)
                        titers$File[rows] <- list_of_conditions[u]
                        # #      print("Conditions assigned")
                        data_input_file_titers[1:length(titers$Titer[which((dplyr::select(.data = titers,"File") == list_of_conditions[u]) ==TRUE)]),list_of_conditions[u]] <- titers$Titer[which((dplyr::select(.data = titers,"File") == list_of_conditions[u]) ==TRUE)]
                        # #      print("Data Input File Modified")
                    }
                    data_input_file_doublet_indices <- data_input_file_doublet_indices[rowSums(is.na(data_input_file_doublet_indices)) != ncol(data_input_file_doublet_indices), ]
                    data_input_file_titers <- data_input_file_titers[rowSums(is.na(data_input_file_titers)) != ncol(data_input_file_titers),]
                    write.table(x=data_input_file_doublet_indices,file="Data_Analysis_Input_File_Doublet_Indices.csv",row.names = FALSE,col.names = TRUE)
                    write.table(x=data_input_file_titers,file="Data_Analysis_Input_File_Titers.csv",row.names = FALSE,col.names = TRUE)


                    # Find and cp condition files to dirs
                    l <- NULL
                    for (l in seq_along(list_of_conditions)) {
                        condition_files<-list.files(pattern=list_of_conditions[l])
                        folder_name<-paste0(list_of_conditions[l],"_folder")
                        condition_files<-condition_files[condition_files != folder_name]
                        #fs::file_move(path=condition_files,new_path=paste0("./",folder_name))
                        fs::file_move(path=condition_files,new_path=paste0(folder_name))
                        #incProgress(amount=1/l, detail = "Organized Files into Folders")
                    }
                    incProgress(amount=1, detail = "Organized Files into Folders")
                })

            })





            #Download Analysis files as a Zip
            output$analysis_files_download_button <- downloadHandler(
                filename = paste0("Analysis_Files_",
                                  gsub(x=format(Sys.Date(),format="%B %a %d %Y"),
                                       pattern=" ",replacement="_"),
                                  "Time_",gsub(x=format(Sys.time()),pattern=paste0(Sys.Date()," "),replacement=""),
                                  ".zip"),
                content = function(con) {
                    download_files <- list.dirs(recursive=FALSE)
                    download_files <- download_files[download_files != "./rsconnect"]
                    download_files <- c(download_files,list.files(pattern=".xlsx"))
                    download_files <- c(download_files,list.files(pattern=".csv"))
                    download_files <- c(download_files,list.files(pattern=".pdf"))
                    download_files <- c(download_files,list.files(pattern=".eps"))
                    zip(zipfile = con, files = download_files)},
                contentType = "application/zip")
        }
        if(input$real_data_synth_data_generation == 'Synth'){
            cat(file=stderr(),"Synth Code Block", "\n")
            # Show Binary Fission Image next to Title
            output$binary_fission_image<-renderImage({
                list(src = "./binary_fission_image.png")}, deleteFile = FALSE)

            # Remove old Analysis files
            start_fresh <- function(){
                unlink(x = list.files(pattern = ".csv"))
                unlink(x = list.files(pattern = ".xlsx"))
                unlink(x = list.files(pattern = ".pdf"))
                unlink(x = list.files(pattern = ".eps"))
                unlink(x = list.files(pattern = ".svg"))
                unlink(x = list.files(pattern = ".tiff"))
                unlink(x = list.dirs(path = "./packrat"), recursive=TRUE)
                fs::dir_delete(list.dirs(recursive=FALSE)[list.dirs(recursive=FALSE) != "./rsconnect"])
            }
            start_fresh()

            # Create Synthetic Images

            observeEvent(eventExpr = input$create_synthetic_images_button,{
                withProgress(message = 'Creating Synthetic Images', value = 0, style = "old", {
                    create_synthetic_images <- function(){

                        dot_size_vec <<- as.numeric(unlist(stringr::str_split(input$dot_sizes,pattern=", ")))

                        dot_seq_vec <<- as.numeric(unlist(stringr::str_split(input$dot_number_sequence,pattern=", ")))

                        number_of_dots_input <- seq(dot_seq_vec[1],dot_seq_vec[2],dot_seq_vec[3])

                        if(input$dot_sizes == "1, 25"){
                            prob_dist <<- c(16354.16667,14380.83333,0.00000,7670.00000,0.00000,4632.33333,0.00000,2056.00000,0.00000,1102.50000,0.00000,654.00000,0.00000,414.00000,0.00000,275.83333,0.00000,185.66667,0.00000,136.83333,0.00000,100.00000,0.00000,72.16667,0.00000)
                        }
                        if(input$dot_sizes == "1, 2"){
                            prob_dist <<- c(0.5,0.5)
                        }
                        ##########
                        # if(input$dot_sizes != "1, 25" | input$dot_sizes != "1,2"){
                        #     store_prob_dist_file_input_on_disk_and_read_into_mem <- function() {
                        #         file.copy(from = input$prob_dist_file_input$datapath,to="Prob_Dist_File.csv", overwrite = TRUE);
                        #         prob_dist_file<<-read.table(file = "Prob_Dist_File.csv", header = TRUE, sep = ",", dec = ".");
                        #         openxlsx::write.xlsx(x=Results,file="Prob_Dist_File.xlsx",col.names = TRUE,row.names = FALSE)
                        #     };
                        #     store_prob_dist_file_input_on_disk_and_read_into_mem();
                        #
                        # }
                        if(is.null(input$prob_dist_file_input)==FALSE){
                            store_prob_dist_file_input_on_disk_and_read_into_mem <- function() {
                                file.copy(from = input$prob_dist_file_input$datapath,to="Prob_Dist_File.csv", overwrite = TRUE);
                                prob_dist_file<<-read.table(file = "Prob_Dist_File.csv", header = TRUE, sep = ",", dec = ".");
                                #write.csv(prob_dist_file,)
                                openxlsx::write.xlsx(x=prob_dist_file,file="Prob_Dist_File.xlsx",col.names = TRUE,row.names = FALSE)
                            };
                            store_prob_dist_file_input_on_disk_and_read_into_mem();
                            prob_dist <<- prob_dist_file[,1]

                        }
                        ###########
                        plot_dot_cloud_reduced_margin_true_area_size_replacement_unique <- function(number_of_dots=dot_seq_vec,margin,sizes=dot_size_vec,dims_image=input$image_dim,image_num=i,cell_type) {

                            #for(n in 1:length(as.numeric(images_set_number))){
                            #img_dim <- 512
                            img_dim <- as.numeric(dims_image)

                            number_dots <- as.numeric(number_of_dots)
                            margin <- as.numeric(margin)
                            # Dot size
                            # ADJUST SIZE CHOICE
                            #size <- sample(x=1:25,size=number_dots,replace=TRUE,prob = prob_dist)
                            #size <- sample(x=1:2,size=number_dots,replace=TRUE,prob = c(0.5,0.5))
                            size <- sample(x=as.numeric(sizes[1]):as.numeric(sizes[2]),size=number_dots,replace=TRUE,prob = prob_dist)

                            # Dot position
                            #x <- sample(x=1:margin,size=number_dots,replace=TRUE)
                            x <- sample(x=1:margin,size=1000000,replace=TRUE)
                            #y <- sample(x=1:img_dim,size=number_dots,replace=TRUE)
                            y <- sample(x=1:img_dim,size=1000000,replace=TRUE)
                            x_y_points <- matrix(data=c(x,y),ncol=2)
                            x_y_points <- unique(x_y_points)
                            x_y_points <- x_y_points[c(1:number_dots),]
                            #x_y_points <- na.exclude(unique(x_y_points))[1:number_dots]
                            number_of_points <- nrow(x_y_points)

                            #print(paste0("Num_Dots: ",number_of_points))

                            while (number_of_points != number_dots){
                                x <- sample(x=1:margin,size=number_dots,replace=TRUE)
                                y <- sample(x=1:img_dim,size=number_dots,replace=TRUE)
                                x_y_points <- matrix(data=c(x,y),ncol=2)
                                number_of_points <- nrow(unique(x_y_points))
                                #print(paste0("Num_Dots: ",number_of_points))
                            }
                            n <- as.numeric(image_num)
                            #file_number <- sample(x=1:500000000,size=1,replace=FALSE)
                            file_name <- paste0("Number_Dots_",number_dots,"_synth_",cell_type,"_image-",n)

                            #file_name <- paste0(number_dots,"_synth_image_",)#,"_image",n)

                            pdf(file=paste0(file_name,".pdf"))
                            par(bg="black")
                            plot(x_y_points,col="white",pch=20,cex=size/10,xlim=c(1,img_dim),ylim=c(img_dim,1))
                            dev.off()


                        }
                        f <- NULL
                        i <- NULL
                        if(input$cell_type == "Nurse"){
                            for (f in 1:length(number_of_dots_input)) {
                                for (i in 1:(as.numeric(input$number_of_images))){
                                    plot_dot_cloud_reduced_margin_true_area_size_replacement_unique(number_of_dots=number_of_dots_input[f],margin=175,sizes=dot_size_vec,dims_image=input$image_dim,image_num=i,cell_type = "nurse")
                                }
                            }
                            for(i in 1:length(list.files(pattern=".pdf"))){
                                pdftools::pdf_convert(pdf = list.files(pattern=".pdf")[i],format = "tiff")
                            }
                            unlink(x = list.files(pattern = ".pdf"))
                            fs::file_copy(path=list.files(pattern=".tiff"),new_path = sub(list.files(pattern=".tiff"),pattern = "_1.tiff",replacement = ".tiff"))
                            unlink(x = list.files(pattern = "_1.tiff"))
                        }
                        if(input$cell_type == "Oocyte"){
                            for (f in 1:length(number_of_dots_input)) {
                                for (i in 1:(as.numeric(input$number_of_images))){
                                    plot_dot_cloud_reduced_margin_true_area_size_replacement_unique(number_of_dots=number_of_dots_input[f],margin=144,sizes=dot_size_vec,dims_image=input$image_dim,image_num=i,cell_type = "oocyte")
                                }
                            }
                            for(i in 1:length(list.files(pattern=".pdf"))){
                                pdftools::pdf_convert(pdf = list.files(pattern=".pdf")[i],format = "tiff")
                            }
                            unlink(x = list.files(pattern = ".pdf"))
                            fs::file_copy(path=list.files(pattern=".tiff"),new_path = sub(list.files(pattern=".tiff"),pattern = "_1.tiff",replacement = ".tiff"))
                            unlink(x = list.files(pattern = "_1.tiff"))
                        }
                        if(input$cell_type == "Other"){
                            for (f in 1:length(number_of_dots_input)) {
                                for (i in 1:(as.numeric(input$number_of_images))){
                                    plot_dot_cloud_reduced_margin_true_area_size_replacement_unique(number_of_dots=number_of_dots_input[f],margin=as.numeric(input$custom_margin_value),sizes=dot_size_vec,dims_image=input$image_dim,image_num=i,cell_type = "nurse")
                                }
                            }
                            for(i in 1:length(list.files(pattern=".pdf"))){
                                pdftools::pdf_convert(pdf = list.files(pattern=".pdf")[i],format = "tiff")
                            }
                            unlink(x = list.files(pattern = ".pdf"))
                            fs::file_copy(path=list.files(pattern=".tiff"),new_path = sub(list.files(pattern=".tiff"),pattern = "_1.tiff",replacement = ".tiff"))
                            unlink(x = list.files(pattern = "_1.tiff"))
                        }

                    }

                    create_synthetic_images()
                    incProgress(amount=1, detail = "Created All Synthetic Images")
                    cat(file=stderr(),"Created All Synthetic Images", "\n")

                })})

            #Download Synthetic Files as a Zip
            output$synthetic_files_download_button <- downloadHandler(
                filename = paste0("Synthetic_Images_",
                                  gsub(x=format(Sys.Date(),format="%B %a %d %Y"),
                                       pattern=" ",replacement="_"),
                                  "Time_",gsub(x=format(Sys.time()),pattern=paste0(Sys.Date()," "),replacement=""),
                                  ".zip"),
                content = function(con) {
                    download_files <- list.dirs(recursive=FALSE)
                    download_files <- download_files[download_files != "./rsconnect"]
                    download_files <- c(download_files,list.files(pattern=".tif"))
                    #download_files <- c(download_files,list.files(pattern=".csv"))
                    #download_files <- c(download_files,list.files(pattern=".pdf"))
                    #download_files <- c(download_files,list.files(pattern=".eps"))
                    zip(zipfile = con, files = download_files)},
                contentType = "application/zip")
            # Carry out the Analysis
            output$text<-renderTable({
                if (is.null(input$results_csv_file_2) == FALSE) {
                    withProgress(message = "Obtaining coordinates from Results.csv", min = 0, max = 1, style = "old", value = 0,
                                 expr = {
                                     req(input$results_csv_file_2);
                                     #req(Results);

                                     store_Results_input_on_disk_and_read_into_mem <- function() {
                                         file.copy(from = input$results_csv_file_2$datapath,to="Results.csv", overwrite = TRUE);
                                         Results<<-read.table(file = "Results.csv", header = TRUE, sep = ",", dec = ".");
                                         openxlsx::write.xlsx(x=Results,file="Results.xlsx",col.names = TRUE,row.names = FALSE)
                                     };
                                     store_Results_input_on_disk_and_read_into_mem();

                                     extract_coordinates_from_Results_to_files<-function() {
                                         #Results<<-Results[,c("X","Y","Filename")]
                                         Results<<-Results[,c("Area","X","Y","Filename")]
                                         file_names<<-unique(Results$Filename)
                                         files<<-paste0(sub(file_names,
                                                            pattern = ".tiff",
                                                            replacement = ""), "_coords")
                                         n<-NULL
                                         for (n in 1:length(file_names)){
                                             results_filtered<-dplyr::filter(Results,
                                                                             Results$Filename == file_names[n]);
                                             write.table(x = results_filtered,
                                                         file = paste0(files[n],".csv"),
                                                         row.names = FALSE)
                                         }
                                     };
                                     extract_coordinates_from_Results_to_files();

                                     cat(file=stderr(),"Coordinates extracted","\n");

                                     count_doublets<-function() {
                                         req(input$image_summary_plot_button_2);
                                         get_files <- function() {
                                             directory<-getwd();
                                             files<-list.files(path = directory, pattern = ".csv");
                                             files<-files[files != "Results.csv"];
                                             files<<-sort(files);
                                             return(files)
                                         }
                                         get_files() %>% invisible()
                                         cat(file=stderr(),"Files listed","\n")
                                         set_variables <- function() {
                                             x<<-NULL
                                             i<<-NULL
                                             thres<<-as.numeric(input$thres_value_2)
                                             area_val<<-as.numeric(input$area_value_2)
                                             number1<<-NULL
                                             number2<<-NULL
                                             number3<<-NULL
                                             number4<<-NULL
                                             number5<<-NULL
                                             doublets<<-NULL
                                             dots<<-NULL
                                             data_table<<-NULL
                                             img_file_name<<-NULL
                                             num_neighbors<<-NULL
                                             image_summary_plot<<-input$image_summary_plot_button
                                             hc.c <<- NULL
                                             member_dynam.c <<- NULL
                                             member_dynam_table <<-NULL

                                         }
                                         set_variables()
                                         cat(file=stderr(),"Variables set","\n")

                                         for (x in seq_along(files)){
                                             cat(file=stderr(),"Analysis in progress","\n");
                                             print(x);
                                             print(files[x]) #cat(file=stderr(),print(file[x]))
                                             incProgress(amount=(1/(x*1000)),message="Calculating doublets...");
                                             data_coords<<-read.table(file = files[x], header = TRUE, dec = ".");
                                             #data_coords<<-data_coords[,c("X","Y")];
                                             data_coords<<-data_coords[,c("Area","X","Y")];
                                             coord_mat2<<-data_coords[c("X","Y")]
                                             #
                                             possible_multiple_coords<<-data_coords[c("X","Y")][which(data_coords$Area > area_val),]
                                             #num_area_multiples <<- nrow(possible_multiple_coords)
                                             # Doublet dots in line below
                                             data_coords<<-data_coords[c("X","Y")][which(data_coords$Area<=area_val),]

                                             if (all(is.na(data_coords)) == TRUE) {
                                                 empty_data_coords <- function(){
                                                     dots<<-dim(coord_mat2)[1];
                                                     num_neighbors<<-0;
                                                     doublets<<-0;
                                                     #
                                                     num_area_multiples <<- nrow(possible_multiple_coords);
                                                     num_dist_multiples <<- 0

                                                     #
                                                     number1<<-append(x=number1,values=dots);
                                                     number2<<-append(x=number2,values=num_neighbors);
                                                     number3<<-append(x=number3,values=doublets);
                                                     number4<<-append(x=number4,values=num_area_multiples);
                                                     number5<<-append(x=number5,values=num_dist_multiples);
                                                     return(c(number1,number2,number3,number4,number5))
                                                 }
                                                 empty_data_coords()
                                             } else {

                                                 coord_mat<<-as.matrix(round(data_coords, digits=2));
                                                 num_points<<-nrow(data_coords);
                                                 dist_p1_p2<<-round(pointDistance(p1=coord_mat[1:num_points,],lonlat=FALSE,allpairs=TRUE),digits=2);
                                                 neighbors<<-which(dist_p1_p2 <= thres & dist_p1_p2 != 0,arr.ind=TRUE);
                                                 cat(file=stderr(),"Neighbors","\n");

                                                 if (all(is.na(neighbors)) == "TRUE") {
                                                     assign_values <- function() {
                                                         dots<<-dim(coord_mat2)[1];
                                                         num_neighbors<<-0;
                                                         doublets<<-0;
                                                         #
                                                         num_area_multiples <<- nrow(possible_multiple_coords);
                                                         num_dist_multiples <<- 0
                                                         #
                                                         number1<<-append(x=number1,values=dots);
                                                         number2<<-append(x=number2,values=num_neighbors);
                                                         number3<<-append(x=number3,values=doublets);
                                                         #
                                                         number4<<-append(x=number4,values=num_area_multiples);
                                                         number5<<-append(x=number5,values=num_dist_multiples)
                                                         #
                                                         return(c(number1,number2,number3,number4,number5))
                                                     }
                                                     assign_values()
                                                 } else {
                                                     return_values <- function() {

                                                         neighbor_coords<<-unique(coord_mat[neighbors,]);
                                                         cat(file=stderr(),"Neighbor_Coords created","\n");
                                                         neighbor_distances<<-pointDistance(p1=neighbor_coords,lonlat=FALSE);
                                                         neighbor_distances[neighbor_distances == 0]<-NA ;
                                                         sum_dist_neigh_coords<<-apply(neighbor_distances<=thres,1,sum,na.rm=TRUE);
                                                         neighbor_coords2<<-neighbor_coords[which(sum_dist_neigh_coords==1),];
                                                         cat(file=stderr(),"Neighbor_Coords2, just before multiple mitigation","\n");
                                                         neighbor_distances2<<-pointDistance(p1=neighbor_coords2,lonlat=FALSE);
                                                         if (length(neighbor_distances2) == 1) {
                                                             assign_values2 <- function() {
                                                                 dots<<-dim(coord_mat2)[1];
                                                                 num_neighbors<<-0;
                                                                 doublets<<-0;
                                                                 num_area_multiples <<- nrow(possible_multiple_coords);
                                                                 #
                                                                 num_dist_multiples <<- 0
                                                                 #
                                                                 number1<<-append(x=number1,values=dots);
                                                                 number2<<-append(x=number2,values=num_neighbors);
                                                                 number3<<-append(x=number3,values=doublets);
                                                                 number4<<-append(x=number4,values=num_area_multiples)
                                                                 #
                                                                 number5<<-append(x=number5,values=num_dist_multiples)
                                                                 #
                                                                 cat(file=stderr(),"Neighbor distances = 0 block","\n")
                                                                 return(c(number1,number2,number3,number4,number5))
                                                             }
                                                             assign_values2()
                                                         } else {
                                                             neighbor_distances2[neighbor_distances2 == 0]<-NA ;
                                                             indices_original_points<<-which(sum_dist_neigh_coords==1);
                                                             indices_new_points<<-which(apply(neighbor_distances2<=thres,1,sum,na.rm=TRUE)==0);
                                                             if (all(is.na(indices_new_points)) == "TRUE") {} else {
                                                                 neighbor_coords2[indices_new_points,]<-NA ;
                                                                 cat(file=stderr(),"Neighbor_Coords2 , NA Introducuction","\n");
                                                                 na_removal_indices<-which(is.na(neighbor_coords2[,1]) == "TRUE");
                                                                 cat(file=stderr(),"Neighbor_Coords2 , NA Removal","\n");
                                                                 neighbor_coords2<-neighbor_coords2[-na_removal_indices,];
                                                                 cat(file=stderr(),"Neighbor_Coords2 truncated to true size","\n");
                                                             }

                                                             if (all(is.na(neighbor_coords2)) == "TRUE") {
                                                                 assign_values2 <- function() {
                                                                     dots<<-dim(coord_mat2)[1];
                                                                     num_neighbors<<-dim(neighbor_coords)[1];
                                                                     cat(file=stderr(),"Neighbor_Coords2 is Missing Block","\n");
                                                                     doublets<<-0;
                                                                     num_area_multiples <<- nrow(possible_multiple_coords);

                                                                     #
                                                                     neighbors_not_doublets <<- dplyr::anti_join(data.frame(neighbor_coords),data.frame(neighbor_coords2))
                                                                     if (all(is.na(neighbors_not_doublets)) == "TRUE"){
                                                                         assign_values5 <- function(){
                                                                             num_dist_multiples <<- 0
                                                                             number5<<-append(x=number5,values=num_dist_multiples)
                                                                             return(number5)
                                                                         }
                                                                         assign_values5()
                                                                     } else {
                                                                         custom_distance <<- dist(x=neighbors_not_doublets)
                                                                         hc.c <<- hclust(custom_distance)
                                                                         # member_dynam.c <- dynamicTreeCut::cutreeDynamic(dendro = hc.c,
                                                                         #                                                 minClusterSize = 3,
                                                                         #                                                 method = "hybrid",
                                                                         #                                                 distM = as.matrix(custom_distance),
                                                                         #                                                 deepSplit = 4)
                                                                         member_dynam.c <<- dynamicTreeCut::cutreeDynamic(dendro = hc.c,
                                                                                                                          minClusterSize = 2,
                                                                                                                          method = "hybrid",
                                                                                                                          distM = as.matrix(custom_distance),
                                                                                                                          deepSplit = 3)

                                                                         num_dist_multiples <<- length(as.vector(table(member_dynam.c))) # of clusters
                                                                         number5 <<- append(x=number5,values=num_dist_multiples)
                                                                     }
                                                                     #

                                                                     number1<<-append(x=number1,values=dots);
                                                                     number2<<-append(x=number2,values=num_neighbors);
                                                                     number3<<-append(x=number3,values=doublets);
                                                                     number4<<-append(x=number4,values=num_area_multiples)
                                                                     return(c(number1,number2,number3,number4,number5))
                                                                 }
                                                                 assign_values2()
                                                             } else {
                                                                 assign_values3 <- function() {
                                                                     neighbor_coords2<<-neighbor_coords2[1:dim(neighbor_coords2)[1],];
                                                                     write.table(x=neighbor_coords2,file=paste0("Neighb_Coords_2_",sub(x=files[x],pattern="_coords.csv",replacement=""),".csv"))
                                                                     openxlsx::write.xlsx(x=neighbor_coords2,file=paste0("Neighb_Coords_2_",sub(x=files[x],pattern="_coords.csv",replacement=""),".xlsx"),col.names=TRUE,row.names=FALSE)
                                                                     cat(file=stderr(),"Neighbor_Coords2, Dim","\n");
                                                                     neighbor_distances2<<-pointDistance(p1=neighbor_coords2,lonlat=FALSE);
                                                                     neighbor_distances2[neighbor_distances2 == 0]<-NA ;
                                                                     dots<<-dim(coord_mat2)[1] ;
                                                                     num_neighbors<-dim(neighbor_coords2)[1] ;
                                                                     doublets<<-(sum(apply(neighbor_distances2<=thres,1,sum,na.rm=TRUE),na.rm=TRUE)/2);
                                                                     doublets<<-round(doublets,digits=0);
                                                                     num_area_multiples <<- nrow(possible_multiple_coords);

                                                                     #
                                                                     neighbors_not_doublets <<- dplyr::anti_join(data.frame(neighbor_coords),data.frame(neighbor_coords2))
                                                                     if (all(is.na(neighbors_not_doublets)) == "TRUE"){
                                                                         assign_values5 <- function(){
                                                                             num_dist_multiples <<- 0
                                                                             number5<<-append(x=number5,values=num_dist_multiples)
                                                                             return(number5)
                                                                         }
                                                                         assign_values5()
                                                                     } else {
                                                                         custom_distance <<- dist(x=neighbors_not_doublets)
                                                                         hc.c <<- hclust(custom_distance)
                                                                         # member_dynam.c <- dynamicTreeCut::cutreeDynamic(dendro = hc.c,
                                                                         #                                                 minClusterSize = 3,
                                                                         #                                                 method = "hybrid",
                                                                         #                                                 distM = as.matrix(custom_distance),
                                                                         #                                                 deepSplit = 4)
                                                                         member_dynam.c <<- dynamicTreeCut::cutreeDynamic(dendro = hc.c,
                                                                                                                          minClusterSize = 2,
                                                                                                                          method = "hybrid",
                                                                                                                          distM = as.matrix(custom_distance),
                                                                                                                          deepSplit = 3)

                                                                         if (all(member_dynam.c) == 0) {
                                                                             member_dynam.c <<- rep(x=1,times=length(member_dynam.c))
                                                                         }

                                                                         num_dist_multiples <<- length(as.vector(table(member_dynam.c))) # of clusters
                                                                         number5 <<- append(x=number5,values=num_dist_multiples)
                                                                     }


                                                                     if (input$image_summary_plot_button_2=="y"){
                                                                         image_files<-sub(sub(x=files,pattern="_coords",replacement=".tiff"),pattern=".csv",replacement="")
                                                                         image_files<-sort(image_files)
                                                                         combined_name_path<-transform(input$images_file_input_2,newcol=paste(name,datapath))
                                                                         combined_name_path<-sort(combined_name_path$newcol)
                                                                         combined_name_path<-as.character(combined_name_path)
                                                                         sep_list<-strsplit(x=combined_name_path,split=" ")
                                                                         a<-NULL
                                                                         names<-vector()
                                                                         paths<-vector()
                                                                         for (a in 1:length(sep_list)){
                                                                             img_name<<-sep_list[[a]][1]
                                                                             names<-append(x=names,values=img_name)
                                                                             img_path<<-sep_list[[a]][2]
                                                                             paths<-append(x=paths,values=img_path)
                                                                         }
                                                                         img_file_name<-names[x]
                                                                         img_file_path<-paths[x]

                                                                         actual_img<-EBImage::readImage(img_file_path)

                                                                         # Doublets Plot PDF
                                                                         # pdf(file=paste0("Doublets_Plotted_",sub(img_file_name,pattern=".tif",replacement=""),".pdf"))
                                                                         # par(oma=c(0,0,5,0),mar=c(5,5,5,5),mfrow=c(1,1))
                                                                         # plot(actual_img)
                                                                         # ggplot2::xlim(c(1,dim(actual_img)[1]))
                                                                         # ggplot2::ylim(ylim=c(dim(actual_img)[1],1))
                                                                         # mtext(side=3,outer=TRUE,text=paste(dots,"","Dots"," ",num_neighbors,"","Neighbors"," ",doublets,"","Doublets (Red)","\n",img_file_name))
                                                                         # points(neighbor_coords2,col="red",pch=20,cex=0.3,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         # par(bg="black")
                                                                         # plot(neighbor_coords2,bg="black",col="red",pch=1,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         # dev.off()

                                                                         # Doublets Plot Postscript (EPS)
                                                                         postscript(file=paste0("Doublets_Plotted_",sub(img_file_name,pattern=".tiff",replacement=""),".eps"),horizontal = FALSE)
                                                                         par(oma=c(0,0,5,0),mar=c(5,5,5,5),mfrow=c(1,1))
                                                                         plot(actual_img)
                                                                         ggplot2::xlim(c(1,dim(actual_img)[1]))
                                                                         ggplot2::ylim(ylim=c(dim(actual_img)[1],1))
                                                                         mtext(side=3,outer=TRUE,text=paste(dots,"","Dots"," ",num_neighbors,"","Neighbors"," ",doublets,"","Doublets (Red)","\n",img_file_name))
                                                                         points(neighbor_coords2,col="red",pch=20,cex=0.3,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         #par(bg="black")
                                                                         #plot(neighbor_coords2,bg="black",col="red",pch=1,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         dev.off()

                                                                         ## Area and Dist Multiples Plot PDF
                                                                         #pdf(file=paste0("Area_Dist_Multiples_Plotted_",sub(img_file_name,pattern=".tif",replacement=""),".pdf"))
                                                                         pdf(file=paste0("Large_Blobs_Dist_Multiples_Plotted_",sub(img_file_name,pattern=".tiff",replacement=""),".pdf"))
                                                                         par(oma=c(0,0,5,0),mar=c(5,5,5,5),mfrow=c(1,1))
                                                                         plot(actual_img)
                                                                         ggplot2::xlim(c(1,dim(actual_img)[1]))
                                                                         ggplot2::ylim(ylim=c(dim(actual_img)[1],1))
                                                                         mtext(side=3,outer=TRUE,text=paste(dots,"","Dots"," ",num_neighbors,"","Neighbors"," ",num_area_multiples,"","Large Blobs (Green)"," ",num_dist_multiples,"","Dist_Multiples (Multi-Color)","\n",img_file_name))
                                                                         points(possible_multiple_coords,col="green",pch=20,cex=0.3,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         #
                                                                         #points(neighbors_not_doublets,col="yellow",pch=20,cex=0.3,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         #color_vec <- RColorBrewer::brewer.pal(n = 12, name = "Set3")
                                                                         color_vec <- c(RColorBrewer::brewer.pal(n = 12, name = "Paired"),RColorBrewer::brewer.pal(name="PiYG",n=4)[1:2])
                                                                         points(neighbors_not_doublets,col=color_vec[member_dynam.c],pch=20,cex=0.3,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         #
                                                                         par(bg="black")
                                                                         plot(possible_multiple_coords,bg="black",col="green",pch=20,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         points(neighbors_not_doublets,col=color_vec[member_dynam.c],pch=1,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         dev.off()

                                                                         # Area and Dist Multiples Plot Postscript (EPS)

                                                                         #postscript(file=paste0("Area_Dist_Multiples_Plotted_",sub(img_file_name,pattern=".tif",replacement=""),".eps"),horizontal = FALSE)
                                                                         postscript(file=paste0("Large_Blobs_Dist_Multiples_Plotted_",sub(img_file_name,pattern=".tiff",replacement=""),".eps"),horizontal = FALSE)
                                                                         par(oma=c(0,0,5,0),mar=c(5,5,5,5),mfrow=c(1,1))
                                                                         plot(actual_img)
                                                                         ggplot2::xlim(c(1,dim(actual_img)[1]))
                                                                         ggplot2::ylim(ylim=c(dim(actual_img)[1],1))
                                                                         mtext(side=3,outer=TRUE,text=paste(dots,"","Dots"," ",num_neighbors,"","Neighbors"," ",num_area_multiples,"","Large Blobs (Green)"," ",num_dist_multiples,"","Dist_Multiples (Multi-Color)","\n",img_file_name))
                                                                         points(possible_multiple_coords,col="green",pch=20,cex=0.3,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         #
                                                                         #points(neighbors_not_doublets,col="yellow",pch=20,cex=0.3,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         #color_vec <- RColorBrewer::brewer.pal(n = 12, name = "Set3")
                                                                         color_vec <- c(RColorBrewer::brewer.pal(n = 12, name = "Paired"),RColorBrewer::brewer.pal(name="PiYG",n=4)[1:2])
                                                                         points(neighbors_not_doublets,col=color_vec[member_dynam.c],pch=20,cex=0.3,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         #
                                                                         #par(bg="black")
                                                                         #plot(possible_multiple_coords,bg="black",col="green",pch=20,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         #points(neighbors_not_doublets,col=color_vec[member_dynam.c],pch=1,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         dev.off()

                                                                         # Doublets and Area Multiples Plot
                                                                         # pdf(file=paste0("Doublets_and_Area_Dist_Multiples_Plotted_",sub(img_file_name,pattern=".tif",replacement=""),".pdf"))
                                                                         # par(oma=c(0,0,5,0),mar=c(5,5,5,5),mfrow=c(1,1))
                                                                         # plot(actual_img)
                                                                         # ggplot2::xlim(c(1,dim(actual_img)[1]))
                                                                         # ggplot2::ylim(ylim=c(dim(actual_img)[1],1))
                                                                         # mtext(side=3,outer=TRUE,text=paste(dots,"","Dots"," ",num_neighbors,"","Neighbors"," ",doublets,"","Doublets (Red)"," ",num_area_multiples,"","Area_Multiples (Green)"," ",num_dist_multiples,"","Dist_Multiples (Yellow)","\n",img_file_name))
                                                                         # points(neighbor_coords2,col="red",pch=20,cex=0.3,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         # points(possible_multiple_coords,col="green",pch=20,cex=0.3,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         # points(neighbors_not_doublets,col="yellow",pch=20,cex=0.3,xlim=c(1,dim(actual_img)[1]),ylim=c(dim(actual_img)[1],1))
                                                                         # dev.off()
                                                                     };

                                                                     number1<<-append(x=number1,values=dots);
                                                                     number2<<-append(x=number2,values=num_neighbors);
                                                                     number3<<-append(x=number3,values=doublets);
                                                                     #
                                                                     number4<<-append(x=number4,values=num_area_multiples)
                                                                     #
                                                                     return(c(neighbor_coords,neighbor_distances,sum_dist_neigh_coords,neighbor_coords2,neighbor_distances2,indices_original_points,indices_new_points,dots,num_neighbors,doublets,num_area_multiples,number1,number2,number3,number4,number5))

                                                                 }
                                                                 assign_values3()

                                                             }
                                                             return(c(neighbor_coords,neighbor_distances,sum_dist_neigh_coords,neighbor_coords2,neighbor_distances2,indices_original_points,indices_new_points,dots,num_neighbors,doublets,num_area_multiples,number1,number2,number3,number4,number5))

                                                         };
                                                     }
                                                     return_values()

                                                 };

                                             };
                                         };
                                         cat(file=stderr(),"Doublet Count Summary CSV file about to be created","\n")
                                         data_table<-data.frame()
                                         data_table<-data.frame(files,number1,number2,number3,number4,number5)
                                         names(data_table)<-c("File","Dots","Neighbors","Doublets","Large_Blobs","Dist_Multiples")
                                         write.csv2(x=data_table,file=paste0("Doublet_Count_Summary",".csv"))
                                         openxlsx::write.xlsx(x=data_table,file="Doublet_Count_Summary.xlsx",col.names = TRUE,row.names = FALSE)
                                         doublet_count_file<<-read.csv2(file="Doublet_Count_Summary.csv",header=TRUE)
                                         sum_Dots<-sum(doublet_count_file$Dots)
                                         sum_Neighbors<-sum(doublet_count_file$Neighbors)
                                         sum_Doublets<-sum(doublet_count_file$Doublets)
                                         #
                                         #sum_Area_Multiples<-sum(doublet_count_file$Area_Multiples)
                                         sum_Area_Multiples<-sum(doublet_count_file$Large_Blobs)
                                         sum_Dist_Multiples<-sum(doublet_count_file$Dist_Multiples)

                                         sums<-c(sum_Dots,sum_Neighbors,sum_Doublets,sum_Area_Multiples,sum_Dist_Multiples)
                                         sums<-matrix(sums,ncol=5)
                                         colnames(sums)<-c("Sum_of_Dots","Sum_of_Neighbors","Sum_of_Doublets","Sum_of_Area_Multiples","Sum_of_Dist_Multiples")
                                         #
                                         write.table(x=sums,file="Totals.csv",row.names=FALSE,col.names=TRUE)
                                         openxlsx::write.xlsx(x=sums,file="Totals.xlsx",col.names = TRUE,row.names = FALSE)

                                     };
                                     count_doublets();
                                     cat(file=stderr(),"Doublets counted","\n");

                                     create_combined_summary_file <- function(){
                                         req(Results)

                                         unpadded_file_names <<- sort(unique(Results$Filename))
                                         cat(file=stderr(),"UNPFN","\n")

                                         unpadded_strings <<- NULL
                                         s <- NULL
                                         for (s in 1:length(sort(unique(Results$Filename)))){
                                             #string <- stringr::str_split(string = sort(unique(Results$Filename)),pattern = "Number_Dots_")[[s]][2]
                                             string <- stringr::str_split(string = unpadded_file_names,pattern = "Number_Dots_")[[s]][2]
                                             unpadded_strings <- c(unpadded_strings, string)
                                         }
                                         cat(file=stderr(),"UNPS","\n")

                                         #stringr::str_split(string = sort(unique(Results$Filename)),pattern = "Number_Dots_")[[s]]
                                         #unlist(stringr::str_split(string = sort(unique(Results$Filename)),pattern = "Number_Dots_"))

                                         #stringr::str_pad(string=unpadded_strings,width=max(nchar(unpadded_strings)),pad = "0")
                                         padded_strings <<- sort(stringr::str_pad(string=unpadded_strings,width=max(nchar(unpadded_strings)),pad = "0"))
                                         cat(file=stderr(),"PS","\n")

                                         #full_width_padded <<- str_pad(string=padded_strings,side="left",pad="_",width=max(nchar(paste0("Number_Dots_",padded_strings[1:length(sort(unique(Results$Filename)))]))))
                                         full_width_padded <<- str_pad(string=padded_strings,side="left",pad="_",width=max(nchar(paste0("Number_Dots_",padded_strings[1:length(unpadded_file_names)]))))

                                         cat(file=stderr(),"FWPS","\n")

                                         ordered_full_width_padded_strings <<- str_replace(string=full_width_padded,pattern="____________",replacement = "Number_Dots_")
                                         cat(file=stderr(),"OFWPS","\n")

                                         pad_size <<- nchar(max(dot_seq_vec)) - nchar(min(dot_seq_vec))
                                         max_pad <<- paste0("_",as.character(stringr::str_dup("0",times=pad_size)))
                                         ordered_Results_FileNames <<- sub(x=ordered_full_width_padded_strings,pattern=max_pad,replacement="_")
                                         for (p in 1:(pad_size)){
                                             print(p)
                                             pad <<- paste0("_",as.character(stringr::str_dup("0",times=p)))
                                             ordered_Results_FileNames <<- sub(x=ordered_Results_FileNames,pattern=pad,replacement="_")
                                         }
                                         # ordered_Results_FileNames <<- sub(x=ordered_full_width_padded_strings,pattern="_0",replacement="_")
                                         # cat(file=stderr(),"ORFN","\n")

                                         # create_areas_list <- function(){
                                         #    areas_list <- list()
                                         #    for(e in 1:length(ordered_Results_FileNames)){
                                         #        list_placeholder <- list(Results$Area[which(Results$Filename == ordered_Results_FileNames[e])])
                                         #        areas_list <- c(areas_list,list_placeholder)
                                         #        cat(file=stderr(),"Areas_List Created","\n")
                                         #    }
                                         #    return(areas_list)
                                         # }
                                         # areas_list <- reactive(create_areas_list())

                                         #areas_list <- create_areas_list()
                                         areas_list <<- list()
                                         e <- NULL
                                         for(e in 1:length(ordered_Results_FileNames)){
                                             list_placeholder <- list(Results$Area[which(Results$Filename == ordered_Results_FileNames[e])])
                                             areas_list <<- c(areas_list,list_placeholder)
                                         }

                                         #areas_vector <<- unlist(lapply(X=areas_list,FUN = length))
                                         areas_vector <<- sapply(X=areas_list,FUN = length)
                                         cat(file=stderr(),"Areas_List Created","\n")

                                         repeated_ordered_full_width_padded_strings <<- NULL
                                         w <- NULL
                                         for (w in 1:length(ordered_full_width_padded_strings)){
                                             #areas_vector
                                             repeated_ordered_full_width_padded_string <- rep(x=ordered_full_width_padded_strings[w],times=areas_vector[w])
                                             #repeated_ordered_full_width_padded_string <- rep(x=ordered_full_width_padded_strings[w],times=unlist(lapply(areas_list,FUN = length))[w])
                                             repeated_ordered_full_width_padded_strings <- c(repeated_ordered_full_width_padded_strings, repeated_ordered_full_width_padded_string)
                                         }
                                         cat(file=stderr(),"Areas_List 1 ROFWPS","\n")
                                         #repeated_ordered_full_width_padded_strings <- sort(repeated_ordered_full_width_padded_strings)
                                         #ordered_Results_FileNames <- sub(x=ordered_full_width_padded_strings,pattern="_0",replacement="_")

                                         numbers <<- NULL
                                         q <- NULL
                                         for(q in 1:length(unpadded_strings)){
                                             number <- as.numeric(str_split(string = unpadded_strings,pattern="_synth")[[q]][1])
                                             numbers <- c(numbers,number)
                                         }

                                         repeated_numbers <<- NULL
                                         r <- NULL
                                         for(r in 1 : length(sort(numbers))){
                                             number_vector <- as.numeric(rep(sort(numbers)[r],times=areas_vector[r]))
                                             #number_vector <- as.numeric(rep(sort(numbers)[r],times=unlist(lapply(areas_list,FUN = length))[r]))
                                             repeated_numbers <- c(repeated_numbers, number_vector)
                                         }
                                         cat(file=stderr(),"Areas_List 2 RN","\n")
                                         sorted_repeated_numbers <<- sort(repeated_numbers)

                                         unordered_dots <<- subset(doublet_count_file$Dots,doublet_count_file$File==sub(x = unpadded_file_names,pattern=".tiff",replacement = "_coords.csv"))
                                         ordered_dots <<- unordered_dots[order(unordered_dots,ordered_full_width_padded_strings)]

                                         repeated_ordered_dots <<- NULL
                                         t <- NULL
                                         for(t in 1:length(ordered_dots)){
                                             dot_vector <<- rep(ordered_dots[t],times=areas_vector[t])
                                             #dot_vector <<- rep(ordered_dots[t],times=unlist(lapply(areas_list,FUN = length))[t])
                                             repeated_ordered_dots <<- c(repeated_ordered_dots,dot_vector)
                                         }
                                         cat(file=stderr(),"Areas_List 3 ROD","\n")



                                         combined_summary_file <<- cbind(repeated_ordered_full_width_padded_strings,
                                                                         as.numeric(sorted_repeated_numbers),
                                                                         as.numeric(repeated_ordered_dots),
                                                                         as.numeric(unlist(areas_list)))
                                         combined_summary_file <<- data.frame(combined_summary_file)
                                         names(combined_summary_file) <- c("File_Name","Dots_Plotted","Dots_Read","Dot_Area")
                                         write.csv2(combined_summary_file,file="Combined_Summary_File.csv",sep = ",",dec=".",row.names = FALSE)
                                         write.xlsx(combined_summary_file,file="Combined_Summary_File.xlsx")
                                     }
                                     create_combined_summary_file()
                                     cat(file=stderr(),"CSF counted","\n");

                                 }
                    )
                }
            })

            output$analysis_files_download_button_2 <- downloadHandler(
                filename = paste0("Analysis_Files_",
                                  gsub(x=format(Sys.Date(),format="%B %a %d %Y"),
                                       pattern=" ",replacement="_"),
                                  "Time_",gsub(x=format(Sys.time()),pattern=paste0(Sys.Date()," "),replacement=""),
                                  ".zip"),
                content = function(con) {
                    download_files <- list.dirs(recursive=FALSE)
                    download_files <- download_files[download_files != "./rsconnect"]
                    download_files <- c(download_files,list.files(pattern=".xlsx"))
                    download_files <- c(download_files,list.files(pattern=".csv"))
                    download_files <- c(download_files,list.files(pattern=".pdf"))
                    download_files <- c(download_files,list.files(pattern=".eps"))
                    zip(zipfile = con, files = download_files)},
                contentType = "application/zip")

        }
    })

})
shinyApp(ui = ui_function , server)
