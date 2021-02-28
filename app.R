load_packages <- function() {
    library(fs)
    library(openxlsx)
    library(base64enc)
    library(ggplot2)
    library(shiny)
    library(magrittr)
    library(raster)
    library(dplyr)
    options(repos = BiocManager::repositories())
    library(EBImage)
}
load_packages()

# Max file upload size of 50 MB
options(shiny.maxRequestSize=50*1024^2)

rm(list=ls())

ui_function <- function(){ fluidPage(

    titlePanel(title=div(img(src=base64enc::dataURI(file="binary_fission_image.png", mime="image/png"),width=70,height=70),"Doublet Counter"),windowTitle = "Doublet Counter"),

    tags$a(href="https://github.com/Juan-CN/doublet-counter/","Macro for Generating Results.csv file --- Instructions for its Use --- and Program Source Code","\n"),

    sidebarLayout(position = "left",
                  sidebarPanel = sidebarPanel(
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
                                label = "2. Set threshold value",
                                value = 4.0,
                                placeholder = "number"),
                      fileInput(inputId = "results_csv_file",
                                label = "3. Choose Results.csv File from your computer.",
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
                                placeholder="word1, word2, word3, etc")),
                      downloadButton(outputId = "analysis_files_download_button",
                                     label = "4. Download Analysis Files"),
                      cat(file = stderr(), "UI created", "\n"),
                  ),
                  mainPanel= mainPanel(
                      tableOutput(outputId = "contents"),
                      tableOutput(outputId = "text"),
                      tableOutput(outputId = "uploaded_image_files")
                  )
    ),
)

}

server <- shinyServer(function(input, output) {

    # Show Binary Fission Image next to Title
    output$binary_fission_image<-renderImage({
        list(src = "./binary_fission_image.png")}, deleteFile = FALSE)

    # Remove old Analysis files
    start_fresh <- function(){
        unlink(x = list.files(pattern = ".csv"))
        unlink(x = list.files(pattern = ".xlsx"))
        unlink(x = list.files(pattern = ".pdf"))
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
    output$uploaded_image_files <- renderTable(striped = TRUE, bordered = TRUE, rownames = FALSE,colnames = TRUE,{
        if (is.null(input$images_file_input) == FALSE) {
            req(input$images_file_input$name);
            Images<-input$images_file_input$name;
            data.frame(Images)
        }
    })

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
                                 Results<<-Results[,c("X","Y","Filename")]
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
                                    number1<<-NULL
                                    number2<<-NULL
                                    number3<<-NULL
                                    doublets<<-NULL
                                    dots<<-NULL
                                    data_table<<-NULL
                                    img_file_name<<-NULL
                                    num_neighbors<<-NULL
                                    image_summary_plot<<-input$image_summary_plot_button
                                }
                                set_variables()
                                cat(file=stderr(),"Variables set","\n")

                                for (x in seq_along(files)){
                                    cat(file=stderr(),"Analysis in progress","\n");
                                    print(x);
                                    incProgress(amount=(1/(x*1000)),message="Calculating doublets...");
                                    data_coords<<-read.table(file = files[x], header = TRUE, dec = ".");
                                    data_coords<<-data_coords[,c("X","Y")];
                                    coord_mat<<-as.matrix(round(data_coords, digits=2));
                                    num_points<<-nrow(data_coords);
                                    dist_p1_p2<<-round(pointDistance(p1=coord_mat[1:num_points,],lonlat=FALSE,allpairs=TRUE),digits=2);
                                    neighbors<<-which(dist_p1_p2 <= thres & dist_p1_p2 != 0,arr.ind=TRUE);
                                    cat(file=stderr(),"Neighbors","\n");

                                    if (all(is.na(neighbors)) == "TRUE") {
                                        assign_values <- function() {
                                            dots<<-dim(coord_mat)[1];
                                            num_neighbors<<-0;
                                            doublets<<-0;
                                            number1<<-append(x=number1,values=dots);
                                            number2<<-append(x=number2,values=num_neighbors);
                                            number3<<-append(x=number3,values=doublets);
                                            return(c(number1,number2,number3))
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
                                                    dots<<-dim(coord_mat)[1];
                                                    num_neighbors<<-0;
                                                    doublets<<-0;
                                                    number1<<-append(x=number1,values=dots);
                                                    number2<<-append(x=number2,values=num_neighbors);
                                                    number3<<-append(x=number3,values=doublets);
                                                    cat(file=stderr(),"Neighbor distances = 0 block","\n")
                                                    return(c(number1,number2,number3))
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
                                                    dots<<-dim(coord_mat)[1];
                                                    num_neighbors<<-dim(neighbor_coords)[1];
                                                    cat(file=stderr(),"Neighbor_Coords2 is Missing Block","\n");
                                                    doublets<<-0;
                                                    number1<<-append(x=number1,values=dots);
                                                    number2<<-append(x=number2,values=num_neighbors);
                                                    number3<<-append(x=number3,values=doublets);
                                                    return(c(number1,number2,number3))
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
                                                    dots<<-dim(coord_mat)[1] ;
                                                    num_neighbors<-dim(neighbor_coords2)[1] ;
                                                    doublets<<-(sum(apply(neighbor_distances2<=thres,1,sum,na.rm=TRUE),na.rm=TRUE)/2);
                                                    doublets<<-round(doublets,digits=0);
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
                                                        pdf(file=paste0("Doublets_Plotted_",sub(img_file_name,pattern=".tif",replacement=""),".pdf"))
                                                        par(oma=c(0,0,5,0),mar=c(5,5,5,5),mfrow=c(1,1))
                                                        plot(actual_img)

                                                        ggplot2::xlim(c(1,dim(actual_img)[1]))
                                                        ggplot2::ylim(ylim=c(dim(actual_img)[1],1))


                                                        mtext(side=3,outer=TRUE,text=paste(dots,"","Dots","   ",num_neighbors,"","Neighbors","   ",doublets,"","Doublets","\n",img_file_name))
                                                        points(neighbor_coords2,col="red",pch=20,cex=0.03,xlim=c(1,800),ylim=c(800,1))
                                                        dev.off()
                                                    };

                                                    number1<<-append(x=number1,values=dots);
                                                    number2<<-append(x=number2,values=num_neighbors);
                                                    number3<<-append(x=number3,values=doublets);

                                                    return(c(neighbor_coords,neighbor_distances,sum_dist_neigh_coords,neighbor_coords2,neighbor_distances2,indices_original_points,indices_new_points,dots,num_neighbors,doublets,number1,number2,number3))

                                                }
                                                assign_values3()

                                            }
                                            return(c(neighbor_coords,neighbor_distances,sum_dist_neigh_coords,neighbor_coords2,neighbor_distances2,indices_original_points,indices_new_points,dots,num_neighbors,doublets,number1,number2,number3))

                                        };
                                        }
                                        return_values()

                                    };

                                };
                                cat(file=stderr(),"Doublet Count Summary CSV file about to be created","\n")
                                data_table<-data.frame()
                                data_table<-data.frame(files,number1,number2,number3)
                                names(data_table)<-c("File","Dots","Neighbors","Doublets")
                                write.csv2(x=data_table,file=paste0("Doublet_Count_Summary",".csv"))
                                openxlsx::write.xlsx(x=data_table,file="Doublet_Count_Summary.xlsx",col.names = TRUE,row.names = FALSE)
                                doublet_count_file<-read.csv2(file="Doublet_Count_Summary.csv",header=TRUE)
                                sum_Dots<-sum(doublet_count_file$Dots)
                                sum_Neighbors<-sum(doublet_count_file$Neighbors)
                                sum_Doublets<-sum(doublet_count_file$Doublets)
                                sums<-c(sum_Dots,sum_Neighbors,sum_Doublets)
                                sums<-matrix(sums,ncol=3)
                                colnames(sums)<-c("Sum of Dots","Sum of Neighbors","Sum of Doublets")
                                write.table(x=sums,file="Totals.csv",row.names=FALSE,col.names=TRUE)
                                openxlsx::write.xlsx(x=sums,file="Totals.xlsx",col.names = TRUE,row.names = FALSE)

                            };
                            count_doublets();
                            cat(file=stderr(),"Doublets counted","\n");
                        }
            )
}
        })

    # Download Analysis files as a Zip
    output$analysis_files_download_button <- downloadHandler(
        filename = paste0("Analysis_Files_",gsub(x=format(Sys.Date(),format="%B %a %d %Y"),pattern=" ",replacement="_"),"Time_",gsub(x=format(Sys.time()),pattern=paste0(Sys.Date()," "),replacement=""),".zip"),
        content = function(con) {
                    if (input$organize_analysis_files_conditions == ""){
                        download_files <- list.files(pattern=".csv");
                        download_files <- c(download_files,list.files(pattern=".pdf"))
                        download_files <- c(download_files,list.files(pattern=".xlsx"))
                    } else {
                    conditions<-input$organize_analysis_files_conditions
                    list_of_conditions<-strsplit(x=conditions,split=", ")[[1]]
                    doublet_count_summary_file<-read.csv2(file="Doublet_Count_Summary.csv")
                    l<-NULL
                        for (l in seq_along(list_of_conditions)){
                            folder_name<-paste0(list_of_conditions[l],"_folder")
                            dir.create(path=paste0("./",folder_name))
                            sub_file_dcsf<-doublet_count_summary_file[which(grepl(list_of_conditions[l],doublet_count_summary_file$File, fixed = TRUE) == TRUE),]
                            sub_file_dcsf<-sub_file_dcsf[,c("File","Dots","Doublets")]
                            write.table(x=sub_file_dcsf,file=paste0("DCSF_",list_of_conditions[l],".csv"),row.names = FALSE,col.names = TRUE)
                            condition_files<-list.files(pattern=list_of_conditions[l])
                            condition_files<-condition_files[condition_files != folder_name]
                            fs::file_move(path=condition_files,new_path=paste0("./",folder_name))
                            }
                        download_files <- list.dirs(recursive=FALSE)
                        download_files<-download_files[download_files != "./rsconnect"]
                        download_files <- c(download_files,list.files(pattern=".xlsx"))
                        download_files <- c(download_files,list.files(pattern=".csv"))
                        }
                    zip(con, download_files)}
        ,contentType = "application/zip"
    )
})

shinyApp(ui = ui_function , server)
