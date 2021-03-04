
library(fs)



count_doublets<-function(image_summary_plot="n",thres_dist=4) {
    require(raster)
    require(openxlsx)
    require(ggplot2)
    require(dplyr)
    options(repos = BiocManager::repositories())
    require(EBImage)
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
        thres<<-as.numeric(thres_dist)
        number1<<-NULL
        number2<<-NULL
        number3<<-NULL
        doublets<<-NULL
        dots<<-NULL
        data_table<<-NULL
        img_file_name<<-NULL
        num_neighbors<<-NULL
    }
    set_variables()
    cat(file=stderr(),"Variables set","\n")

    for (x in seq_along(files)){
        cat(file=stderr(),"Analysis in progress","\n");
        print(x);

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
                            if (image_summary_plot=="y"){
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
    cat(file=stderr(),"Doublets counted","\n")

}

