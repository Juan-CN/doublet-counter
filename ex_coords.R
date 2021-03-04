#' This function extracts individual image coordinates into separate files
#'  from the cumulative Results.csv file generated using the ImageJ macro.
#' ImageJ macro at https://github.com/Juan-CN/doublet-counter
#' @title extract_coordinates_from_Results_to_files
#' @description This function extracts individual image coordinates into separate files from the cumulative Results.csv file generated using the ImageJ macro.
#' @author Juan Carabeo-Nieva, \email{jcarabeonieva@@gmail.com}, \url{https://github.com/Juan-CN/doublet-counter}
#' @details
#' @export
#' @examples extract_coordinates_from_Results_to_files()
require(dplyr)

extract_coordinates_from_Results_to_files<-function() {
  Results<-read.table(file="Results.csv",header=TRUE,dec=".",sep=",")
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
}
