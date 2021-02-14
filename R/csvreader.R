
#' Title
#' @title CSV reader
#'
#' @param csv the name of the CSV file
#' @param dird the directory housing the CSV file (ending with "\\\")
#'
#' @return a dataframe object containing the data in the csv file.
#'
#' @export
#'
#' @examples mpg.df=myread(paste0("C:\\", "CSVdirectory\\"), "Data.csv")
myread=function(dird, csv){
  fl=paste(dird,csv,sep="")
  read.table(fl,header=TRUE,sep=",")
}
