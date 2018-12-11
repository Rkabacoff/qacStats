csv_file_table <- function(path, bin_var, nbins=10, bp=NULL, adherentvar=NULL, export_file_name){
  require(dplyr)
  require(readxl)

  #Reading in dataset from Excel (.xls or .xlsx), turning dataset to dataframe
  dataset <- read_excel(path, na=c("", "NA"))
  dataset <- as.data.frame(dataset)

  #Binning variable
  bin_var <- as.numeric(as.character(dataset[[bin_var]]))
  if(!is.null(bp)){
    bp <- bp
  } else{
    bp <- pretty(bin_var, n=nbins)
  }
  y <- cut(bin_var, breaks=bp, labels=NULL)

  #Summary table w/ dplyr
  y <- enquo(y)
  results = dataset %>%
    group_by(!!y) %>%
    summarise(n=n()) %>%
    mutate(prop=n/sum(n)) %>%
    mutate(cum_freq=cumsum(n)) %>%
    mutate(cum_prop=cumsum(prop))

  #Adding adherent var to summary table w/ dplyr
  if(!is.null(adherentvar)){
    adherentvar <- enquo(adherentvar)
    adherent_results = dataset %>%
      group_by(!!adherentvar) %>%
      summarise(n=n()) %>%
      mutate(prop=n/sum(n))
    results = cbind(results, adherent_results)
  }

  #Final .CSV file
  write.table(results, file=export_file_name, sep=",", row.names=FALSE)
}

line_plot <- function(path, primaryaxis, secaxis){
  #Reading in .csv file, converting to a dataframe
  results_csv <- read.csv(path)
  results_csv <- as.data.frame(results_csv)

  #Renaming first column to intervals, omitting missing values, & turning 'intervals' column from factor to numeric
  names(results_csv)[1] = "intervals"
  results_csv = results_csv[results_csv$intervals != "NA",]
  results_csv <- na.omit(results_csv)

  #Plot
  primaryaxis <- deparse(substitute(primaryaxis))
  secaxis <- deparse(substitute(secaxis))
  par(mar = c(5, 5, 3, 5))
  plot(results_csv[[primaryaxis]], type ="l", ylab = "Cumulative Proportion",
       main = "Plot of Cumulative Proportion and Proportion", xlab = "Intervals",
       col = "blue")
  axis(side=4)
  par(new = TRUE)
  plot(results_csv[[secaxis]], type = "l", xaxt = "n", yaxt = "n",
       ylab = "", xlab = "", col = "red", lty = 2)
  mtext("Proportion", side = 4, line = 3)
  legend("topleft", c("Cumulative Proportion", "Proportion"),
         col = c("blue", "red"), lty = c(1, 2), cex = 0.4)
}
