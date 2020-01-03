################################################################################
## This code creates the object DevTimeSBWv2.
##     - It takes Excel spreadsheets of reraing results created by Kerry in
##       Oct-Dec 2019. I made some modifications of these files mostly to
##       correct the column names
##     - The object produced is a list of experiments (Prov_Temp_Gen), each
##       experiment contains the duration of development (in days) for each
##       larval stage and each individual
##
##  Dec. 2019
################################################################################
create_DevTimeSBWv2 <- function() {

  library(readxl)

  sbw.path <- "~/Dropbox/sbw/completed spreadsheets with txt files/"

  files.list <- grep("^(?=.*xlsx)(?!.*~)",list.files(sbw.path),perl=TRUE,value=TRUE,ignore.case = TRUE)

  # Calculate stage duration for temperature transfert experiments
  # x.data is the excel spreadsheet, x.col is the column number for each stage
  Duration_T_transfert <- function(x.data,x.col) {

    if (length(x.col) != 11) {
      stop("x.col should be a list with 11 elements")
    }

    # Reads the column where date of death is entered, if there is a date,
    # the individual is eliminated
    Dead.col <- grep("^(?=.*[Dd]ead)",colnames(x.data),perl=TRUE)

    # Remove observations not good for analysis or dead before pupa
    x.clean <- x.data[x.data$Use == "Y" & is.na(x.data[,Dead.col]),]

    # Excel does not indicate the time zone so it is set to UTC by default
    # This has to be corrected to Eastern
    x.clean <- as.data.frame(lubridate::force_tz(x.clean,"US/Eastern"))

    # Some dates are entered in 1 column, others are in 2 columns (date and time)
    if (length(x.col[["start"]]) == 1) {
      start <- x.clean[,x.col[["start"]]]
    } else {
      start <- as.POSIXct(paste(strftime(x.clean[,x.col[["start"]][1]],format="%d-%m-%Y"),
                                strftime(x.clean[,x.col[["start"]][2]],format="%H:%M:%S"),
                                sep=" "),format="%d-%m-%Y %H:%M:%S")
    }

    # keep starting date to be used at the end for testing that everything has
    # been read correctly
    start.0 <- start

    # Where results are recorded
    res <- as.data.frame(matrix(0,ncol=12,nrow=nrow(x.clean)))
    colnames(res) <- c("Cup",names(x.col)[2:11],"Sex")
    class(res[,2:12]) <- "POSIXct"

    # Go through the larval stages
    for (i in 2:11) {

      # Some dates are entered in 1 column, others are in 2 columns (date and time)
      if (length(x.col[[i]]) == 1) {
        end <- x.clean[,x.col[[i]]]
      } else {
        end <- as.POSIXct(paste(strftime(x.clean[,x.col[[i]][1]],format="%d-%m-%Y"),
                                strftime(x.clean[,x.col[[i]][2]],format="%H:%M:%S"),
                                sep=" "),format="%d-%m-%Y %H:%M:%S")
      }

      # Calculate duration
      duration <- as.numeric(end - start,units="days")

      res[,i] <- duration

      # Date of end of last stage becomes the start of the next one
      start <- end

    }

    res[,1] <- x.clean$`Cup #`
    res[,12] <- x.clean$`M/F`

    # If everything goes well, the sum of durations should be roughly equal to
    # the difference between the first and last dates
    test1 <- end-start.0
    test2 <- apply(sapply(2:11,function(x) as.numeric(res[,x])),1,sum)
    if (sum(abs(test1-test2)) > 1) {
      stop("Something went wrong: the sum of stage durations is different from the duration between the start and the end of the experiment")
    }

    return(res)
  }

  # IPU_5_F1 was different than the others because the individuals were put in
  # treatment T at L3, then 20C, then back in treatement, so it has to be read
  # with different function
  Duration_T_IPU_5_F1 <- function(x.data,x.col) {

    if (length(x.col) != 12) {
      stop("x.col should be a list with 11 elements")
    }

    Dead.col <- grep("^(?=.*[Dd]ead)",colnames(x.data),perl=TRUE)

    # Remove observations not good for analysis or dead before pupa
    x.clean <- x.data[x.data$Use == "Y" & is.na(x.data[,Dead.col]),]

    # Excel does not indicate the time zone so it is set to UTC by default
    # This has to be corrected to Eastern
    x.clean <- as.data.frame(lubridate::force_tz(x.clean,"US/Eastern"))

    if (length(x.col[["start"]]) == 1) {
      start <- x.clean[,x.col[["start"]]]
    } else {
      start <- as.POSIXct(paste(strftime(x.clean[,x.col[["start"]][1]],format="%d-%m-%Y"),
                                strftime(x.clean[,x.col[["start"]][2]],format="%H:%M:%S"),
                                sep=" "),format="%d-%m-%Y %H:%M:%S")
    }

    start.0 <- start

    res <- as.data.frame(matrix(0,ncol=12,nrow=nrow(x.clean)))
    colnames(res) <- c("Cup",names(x.col)[c(2:5,7:12)],"Sex")
    class(res[,2:12]) <- "POSIXct"

    # L2.treatment
    end <- as.POSIXct(paste(strftime(x.clean[,x.col[[2]][1]],format="%d-%m-%Y"),
                            strftime(x.clean[,x.col[[2]][2]],format="%H:%M:%S"),
                            sep=" "),format="%d-%m-%Y %H:%M:%S")

    duration <- as.numeric(end - start,units="days")
    res[,2] <- duration
    start <- end
    # L2.20C
    end <- as.POSIXct(paste(strftime(x.clean[,x.col[[3]][1]],format="%d-%m-%Y"),
                            strftime(x.clean[,x.col[[3]][2]],format="%H:%M:%S"),
                            sep=" "),format="%d-%m-%Y %H:%M:%S")

    duration <- as.numeric(end - start,units="days")
    res[,3] <- duration
    start <- end
    # L3.treatment
    end1 <- as.POSIXct(paste(strftime(x.clean[,x.col[[4]][1]],format="%d-%m-%Y"),
                             strftime(x.clean[,x.col[[4]][2]],format="%H:%M:%S"),
                             sep=" "),format="%d-%m-%Y %H:%M:%S")

    duration1 <- as.numeric(end1 - start,units="days")
    start2 <- as.POSIXct(paste(strftime(x.clean[,x.col[[5]][1]],format="%d-%m-%Y"),
                               strftime(x.clean[,x.col[[5]][2]],format="%H:%M:%S"),
                               sep=" "),format="%d-%m-%Y %H:%M:%S")
    end2 <- x.clean[,x.col[[6]][1]]
    duration2 <- as.numeric(end2 - start2,units="days")
    res[,4] <- duration1 + duration2
    # L3.20C
    duration <- as.numeric(start2 - end1,units="days")
    res[,5] <- duration

    start <- end2

    for (i in 7:12) {

      if (length(x.col[[i]]) == 1) {
        end <- x.clean[,x.col[[i]]]
      } else {
        end <- as.POSIXct(paste(strftime(x.clean[,x.col[[i]][1]],format="%d-%m-%Y"),
                                strftime(x.clean[,x.col[[i]][2]],format="%H:%M:%S"),
                                sep=" "),format="%d-%m-%Y %H:%M:%S")
      }

      duration <- as.numeric(end - start,units="days")

      res[,i-1] <- duration

      start <- end
    }

    res[,1] <- x.clean$`Cup #`
    res[,12] <- x.clean$`M/F`

    test1 <- end-start.0
    test2 <- apply(sapply(2:11,function(x) as.numeric(res[,x])),1,sum)

    if (sum(abs(test1-test2)) > 1) {
      stop("Something went wrong: the sum of stage durations is different from the duration between the start and the end of the experiment")
    }

    return(res)
  }

  # Calculate stage duration for normal temperature experiments
  # x.data is the excel spreadsheet, x.col is the column number for each stage
  Duration_T_normal <- function(x.data,x.col) {

    if (length(x.col) != 6) {
      stop("x.col should be a list with 6 elements")
    }

    Dead.col <- grep("^(?=.*[Dd]ead)",colnames(x.data),perl=TRUE)

    # Remove observations not good for analysis or dead before pupa
    x.clean <- x.data[x.data$Use == "Y" & is.na(x.data[,Dead.col]),]

    # Excel does not indicate the time zone so it is set to UTC by default
    # This has to be corrected to Eastern
    x.clean <- as.data.frame(lubridate::force_tz(x.clean,"US/Eastern"))

    start <- x.clean[,x.col[["start"]]]

    start.0 <- start

    res <- as.data.frame(matrix(0,ncol=7,nrow=nrow(x.clean)))
    colnames(res) <- c("Cup",names(x.col)[2:6],"Sex")
    class(res[,2:6]) <- "POSIXct"

    for (i in 2:6) {

      end <- x.clean[,x.col[[i]]]

      duration <- as.numeric(end - start,units="days")

      res[,i] <- duration

      start <- end

      res[,1] <- x.clean$`Cup #`
      res[,7] <- x.clean$`M/F`
    }

    test1 <- end-start.0
    test2 <- apply(sapply(2:6,function(x) as.numeric(res[,x])),1,sum)

    if (sum(abs(test1-test2)) > 1) {
      stop("Something went wrong: the sum of stage durations is different from the duration between the start and the end of the experiment")
    }

    return(res)
  }

  # Where final results are saved
  res <- list()

  # Go through the Excel files in the directory
  for (f in 1:length(files.list)) {

    file.p <- paste0(sbw.path,files.list[f])

    sheets <- excel_sheets(file.p)

    # AB_F1 is not completed yet, needs the 5ºC
    if (files.list[f] == "AB_F1 Temp Trial Data macro fixed except5.xlsx") {
      sheets <- sheets[2:7]
    }

    # IPU_F0 is a bit of a mess for some temperatures such as 20ºC
    if (files.list[f] == "IPU_F0 fixed 20 ADDED.xlsx") {
      sheets <- sheets[c(1:4,6:8)]
    }

    # Go through the sheets in an Excel file
    for (n in 1:length(sheets)) {

      # read the sheet
      sheet <-  read_excel(file.p,sheet = sheets[n])

      # IPU is different (see above)
      if (sheets[n] == "IPU_5_F1") {
        # Reognize the column number by checking the label, search for "L2",
        # "L3", "L4",... but not "5.5" or "6.5"
        start <- grep("^(?=.*L2)(?!.*L2.5)",names(sheet),perl=TRUE)[1:2]
        L2.Treatment <- grep("^(?=.*L2)(?!.*L2.5)",names(sheet),perl=TRUE)[3:4]
        L2.20C <- grep("^(?=.*L3)(?!.*L3.5)",names(sheet),perl=TRUE)[1:2]
        L3.Treatment <- grep("^(?=.*L3)(?!.*L3.5)",names(sheet),perl=TRUE)[3:4]
        L3.20C <- grep("^(?=.*L3)(?!.*L3.5)",names(sheet),perl=TRUE)[5:6]
        L3.back <- grep("^(?=.*L4)(?!.*L4.5)",names(sheet),perl=TRUE)[1]
        L4.Treatment <- grep("^(?=.*L4)(?!.*L4.5)",names(sheet),perl=TRUE)[3:4]
        L4.20C <- grep("^(?=.*L5)(?!.*L5.5)",names(sheet),perl=TRUE)[1:2]
        L5.Treatment <- grep("^(?=.*L5)(?!.*L5.5)",names(sheet),perl=TRUE)[3:4]
        L5.20C <- grep("^(?=.*L6)(?!.*L6.5)",names(sheet),perl=TRUE)[1:2]
        L6.Treatment <- grep("^(?=.*L6)(?!.*L6.5)",names(sheet),perl=TRUE)[3:4]
        L6.20C <- grep("^(?=.*Pupa)",names(sheet),perl=TRUE)[1]
        sheet.col <- list(start=start,L2.Treatment=L2.Treatment,L2.20C=L2.20C,
                          L3.Treatment=L3.Treatment,L3.20C=L3.20C,L3.back=L3.back,
                          L4.Treatment=L4.Treatment,L4.20C=L4.20C,
                          L5.Treatment=L5.Treatment, L5.20C=L5.20C,
                          L6.Treatment=L6.Treatment, L6.20C=L6.20C)

        res.temp <- Duration_T_IPU_5_F1(sheet,sheet.col)


      } else {

        # Test if this is a treatment experiment
        if (length(grep("treatment",names(sheet),perl=TRUE))>0) {
          start <- grep("^(?=.*L2)(?!.*'L2.5')",names(sheet),perl=TRUE)[1:2]
          L2.Treatment <- grep("^(?=.*L2)(?!.*'L2.5')",names(sheet),perl=TRUE)[3:4]
          L2.20C <- grep("^(?=.*L3)(?!.*'L3.5')",names(sheet),perl=TRUE)[1:2]
          L3.Treatment <- grep("^(?=.*L3)(?!.*'L3.5')",names(sheet),perl=TRUE)[3:4]
          L3.20C <- grep("^(?=.*L4)(?!.*'L4.5')",names(sheet),perl=TRUE)[1:2]
          L4.Treatment <- grep("^(?=.*L4)(?!.*'L4.5')",names(sheet),perl=TRUE)[3:4]
          L4.20C <- grep("^(?=.*L5)(?!.*'L5.5')",names(sheet),perl=TRUE)[1:2]
          L5.Treatment <- grep("^(?=.*L5)(?!.*'L5.5')",names(sheet),perl=TRUE)[3:4]
          L5.20C <- grep("^(?=.*L6)(?!.*'L6.5')",names(sheet),perl=TRUE)[1:2]
          L6.Treatment <- grep("^(?=.*L6)(?!.*'L6.5')",names(sheet),perl=TRUE)[3:4]
          L6.20C <- grep("^(?=.*Pupa)",names(sheet),perl=TRUE)[1]

          sheet.col <- list(start=start,L2.Treatment=L2.Treatment,L2.20C=L2.20C,
                            L3.Treatment=L3.Treatment,L3.20C=L3.20C,
                            L4.Treatment=L4.Treatment,L4.20C=L4.20C,
                            L5.Treatment=L5.Treatment, L5.20C=L5.20C,
                            L6.Treatment=L6.Treatment, L6.20C=L6.20C)

          res.temp <- Duration_T_transfert(sheet,sheet.col)

        } else {
          start <- grep("^(?=.*L2)(?!.*L2.5)",names(sheet),perl=TRUE)[1]
          L2 <- grep("^(?=.*L3)(?!.*L3.5)",names(sheet),perl=TRUE)[1]
          L3 <- grep("^(?=.*L4)(?!.*L4.5)",names(sheet),perl=TRUE)[1]
          L4 <- grep("^(?=.*L5)(?!.*L5.5)",names(sheet),perl=TRUE)[1]
          L5 <- grep("^(?=.*L6)(?!.*L6.5)",names(sheet),perl=TRUE)[1]
          L6 <- grep("^(?=.*Pupa)",names(sheet),perl=TRUE)[1]

          sheet.col <- list(start=start,L2=L2,L3=L3,L4=L4,
                            L5=L5, L6=L6)

          res.temp <- Duration_T_normal(sheet,sheet.col)
        }
      }

      # Look for duration that would be negative for more than a day
      test <- sum(as.integer(apply(res.temp[,unlist(lapply(res.temp,is.numeric))],1,
                                   function(x) min(x)< -1)))

      # if no durations negative with more than a day then it's good but we have
      # to set negative durations for less than a day (when stage changes in
      # less than a day) to 0
      if (test == 0) {
        res.temp[,unlist(lapply(res.temp,is.numeric))] <- apply(res.temp[,unlist(lapply(res.temp,is.numeric))],2,function(x) ifelse(x<0,0,x))
        res[[sheets[n]]] <- res.temp
      } else {
        stop(paste0("Negative duration for more than a day in ",sheets[[n]]))
      }
    }

    # finalize the list of results by standardizing elements names, removing
    # IPU2 and extra spaces. This helps when the list is used in lapply functions
    names(res) <- gsub("IPU2","IPU",names(res))
    names(res) <- gsub(" ","",names(res))
  }
  return(res)
}

DevTimeSBWv2 <- create_DevTimeSBWv2()

#####

usethis::use_data(DevTimeSBWv2,overwrite = TRUE)


