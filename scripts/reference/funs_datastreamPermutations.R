# Functions for various datastream permutations
# Created by Kaija Gahm, 2023-02-07

p_randomDays <- function(dataset, idCol = "Nili_id", dateCol = "dateOnly", timeCol = "timeOnly"){
  permuted <- dataset %>%
    group_by(.data[[idCol]]) %>%
    group_split(.keep = T) %>%
    map_dfr(~{
      days <- unique(.x[[dateCol]])
      daysShuffled <- sample(days, size = length(days), replace = F)
      daysDF <- bind_cols({{dateCol}} := days, "newDate" = daysShuffled)
      out <- .x %>%
        left_join(daysDF, by = dateCol) %>%
        mutate(timestamp = lubridate::ymd_hms(paste(newDate, .data[[timeCol]]))) %>%
        rename("oldDate" = all_of(dateCol), {{dateCol}} := "newDate")
      return(out)
    })
  return(permuted)
}

p_shift <- function(dataset, shiftMax = 10, idCol = "Nili_id", dateCol = "dateOnly", timeCol = "timeOnly"){
  out <- dataset %>%
    group_by(.data[[idCol]]) %>%
    group_split(.keep = T) %>%
    map_dfr(~{
      shift <- sample(-shiftMax:shiftMax, size = 1)
      .x <- .x %>%
        mutate("oldDate" = .data[[dateCol]],
               {{dateCol}} := oldDate + shift,
               timestamp = lubridate::ymd_hms(paste(.data[[dateCol]], .data[[timeCol]])))
      return(.x)
    })
  return(out)
}

p_conveyor <- function(dataset, shiftMax, mode = "global", idCol = "Nili_id", dateCol = "dateOnly", timeCol = "timeOnly"){
  checkmate::assertChoice(mode, choices = c("global", "self"))
  checkmate::assertChoice(idCol, choices = names(dataset))
  checkmate::assertChoice(dateCol, choices = names(dataset))
  globalMaxDate <- max(dataset[[dateCol]], na.rm = T)
  globalMinDate <- min(dataset[[dateCol]], na.rm = T)
  globalDateRange <- as.numeric((globalMaxDate - globalMinDate)+1)
  checkmate::assertNumeric(shiftMax, len = 1, upper = globalDateRange) # can't shift by more days than the date range represented in the dataset.
  indivList <- dataset %>%
    group_by(.data[[idCol]]) %>%
    group_split(.keep = T)
  joined <- vector(mode = "list", length = length(indivList))
  for(indiv in 1:length(indivList)){
    x <- indivList[[indiv]]
    shift <- sample(-(shiftMax):shiftMax, size = 1)
    #cat(shift, "\n")
    # get all unique days that show up
    days <- sort(unique(x[[dateCol]]))
    if(mode == "self"){ # self mode
      # get min and max dates to shift around (the "poles" of the conveyor)
      selfMinDate <- min(days, na.rm = T)
      selfMaxDate <- max(days, na.rm = T)
      # create a total sequence of dates to select from
      daysFilled <- seq(lubridate::ymd(selfMinDate), lubridate::ymd(selfMaxDate), by = "day")
      # converting to numbers so we can use %%--which dates are the ones we started with?
      vec <- which(daysFilled %in% days)
      shiftedvec <- vec + shift # shift
      new <- (shiftedvec - min(vec)) %% (max(vec)-min(vec)+1)+1 # new dates as numbers
      shiftedDates <- daysFilled[new] # select those dates from the possibilities
    }else{ # global mode
      # create a total sequence of dates to select from
      daysFilled <- seq(lubridate::ymd(globalMinDate), lubridate::ymd(globalMaxDate), by = "day")
      # converting to numbers so we can use %%--which dates are the ones we started with?
      vec <- which(daysFilled %in% days)
      shiftedvec <- vec + shift #shift
      new <- (shiftedvec - min(vec)) %% (max(vec)-min(vec)+1)+1 # new dates as numbers
      shiftedDates <- daysFilled[new] # select those dates from the possibilities
    }
    
    # Make a data frame to hold the old and new dates
    daysDF <- bind_cols({{dateCol}} := days, 
                        "newDate" = shiftedDates,
                        shift = shift)
    nw <- left_join(x, daysDF, by = dateCol)
    nw$oldDate <- nw[[dateCol]]
    nw[[dateCol]] <- nw$newDate
    
    if(!is.null(timeCol)){
      nw$timestamp <- lubridate::ymd_hms(paste(nw$newDate, nw[[timeCol]]))
    }
    joined[[indiv]] <- nw
  }
  out <- do.call(rbind, lapply(joined, st_sf)) # to avoid losing the sf class
  return(out)
}

