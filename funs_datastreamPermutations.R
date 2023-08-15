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
  globalMaxDate <- max(dataset[[dateCol]], na.rm = T)
  globalMinDate <- min(dataset[[dateCol]], na.rm = T)
  globalDateRange <- as.numeric((globalMaxDate - globalMinDate)+1)
  checkmate::assertNumeric(shiftMax, len = 1, upper = globalDateRange) # can't shift by more days than the date range represented in the dataset.
  out <- dataset %>%
    group_by(.data[[idCol]]) %>%
    group_split(.keep = T) %>%
    map_dfr(~{
      shift <- sample(-(shiftMax):shiftMax, size = 1)
      # get all unique days that show up
      days <- sort(unique(.x[[dateCol]]))
      shiftedDates <- days + shift
      #browser()
      if(mode == "self"){ # self mode
        selfMinDate <- min(days, na.rm = T)
        selfMaxDate <- max(days, na.rm = T)
        daysFilled <- seq(lubridate::ymd(selfMinDate), lubridate::ymd(selfMaxDate), by = "day")
        above <- shiftedDates - selfMaxDate
        below <- selfMinDate - shiftedDates
      }else{ # global mode
        daysFilled <- seq(lubridate::ymd(globalMinDate), lubridate::ymd(globalMaxDate), by = "day")
        above <- shiftedDates - globalMaxDate
        below <- globalMinDate - shiftedDates
      }
      
      # Enact the conveyor belt
      if(any(above > 0)){
        shiftedDates[which(above > 0)] <- daysFilled[as.numeric(above[which(above > 0)])]
      }
      if(any(below > 0)){
        shiftedDates[which(below > 0)] <- rev(daysFilled)[as.numeric(below[which(below > 0)])]
      }
      
      # Make a data frame to hold the data
      daysDF <- bind_cols({{dateCol}} := days, "newDate" = shiftedDates)
      new <- .x %>%
        left_join(daysDF, by = dateCol) %>%
        rename("oldDate" = all_of(dateCol), {{dateCol}} := "newDate") %>%
        mutate(shift = shift)
      
      if(!is.null(timeCol)){
        new$timestamp <- lubridate::ymd_hms(paste(new$newDate, new[[timeCol]]))
      }
      
      return(new)
    })
  return(out)
}

