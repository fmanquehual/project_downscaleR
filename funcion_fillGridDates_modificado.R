fillGridDates_modificado <- function (grid, tz = "GMT", estaciones_metereologicas=FALSE) 
{ # grid <- pr.sum
  # grid <- y
  station <- ("loc" %in% getDim(grid))
  grid <- redim(grid, runtime = TRUE, var = TRUE)
  start <- grid$Dates$start
  end <- grid$Dates$end
  
  # Aqui modificaste ----
  
  # day.step <- as.numeric(names(which.max(table(difftime(c(start, # original
  #                                                         NA), c(NA, start), units = "days")))))
  
  if(estaciones_metereologicas==TRUE){
    day.step <- as.numeric(names(which.max(table(difftime(c(start, 
                                                            NA), c(as.POSIXlt(NA, tz), start), units = "days")))))
  } else(
    day.step <- as.numeric(names(which.max(table(difftime(c(start, 
                                                            NA), c(NA, start), units = "days", tz=tz)))))
  )
  
  # fin ---
  
  message("Time difference of ", day.step, " days")
  formato <- "%Y-%m-%d %H:%M:%S"
  if (day.step >= 1) 
    formato <- "%Y-%m-%d"
  start <- as.POSIXlt(start, format = formato, tz = tz)
  end <- as.POSIXlt(end, format = formato, tz = tz)
  xs <- as.POSIXlt(as.character(seq.POSIXt(start[1], start[length(start)], 
                                           by = day.step * 24 * 60 * 60)), format = formato, tz = tz)
  xe <- as.POSIXlt(as.character(seq.POSIXt(end[1], end[length(end)], 
                                           by = day.step * 24 * 60 * 60)), format = formato, tz = tz)
  test <- data.frame(date = start, wh = TRUE)
  result <- merge(data.frame(date = xs), test, by.y = "date", 
                  by.x = "date", all.x = TRUE)
  ind <- which(result[, "wh"])
  sh <- getShape(grid)
  sh[names(sh) == "time"] <- nrow(result)
  arr <- array(data = NA, dim = sh)
  arr[, , , ind, , ] <- grid[["Data"]]
  grid[["Data"]] <- arr
  arr <- NULL
  attr(grid[["Data"]], "dimensions") <- names(sh)
  grid[["Dates"]][["start"]] <- strftime(xs, format = formato, 
                                         tz = tz, usetz = TRUE)
  grid[["Dates"]][["end"]] <- strftime(xe, format = formato, 
                                       tz = tz, usetz = TRUE)
  grid <- redim(grid, drop = TRUE, loc = station)
  return(grid)
}
