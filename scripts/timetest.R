# p_conveyor way
# separate datetimes
load("data/sd_s.Rda")
start <- Sys.time()
sd_s$date <- lubridate::date(sd_s$datetime)
sd_s$time <- as.character(substr(as.character(sd_s$datetime), start = 12, stop = 19))
sd_s$time[sd_s$time == ""] <- "00:00:00"
conv <- p_conveyor(dataset = sd_s, shiftMax = 5, idCol = "indiv", dateCol = "date", timeCol = "time")
end <- Sys.time()
(end-start)

start2 <- Sys.time()
conv2 <- as.data.frame(rotate_data_table(data = sd_ns, idCol = "indiv", dateCol = "datetime", shiftMax = 5))
end2 <- Sys.time()
(end2-start2) # okay, so this is a HUGE difference in favor of my split/apply/combine method.

# Note: there may be a way to preserve Ryan's method and make it able to go backwards and still vectorize it, but I haven't quite figured out how.
