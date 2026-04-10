################################################
# ADVANCED EXAMPLE:
# UNIX time (see https://en.wikipedia.org/wiki/Unix_time) is one way computer 
# systems keep track of the timing of events.
# Quoting from wikipedia: "Unix time is a date and time representation widely
# used in computing. It measures time by the number of non-leap seconds that 
# have elapsed since 00:00:00 UTC on 1 January 1970"
# They are straightforward to use for arithmetic calculations 
# (e.g., computing seconds elapsed between two events)
# but can be hard to interpret. Thus, in some cases, you may want to convert
# UNIX time to a format a human can understand, perhaps to enable quick spot checks.
# Here is one example of how to do so with the ts_emi_resp variable.
# Because the computer system recorded this event in terms of milliseconds and
# the with_tz function in the lubridate package expects UNIX time in seconds, 
# we divide by 1000.
usmountain <- with_tz(with_tz(as.numeric(merged_ema_and_app_tracked[["ts_emi_resp"]])/1000, tz = "UTC"), tz = "US/Mountain")

uscentral <- with_tz(with_tz(as.numeric(merged_ema_and_app_tracked[["ts_emi_resp"]])/1000, tz = "UTC"), tz = "US/Central")

uspacific <- with_tz(with_tz(as.numeric(merged_ema_and_app_tracked[["ts_emi_resp"]])/1000, tz = "UTC"), tz = "US/Pacific")

new_col <- data.frame(hrts_emi_resp = c(force_tz(usmountain, tz = "UTC"),
                                        uscentral,
                                        uspacific))

# There are some columns in the dataset that do not require dividing by 1000
# as UNIX time is already represented in seconds (you can tell by the number of digits).
# Try comparing the output of the code below to the column
with_tz(with_tz(as.numeric(merged_ema_and_app_tracked[["ec_block_start_unix"]]), tz = "UTC"), tz = "US/Mountain")

with_tz(with_tz(as.numeric(merged_ema_and_app_tracked[["ec_block_start_unix"]]), tz = "UTC"), tz = "US/Central")

with_tz(with_tz(as.numeric(merged_ema_and_app_tracked[["ec_block_start_unix"]]), tz = "UTC"), tz = "US/Pacific")



