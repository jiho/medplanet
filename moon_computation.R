# Test bed: April 2016
#
# Times are local time
library("lubridate")
with_tz(ymd_hms("2016-04-01 12:00:00"), "Europe/Paris")
local <- hours(2)
# local = UTC + 2h
#
# http://www.timeanddate.com/astronomy/france/nice
# 
# 2016  Moonrise/Moonset                             Meridian Passing
# Apr Moonrise        Moonset         Moonrise       Time Distance (km) Illumination
# 1   03:13 ↑ (115°)  13:13 ↑ (246°)  -              08:12  (28.1°) 386,087 43.7%
# 2   03:58 ↑ (113°)  14:15 ↑ (249°)  -              09:04  (29.9°) 379,779 33.0%
# 3   04:40 ↑ (109°)  15:21 ↑ (253°)  -              09:57  (32.7°) 373,347 22.7%
# 4   05:19 ↑ (104°)  16:31 ↑ (258°)  -              10:51  (36.4°) 367,324 13.6%
# 5   05:56 ↑ (98°)   17:44 ↑ (265°)  -              11:46  (40.8°) 362,289 6.3%
# 6   06:33 ↑ (92°)   18:59 ↑ (272°)  -              12:41  (45.7°) 358,788 1.6%
# 7   07:09 ↑ (85°)   20:15 ↑ (278°)  -              13:37  (50.6°) 357,225 0.0%
# 8   07:47 ↑ (79°)   21:31 ↑ (285°)  -              14:34  (55.3°) 357,777 1.8%
# 9   08:28 ↑ (73°)   22:45 ↑ (290°)  -              15:32  (59.2°) 360,356 6.7%
# 10  09:13 ↑ (68°)   23:55 ↑ (293°)  -              16:31  (62.1°) 364,624 14.3%
# 11  10:03 ↑ (66°)   -               -              17:29  (63.7°) 370,079 23.8%
# 12  -               00:58 ↑ (295°)  10:56 ↑ (64°)  18:26  (64.2°) 376,155 34.4%
# 13  -               01:55 ↑ (295°)  11:53 ↑ (65°)  19:20  (63.4°) 382,319 45.4%
# 14  -               02:44 ↑ (294°)  12:53 ↑ (67°)   20:12 (61.7°) 388,139 56.3%
# 15  -               03:26 ↑ (291°)  13:53 ↑ (70°)   21:01 (59.1°) 393,310 66.5%
# 16  -               04:03 ↑ (287°)  14:53 ↑ (75°)   21:48 (56.0°) 397,651 75.8%
# 17  -               04:36 ↑ (283°)  15:52 ↑ (79°)   22:33 (52.4°) 401,086 83.8%
# 18  -               05:05 ↑ (278°)  16:50 ↑ (85°)   23:16 (48.6°) 403,618 90.4%
# 19  -               05:34 ↑ (273°)  17:48 ↑ (90°)   23:58 (44.7°) 405,292 95.3%
# 20  -               06:01 ↑ (267°)  18:46 ↑ (95°)   - - -
# 21  -               06:29 ↑ (262°)  19:43 ↑ (101°)  00:41 (40.9°) 406,174 98.5%
# 22  -               06:58 ↑ (257°)  20:41 ↑ (105°)  01:24 (37.3°) 406,320 99.9%
# 23  -               07:29 ↑ (253°)  21:38 ↑ (109°)  02:07 (34.0°) 405,766 99.3%
# 24  -               08:03 ↑ (249°)  22:34 ↑ (113°)  02:52 (31.2°) 404,514 96.9%
# 25  -               08:41 ↑ (246°)  23:28 ↑ (115°)  03:39 (29.1°) 402,542 92.7%
# 26  -               09:23 ↑ (245°)  - 04:27 (27.7°) 399,813 86.7%
# 27  00:21 ↑ (116°)  10:11 ↑ (244°)  - 05:16 (27.1°) 396,295 79.1%
# 28  01:10 ↑ (116°)  11:05 ↑ (245°)  - 06:06 (27.5°) 391,994 70.1%
# 29  01:55 ↑ (114°)  12:03 ↑ (247°)  - 06:57 (28.9°) 386,979 60.0%
# 30 02:37 ↑ (111°)  13:05 ↑ (251°)  - 07:49 (31.3°) 381,414 49.1%
#
#
# * All times are local time for Nice. Time is adjusted for DST when applicable. Dates are based on the Gregorian calendar. Illumination is calculated at lunar noon
#
# Moon phases
# http://www.timeanddate.com/astronomy/france/nice
# 7 New Moon at 13:23
# 14 First Quarter at 05:59
# 22 Full Moon at 07:23
# 23 Third Quarter at 05:28
#
# calendar
# New moon :  7 Apr 13:25
# 1 quart  : 14 Apr 06:00
# Full moon: 22 Apr 07:25
# 2 quart  : 30 Apr 05:30
#

# NB: library("lunar") tested but less flexible
library("oce")


# times
x <- ymd("2016-04-01")
x <- x + hours(0:(31*24*2))

ma <- moonAngle(x, lon=7.31, lat=43.68)
str(ma)
# qplot(x, ma$azimuth)
qplot(x, ma$altitude) # moon rise and moon set
qplot(x, ma$rightAscension)
qplot(x, ma$declination)
qplot(x, ma$lambda)
qplot(x, ma$beta)
qplot(x, ma$diameter)
qplot(x, ma$distance)
qplot(x, ma$illuminatedFraction)  # moon phase
qplot(x, ma$phase)
qplot(x, (ma$phase - floor(ma$phase)))

intersections <- function(x, y, value) {
  if ( is.POSIXct(x) ) {
    xn <- as.numeric(x)
  } else if ( is.numeric(x) ) {
    xn <- x
  } else {
    stop("Cannot find intersection of non-numeric objects.")
  }
  i <- which(diff(sign(y - value)) != 0)
  d <- data.frame(x1=xn[i], x2=xn[i+1], y1=y[i], y2=y[i+1])
  ints <- apply(d, 1, function(X) {approx(X[3:4], X[1:2], value)$y})
  if (is.POSIXct(x)) {
    ints <- as.POSIXct(ints, tz=tz(x), origin="1970-01-01")
  }
  return(ints)
} 


# Moon phases
# New moon :  7 Apr 13:25    6 May 21:31
# 1 quart  : 14 Apr 06:00   13 May 19:03
# Full moon: 22 Apr 07:25   21 May 23:16
# 2 quart  : 30 Apr 05:30   29 May 14:13

# new moon
ma$time[which.min(ma$illuminatedFraction)] + local
ma$time[which(ma$illuminatedFraction < 0.00001)] + local

mad <- data.frame(date=ma$time, ill=ma$illuminatedFraction, phase=ma$phase, lunar_month=floor(ma$phase))
ddply(filter(mad, ill<0.05), ~lunar_month, function(x) {x$date[which.min(x$ill)]})


# full moon
ma$time[which.max(ma$illuminatedFraction)] + local
ma$time[which(ma$illuminatedFraction > 0.999995)] + local
ddply(filter(mad, ill>0.95), ~lunar_month, function(x) {x$date[which.max(x$ill)]})

# first and last quarters
intersections(x=ma$time, y=ma$illuminatedFraction, value=0.5) + local
# intersections(x=ma$time, y=(ma$phase - floor(ma$phase)), value=0.25) + local



# Moon rise and set
# Apr Moonrise        Moonset       
# 1   03:13 ↑ (115°)  13:13 ↑ (246°)
# 2   03:58 ↑ (113°)  14:15 ↑ (249°)
# 3   04:40 ↑ (109°)  15:21 ↑ (253°)
# 4   05:19 ↑ (104°)  16:31 ↑ (258°)

x <- ymd("2016-04-01") + hours(0:(24*4))
ma <- moonAngle(x, lon=7.31, lat=43.68)
qplot(x, ma$altitude)

moon_rise_set <- function(x, y) {
  xn <- as.numeric(x)
  i <- which(diff(sign(y - value)) != 0)
  d <- data.frame(x1=xn[i], x2=xn[i+1], y1=y[i], y2=y[i+1])

  times <- apply(d, 1, function(X) {approx(X[3:4], X[1:2], value)$y})
  times <- as.POSIXct(times, tz=tz(x), origin="1970-01-01")

  rise_set <- apply(d, 1, function(X) {ifelse(X[3]<X[4], "rise", "set")})

  return(data.frame(times, rise_set))
} 

moon_rise_set(x=ma$time, y=ma$altitude)
