load("../data/daily_weather.RData")

# assuming station id is WBANNO
get_station_data = function(station_id, start_date = "2000-01-01", end_date = "3000-01-01"){
  cond = (daily_weather$WBANNO == station_id) & (daily_weather$LST_DATE <= end_date) & (daily_weather$LST_DATE >= start_date)
  return (daily_weather[cond, ])
}
