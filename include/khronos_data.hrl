-record(check, {
  id,
  type = tcp,
  interval = 5000
}).
-record(metric, {
  check = #check{},
  timestamp,
  response_time,
  result = ok
}).