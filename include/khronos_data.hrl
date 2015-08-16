-record(check, {
  id,
  type = tcp,
  port,
  interval = 5000
}).
-record(metric, {
  check = #check{},
  timestamp,
  result = {ok}
}).