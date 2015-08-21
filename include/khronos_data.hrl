-record(metric, {
  timestamp,
  result = {ok}
}).

-record(check, {
  id,
  type = tcp,
  port,
  interval = 5000,
  metrics = []
}).