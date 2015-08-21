-record(metric, {
  timestamp,
  result = {ok}
}).

-record(target, {
  id,
  type = tcp,
  port,
  interval = 5000,
  metrics = []
}).