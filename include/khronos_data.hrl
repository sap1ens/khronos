-record(metric, {
  timestamp,
  result = {ok}
}).

-record(target, {
  id,
  type = tcp,
  port,
  address,
  interval = 30000,
  timeout = 5000,
  metrics = []
}).