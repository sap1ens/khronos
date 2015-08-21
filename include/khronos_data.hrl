-record(metric, {
  timestamp,
  result = {ok}
}).

-record(target, {
  id,
  type = tcp,
  port,
  address,
  interval = 5000,
  metrics = []
}).