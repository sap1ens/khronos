# khronos

*khronos is my first Erlang application. It's mostly a training app right now, but I'm planning to make it a real tool for production uptime monitoring.*

khronos is a simple uptime service for TCP ports implemented using Erlang/OTP.

Planning core features for the version 0.1:
- REST API for creating monitoring goals ("targets") and fetching statistics ("metrics")
- Scheduler for TCP ports checks
- In memory storage (no persistence)

In Future:
- Multi-node setup
- Riak persistence
- More target types including webhooks

## Little demo

```
$ ./run.sh
rr(khronos_data).
{ok, _} = khronos_data:create_target(1, tcp, 8871, "google.com", 10000).
{ok, Target} = khronos_data:get_target(1).
khronos_data:get_all_targets().
khronos_sup:schedule_check(Target).
```