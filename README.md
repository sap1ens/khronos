# khronos

*khronos is my first Erlang application. It's mostly a training app right now, but I'm planning to make it a real tool for production uptime monitoring.*

khronos is a simple uptime service for TCP and UDP ports implemented using Erlang/OTP.

Planning core features for the version 0.1:
- REST API for creating monitoring goals ("targets") and fetching statistics ("metrics")
- Scheduler for TCP and UDP ports
- In memory storage (no persistence)

In Future:
- Multi-node setup
- Riak persistence
- More target types including webhooks