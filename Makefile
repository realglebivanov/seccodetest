PROFILE=default

check:
	rebar3 as $(PROFILE) check

client:
	rebar3 as $(PROFILE) release -n client
	sh _build/$(PROFILE)/rel/client/bin/client foreground

server:
	rebar3 as $(PROFILE) release -n server
	sh _build/$(PROFILE)/rel/server/bin/server foreground
