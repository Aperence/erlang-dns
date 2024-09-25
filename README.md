## Build

```
rebar3 compile
```

## Usage

```erl
1> application:start(dns).
ok

2> Pid=resolver:resolve("www.google.com").
<0.ID.0>
3> host:ping(Pid).
{pong,"www.google.com"}

4> Pid2=resolver:resolve("www.uclouvain.be").
<0.ID2.0>
5> host:ping(Pid2).
{pong,"www.uclouvain.be"}
```