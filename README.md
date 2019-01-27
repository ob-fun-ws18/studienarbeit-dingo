# P2PChat [![Build Status](https://www.travis-ci.com/ob-fun-ws18/studienarbeit-p2pchat.svg?branch=master)](https://www.travis-ci.com/ob-fun-ws18/studienarbeit-p2pchat)
_Chris Brammer, Wolfgang Gabler_

[Documentation](https://ob-fun-ws18.github.io/studienarbeit-p2pchat/doc/)

## Build
```stack build```

## Usage
### Host
To start host on the default port (4242) ```stack exec p2pchat-exe -- -u HostUsername```

Port of the Host can be changed with ```-p 1234```

### Client
To connect to a Host running on "127.0.0.1:4242" ```stack exec p2pchat-exe -- -u ClientUsername1 -c 127.0.0.1:4242```

## Responsibilities
### Chris Brammer
- Host Migration
- Parser
- First Prototype (Sockets, Chat, Windows/Mac IO Madness, Heartbeat)
- Documentation
- Travis
- Specs

### Wolfgang Gabler
- Groupchat with fixed Host
- Threading / Channels
- Heartbeat
- Host Chat Functionality
- JSON
- Specs
