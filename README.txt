This directory tree provides RUFTP (Reliable UDP-based File Transfer)
libraries for multiple languages, in particular libruftp.a for C,
from which client executables and server daemons can be built.

libruftp.a provides for the 
1. de/construction of packets
2. server state machine
3. and a client state machine encapsulated in a (blocking) call

A non-blocking (maybe async/callback) version of the client
would be desirable.

