in gdb, hook connect with:
```
b connect if (char) *$rsi == 0x02 && (char) *($rsi + 4) == 0x42
```

tib also uses recvfrom (not recv) and send (not sendto). don't listen to strace,
which logs send calls as sendto (which presented a huge misleading point for
me). hook these with:
```
b recvfrom if $rdi != 0x09
```
which avoids the spammy traffic from xcb socket (file desc 9).

upon connect, server sends a 21-byte challenge that always begins with `00h 13h
84h 10h` and always ends in `56h`. client must send back a 160-byte response
derived from username, password, and some other info. not easy to break.

after challenge auth seq is satisfied, server sends a very large copy of server
state including list of connected players, their stats, probably their positions
on the map, and some recent chat history.

every so often a ping is sent of the form, `00h 01h 86h`. server echos.

chat command has the form:

some examples:

```
# sent over universe chat
> 00 10 be 0b 68 69 20 75 6e 69 76 65 72 73 65 00 00 0a
              h  i     u  n  i  v  e  r  s  e

< 
