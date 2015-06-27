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
#    ^  ^  ^-- length of text                         ^
#    |  +-- 0xbe == universe?                         |
#    +-- len till end                                 +-- end delimiter

< 
```

```python
def send_universe(msg):
    packet = "\xbe" + chr(len(msg)) + msg + "\x00\x00\x0a"
    msg = "\x00" + chr(len(packet)) + packet
    return msg
```

## client request codes

corroborates with login and chat consts. found from monodis (trimmed for space):

```
int8 DISCONNECT = int8(0x82)
int8 ALIVE = int8(0x86)
int8 CHAT_MESSAGE = int8(0xbe)                  <<<<<<
int8 REQUEST_LOGIN = int8(0xbd)                 <<<<<<
int8 REQUEST_PLAYER = int8(0xbf)
int8 REQUEST_PLAYER_STATS = int8(0xc0)
int8 REQUEST_CORP_ROSTER = int8(0xc1)
int8 REQUEST_INVENTORY = int8(0xc2)
int8 REQUEST_MOVE = int8(0xc6)
int8 REQUEST_HARVEST = int8(0xc8)
int8 REQUEST_LOOT = int8(0xca)
int8 REQUEST_FOLLOW = int8(0xcb)
int8 REQUEST_ATTACK = int8(0xcc)
int8 REQUEST_REPAIR_TARGET = int8(0xcd)
int8 REQUEST_REPAIR_BLACKDOLLAR = int8(0xce)
int8 REQUEST_REPAIR_STARPORT = int8(0xcf)
int8 REQUEST_EARTH_JUMP = int8(0xd0)
int8 REQUEST_BUY_SHIP = int8(0xd2)
int8 REQUEST_SWAP_SHIP = int8(0xd3)
int8 REQUEST_BD_ITEM_PURCHASE = int8(0xd4)
int8 REQUEST_TRADE = int8(0xd8)
int8 REQUEST_CANCEL_TRADE = int8(0xd9)
int8 REQUEST_CORP_ACCEPT_INVITE = int8(0xda)
int8 REQUEST_CORP_REJECT_INVITE = int8(0xdb)
int8 REQUEST_ALLIANCE_ACCEPT_INVITE = int8(0xdc)
int8 REQUEST_ALLIANCE_REJECT_INVITE = int8(0xdd)
int8 REQUEST_SET_TECHNOLOGY = int8(0xde)
int8 REQUEST_DEVELOP_BLACKDOLLARS = int8(0x02)
int8 REQUEST_DEVELOP_RESOURCES = int8(0x03)
int8 REQUEST_JETTISON = int8(0x0c)
int8 REQUEST_RESOURCE_BUY = int8(0x0d)
int8 REQUEST_RESOURCE_SELL = int8(0x0e)
int8 REQUEST_RESOURCE_TRANSFER = int8(0x10)
int8 REQUEST_ITEM_JETTISON = int8(0x16)
int8 REQUEST_ITEM_EQUIP = int8(0x17)
int8 REQUEST_ITEM_UNEQUIP = int8(0x18)
int8 REQUEST_ITEM_MOVE_TO_BANK = int8(0x19)
int8 REQUEST_ITEM_REMOVE_FROM_BANK = int8(0x1a)
int8 REQUEST_ITEM_MOVE_TO_GARRISON = int8(0x1b)
int8 REQUEST_ITEM_REMOVE_FROM_GARRISON = int8(0x1c)
int8 REQUEST_ITEM_BUY = int8(0x1d)
int8 REQUEST_ITEM_SELL = int8(0x1e)
int8 REQUEST_ITEM_ENGINEER = int8(0x1f)
int8 ACCOUNT_VARIABLES = int8(0x20)
int8 REQUEST_SET_CORP_SHIP_COLOR = int8(0x21)
int8 AH_ITEM_REQUEST_PAGE = int8(0x2c)
int8 AH_ITEM_REQUEST_BUYOUT = int8(0x2d)
int8 AH_ITEM_REQUEST_BID = int8(0x2e)
int8 AH_ITEM_REQUEST_SELL = int8(0x2f)
int8 AH_ITEM_REQUEST_REMOVE = int8(0x30)
int8 AH_ITEM_REQUEST_ITEM = int8(0x31)
```

so all packets are of the form:

```
> 00 aa bb ...
  ^  ^  ^  ^^^-- request-specific data
  |  |  +-- client request code
  |  +-- num bytes till end of packet
  +-- packet start delimiter
```
