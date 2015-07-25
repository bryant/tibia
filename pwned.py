from Crypto.Cipher import AES
import logging

logger = logging.getLogger(__name__)

TIB_AES128_KEY = [0x02, 0x27, 0x75, 0xfe, 0xfe, 0x08, 0xbd, 0x35, 0xf7, 0x92,
                  0x58, 0x5d, 0xd6, 0xc1, 0x18, 0xd9]
TIB_AES128_KEY = "".join(chr(c) for c in TIB_AES128_KEY)

def tibcrypt(pt, iv):
    """ encrypt the plaintext `pt` with aes128/pkcs7 padding. """
    z = AES.new(key=TIB_AES128_KEY, mode=AES.MODE_CBC, IV=iv)
    nearest_16 = ((len(pt) + 16)/ 16) * 16  # fast int ceil
    padlen = nearest_16 - len(pt)  # block size 128 bits = 16 bytes
    logger.debug("padding length: %d" % padlen)
    return z.encrypt(pt + chr(padlen) * padlen)

class Account(object):
    def __init__(self, username, password, devid, devtype, devversion):
        pass

    def gen_auth(self, iv):
        pass

def parse_challenge(raw):
    assert raw[0] == "\x84"  # control code 0x84
    assert raw[1] == "\x10"  # expect 16-byte iv
    logger.info("server challenged with version " + str(raw[-1]))
    iv = raw[2:-1]
    return iv

def parse_packet(raw):
    pass
