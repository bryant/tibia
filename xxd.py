from itertools import izip_longest

def chunk(it, chunksize):
    rotate = [iter(it)] * chunksize
    for rvs in izip_longest(*rotate, fillvalue=None):
        yield [r for r in rvs if r is not None]

def as_ascii(c, fillvalue=" "):
    return c if 32 <= ord(c) <= 126 else fillvalue

def xxd(raw, groups=4, perrow=16):
    spacer = " " * 3
    def chunk_hex(chunk):
        return " ".join("%02X" % ord(c) for c in chunk)

    def chunk_chr(chunk):
        return " ".join(as_ascii(c).ljust(2) for c in chunk)

    def do_row(row):
        hexes = spacer.join(chunk_hex(ch) for ch in chunk(row, groups))
        chars = spacer.join(chunk_chr(ch) for ch in chunk(row, groups))
        asc = "".join(as_ascii(c, fillvalue=".") for c in row)
        return hexes + spacer + asc + "\n" + chars

    return "\n".join(do_row(r) for r in chunk(raw, perrow))
