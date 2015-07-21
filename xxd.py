# -*- coding: utf-8 -*-
# ^ because of the unicode spacer used below

from itertools import izip_longest, izip, count

def chunk(it, chunksize):
    rotate = [iter(it)] * chunksize
    for rvs in izip_longest(*rotate, fillvalue=None):
        yield [r for r in rvs if r is not None]

def as_ascii(c, fillvalue=" "):
    return c if 32 <= ord(c) <= 126 else fillvalue

def xxd(raw, groups=4, rowsize=16):
    spacer = " " * 2
    wall = " │ "

    def chunk_hex(chunk):
        return " ".join("%02X" % ord(c) for c in chunk)

    def chunk_chr(chunk):
        return " ".join(as_ascii(c).ljust(2) for c in chunk)

    def do_row(offset, row):
        hexes = spacer.join(chunk_hex(ch) for ch in chunk(row, groups))
        numspacers = (rowsize - 1) / groups
        groupwidth = 3 * rowsize - (numspacers + 1)
        hexwidth = groupwidth + numspacers * len(spacer)
        chars = spacer.join(chunk_chr(ch) for ch in chunk(row, groups))
        asc = "".join(as_ascii(c, fillvalue="·") for c in row)

        rv = ("%08x" % offset + wall + hexes.ljust(hexwidth) + wall + asc +
              "\n" + " "*8 + wall + chars.ljust(hexwidth) + wall)
        return rv

    return "\n".join(do_row(offset, r)
                     for offset, r in izip(count(0, 16), chunk(raw, rowsize)))
