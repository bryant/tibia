from xxd import xxd
import re

def filter_tib_stream(raw):
    tibops = re.compile(
        r"(?:(sendto)\(12, |(recvfrom)(?: resumed> |\(12, ))(.+), " +
        r"(?:NULL|\d+), (?:NULL|\d+), (?:NULL|\d+), (?:NULL|\d+)\) = \d+"
    )
    return [(sr0 + sr1, bs.decode("string_escape").strip("\""))
            for sr0, sr1, bs in tibops.findall(raw)]

def xxd_stream(stream):
    dirs = {"sendto": ">" * 8, "recvfrom": "<" * 8}
    return "\n\n".join(dirs[direction] + "\n" + xxd(dat)
                       for direction, dat in stream if len(dat) > 0)

if __name__ == "__main__":
    from sys import argv
    print xxd_stream(filter_tib_stream(open(argv[1]).read()))
