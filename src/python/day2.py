def parse_input(s):
    return map(lambda x: map(int, x.split()), s.splitlines())

def checksum(s):
    return sum(map(lambda l: max(l) - min(l), parse_input(s)))
