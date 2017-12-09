def is_valid (line):
    ws = line.split(' ')
    return len(ws) == len(set(ws))

def num_valid (s):
    ls = s.splitlines()
    return reduce(lambda k, l: k + 1 if is_valid(l) else k, ls, 0)

def is_valid_2 (line):
    ws = [''.join(sorted(w)) for w in line.split(' ')]
    return len(ws) == len(set(ws))

def num_valid_2 (s):
    ls = s.splitlines()
    return reduce(lambda k, l: k + 1 if is_valid_2(l) else k, ls, 0)
