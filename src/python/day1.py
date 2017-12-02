def captcha(s):
    total = 0
    n = len(s)
    for i in range(n):
        k = int(s[i])
        if k == int(s[(i + 1) % n]):
            total += k
    return total

def captcha_halfway(s):
    total = 0
    n = len(s)
    for i in range(n):
        k = int(s[i])
        h = int(s[(i + (n / 2)) % n])
        if k == h:
            total += k
    return total
