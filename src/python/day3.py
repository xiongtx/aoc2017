## Part 1

# Input: 265149

def next_dir(d, ds):
    return ds[(ds.index(d) + 1) % len(directions)]

def coordinates(n):
    x = 0
    y = 0

    d = 'R'
    ds = ['R', 'U', 'L', 'D']
    total_steps = 1
    steps = 1
    increment_steps = False

    while n > 1:
        if steps == 0:
            d = next_dir(d, ds)
            if increment_steps == True:
                total_steps += 1
            steps = total_steps
            increment_steps = not increment_steps
            continue

        if d == 'R':
            x += 1
        elif d == 'U':
            y += 1
        elif d == 'L':
            x -= 1
        else:
            y -= 1
        steps -= 1
        n -= 1
    return (x, y)

def manhattan(x, y):
    return abs(x) + abs(y)

def steps(n):
    x, y = coordinates(n)
    return manhattan(x, y)

## Part 2

def coordinates_to_index(x, y):
    if y*y >= x*x:
        idx = 4*y*y - y - x
        if y < x:
            return idx - 2*(y - x)
        else:
            return idx
    else:
        idx = 4*x*x - y - x
        if y < x:
            return idx + 2*(y - x)
        else:
            return idx

def index_to_coordinates(idx):
    return coordinates(idx + 1)

def neighbors(x, y):
    return [(i, j)
            for i in [x - 1, x, x + 1]
            for j in [y - 1, y, y + 1]
            if not (x == i and y == j)]

def sum_greater_than(k):
    spiral = [1]
    while spiral[-1] <= k:
        n = len(spiral)
        x, y = index_to_coordinates(n)
        neighs = neighbors(x, y)
        idxs = map(lambda x: coordinates_to_index(x[0], x[1]), neighs)
        v = reduce(lambda x, y: x + y, [spiral[idx] for idx in idxs if 0 <= idx < n])
        spiral.append(v)
    return v
