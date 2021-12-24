#!/usr/bin/env python3

def parse_height_map(lines):
    height_map = {}

    i = 0
    for line in lines:
        height_map[i] = [int(x) for x in line.strip()]
        i += 1

    return height_map


def isOk(r, c, hm):
    return r in hm and c >= 0 and c < len(hm[r])


def adjacents(r, c, hm):
    directions = [
        (r - 1, c)    # top
        , (r, c + 1)  # right
        , (r + 1, c)  # bottom
        , (r, c - 1)  # left
        , (r, c)      # current
    ]

    result = []
    for (r1, c1) in directions:
        if (isOk(r1, c1, hm)):
            v = hm[r1][c1]
            result.append((r1, c1, v))

    return result


def min_adjacent(r, c, hm):
    adjs = adjacents(r, c, hm)

    m = 9
    for (_, _, v) in adjs:
        if v < m:
            m = v

    return m


def calculateLows(height_map):
    lows = []
    for (r, cs) in height_map.items():
        for c in range(0, len(cs)):
            curr = height_map[r][c]
            if curr == min_adjacent(r, c, height_map):
                lows.append((r, c, curr))

    return lows


def calculateRisks(height_map):
    lows = calculateLows(height_map)

    print(lows)

    total = 0
    for (_, _, v) in lows:
        risk = v + 1
        total += risk

    return total


def calculateBasins(height_map):
    lows = calculateLows(height_map)

    basins = []
    for (r, c, v) in lows:
        items = _calculateBasins(r, c, height_map)
        basins.append(items)

    return basins


def isVisited(r, c, vm):
    return r in vm and c in vm[r]


def visit(r, c, vm):
    if r in vm:
        vm[r].append(c)
    else:
        vm[r] = [c]


def _calculateBasins(r, c, height_map, visited={}):
    if not isOk(r, c, height_map) or isVisited(r, c, visited):
        return []

    visit(r, c, visited)

    curr = height_map[r][c]
    if curr == 9:
        return []

    adjs = adjacents(r, c, height_map)

    result = [curr]
    for (ar, ac, av) in adjs:
        if not isVisited(ar, ac, visited) and av > curr:
            items = _calculateBasins(ar, ac, height_map, visited)
            for item in items:
                result.append(item)

    return result


def product(ns):
    if len(ns) == 0:
        return 0

    p = 1
    for n in ns:
        p *= n

    return p


def main():
    with open('input.txt', 'r') as f:
        height_map = parse_height_map(f.readlines())

        print(calculateRisks(height_map))

        basin_lengths = sorted(
            map(len, calculateBasins(height_map)), reverse=True)

        print(basin_lengths[0] * basin_lengths[1] * basin_lengths[2])


if __name__ == '__main__':
    main()
