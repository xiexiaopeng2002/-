from sympy import *

x = Symbol("x")
y = Symbol("y")
z = Symbol("z")

print(
    solve(
        [
            Eq(x + y + z, 0.2),
            Eq((x / 105.989) + (z / 39.9971), (0.1025 * 12.31 / 1000)),
            Eq(
                (2 * x / 105.989) + (y / 84.007) + (z / 39.9971),
                (0.1025 * 31.15 / 1000),
            ),
        ],
        [x, y, z],
    )
)
