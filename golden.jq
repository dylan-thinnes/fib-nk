# Generates the next in a cycle
def next($n): 
    .[1:] + [add % $n]
;

# Finds a cycle from a seed
def cycle($n):
    . as $initial
|   [   next($n)
    |   while(
            $initial != .;
            next($n)
        )
    ]
|   [$initial] + .
;

# Creates the entire domain
def init_domain($k; $n):
    [
        reduce range($k) as $_ (
            0;
            [range($n) as $_ | .]
        )
    |   leaf_paths
    ]
;

# Find the next cycle, given a [domain, cycles] state tuple
def next_cycle($n):
    (.[0][0] | cycle($n)) as $cycle
|   .[0] -= $cycle
|   .[1] += [$cycle]
;

# Read in k & n
    .[0] as $k
|   .[1] as $n
|   null

# Generate domain & cycles tuple
|   [init_domain($k; $n), []]

# Find all cycles
|   [while(
        (.[0] | length) > 0;
        next_cycle($n)
    )]
|   last
|   next_cycle($n)

# Pretty print cycles - works best with -r flag
|   .[1]
|   .[]
|   [
        [.[] | .[0]] + .[0][0:-1] 
    |   .[] | tostring
    ]
|   join(" ")
