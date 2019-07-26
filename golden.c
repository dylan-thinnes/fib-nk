#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// STRUCTURES

// Logs all cycles for a given n & k
// Under the hood is a dynamic array w/ some metadata
typedef struct CyclesLog {
    struct Cycle** cycles;
    int capacity;
    int usage;

    // k is how many terms to sum up
    int k;
    // n is modulo to put on each term
    int n;
} CyclesLog;

// Records a single cycle that eventually loops on itself
typedef struct Cycle {
    int length;
    struct Node* cycle;
} Cycle;

// Tracks which seeds have already been traversed
typedef struct History {
    int pos;
    int* found;
    int length;
} History;

// Iterates along until we discover a cycle
typedef struct Path {
    struct Node* start;
    
    // Metadata to step forward (refer to CyclesLog)
    int k;
    int n;

    // Seed & state to check if we've found a loop
    int seed;
    int state;
} Path;

// Basic linked list node - used by Path & Cycle
// TODO: Build linked list on top of nodes, to allow append & prepend w/o
// pointer reassignment
typedef struct Node {
    int value;
    struct Node* next;
} Node;

// MEMORY MANAGEMENT

// Methods for freeing different structs
void free_node (Node* head) {
    Node* next;
    while (head) {
        next = head->next;
        free(head);
        head = next;
    }
}

void free_hist (History* hist) {
    free(hist->found);
    free(hist);
}

void free_cycle (Cycle* cycle) {
    free_node(cycle->cycle);
    free(cycle);
}

void free_log (CyclesLog* log) {
    int ii = log->usage;
    while (ii--) {
#ifdef DEBUG
        printf("Freeing #%d: %u from log\n", ii, log->cycles[ii]);
#endif
        free_cycle(log->cycles[ii]);
    }
    free(log->cycles);
    free(log);
}

// UTILS

// Integer exponentiation
int intpow (int n, int e) {
    int res = 1;
    while (e--) res *= n;
    return res;
}

// LINKED LIST

// Append & reversal of linked list
Node* append (int val, Node* rest) {
    Node* new = malloc(sizeof(Node));
    new->value = val;
    new->next  = rest;

    return new;
}

Node* reverse (Node* orig) {
    Node* rev = NULL;
    while (orig) {
        rev = append(orig->value, rev);
        orig = orig->next;
    }

    return rev;
}

// LOG MANIPULATION

// Initialize a log to size 16
CyclesLog* init_log (int k, int n) {
    CyclesLog* log = malloc(sizeof(CyclesLog));
    log->capacity = 16;
    log->cycles = calloc(log->capacity, sizeof(Cycle*));
    log->usage = 0;
    log->n = n;
    log->k = k;
}

// Double a log's size, copy over old data
void expand_log (CyclesLog* log) {
#ifdef DEBUG
    printf("Expanding log up from %d\n", log->capacity);
#endif
    log->capacity *= 2;
    Cycle** old_data = log->cycles;
    log->cycles = calloc(log->capacity, sizeof(Cycle*));
    memcpy(log->cycles, old_data, sizeof(Cycle*) * log->capacity / 2);
    free(old_data);
}

// Add a cycle to the log
void record_cycle (CyclesLog* log, Cycle* cycle) {
    if (log->capacity == log->usage) {
        expand_log(log);
    }
    
#ifdef DEBUG
    printf("Adding %u to log\n", cycle);
#endif
    log->cycles[log->usage] = cycle;
    log->usage++;
}

// PATHS & CYCLES

// Find the next value for a path by looking at previous k terms and modding
// them by n
int next_value (Path* path) {
    Node* head = path->start;
    int total = 0;

    int lookbehind = path->k;
    int mod = path->n;

    while (lookbehind--) {
        total += head->value % mod;
        total %= mod;
        head = head->next;
    }

    return total;
}

// Update the state, which can be marked in history & lets us check when we've
// looped back to the path's original seed
void update_state (Path* path, int new_value) {
    int lookbehind = path->k;
    int mod = path->n;

    path->state = new_value * intpow(mod, lookbehind - 1) 
                + path->state / mod;
}

// Use a new value to append to path & update the state
void step (Path* path, int new_value) {
    update_state(path, new_value);
    path->start = append(new_value, path->start);
}

// Initialize a path w/ given integer seed, k terms to look at, n to modulo by
Path* init_path (int seed, int k, int n) {
    Path* path = malloc(sizeof(Path));
    path->k = k;
    path->n = n;

    path->state = 0;
    path->seed = seed;

    path->start = NULL;
    for (int ii = 0; ii < k; ii++) {
        step(path, seed % n);
        seed /= n;
    }

    return path;
}

// Using hist to mark all new states, find the cycle emanating from given seed
Cycle* find_cycle (int seed, int k, int n, History* hist) {
    Path* path = init_path(seed, k, n);
    hist->found[path->state] = 1;

    // As long as the path's state hasn't looped back to the seed, continue
    // stepping
    do {
        step(path, next_value(path));
        hist->found[path->state] = 1;
    } while (path->state != path->seed);

    Cycle* cycle = malloc(sizeof(Cycle));
    // Reverse the path so that it starts with the seed
    cycle->cycle = reverse(path->start->next);
    cycle->length = path->k;

    free_node(path->start);
    free(path);

    return cycle;
}

// HISTORY

// Initialize a space large enough to mark each possible state in the search
// space for given k & n
History* init_hist (int k, int n) {
    History* hist = malloc(sizeof(History));
    hist->pos = 0;
    hist->found = calloc(sizeof(int), intpow(n, k));
    hist->length = intpow(n, k);

    return hist;
}

// Find the next untested seed in the history
int next_seed (History* hist) {
    while (hist->pos < hist->length && hist->found[hist->pos] == 1) {
        hist->pos++;
    }
    return hist->pos;
}

// LOGGING

void print_hist (History* hist) {
    for (int ii = 0; ii < hist->length; ii++) {
        printf("%d", hist->found[ii]);
    }
    printf("\n");
}

void pretty_log (CyclesLog* log) {
    int ii = 0;
    Node* curr;
    while (ii < log->usage) {
        printf("Cycle #%d:\n", ii);

        curr = log->cycles[ii]->cycle;
        int length = 0;
        while (curr != NULL) {
            printf("%d ", curr->value);
            curr = curr->next;
            length++;
        }
        printf("\n  length: %d\n", length - log->k + 1);

        ii++;
    }
    printf("%d cycles found for k = %d, n = %d\n", ii, log->k, log->n);
}

void print_log (CyclesLog* log) {
    int ii = 0;
    Node* curr;
    while (ii < log->usage) {
        curr = log->cycles[ii]->cycle;
        while (curr != NULL) {
            printf("%d", curr->value);
            curr = curr->next;
            if (curr != NULL) printf(" ");
        }

        ii++;
        printf("\n");
    }
}

// HEAVY LIFTING

void find_all_cycles (CyclesLog* log) {
    History* hist = init_hist(log->k, log->n);

    Cycle* cycle = NULL;

    int seed = next_seed(hist);
    while (seed < hist->length) {
        cycle = find_cycle(seed, log->k, log->n, hist);
        record_cycle(log, cycle);
        seed = next_seed(hist);
    }

    free_hist(hist);
}

int main (int argc, char** argv) {
    int k = strtol(argv[1], NULL, 10);
    int n = strtol(argv[2], NULL, 10);

    CyclesLog* log = init_log(k, n);
    find_all_cycles(log);
    print_log(log);
    free_log(log);

    return 0;
}
