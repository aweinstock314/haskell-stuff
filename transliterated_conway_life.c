#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define bool char

double randDouble(void) {
    double r = rand();
    return r / RAND_MAX;
}

void showCell(bool cell) {
    putchar(cell ? '#' : '_');
}

bool* makeRandomBoard(size_t w, size_t h, double density) {
    bool* brd = malloc(w*h*sizeof(bool)); // TODO: maybe consider overflow checking/handling?
    for(size_t i=0; i<w*h; i++) { brd[i] = randDouble() < density; }
    return brd;
}

void dogrid(size_t w, size_t h, void (*eachIndex)(size_t, size_t, void*), void (*eachLine)(void*), void* data) {
    for(size_t y=0; y<h; y++) {
        for(size_t x=0; x<w; x++) {
            (*eachIndex)(x, y, data);
        }
        (*eachLine)(data);
    }
}

struct showBoard_closure {
    bool* brd;
    size_t w, h;
};
void showBoard_eachIndex(size_t x, size_t y, void* data) {
    struct showBoard_closure* sB_c = data;
    showCell(sB_c->brd[y * (sB_c->w) + x]);
}
void showBoard_eachLine(void* data) { putchar('\n'); }
void showBoard(size_t w, size_t h, bool* brd) {
    struct showBoard_closure sB_c = { brd = brd, w = w, h = h };
    dogrid(w, h, &showBoard_eachIndex, &showBoard_eachLine, &sB_c);
}

struct point { size_t x, y; };
struct point* truncIdx(size_t w, size_t h, size_t x, size_t y) {
    if((0 <= x) && (x < w) && (0 <= y) && (y < h)) {
        struct point* ret = malloc(sizeof(struct point));
        ret->x = x; ret->y = y;
        return ret;
    } else { return NULL; }
}
long wrap(long val, long lo, long hi) {
    // use longs to get negative numbers (possible issues with values > approx. 2 billion, but ignore for now)
    if(val < lo) { return hi-1; }
    if(val >= hi) { return lo; }
    return val;
}
struct point* wrapIdx(size_t w, size_t h, size_t x, size_t y) {
    struct point* ret = malloc(sizeof(struct point));
    //printf("w, h = %d, %d\n", w, h);
    ret->x = wrap(x, 0, w); ret->y = wrap(y, 0, h);
    //printf("wrapping (%d, %d) to (%d, %d)\n", x, y, ret->x, ret->y);
    return ret;
}

// this is written all-in-one (rather than as abstract 
//  stages, composed together) because writing the stages 
//   seperately would be much messier in C
int sumOfNeighbors(struct point* (*handleEdges)(size_t, size_t, size_t, size_t), bool* brd,
                    size_t w, size_t h, size_t x, size_t y) {
    int sum = 0;
    for(int dx=-1; dx<=1; dx++) {
        for(int dy=-1; dy<=1; dy++) {
            if((dx == 0) && (dy == 0)) { continue; }
            struct point* adjusted = (*handleEdges)(w, h, x+dx, y+dy); // maybe worry about underflow bugs?
            if(!adjusted) { continue; }
            //printf("sON(%d, %d), (dx %d, dy %d), (adj %d, adj %d)\n", x, y, dx, dy, adjusted->x, adjusted->y);
            //printf("brd[%d * %d + %d (== %d)] = %d\n", adjusted->y, w, adjusted->x, adjusted->y * w + adjusted->x, brd[adjusted->y * w + adjusted->x]);
            if(brd[adjusted->y * w + adjusted->x]) { sum += 1; }
            free(adjusted);
        }
    }
    return sum;
}

bool* evolveBoard(struct point* (*handleEdges)(size_t, size_t, size_t, size_t), bool* brd, size_t w, size_t h) {
    bool* newBrd = malloc(w*h*sizeof(bool));
    for(size_t y=0; y<h; y++) {
        for(size_t x=0; x<w; x++) {
            int neighborsAlive = sumOfNeighbors(handleEdges, brd, w, h, x, y);
            //printf("neighborsAlive(%d, %d) = %d\n", x, y, neighborsAlive);
            bool alive = brd[y * w + x];
            newBrd[y * w + x] = alive ?
                ((neighborsAlive == 2) || (neighborsAlive == 3))
                : (neighborsAlive == 3);
        }
    }
    free(brd);
    return newBrd;
}

void showAutomaton(size_t w, size_t h, size_t* iters,
                    struct point* (*handleEdges)(size_t, size_t, size_t, size_t)) {
    srand(time(NULL));
    bool* brd = makeRandomBoard(w, h, 0.3);
    for(size_t i=0; (!iters) || (i<*iters); i++) {
        showBoard(w, h, brd);
        putchar('\n');
        brd = evolveBoard(handleEdges, brd, w, h);
    }
    free(brd);
}

int main(int argc, char** argv) {
    // hard-code parameters for now
    size_t w=50, h=50;
    size_t* iters = malloc(sizeof(size_t));
    *iters = 100;
    struct point* (*handleEdges)(size_t, size_t, size_t, size_t) = &wrapIdx;
    showAutomaton(w, h, iters, handleEdges);
    free(iters);
    return 0;
}

/*
Bugs found so far:
unsigned overflow (worked around with wrap function)

missing "return sum;" in sumOfNeighbors (added)

accidentally counted self as a neighbor (fixed with "if((dx == 0) && (dy == 0)) { continue; }",
although to be fair to C, that was also a problem in an earlier draft of the Haskell version)

S23/B2 (minor typo)

Surprisingly, there were no memory errors (either leaks or 
corruption) on the first try (although I'm sure that 
there's a latent unsigned overflow bug that'll trigger if 
width * height > either 2 or 4 billion, but if my intuition 
is correct, it'll "just" spray PRNG output over memory if 
triggered, so it's not (directly) exploitable)

Performance:
Compiler versions:
gcc --version | head -n 1
gcc (Debian 4.7.2-5) 4.7.2
ghc --version
The Glorious Glasgow Haskell Compilation System, version 7.4.1

Compile strings:
gcc -std=c99 -O3 transliterated_conway_life.c
ghc -O2 conway_life.hs

Run strings:
time ./a.out
time ./conway_life -i 100
time ./a.out > /dev/null
time ./conway_life -i 100 > /dev/null

The versions that don't have output supressed seem to be (eyeball-averaging the results):
(real .14s, user .09s, sys .02s) for the C version
(real .70s, user .65s, sys .02s) for the Haskell version
The versions with output supressed to /dev/null:
(real .07s, user .07s, sys .000s) for the C version
(real .59s, user .58s, sys .004s) for the Haskell version

Dividing real Haskell time by real C time, Haskell is 5x slower than C (8.4x with output masked)

(Rough microbenchmark numbers, standard caveats, etc...)
*/
