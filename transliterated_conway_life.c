#ifndef ENABLE_LONG_OPTIONS
#define _XOPEN_SOURCE
#include <unistd.h>
#else
#define _GNU_SOURCE
#include <getopt.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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

long wrap(long val, long lo, long hi) {
    // use longs to get negative numbers (possible issues with values > approx. 2 billion, but ignore for now)
    if(val < lo) { return hi-1; }
    if(val >= hi) { return lo; }
    return val;
}

int truncIdx(size_t w, size_t h, size_t* px, size_t* py) {
    int x = *px, y = *py;
    return ((0 <= x) && (x < w) && (0 <= y) && (y < h));
}
int wrapIdx(size_t w, size_t h, size_t* x, size_t* y) {
    *x = wrap(*x, 0, w);
    *y = wrap(*y, 0, h);
    return 1;
}

// this is written all-in-one (rather than as abstract 
//  stages, composed together) because writing the stages 
//   seperately would be much messier in C
int sumOfNeighbors(int (*handleEdges)(size_t, size_t, size_t*, size_t*), bool* brd,
                    size_t w, size_t h, size_t x, size_t y) {
    int sum = 0;
    size_t tmpx, tmpy;
    for(int dx=-1; dx<=1; dx++) {
        for(int dy=-1; dy<=1; dy++) {
            if((dx == 0) && (dy == 0)) { continue; }
            tmpx = x+dx; tmpy = y+dy;
            if((*handleEdges)(w, h, &tmpx, &tmpy) && brd[tmpy * w + tmpx]) {
                sum += 1;
            }
        }
    }
    return sum;
}

bool* evolveBoard(int (*handleEdges)(size_t, size_t, size_t*, size_t*), bool* brd, size_t w, size_t h) {
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
                    int (*handleEdges)(size_t, size_t, size_t*, size_t*)) {
    bool* brd = makeRandomBoard(w, h, 0.3);
    size_t buf_size = ((w+1)*h+1)*sizeof(char);
    char* output_buf = malloc(buf_size);
    setbuffer(stdout, output_buf, buf_size);
    for(size_t i=0; (!iters) || (i<*iters); i++) {
        showBoard(w, h, brd);
        putchar('\n');
        brd = evolveBoard(handleEdges, brd, w, h);
    }
    fflush(stdout);
    free(output_buf);
    free(brd);
}

void show_usage(const char* progname) {
#ifndef ENABLE_LONG_OPTIONS
#define SHOW_OPT(short_opt, long_opt, placeholder, descr)\
    printf("  -%-14s  %-s\n", short_opt " " placeholder, descr)
#else
#define SHOW_OPT(short_opt, long_opt, placeholder, descr)\
    printf("  -%-14s  --%-25s  %-s\n", short_opt " " placeholder, long_opt placeholder, descr)
#endif
    printf("%s:\n", progname);
    SHOW_OPT("w", "width=", "WIDTH", "Width of the board");
    SHOW_OPT("h", "height=", "HEIGHT", "Height of the board");
    SHOW_OPT("i", "iterations=", "ITERS", "Number of iterations");
    SHOW_OPT("s", "seed=", "SEED", "Seed for the random number generator");
    SHOW_OPT("e", "edgehandling=", "[wrap|trunc]", "How to handle cells at the edge");
    SHOW_OPT("?", "help", "", "Show this output");
    puts("");
#undef SHOW_OPT
}

int parse_edgehandling(const char* str, int (**handleEdges)(size_t, size_t, size_t*, size_t*)) {
    if(!strcmp(str, "wrap")) { *handleEdges = &wrapIdx; return 0; }
    if(!strcmp(str, "trunc")) { *handleEdges = &truncIdx; return 0; }
    return 1;
}

const char short_options[] = "w:h:i:s:e:?";

#ifndef ENABLE_LONG_OPTIONS
#define GETOPT_INVOKATION getopt(argc, argv, short_options)
#else
const struct option long_options[] = {
    {"width",         required_argument, NULL, 'w'},
    {"height",        required_argument, NULL, 'h'},
    {"iterations",    required_argument, NULL, 'i'},
    {"seed",          required_argument, NULL, 's'},
    {"edge_handling", required_argument, NULL, 'e'},
    {"help",          no_argument,       NULL, '?'}
};
#define GETOPT_INVOKATION getopt_long(argc, argv, short_options, long_options, NULL)
#endif

int main(int argc, char** argv) {
    size_t w=50, h=50, *iters=NULL;
    int (*handleEdges)(size_t, size_t, size_t*, size_t*) = &wrapIdx;
    unsigned int random_seed = time(NULL);
    int c;
    while(c=GETOPT_INVOKATION, (c != -1)) {
        if(c == '?') { show_usage(argv[0]); return 1; }
        else switch(c) {
            case 'w': w = atol(optarg); break;
            case 'h': h = atol(optarg); break;
            case 'i': free(iters); iters = malloc(sizeof(size_t)); *iters = atol(optarg); break;
            case 's': random_seed = atoi(optarg); break;
            case 'e': if(parse_edgehandling(optarg, &handleEdges)) {
                printf("Invalid edge handling mode: %s\n", optarg);
                return 1;
            } break;
            default: printf("Unrecognized option '%c'\n", c); return 1;
        }
    }
    srand(random_seed);
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
