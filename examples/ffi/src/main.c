#include "types.h"

struct Outer returns() {
    struct Outer outer;
    outer.a = 1;

    struct Inner inner;
    inner.a = 2;
    inner.b = 3;
    inner.c = 4;
    inner.d = 5;
    outer.b = inner;

    outer.c = 6;

    return outer;
}

unsigned int takes(struct Outer outer) {
    if (outer.a != 1) return 10;
    if (outer.b.a != 2) return 20;
    if (outer.b.b != 3) return 30;
    if (outer.b.c != 4) return 40;
    if (outer.b.d != 5) return 50;
    if (outer.c != 6) return 60;

    return 100;
}
