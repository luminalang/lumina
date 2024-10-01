struct Inner {
    unsigned int  a;
    unsigned char b;
    unsigned long long c;
    unsigned short d;
};

struct Outer {
    unsigned short a;
    struct Inner b;
    unsigned char c;
};


struct Outer returns();
unsigned int takes(struct Outer outer);
