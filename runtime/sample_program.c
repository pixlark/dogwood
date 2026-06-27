/*
this file is meant to roughly model what would be generated from the following source:

struct Link {
    value: int,
    next: &Link,
}

fn main() {
    let link1: &Link = new Link { value: 0, next: nil };
    let link2: &Link = new Link { value: 1, next: 1 };

    let iter: &Link = link2;
    loop {
        if iter == nil {
            break;
        }
        print(iter.value);
        iter = iter.next;
    }
}

*/

#include <runtime.h>
#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <stdbool.h>

// just for testing, wouldn't be generated in the actual code
#ifdef TEST
#include <assert.h>
#define EXPECT_ALLOCATIONS(n) do { collect_garbage(); assert(debug_get_allocation_count() == (n)); } while (0)
#else
#define EXPECT_ALLOCATIONS(...)
#endif

void _builtin_print_int(int n)
{
    printf("%d\n", n);
}

static uint8_t _bitmap__empty[1] = { 0 };

struct Link {
    int value;
    struct Link *next;
};
static uint8_t _bitmap_Link[1] = { 0b10 };

static struct Link *_global_0;

void _function_1(int n) // `print[int]`
{
    // no local pointers, so the code generator can omit all the shadow stack pre/postamble
    _builtin_print_int(n);
}

void _function_0() // `main`
{
    //
    // -- function preamble --
    //

    int                 _0      = 0;
    struct Link        *_1      = NULL;
    struct Link        *_2      = NULL;
    int                 _3      = 0;
    struct Link        *_4      = NULL;
    struct Link        *_5      = NULL;
    struct Link        *_6      = NULL;
    struct Link        *_7_phi  = NULL;
    struct Link        *_8      = NULL;
    struct Link        *_9      = NULL;
    bool                _10     = NULL;
    int                 _11     = 0;
    struct Link        *_12     = NULL;

    void **_locals[9] = {
        (void **)&_1,
        (void **)&_2,
        (void **)&_4,
        (void **)&_5,
        (void **)&_6,
        (void **)&_7_phi,
        (void **)&_8,
        (void **)&_9,
        (void **)&_12,
    };
    shadow_stack_push_frame(9, _locals);

    EXPECT_ALLOCATIONS(0);

    //
    // -- actual function body --
    //

//----------------------------------------------------------------
__0:

    // > let link1: &Link = new Link { value: 0, next: nil };
    _0          = 0;
    _1          = NULL;
    _2          = allocate(sizeof(struct Link), _bitmap_Link);
    _2->value   = _0;
    _2->next    = _1;

    EXPECT_ALLOCATIONS(1);

    // > let link2: &Link = new Link { value: 1, next: link1 };
    _3          = 1;
    _4          = _2;
    _5          = allocate(sizeof(struct Link), _bitmap_Link);
    _5->value   = _3;
    _5->next    = _4;

    // > let iter: &Link = link2;
    _6          = _5;

    // prepare phi
    _7_phi      = _6;

    EXPECT_ALLOCATIONS(2);
    goto __1;

//----------------------------------------------------------------
__1:

    // > loop {

    goto __2;

//----------------------------------------------------------------
__2:

    // phi __1[_6], __4[_12]
    _8          = _7_phi;

    // > if iter == nil {
    _9          = NULL;
    _10         = _8 == _9;

    EXPECT_ALLOCATIONS(2);
    if (_10) { goto __3; } else { goto __4; }

//----------------------------------------------------------------
__3:

    // > break;

    goto __5;

//----------------------------------------------------------------
__4:

    // > print(iter.value);
    _11         = _8->value;
                  _function_1(_11); // void return omitted
    
    // > iter = iter.next;
    _12         = _8->next;

    // prepare phi
    _7_phi      = _12;

    EXPECT_ALLOCATIONS(2);
    goto __1;

//----------------------------------------------------------------
__5:

    EXPECT_ALLOCATIONS(2);
    goto __return;

//----------------------------------------------------------------
__return:

    //
    // -- function postamble --
    //

    shadow_stack_pop_frame();

    EXPECT_ALLOCATIONS(0);
}

int main()
{
    // -- runtime initialization --

    gc_init();

    // push a permanent shadow stack frame that references all global variables
    void **_globals[1] = { (void **)&_global_0 };
    shadow_stack_push_frame(1, _globals);

    // -- execute actual program --
    _function_0();
}
