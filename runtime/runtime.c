/*
Basic outline of the language runtime, mostly a garbage collector.

One major issue with this garbage collector design is that it requires the collector to hold a pointer into the stack
for every local reference variable.

This prevents the C optimizer from keeping those variables in registers, which is pretty bad.

One way to mitigate this would be to detect when a function never reassigns a reference variable, and issue some sort of
"void keep_alive(void *ptr)" command to tell the runtime that this reference should be valid for as long as the function
is running (followed by a "void undo_keep_alive(void *ptr)" before the function returns).

My guess would be that for the vast majority of cases, this optimization would apply and allow the variable to be moved
into a register.

I think it could be implemented just by having a global set of keepalive pointers, and then the mark-and-sweep algorithm
traverses that set in addition to the stack roots.
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>

#ifndef ENABLE_TESTS
#define printf(...)
#define fprintf(...)
#endif

#define FORCEINLINE __attribute__((always_inline)) inline

static uint64_t hash_ptr(void *ptr);
static bool compare_ptr(void *ptr1, void *ptr2);

#define NAME ptr_ptr_map
#define KEY_TY void *
#define VAL_TY void *
#define HASH_FN hash_ptr
#define CMPR_FN compare_ptr
#include "verstable.h"

static uint64_t hash_ptr(void *ptr)
{
    return vt_hash_integer((uint64_t)ptr);
}

static bool compare_ptr(void *ptr1, void *ptr2)
{
    return vt_cmpr_integer((uint64_t)ptr1, (uint64_t)ptr2);
}

typedef struct ShadowStackFrame
{
    int ptr_count;
    void ***ptrs;
    struct ShadowStackFrame *prev;
} ShadowStackFrame;

static ShadowStackFrame *shadow_stack = NULL;

// typedef struct Allocation
// {
//     void *ptr;
//     uint32_t size;
//     struct Allocation *next;
// } Allocation;

// static Allocation *allocation_list = NULL;

static ptr_ptr_map allocation_map;

void gc_init()
{
    ptr_ptr_map_init(&allocation_map);
}

void shadow_stack_push_frame(int ptr_count, void ***ptrs)
{
    ShadowStackFrame *frame = malloc(sizeof(ShadowStackFrame));
    frame->ptr_count = ptr_count;
    frame->ptrs = ptrs;
    frame->prev = shadow_stack;

    shadow_stack = frame;
}

void shadow_stack_pop_frame()
{
    if (shadow_stack == NULL) {
        fprintf(stderr, "\n\nRUNTIME ERROR: TRIED TO POP THE SHADOW STACK WHILE EMPTY\n\n");
        abort();
    }

    ShadowStackFrame *prev = shadow_stack->prev;
    free(shadow_stack);
    shadow_stack = prev;
}

typedef struct
{
    uint32_t size; // top bit is used for mark and sweep
    uint8_t ptr_bitmap[0];
} AllocationHeader;

static FORCEINLINE bool header_marked(AllocationHeader *header)
{
    return header->size >> 31;
}

static FORCEINLINE uint32_t header_size(AllocationHeader *header)
{
    return header->size & 0x7fffffff;
}

static FORCEINLINE uint32_t bitmap_size_for_data_size(uint32_t data_size)
{
    // each bit in the bitmap represents one 8-byte word of the data structure
    // this works because pointer reads and writes have to be aligned
    size_t minimum_size = (data_size + 64 - 1) / 64;
    // the allocation header needs to be in total size a multiple of the word size,
    // so that the actual allocation data is always aligned properly
    size_t total_size = sizeof(uint32_t) + minimum_size;
    total_size = (1 + (total_size - 1) / 8) * 8;
    return total_size - sizeof(uint32_t);
}

static AllocationHeader *data_to_header(void *data)
{
    ptr_ptr_map_itr itr = ptr_ptr_map_get(&allocation_map, data);
    if (ptr_ptr_map_is_end(itr)) {
        fprintf(stderr, "\n\nRUNTIME ERROR: data_to_header PASSED BAD POINTER %p\n\n", data);
        abort();
    }
    AllocationHeader *header = itr.data->val;
    return header;
}

static void *header_to_data(AllocationHeader *header)
{
    uint8_t *ptr = (uint8_t *)header;
    uint32_t bitmap_size = bitmap_size_for_data_size(header_size(header));
    return (void *)(ptr + sizeof(AllocationHeader) + bitmap_size);
}

void *allocate(size_t size, uint8_t *ptr_bitmap)
{
    size_t bitmap_size = bitmap_size_for_data_size(size);
    size_t header_size = sizeof(AllocationHeader) + bitmap_size;
    size_t total_size = size + header_size;
    AllocationHeader *header = malloc(total_size);
    header->size = size;
    memcpy(header->ptr_bitmap, ptr_bitmap, bitmap_size);
    void *data = header_to_data(header);

    // update allocation list
    // Allocation *allocation = malloc(sizeof(Allocation));
    // allocation->size = size;
    // allocation->ptr = data;
    // allocation->next = allocation_list;
    // allocation_list = allocation;

    // update allocation map
    ptr_ptr_map_insert(&allocation_map, data, header);

    return data;
}

static FORCEINLINE bool is_marked(AllocationHeader *header)
{
    return header->size >> 31;
}

static FORCEINLINE void mark_allocation(AllocationHeader *header)
{
    fprintf(stderr, "marking allocation: %p\n", header_to_data(header));
    header->size |= 0x80000000;
}

static FORCEINLINE void unmark_allocation(AllocationHeader *header)
{
    header->size &= 0x7fffffff;
}

static void unmark_all_allocations()
{
    for (ptr_ptr_map_itr itr = ptr_ptr_map_first(&allocation_map);
         !ptr_ptr_map_is_end(itr);
         itr = ptr_ptr_map_next(itr)) {
        AllocationHeader *header = itr.data->val;
        fprintf(stderr, "unmarking %p\n", itr.data->key);
        unmark_allocation(header);
    }
}

static void mark_from_root(void *root)
{
    // mark this allocation
    AllocationHeader *header = data_to_header(root);
    if (is_marked(header)) {
        // if it's already marked, then we've traversed through a cycle
        return;
    }
    mark_allocation(header);

    // go recursively through all the allocations referenced from this allocation
    uint32_t length = bitmap_size_for_data_size(header_size(header));
    uint8_t *bitmap = header->ptr_bitmap;
    for (int i = 0; i < length; i++) {
        uint8_t chunk = bitmap[i];
        for (int j = 0; j < 8; j++) {
            if (chunk & (1 << j)) {
                int offset = j + i * 8;
                void *ptr_contents = (void *)((uint64_t *)root)[offset];
                if (ptr_contents != NULL) {
                    mark_from_root(ptr_contents);
                }
            }
        }
    }
}

static FORCEINLINE void _collect_garbage(bool dry_run)
{
    // unmark everything
    unmark_all_allocations();

    // mark all reachable objects
    ShadowStackFrame *frame = shadow_stack;
    while (frame != NULL) {
        for (int i = 0; i < frame->ptr_count; i++) {
            // root_ptr points to the part of the stack that contains the pointer we're interested in
            void **root_ptr = frame->ptrs[i];
            void *root = *root_ptr;
            if (root != NULL) {
                fprintf(stderr, "found root: %p\n", root);
                mark_from_root(root);
            }
        }
        frame = frame->prev;
    }

    // collect unreachable objects
    for (ptr_ptr_map_itr itr = ptr_ptr_map_first(&allocation_map);
         !ptr_ptr_map_is_end(itr);) {
        AllocationHeader *header = itr.data->val;
        if (is_marked(header)) {
            itr = ptr_ptr_map_next(itr);
            continue;
        }

        // unmarked means unreachable, so collect
        void *data = itr.data->key;
        itr = ptr_ptr_map_erase_itr(&allocation_map, itr);
        if (!dry_run) {
            free(header);
        }
    }
}

static void collect_garbage_dry_run()
{
    _collect_garbage(true);
}

void collect_garbage()
{
    _collect_garbage(false);
}

size_t debug_get_allocation_count()
{
    return ptr_ptr_map_size(&allocation_map);
}

#ifdef ENABLE_TESTS

#include <assert.h>
#include <stdbool.h>

void test_collect()
{
    fprintf(stderr, "==== test_collect ====\n");

    static uint32_t empty_bitmap = 0;

    typedef struct {
        bool *x;
    } Bar;
    static uint32_t bar_bitmap = 0b1;

    typedef struct {
        bool a;
        int *b;
        Bar *c;
    } Foo;
    static uint32_t foo_bitmap = 0b110;

    // this is what the generated header of a function would look like
    // all the variables are declared up front, and then the pointer variables are
    // passed to the shadow stack so that the collector can introspect them

    Foo *foo = NULL;
    Bar *bar = NULL;
    int *n = NULL;
    bool *b = NULL;

    void **shadow_stack_ptrs[4] = { (void **)&foo, (void **)&bar, (void **)&n, (void **)&b };
    shadow_stack_push_frame(4, shadow_stack_ptrs);

    // now we can actually perform the function body

    foo = allocate(sizeof(Foo), (uint8_t *)&foo_bitmap);
    bar = allocate(sizeof(Bar), (uint8_t *)&bar_bitmap);
    n = allocate(sizeof(int), (uint8_t *)&empty_bitmap);
    b = allocate(sizeof(bool), (uint8_t *)&empty_bitmap);

    fprintf(stderr, "foo: %p\nbar: %p\nn: %p\nb: %p\n", foo, bar, n, b);

    foo->a = true;
    foo->b = n;
    foo->c = bar;
    bar->x = b;
    *n = 5;
    *b = true;

    collect_garbage_dry_run();
    assert(is_marked(data_to_header(foo)));
    assert(is_marked(data_to_header(bar)));
    assert(is_marked(data_to_header(n)));
    assert(is_marked(data_to_header(b)));

    // now if we remove our access to foo
    AllocationHeader *_foo = data_to_header(foo);
    foo = NULL;
    collect_garbage_dry_run();
    assert(!is_marked(_foo));
    assert(is_marked(data_to_header(bar)));
    assert(is_marked(data_to_header(n)));
    assert(is_marked(data_to_header(b)));    

    // if we remove our direct access to b, it shouldn't affect anything
    // because it's still accessible through bar
    // but when we remove our direct access to n, it should be unmarked
    // because we don't have access to foo anymore
    AllocationHeader *_n = data_to_header(n);
    AllocationHeader *_b = data_to_header(b);
    n = NULL;
    b = NULL;
    collect_garbage_dry_run();
    assert(!is_marked(_foo));
    assert(is_marked(data_to_header(bar)));
    assert(!is_marked(_n));
    assert(is_marked(_b));

    // Now if we make everything null and do a real collection, there should
    // be no allocations left
    assert(ptr_ptr_map_size(&allocation_map) == 2);
    bar = NULL;
    _collect_garbage(false);
    assert(ptr_ptr_map_size(&allocation_map) == 0);

    // and at the foot of the function, before it returns, it will always pop
    // the shadow frame from the stack (otherwise the collector would hold)
    // dangling pointers into the former stack frame
    shadow_stack_pop_frame();
    assert(shadow_stack == NULL);
}

void test_cyclic_collect()
{
    fprintf(stderr, "==== test_cyclic_collect ====\n");

    static uint32_t empty_bitmap = 0;

    struct Bar;
    struct Foo;

    typedef struct Bar {
        struct Foo *foo;
    } Bar;
    static uint32_t bar_bitmap = 1;

    typedef struct Foo {
        struct Bar *bar;
    } Foo;
    static uint32_t foo_bitmap = 1;

    Foo *foo;
    Bar *bar;
    void **shadow_stack_ptrs[2] = { (void**)&foo, (void**)&bar };
    shadow_stack_push_frame(2, shadow_stack_ptrs);

    foo = allocate(sizeof(Foo), (uint8_t *)&foo_bitmap);
    bar = allocate(sizeof(Bar), (uint8_t *)&bar_bitmap);
    foo->bar = bar;
    bar->foo = foo;

    collect_garbage();
    assert(is_marked(data_to_header(foo)));
    assert(is_marked(data_to_header(bar)));

    foo = NULL;
    collect_garbage();
    assert(ptr_ptr_map_size(&allocation_map) == 2); // foo kept alive by bar

    bar = NULL;
    collect_garbage();
    assert(ptr_ptr_map_size(&allocation_map) == 0);

    shadow_stack_pop_frame();
    assert(shadow_stack == NULL);
}

int main()
{
    gc_init();
    test_collect();
    test_cyclic_collect();
    fprintf(stderr, "tests complete!\n");
}

#endif
