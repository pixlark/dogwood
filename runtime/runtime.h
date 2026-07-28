#pragma once

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

void gc_init();
void shadow_stack_push_frame(int ptr_count, void ***ptrs);
void shadow_stack_pop_frame();
void *allocate(size_t size, uint8_t *ptr_bitmap);
void collect_garbage();
size_t debug_get_allocation_count();

typedef enum {
    TYPE_ANY,
    TYPE_VOID,
    TYPE_BOOL,
    TYPE_INT,
    TYPE_FN,
    TYPE_REFERENCE,
} TypeTag;

typedef struct Type {
    TypeTag tag;
    size_t inner_count;
    struct Type *inner;
} Type;

typedef struct {
    Type type;
    void *value;
} Box;

Type make_type_any();
Type make_type_void();
Type make_type_bool();
Type make_type_int();
Type make_type_fn(Type ret, size_t arg_count, Type *args);
Type make_type_reference(Type inner);
Box box_value(void *value, Type type);

uint8_t builtin_print(Box box);
