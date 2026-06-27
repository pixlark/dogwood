#pragma once

#include <stdint.h>
#include <stddef.h>

void gc_init();
void shadow_stack_push_frame(int ptr_count, void ***ptrs);
void shadow_stack_pop_frame();
void *allocate(size_t size, uint8_t *ptr_bitmap);
void collect_garbage();
size_t debug_get_allocation_count();
