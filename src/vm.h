#ifndef VM_H
#define VM_H

#include "symbol_table.h"

typedef struct _vm_t
{
    char **program;
    unsigned int line;
    unsigned int column;
    symbol_table_t symbol_table;
} vm_t;

#endif 
