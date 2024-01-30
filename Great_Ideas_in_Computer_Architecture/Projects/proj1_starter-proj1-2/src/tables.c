
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "utils.h"
#include "tables.h"

#define WORD_SIZE 4
const int SYMTBL_NON_UNIQUE = 0;
const int SYMTBL_UNIQUE_NAME = 1;

/*******************************
 * Helper Functions
 *******************************/

void allocation_failed() {
    write_to_log("Error: allocation failed\n");
    exit(1);
}

void addr_alignment_incorrect() {
    write_to_log("Error: address is not a multiple of 4.\n");
}

void name_already_exists(const char* name) {
    write_to_log("Error: name '%s' already exists in table.\n", name);
}

void write_symbol(FILE* output, uint32_t addr, const char* name) {
    fprintf(output, "%u\t%s\n", addr, name);
}

/*******************************
 * Symbol Table Functions
 *******************************/

/* Creates a new SymbolTable containg 0 elements and returns a pointer to that
   table. Multiple SymbolTables may exist at the same time. 
   If memory allocation fails, you should call allocation_failed(). 
   Mode will be either SYMTBL_NON_UNIQUE or SYMTBL_UNIQUE_NAME. You will need
   to store this value for use during add_to_table().
 */
SymbolTable* create_table(int mode) {
    /* YOUR CODE HERE */
    SymbolTable* table = malloc(sizeof(SymbolTable));
    if(table == NULL){
        allocation_failed();
    }
    else if(mode == SYMTBL_NON_UNIQUE){
        table->mode = SYMTBL_NON_UNIQUE;
    }
    else if(mode == SYMTBL_UNIQUE_NAME){
        table->mode = SYMTBL_UNIQUE_NAME;
    }
    else {
        fprintf(stdout, "mode illdefined\n");
        exit(1);
    }
    table->len = 0;
    table->tbl = NULL;
    return table;
}

/* Frees the given SymbolTable and all associated memory. */
void free_table(SymbolTable* table) {
    /* YOUR CODE HERE */
    Symbol * tmp;
    Symbol * head_symbol = table->tbl;
    while (head_symbol != NULL){
        tmp = head_symbol;
        head_symbol = head_symbol->next;
        free(tmp->name);
        free(tmp);
    }
    free(table);
}

/* Adds a new symbol and its address to the SymbolTable pointed to by TABLE. 
   ADDR is given as the byte offset from the first instruction. The SymbolTable
   must be able to resize itself as more elements are added. 

   Note that NAME may point to a temporary array, so it is not safe to simply
   store the NAME pointer. You must store a copy of the given string.

   If ADDR is not word-aligned, you should call addr_alignment_incorrect() and
   return -1. If the table's mode is SYMTBL_UNIQUE_NAME and NAME already exists 
   in the table, you should call name_already_exists() and return -1. If memory
   allocation fails, you should call allocation_failed(). 

   Otherwise, you should store the symbol name and address and return 0.
 */
int add_to_table(SymbolTable* table, const char* name, uint32_t addr) {
    /* YOUR CODE HERE */
    if(addr % WORD_SIZE){
        addr_alignment_incorrect();
        return -1;
    }
    Symbol* new_tbl = malloc(sizeof(Symbol));
    if (new_tbl == NULL){
        allocation_failed();
        return -1;
    }
    if (table->mode && (get_addr_for_symbol(table, name) != -1)){
        name_already_exists(name);
        return -1;
    }
    new_tbl->name = (char *) malloc(strlen(name) + 1);
    strcpy(new_tbl->name, name);
    new_tbl->addr = addr;
    new_tbl->next = table->tbl;
    table->tbl = new_tbl;
    table->len++;
    
    
    return 0;
}

/* Returns the address (byte offset) of the given symbol. If a symbol with name
   NAME is not present in TABLE, return -1.
 */
int64_t get_addr_for_symbol(SymbolTable* table, const char* name) {
    /* YOUR CODE HERE */
    Symbol* next_sym = table->tbl;
    for (int i = 0; i < table->len; i++){
        if(strcmp(next_sym->name, name) == 0){
            return next_sym->addr;
        }
        next_sym = next_sym->next;
    }
    return -1;   
}

/* Writes the SymbolTable TABLE to OUTPUT. You should use write_symbol() to
   perform the write. Do not print any additional whitespace or characters.
 */
void write_table(SymbolTable* table, FILE* output) {
    /* YOUR CODE HERE */
    Symbol* next_sym = table->tbl;
    for (int i = 0; i < table->len; i++){
        write_symbol(output, next_sym->addr, next_sym->name);
        next_sym = next_sym->next;
    }
}
