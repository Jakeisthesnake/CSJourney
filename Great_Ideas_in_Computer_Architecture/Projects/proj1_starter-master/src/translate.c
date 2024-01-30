#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include "tables.h"
#include "translate_utils.h"
#include "translate.h"
#include <limits.h>

/* Writes instructions during the assembler's first pass to OUTPUT. The case
   for general instructions has already been completed, but you need to write
   code to translate the li and blt pseudoinstructions. Your pseudoinstruction
   expansions should not have any side effects.

   NAME is the name of the instruction, ARGS is an array of the arguments, and
   NUM_ARGS specifies the number of items in ARGS.

   Error checking for regular instructions are done in pass two. However, for
   pseudoinstructions, you must make sure that ARGS contains the correct number
   of arguments. You do NOT need to check whether the registers / label are
   valid, since that will be checked in part two.

   Also for li:
    - make sure that the number is representable by 32 bits. (Hint: the number
        can be both signed or unsigned).
    - if the immediate can fit in the imm field of an addiu instruction, then
        expand li into a single addiu instruction. Otherwise, expand it into
        a lui-ori pair.

   And for blt:
    - your expansion should use the fewest number of instructions possible.

   MARS has slightly different translation rules for li, and it allows numbers
   larger than the largest 32 bit number to be loaded with li. You should follow
   the above rules if MARS behaves differently.

   Use fprintf() to write. If writing multiple instructions, make sure that
   each instruction is on a different line.

   Returns the number of instructions written (so 0 if there were any errors).
 */
unsigned write_pass_one(FILE *output, const char *name, char **args, int num_args)
{
    if (strcmp(name, "li") == 0)
    {
        /* YOUR CODE HERE */

        if ((num_args > 2) || (num_args < 2))
        {
            return 0;
        }
        int rt = translate_reg(args[0]);
        int32_t arg1;
        if (translate_num(&arg1, args[1], -2147483648, 2147483647) == -1)
        {
            return 0;
        }
        if (arg1 <= INT16_MAX && arg1 >= INT16_MIN)
        {
            char *name_addiu = "addiu";
            char *args_zero = "$0";
            char *args_addiu[] = {args[0], args_zero, args[1]};
            printf("arg0: %s arg1: %s arg2: %s\n", args_addiu[0], args_addiu[1], args_addiu[2]);
            num_args = 3;
            write_inst_string(output, name_addiu, args_addiu, num_args);
            return 1;
        }
        else
        {
            int rt = translate_reg(args[0]);
            long int imm;
            translate_num(&imm, args[1], -2147483648, 2147483647);
            int imm_up = imm >> 16;
            int imm_low = imm & 0xFFFF;
            int rs = 0;

            char *name_lui = "lui";
            char *imm_up_str[32];
            sprintf(imm_up_str, "%d", imm_up);
            char *at = "$at";
            char *args_lui[] = {at, imm_up_str};
            num_args = 2;
            write_inst_string(output, name_lui, args_lui, num_args);

            char *name_ori = "ori";
            char *imm_low_str[32];

            sprintf(imm_low_str, "%d", imm_low);
            char *args_ori[] = {args[0], at, imm_low_str};
            num_args = 3;
            write_inst_string(output, name_ori, args_ori, num_args);

            return 2;
        }
        return 0;
    }
    else if (strcmp(name, "blt") == 0)
    {
        /* YOUR CODE HERE */
        if ((num_args > 3) || (num_args < 3))
        {
            return 0;
        }

        char *at = "$at";
        char *name_slt = "slt";
        char *args_slt[] = {at, args[0], args[1]};
        num_args = 3;
        write_inst_string(output, name_slt, args_slt, num_args);

        char *zero = "$0";
        char *name_bne = "bne";
        char *args_bne[] = {at, zero, args[2]};
        num_args = 3;
        write_inst_string(output, name_bne, args_bne, num_args);
        return 2;
    }
    else
    {
        write_inst_string(output, name, args, num_args);
        return 1;
    }
}

/* Writes the instruction in hexadecimal format to OUTPUT during pass #2.

   NAME is the name of the instruction, ARGS is an array of the arguments, and
   NUM_ARGS specifies the number of items in ARGS.

   The symbol table (SYMTBL) is given for any symbols that need to be resolved
   at this step. If a symbol should be relocated, it should be added to the
   relocation table (RELTBL), and the fields for that symbol should be set to
   all zeros.

   You must perform error checking on all instructions and make sure that their
   arguments are valid. If an instruction is invalid, you should not write
   anything to OUTPUT but simply return -1. MARS may be a useful resource for
   this step.

   Note the use of helper functions. Consider writing your own! If the function
   definition comes afterwards, you must declare it first (see translate.h).

   Returns 0 on success and -1 on error.
 */
int translate_inst(FILE *output, const char *name, char **args, size_t num_args, uint32_t addr,
                   SymbolTable *symtbl, SymbolTable *reltbl)
{
    /* for (int i = 0; i < num_args; i++) {
        printf("arg[%d]: %s\n", i, args[i]);
    } */
    if (strcmp(name, "addu") == 0)
        return write_rtype(0x21, output, args, num_args);
    else if (strcmp(name, "or") == 0)
        return write_rtype(0x25, output, args, num_args);
    else if (strcmp(name, "slt") == 0)
        return write_rtype(0x2a, output, args, num_args);
    else if (strcmp(name, "sltu") == 0)
        return write_rtype(0x2b, output, args, num_args);
    else if (strcmp(name, "sll") == 0)
        return write_shift(0x00, output, args, num_args);
    // YOUR CODE HERE
    else if (strcmp(name, "jr") == 0)
        return write_jr(0x08, output, args, num_args);
    else if (strcmp(name, "addiu") == 0)
        return write_addiu(0x09, output, args, num_args);
    else if (strcmp(name, "ori") == 0)
        return write_ori(0x0d, output, args, num_args);
    else if (strcmp(name, "lui") == 0)
        return write_lui(0x0f, output, args, num_args);
    else if (strcmp(name, "lb") == 0)
        return write_mem(0x20, output, args, num_args);
    else if (strcmp(name, "lbu") == 0)
        return write_mem(0x24, output, args, num_args);
    else if (strcmp(name, "lw") == 0)
        return write_mem(0x23, output, args, num_args);
    else if (strcmp(name, "sb") == 0)
        return write_mem(0x28, output, args, num_args);
    else if (strcmp(name, "sw") == 0)
        return write_mem(0x2b, output, args, num_args);
    else if (strcmp(name, "beq") == 0)
        return write_branch(0x04, output, args, num_args, addr, symtbl);
    else if (strcmp(name, "bne") == 0)
        return write_branch(0x05, output, args, num_args, addr, symtbl);
    else if (strcmp(name, "j") == 0)
        return write_jump(0x02, output, args, num_args, addr, reltbl);
    else if (strcmp(name, "jal") == 0)
        return write_jump(0x03, output, args, num_args, addr, reltbl);
    else if (strcmp(name, "li") == 0)
        return write_shift(0x00, output, args, num_args);
    else if (strcmp(name, "blt") == 0)
        return write_shift(0x00, output, args, num_args);
    else
    { // return -1;
        printf("invalid reg\n");
        return -1;
    }
}

/* A helper function for writing most R-type instructions. You should use
   translate_reg() to parse registers and write_inst_hex() to write to
   OUTPUT. Both are defined in translate_utils.h.

   This function is INCOMPLETE. Complete the implementation below. You will
   find bitwise operations to be the cleanest way to complete this function.
 */
int write_rtype(uint8_t funct, FILE *output, char **args, size_t num_args)
{
    // Perhaps perform some error checking?

    int opcode = 0b000000;
    int shamt = 0b00000;
    int rd = translate_reg(args[0]);
    int rs = translate_reg(args[1]);
    int rt = translate_reg(args[2]);
    if (rd == -1 || rs == -1 || rt == -1)
    {
        return -1;
    }

    uint32_t instruction = 0;
    instruction = (opcode << 26) | (rs << 21) | (rt << 16) | (rd << 11) | (shamt << 6) | funct;
    write_inst_hex(output, instruction);
    return 0;
}

int write_addiu(uint8_t opcode, FILE *output, char **args, size_t num_args)
{
    int rs = translate_reg(args[1]);
    int rt = translate_reg(args[0]);
    if (rs == -1 || rt == -1)
    {
        printf("addiu invalid reg rs: %s, rt: %s\n", args[1], args[0]);
        return -1;
    }
    long int imm;
    if ((translate_num(&imm, args[2], -2147483648, 2147483647)) == -1)
    {
        // printf("min: %ld or %lu, max: %ld or %lu, args[2]: %s\n", LONG_MIN, LONG_MIN, LONG_MAX, LONG_MAX, args[2]);
        printf("translate num error\n");
        return -1;
    };
    uint32_t instruction = 0;
    instruction = (opcode << 26) | (rs << 21) | (rt << 16) | imm;
    write_inst_hex(output, instruction);
    // printf("opcode<<26: %08x | rs<<21: %08x | rt<<16: %08x | imm: %08x | instruction: %08x\n", opcode, rs, rt, imm, instruction);
    // sleep(1);
    return 0;
}

int write_jr(uint8_t funct, FILE *output, char **args, size_t num_args)
{
    int opcode = 0b000000;
    int shamt = 0b00000;
    int rd = 0b00000;
    int rt = 0b00000;
    int rs = translate_reg(args[0]);
    if (rs == -1)
    {
        return -1;
    }

    uint32_t instruction = 0;
    instruction = (opcode << 26) | (rs << 21) | (rt << 16) | (shamt << 6) | funct;
    write_inst_hex(output, instruction);
    return 0;
}

int write_ori(uint8_t opcode, FILE *output, char **args, size_t num_args)
{
    int rs = translate_reg(args[1]);
    int rt = translate_reg(args[0]);
    if (rs == -1 || rt == -1)
    {
        return -1;
    }
    long int imm;
    // printf("args[2]: %s\n", args[2]);
    if ((translate_num(&imm, args[2], SHRT_MIN, UINT16_MAX)) == -1)
    {
        return -1;
    };

    uint32_t instruction = 0;
    instruction = (opcode << 26) | (rs << 21) | (rt << 16) | imm;
    // printf("opcode<<26: %08x | rs<<21: %08x | rt<<16: %08x | offset: %08x | instruction: %08x\n", opcode, rs, rt, imm, instruction);
    write_inst_hex(output, instruction);
    return 0;
}

int write_lui(uint8_t opcode, FILE *output, char **args, size_t num_args)
{
    int rt = translate_reg(args[0]);
    long int imm;
    if ((translate_num(&imm, args[1], SHRT_MIN, SHRT_MAX)) == -1)
    {
        return -1;
    };
    int rs = 0;
    if (rt == -1)
    {
        return -1;
    }

    uint32_t instruction = 0;
    instruction = (opcode << 26) | (rs << 21) | (rt << 16) | imm;
    write_inst_hex(output, instruction);
    printf("lui instrunction: %08x\n", instruction);
    return 0;
}

int write_mem(uint8_t opcode, FILE *output, char **args, size_t num_args)
{
    int rt = translate_reg(args[0]);
    int rs = translate_reg(args[2]);
    if (rs == -1 || rt == -1)
    {
        return -1;
    }
    long int offset;
    if ((translate_num(&offset, args[1], -2147483648, 2147483647)) == -1)
    {
        return -1;
    };

    uint32_t instruction = 0;
    instruction = (opcode << 26) | (rs << 21) | (rt << 16) | (offset & 0x0000ffff);
    // printf("opcode<<26: %08x | rs<<21: %08x | rt<<16: %08x | offset: %08x | instruction: %08x\n", opcode, rs, rt, offset, instruction);
    write_inst_hex(output, instruction);
    return 0;
}

int write_branch(uint8_t opcode, FILE *output, char **args, size_t num_args,
                 uint32_t addr, SymbolTable *symtbl)
{
    int rs = translate_reg(args[0]);
    int rt = translate_reg(args[1]);
    if (rs == -1 || rt == -1)
    {
        return -1;
    }
    char *label_name = args[2];
    int64_t address = get_addr_for_symbol(symtbl, label_name);

    int64_t address_diff = address - addr;
    if (address_diff >= INT16_MAX || address_diff <= INT16_MIN)
    {
        return -1;
    }

    int16_t short_address_diff = (int16_t)address_diff;
    uint32_t instruction = 0;
    instruction = (opcode << 26) | (rs << 21) | (rt << 16) | (short_address_diff & 0x000ffff);
    printf("opcode<<26: %08x | rs<<21: %08x | rt<<16: %08x | short addr diff: %08x instruction: %08x\n", opcode, rs, rt, short_address_diff, instruction);
    printf("addr_diff: %08x | address: %08x | addr: %d, | name: %s\n", address_diff, address, addr, label_name, instruction);
    write_inst_hex(output, instruction);
    return 0;
}

int write_jump(uint8_t opcode, FILE *output, char **args, size_t num_args,
               uint32_t addr, SymbolTable *reltbl)
{

    char *label_name = args[2];
    add_to_table(reltbl, label_name, addr);
    uint32_t instruction = 0;
    instruction = opcode << 26;
    write_inst_hex(output, instruction);
    return 0;
}

/* A helper function for writing shift instructions. You should use
   translate_num() to parse numerical arguments. translate_num() is defined
   in translate_utils.h.

   This function is INCOMPLETE. Complete the implementation below. You will
   find bitwise operations to be the cleanest way to complete this function.
 */
int write_shift(uint8_t funct, FILE *output, char **args, size_t num_args)
{
    // Perhaps perform some error checking?

    int rd = translate_reg(args[0]);
    int rt = translate_reg(args[1]);
    if (rd == -1 || rt == -1)
    {
        return -1;
    }

    long int shamt;
    if ((translate_num(&shamt, args[2], 0, 32)) == -1)
    {
        return -1;
    };

    int opcode = 0b000000;
    int rs = 0b00000;
    uint32_t instruction = 0;
    instruction = (opcode << 26) | (rs << 21) | (rt << 16) | (rd << 11) | (shamt << 6) | funct;
    write_inst_hex(output, instruction);
    return 0;
}