#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "src/utils.h"
#include "src/tables.h"
#include "src/translate_utils.h"
#include "src/translate.h"
#include "assembler.h"

const int MAX_ARGS = 3;
const int BUF_SIZE = 1024;
const char* IGNORE_CHARS = " \f\n\r\t\v,()";

/*******************************
 * Helper Functions
 *******************************/

/* You should not be calling this function yourself. */
static void raise_label_error(uint32_t input_line, const char* label) {
    write_to_log("Error - invalid label at line %d: %s\n", input_line, label);
}

/* Call this function if more than MAX_ARGS arguments are found while parsing
   arguments.

   INPUT_LINE is which line of the input file that the error occurred in. Note
   that the first line is line 1 and that empty lines are included in the count.

   EXTRA_ARG should contain the first extra argument encountered.
 */
static void raise_extra_arg_error(uint32_t input_line, const char* extra_arg) {
    write_to_log("Error - extra argument at line %d: %s\n", input_line, extra_arg);
}

/* You should call this function if write_pass_one() or translate_inst() 
   returns -1. 
 
   INPUT_LINE is which line of the input file that the error occurred in. Note
   that the first line is line 1 and that empty lines are included in the count.
 */
static void raise_inst_error(uint32_t input_line, const char* name, char** args,
    int num_args) {
    
    write_to_log("Error - invalid instruction at line %d: ", input_line);
    log_inst(name, args, num_args);
}

/* Truncates the string at the first occurrence of the '#' character. */
static void skip_comment(char* str) {
    char* comment_start = strchr(str, '#');
    if (comment_start) {
        *comment_start = '\0';
    }
}

/* Reads STR and determines whether it is a label (ends in ':'), and if so,
   whether it is a valid label, and then tries to add it to the symbol table.

   INPUT_LINE is which line of the input file we are currently processing. Note
   that the first line is line 1 and that empty lines are included in this count.

   BYTE_OFFSET is the offset of the NEXT instruction (should it exist). 

   Four scenarios can happen:
    1. STR is not a label (does not end in ':'). Returns 0.
    2. STR ends in ':', but is not a valid label. Returns -1.
    3a. STR ends in ':' and is a valid label. Addition to symbol table fails.
        Returns -1.
    3b. STR ends in ':' and is a valid label. Addition to symbol table succeeds.
        Returns 1.
 */
static int add_if_label(uint32_t input_line, char* str, uint32_t byte_offset,
    SymbolTable* symtbl) {
    
    size_t len = strlen(str);
    if (str[len - 1] == ':') {
        str[len - 1] = '\0';
        if (is_valid_label(str)) {
            if (add_to_table(symtbl, str, byte_offset) == 0) {
                printf("inputline: %d, name: %s, offset: %d\n", input_line, str, byte_offset);
                return 1;
            } else {
                return -1;
            }
        } else {
            raise_label_error(input_line, str);
            return -1;
        }
    } else {
        return 0;
    }
}

/*******************************
 * Implement the Following
 *******************************/

/* First pass of the assembler. You should implement pass_two() first.

   This function should read each line, strip all comments, scan for labels,
   and pass instructions to write_pass_one(). The input file may or may not
   be valid. Here are some guidelines:

    1. Only one label may be present per line. It must be the first token present.
        Once you see a label, regardless of whether it is a valid label or invalid
        label, treat the NEXT token as the beginning of an instruction.
    2. If the first token is not a label, treat it as the name of an instruction.
    3. Everything after the instruction name should be treated as arguments to
        that instruction. If there are more than MAX_ARGS arguments, call
        raise_extra_arg_error() and pass in the first extra argument. Do not 
        write that instruction to the output file (eg. don't call write_pass_one())
    4. Only one instruction should be present per line. You do not need to do 
        anything extra to detect this - it should be handled by guideline 3. 
    5. A line containing only a label is valid. The address of the label should
        be the byte offset of the next instruction, regardless of whether there
        is a next instruction or not.

   Just like in pass_two(), if the function encounters an error it should NOT
   exit, but process the entire file and return -1. If no errors were encountered, 
   it should return 0.
 */
int pass_one(FILE* input, FILE* output, SymbolTable* symtbl) {
    /* YOUR CODE HERE */
    char buf[BUF_SIZE];
    int line_num = 1;
    uint32_t line_address = 0;
    char* args[MAX_ARGS];
    int num_toks = 0;
    int num_args = 0;    
    char* name;
    int error_flag = 0;
    int inst_flag = 0;
    int too_many_arg_flag = 0;
    while(fgets(buf, BUF_SIZE, input)){
        skip_comment(buf);
        inst_flag = 0;
        too_many_arg_flag = 0;
        num_args = 0;
        num_toks = 0;
        char* buf_tok = strtok(buf, ", \t\n()");
        
        while(buf_tok != NULL){

            if ((num_toks == 0) && add_if_label(line_num, buf_tok, line_address, symtbl)){
                num_toks++;
            }
            else if(inst_flag == 0){
                name = buf_tok;
                inst_flag = 1;
                num_toks++;
            }
            else if (num_args == MAX_ARGS){
                raise_extra_arg_error(line_num, buf_tok);
                too_many_arg_flag = 1;
                printf("buf_tok: %s, num_args: %d, num_toks: %d, name: %s, arg[0]: %s, arg[1]: %s, arg[2]: %s\n", buf_tok, num_args, num_toks, name, args[0], args[1], args[2]);
                break;
            }
            else {
                args[num_args] = buf_tok;
                num_args++;
                num_toks++;
            }
            buf_tok = strtok(NULL, ", \t\n()");
        }
        if ((!too_many_arg_flag) && (inst_flag == 1)){
            line_address = line_address + (4 * write_pass_one(output, name, args, num_args));
            printf("name: %s\n", name);
        }
        line_num++;
    }
    if (error_flag == 1){
        return -1;
    }
    else {
        return 0;
    }
}

/* Reads an intermediate file and translates it into machine code. You may assume:
    1. The input file contains no comments
    2. The input file contains no labels
    3. The input file contains at maximum one instruction per line
    4. All instructions have at maximum MAX_ARGS arguments
    5. The symbol table has been filled out already

   If an error is reached, DO NOT EXIT the function. Keep translating the rest of
   the document, and at the end, return -1. Return 0 if no errors were encountered. */
int pass_two(FILE *input, FILE* output, SymbolTable* symtbl, SymbolTable* reltbl) {
    /* YOUR CODE HERE */

    
    // Since we pass this buffer to strtok(), the chars here will GET CLOBBERED.
    char buf[BUF_SIZE];
    // Store input line number / byte offset below. When should each be incremented?
    uint32_t line_num = 1;
    uint32_t line_address = 0;
    char* args[MAX_ARGS];
    /* char* args_arg[32];
    for (int i = 0; i < MAX_ARGS; i++) {
        args[i] = args_arg;
    } */
    int num_toks = 0;
    int num_args = 0;    
    char* name;
    int error_flag = 0;

    while(fgets(buf, BUF_SIZE, input) != NULL){
        printf("buf: %s\n", buf);
        //if (feof(input)){
        //    printf("end of file\n");
        //}
        num_toks = 0;
        num_args = 0; 
    // First, read the next line into a buffer.

    // Next, use strtok() to scan for next character. If there's nothing,
    // go to the next line.
        char* buf_tok = strtok(buf, ", \n");
        while(buf_tok != NULL){
            //printf("buf_tok: %s\n", buf_tok);
    // Parse for instruction arguments. You should use strtok() to tokenize
    // the rest of the line. Extra arguments should be filtered out in pass_one(),
    // so you don't need to worry about that here.
            if(num_toks == 0){
                name = buf_tok;
                num_toks++;
            }
            else{
                args[num_args] =  buf_tok;
                //printf("args[%d]: %s\n", num_args, buf_tok);
                num_args++;

            }
            buf_tok = strtok(NULL, ", \n");
        }
        args[num_args] = NULL;
    // Use translate_inst() to translate the instruction and write to output file.
    // If an error occurs, the instruction will not be written and you should call
    // raise_inst_error(). 
        if ((translate_inst(output, name, args, num_args, line_address, symtbl, reltbl)) == -1){
            raise_inst_error(line_num, name, args, num_args);
            error_flag = 1;
        }
        printf("line num: %d\n", line_num);
        printf("name: %s\n", name);
        line_num++;
        line_address = (line_num - 1) * 4;
        free(buf_tok);
    // Repeat until no more characters are left, and the return the correct return val
    }
    if (error_flag == 0){
        return 0;
    }
    else{
        return -1;
    }
}

/*******************************
 * Do Not Modify Code Below
 *******************************/

static int open_files(FILE** input, FILE** output, const char* input_name, 
    const char* output_name) {
    
    *input = fopen(input_name, "r");
    if (!*input) {
        write_to_log("Error: unable to open input file: %s\n", input_name);
        return -1;
    }
    *output = fopen(output_name, "w");
    if (!*output) {
        write_to_log("Error: unable to open output file: %s\n", output_name);
        fclose(*input);
        return -1;
    }
    return 0;
}

static void close_files(FILE* input, FILE* output) {
    fclose(input);
    fclose(output);
}

/* Runs the two-pass assembler. Most of the actual work is done in pass_one()
   and pass_two().
 */
int assemble(const char* in_name, const char* tmp_name, const char* out_name) {
    FILE *src, *dst;
    int err = 0;
    SymbolTable* symtbl = create_table(SYMTBL_UNIQUE_NAME);
    SymbolTable* reltbl = create_table(SYMTBL_NON_UNIQUE);

    if (in_name) {
        printf("Running pass one: %s -> %s\n", in_name, tmp_name);
        if (open_files(&src, &dst, in_name, tmp_name) != 0) {
            free_table(symtbl);
            free_table(reltbl);
            exit(1);
        }

        if (pass_one(src, dst, symtbl) != 0) {
            err = 1;
        }
        close_files(src, dst);
    }

    if (out_name) {
        printf("Running pass two: %s -> %s\n", tmp_name, out_name);
        if (open_files(&src, &dst, tmp_name, out_name) != 0) {
            free_table(symtbl);
            free_table(reltbl);
            exit(1);
        }

        fprintf(dst, ".text\n");
        if (pass_two(src, dst, symtbl, reltbl) != 0) {
            err = 1;
        }
        
        fprintf(dst, "\n.symbol\n");
        write_table(symtbl, dst);

        fprintf(dst, "\n.relocation\n");
        write_table(reltbl, dst);

        close_files(src, dst);
    }
    
    free_table(symtbl);
    free_table(reltbl);
    return err;
}

static void print_usage_and_exit() {
    printf("Usage:\n");
    printf("  Runs both passes: assembler <input file> <intermediate file> <output file>\n");
    printf("  Run pass #1:      assembler -p1 <input file> <intermediate file>\n");
    printf("  Run pass #2:      assembler -p2 <intermediate file> <output file>\n");
    printf("Append -log <file name> after any option to save log files to a text file.\n");
    exit(0);
}

int main(int argc, char **argv) {
    if (argc != 4 && argc != 6) {
        print_usage_and_exit();
    }

    int mode = 0;
    if (strcmp(argv[1], "-p1") == 0) {
        mode = 1;
    } else if (strcmp(argv[1], "-p2") == 0) {
        mode = 2;
    }

    char *input, *inter, *output;
    if (mode == 1) {
        input = argv[2];
        inter = argv[3];
        output = NULL;
    } else if (mode == 2) {
        input = NULL;
        inter = argv[2];
        output = argv[3];
    } else {
        input = argv[1];
        inter = argv[2];
        output = argv[3];
    }

    if (argc == 6) {
        if (strcmp(argv[4], "-log") == 0) {
            set_log_file(argv[5]);
        } else {
            print_usage_and_exit();
        }
    }

    int err = assemble(input, inter, output);

    if (err) {
        write_to_log("One or more errors encountered during assembly operation.\n");
    } else {
        write_to_log("Assembly operation completed successfully.\n");
    }

    if (is_log_file_set()) {
        printf("Results saved to %s\n", argv[5]);
    }

    return err;
}
