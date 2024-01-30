#include <stdio.h>
#include <string.h>
#include <math.h>


#include <unistd.h>
#include <sys/stat.h>

#include "beargit.h"
#include "util.h"

/* Implementation Notes:
 *
 * - Functions return 0 if successful, 1 if there is an error.
 * - All error conditions in the function description need to be implemented
 *   and written to stderr. We catch some additional errors for you in main.c.
 * - Output to stdout needs to be exactly as specified in the function description.
 * - Only edit this file (beargit.c)
 * - You are given the following helper functions:
 *   * fs_mkdir(dirname): create directory <dirname>
 *   * fs_rm(filename): delete file <filename>
 *   * fs_mv(src,dst): move file <src> to <dst>, overwriting <dst> if it exists
 *   * fs_cp(src,dst): copy file <src> to <dst>, overwriting <dst> if it exists
 *   * write_string_to_file(filename,str): write <str> to filename (overwriting contents)
 *   * read_string_from_file(filename,str,size): read a string of at most <size> (incl.
 *     NULL character) from file <filename> and store it into <str>. Note that <str>
 *     needs to be large enough to hold that string.
 *  - You NEED to test your code. The autograder we provide does not contain the
 *    full set of tests that we will run on your code. See "Step 5" in the homework spec.
 */

/* beargit init
 *
 * - Create .beargit directory
 * - Create empty .beargit/.index file
 * - Create .beargit/.prev file containing 0..0 commit id
 *
 * Output (to stdout):
 * - None if successful
 */

int beargit_init(void) {
  fs_mkdir(".beargit");

  FILE* findex = fopen(".beargit/.index", "w");
  fclose(findex);

  FILE* fbranches = fopen(".beargit/.branches", "w");
  fprintf(fbranches, "%s\n", "master");
  fclose(fbranches);
   
  write_string_to_file(".beargit/.prev", "0000000000000000000000000000000000000000");
  write_string_to_file(".beargit/.current_branch", "master");

  return 0;
}


/* beargit add <filename>
 * 
 * - Append filename to list in .beargit/.index if it isn't in there yet
 *
 * Possible errors (to stderr):
 * >> ERROR: File <filename> already added
 *
 * Output (to stdout):
 * - None if successful
 */

int beargit_add(const char* filename) {
  FILE* findex = fopen(".beargit/.index", "r");
  FILE *fnewindex = fopen(".beargit/.newindex", "w");

  char line[FILENAME_SIZE];
  while(fgets(line, sizeof(line), findex)) {
    strtok(line, "\n");
    if (strcmp(line, filename) == 0) {
      fprintf(stderr, "ERROR: File %s already added\n", filename);
      fclose(findex);
      fclose(fnewindex);
      fs_rm(".beargit/.newindex");
      return 3;
    }

    fprintf(fnewindex, "%s\n", line);
  }

  fprintf(fnewindex, "%s\n", filename);
  fclose(findex);
  fclose(fnewindex);

  fs_mv(".beargit/.newindex", ".beargit/.index");

  return 0;
}


/* beargit rm <filename>
 * 
 * See "Step 2" in the homework 1 spec.
 *
 */

int beargit_rm(const char* filename) {
  /* COMPLETE THE REST */
  FILE *fnewindex = fopen(".beargit/.newindex", "w");
  FILE* findex = fopen(".beargit/.index", "r");
  char line[FILENAME_SIZE];
  int in_file_p = 0; 
  char filename_return[FILENAME_SIZE];
  strtok(filename_return, "\n"); 

  

  if (findex != NULL){  
    while(fgets(line, sizeof(line), findex)) {
      strtok(line, "\n");
      

      if (strcmp(line, filename) == 0) {
        in_file_p = 1;
      }
      else{
        fprintf(fnewindex, "%s\n", line);
      } 
    }
    if (in_file_p == 0){
      fprintf(stderr, "ERROR: File %s not tracked\n", filename);
      fclose(findex);
      fclose(fnewindex);
      fs_mv(".beargit/.newindex", ".beargit/.index");
      return 1;
    }
  }

  else{
    fprintf(stderr, "index file doesn't exist or can't open\n");
    fclose(findex);
    fclose(fnewindex);
    return 1;
  }

  fclose(findex);
  fclose(fnewindex);
  fs_mv(".beargit/.newindex", ".beargit/.index");

  return 0;
}

/* beargit commit -m <msg>
 *
 * See "Step 3" in the homework 1 spec.
 *
 */

typedef enum { false, true } bool;

const char* go_bears = "GO BEARS!";

bool str2_starts_str1(const char* str1, const char* str2){
  int len1 = strlen(str1);
  int len2 = strlen(str2);
  if (len1 < len2){
    return false;
  }
  else if( len2 == 0){
    return true;
  }
  else if(str1[0] == str2[0]){
    return str2_starts_str1(str1 + 1, str2 + 1);
  }
  else{
    return false;
  }
}

bool custom_strstr(const char* str1, const char* str2){
  // printf("\n%s\n", str1);
  // printf("%s\n", str1);
  int len1 = strlen(str1);
  int len2 = strlen(str2);
  // printf("len1 %d\n", len1);
  // printf("len2 %d\n", len2);
  if (len1 < len2){
    // printf("len1 < len2 -> false");
    return false;
  }
  else if( len2 == 0){
    // printf("len2 == 0 -> true");
    return true;
  }
  else if(str2_starts_str1(str1, str2)){
    // printf("str2 starts str1 -> true");
    return true;
  }
  else{
    return custom_strstr(str1 + 1, str2);
    // printf("else -> try bf str1, str2");
  }
}


int custom_base_to_int(const char* str){
  int len = strlen(str);
  int num = 0;
  for (int i = 0; i < len; ++i){
    int digit;
    switch (str[i]){
      case 'c': digit = 0; break;
      case '1': digit = 1; break;
      case '6': digit = 2; break;
    }
    num += digit * pow(3, len - i - 1);
  }
  return num;
}



void int_to_custom_base(int num, char* str) {
  int i = 0;
  do {
    int digit = num % 3;
    switch (digit) {
      case 0: str[i] = 'c'; break;
      case 1: str[i] = '1'; break;
      case 2: str[i] = '6'; break;
    }
    num /= 3;
    ++i;
  } while (num > 0);

  while (i < 40) {
    str[i] = 'c';
    ++i;
  }
  str[i] = '\0';

  for (int j = 0; j < i / 2; ++j) {
    char temp = str[j];
    str[j] = str[i - j - 1];
    str[i - j - 1] = temp;
  }
}

int is_commit_msg_ok(const char* msg) {
  /* COMPLETE THE REST */
  char go_bears[] = "GO BEARS!";
  int len = strlen(msg);
  if(!(custom_strstr(msg, go_bears))){
    return 1;
  }
  return 0;
}



void next_commit_id_hw1(char* commit_id) {
  /* COMPLETE THE REST */
  if (strcmp(commit_id, "000000000000000000000000000000") == 0){
    strcpy(commit_id, "cccccccccccccccccccccccccccccc");
  }
  else{
    int num = custom_base_to_int(commit_id);
    num += 1;
    int_to_custom_base(num, commit_id);
  }
}

void prev_commit_id(char* commit_id) {
  /* COMPLETE THE REST */
  if (strcmp(commit_id, "0000000000000000000000000000000000000000") == 0){
    printf("First commit:0000000000000000000000000000000000000000\n");
  }
  else if (strcmp(commit_id, "cccccccccccccccccccccccccccccccccccccccc") == 0){
    strcpy(commit_id, "0000000000000000000000000000000000000000");
  }
  else{
    int num = custom_base_to_int(commit_id);
    num -= 1;
    int_to_custom_base(num, commit_id);
  }

}

int beargit_commit_hw1(const char* msg) {
  if (is_commit_msg_ok(msg)) {
    fprintf(stderr, "ERROR: Message must contain \"GO BEARS!\"\n");
    return 1;
  }

  char commit_id[COMMIT_ID_SIZE];
  read_string_from_file(".beargit/.prev", commit_id, COMMIT_ID_SIZE);
  char old_commit_id[COMMIT_ID_SIZE];
  strncpy(old_commit_id, commit_id, COMMIT_ID_SIZE);
  next_commit_id(commit_id);
  write_string_to_file(".beargit/.prev", commit_id);

  /* COMPLETE THE REST */
  char commit_dir_bear[COMMIT_ID_SIZE + 20];
  sprintf(commit_dir_bear,".beargit/%s", commit_id);
  fs_mkdir(commit_dir_bear);
  
  char commit_dir_prev[COMMIT_ID_SIZE + 20];
  sprintf(commit_dir_prev,".beargit/%s/.prev", commit_id);
  write_string_to_file(commit_dir_prev, old_commit_id);

  char commit_msg[MSG_SIZE];
  strncpy(commit_msg, msg, MSG_SIZE);
  char commit_dir_msg[COMMIT_ID_SIZE + 20];
  sprintf(commit_dir_msg,".beargit/%s/.msg", commit_id);
  printf("message %s\n", commit_msg);
  write_string_to_file(commit_dir_msg, commit_msg);


  char commit_dir_index[COMMIT_ID_SIZE + 20];
  sprintf(commit_dir_index,".beargit/%s/.index", commit_id);
  fs_cp(".beargit/.index", commit_dir_index);

  FILE* findex = fopen(".beargit/.index", "r");
  char filename[FILENAME_SIZE];

  if (findex != NULL){
    while (fgets(filename, FILENAME_SIZE, findex)){
      strtok(filename, "\n");
      char commit_dir_file[FILENAME_SIZE + COMMIT_ID_SIZE + 12];
      sprintf(commit_dir_file,".beargit/%s/%s", commit_id, filename);
      fs_cp(filename, commit_dir_file);
    // should check if the file exists first...
    }
  }

  return 0;
}

/* beargit status
 *
 * See "Step 1" in the homework 1 spec.
 *
 */

int beargit_status() {
  /* COMPLETE THE REST */
  FILE* findex = fopen(".beargit/.index", "r");
  char filename[FILENAME_SIZE];
  fprintf(stdout, "Tracked files: \n\n");
  
  int i = 0;

  if (findex != NULL){
    while (fgets(filename, FILENAME_SIZE, findex)){
      fprintf(stdout, "%s", filename);
      i++;
    }
  }

  fprintf(stdout, "\n%d files total\n", i);
   

  return 0;
}

/* beargit log
 *
 * See "Step 4" in the homework 1 spec.
 *
 */

int beargit_log() {
  /* COMPLETE THE REST */
  char last_commit_id[COMMIT_ID_SIZE];
  char commit_dir_msg[COMMIT_ID_SIZE + 20];
  char commit_msg[MSG_SIZE];

  read_string_from_file(".beargit/.prev", last_commit_id, COMMIT_ID_SIZE);
  fprintf(stdout, "commit\n%s\n", last_commit_id);
  
  if(strcmp(last_commit_id, "0000000000000000000000000000000000000000") == 0){
    return 0;
  }
  sprintf(commit_dir_msg,".beargit/%s/.msg", last_commit_id);
  read_string_from_file(commit_dir_msg, commit_msg, MSG_SIZE);
  fprintf(stdout, "%s\n", commit_msg); //print previous commit MSG

  while (strcmp(last_commit_id, "0000000000000000000000000000000000000000")){
    
    //change commit id 
    prev_commit_id(last_commit_id);
    if(strcmp(last_commit_id, "0000000000000000000000000000000000000000") == 0){
      break;
    }
    else{
      //print commit id 
      fprintf(stdout, "commit %s\n", last_commit_id);

      sprintf(commit_dir_msg,".beargit/%s/.msg", last_commit_id);
      read_string_from_file(commit_dir_msg, commit_msg, MSG_SIZE);
      fprintf(stdout, "%s\n", commit_msg); //print previous commit MSG
    }
  }


  return 0;
}

// ---------------------------------------
// HOMEWORK 2 
// ---------------------------------------

// This adds a check to beargit_commit that ensures we are on the HEAD of a branch.
int beargit_commit(const char* msg) {
  char current_branch[BRANCHNAME_SIZE];
  read_string_from_file(".beargit/.current_branch", current_branch, BRANCHNAME_SIZE);
  printf("commit 1: current_branch %s\n", current_branch);

  if (strlen(current_branch) == 0) {
    fprintf(stderr, "ERROR: Need to be on HEAD of a branch to commit\n");
    return 1;
  }

  return beargit_commit_hw1(msg);
}

const char* digits = "c16";

void next_commit_id(char* commit_id) {
  char current_branch[BRANCHNAME_SIZE];
  read_string_from_file(".beargit/.current_branch", current_branch, BRANCHNAME_SIZE);
  printf("next_commit_id 1: current_branch %s\n", current_branch);
  // The first COMMIT_ID_BRANCH_BYTES=10 characters of the commit ID will
  // be used to encode the current branch number. This is necessary to avoid
  // duplicate IDs in different branches, as they can have the same pre-
  // decessor (so next_commit_id has to depend on something else).
  int n = get_branch_number(current_branch);
  for (int i = 0; i < COMMIT_ID_BRANCH_BYTES; i++) {
    // This translates the branch number into base 3 and substitutes 0 for
    // 6, 1 for 1 and c for 2.
    commit_id[i] = digits[n%3];
    n /= 3;
  }

  // Use next_commit_id to fill in the rest of the commit ID.
  next_commit_id_hw1(commit_id + COMMIT_ID_BRANCH_BYTES);
}


// This helper function returns the branch number for a specific branch, or
// returns -1 if the branch does not exist.
int get_branch_number(const char* branch_name) {
  FILE* fbranches = fopen(".beargit/.branches", "r");

  int branch_index = -1;
  int counter = 0;
  char line[FILENAME_SIZE];
  while(fgets(line, sizeof(line), fbranches)) {
    strtok(line, "\n");
    if (strcmp(line, branch_name) == 0) {
      branch_index = counter;
    }
    counter++;
  }

  fclose(fbranches);

  return branch_index;
}

/* beargit branch
 *
 * See "Step 6" in the homework 1 spec.
 *
 */

int beargit_branch() {
    /* COMPLETE THE REST */
  FILE* fbranches = fopen(".beargit/.branches", "r");
  char branchname[BRANCHNAME_SIZE];
  FILE* f_current_branch = fopen(".beargit/.current_branch", "r");
  char current_branch_name[BRANCHNAME_SIZE];

  if (f_current_branch != NULL){
    fgets(current_branch_name, BRANCHNAME_SIZE, f_current_branch);
  }

  if (fbranches != NULL){
    while (fgets(branchname, BRANCHNAME_SIZE, fbranches)){
      if(strcmp(branchname, current_branch_name) == 0){
        fprintf(stdout, "* ");
      }
      fprintf(stdout, "%s", branchname);
    }
  }

  if (fclose(fbranches) != 0) {
    printf("Failed to close file.\n");
    return -1;
  } 
   if (fclose(f_current_branch) != 0) {
    printf("Failed to close file.\n");
    return -1;
  } 

  return 0;
}

/* beargit checkout
 *
 * See "Step 7" in the homework 1 spec.
 *
 */

int checkout_commit(const char* commit_id) {
  /* COMPLETE THE REST */
  //Going through the index of the current index file, delete all those files (in the current directory; i.e., the directory where we ran beargit).
  FILE* findex = fopen(".beargit/.index", "r");
  char filename[FILENAME_SIZE];

  if (findex != NULL){
    while (fgets(filename, FILENAME_SIZE, findex)){
      fs_rm(filename);
    }
  }

  fclose(findex);

  //Write the ID of the commit that is being checked out into .prev.
  write_string_to_file(".beargit/.prev", commit_id);
  
  //In the special case that the new commit is the 00.0 commit, there are no files to copy and there is no index. 
  //Instead empty the index (but still write the ID into .prev and delete the current index files).
  if(strcmp(commit_id, "0000000000000000000000000000000000000000") == 0){
    return 0;
  }

  //Copy the index from the commit that is being checked out to the .beargit directory,
  char new_index_path[COMMIT_ID_SIZE + 20];
  sprintf(new_index_path,".beargit/%s/.index", commit_id);
  fs_cp(new_index_path, ".beargit/.index");
  // and use it to copy all that commit's tracked files from the commit's directory into the current directory

  FILE* fnew_index = fopen(new_index_path, "r");
  char new_filename[FILENAME_SIZE];
  char commit_dir_file[FILENAME_SIZE + COMMIT_ID_SIZE + 12];

  if (fnew_index != NULL){
    while (fgets(new_filename, FILENAME_SIZE, fnew_index)){
      strtok(new_filename, "\n");
      sprintf(commit_dir_file,".beargit/%s/%s", commit_id, new_filename);
      fs_cp(commit_dir_file, new_filename);
    }
  }
  fclose(fnew_index);

  return 0;

  


}

int is_it_a_commit_id(const char* commit_id) {
  /* COMPLETE THE REST */
  for(int i = 0; i < COMMIT_ID_SIZE; i++){
    if(!(strchr("61c", commit_id[i]))){
      return 0;
    }
  }
  return 1;
}

int beargit_checkout(const char* arg, int new_branch) {
  // Get the current branch
  char current_branch[BRANCHNAME_SIZE];
  printf("checkout arg %s\n", arg);
  read_string_from_file(".beargit/.current_branch", current_branch, BRANCHNAME_SIZE);
  printf("checkout current_branch %s\n", current_branch);

  // If not detached, update the current branch by storing the current HEAD into that branch's file...
  // Even if we cancel later, this is still ok.
  if (strlen(current_branch)) {
    char current_branch_file[BRANCHNAME_SIZE+50];
    sprintf(current_branch_file, ".beargit/.branch_%s", current_branch);
    fs_cp(".beargit/.prev", current_branch_file);
  }

  // Check whether the argument is a commit ID. If yes, we just stay in detached mode
  // without actually having to change into any other branch.
  if (is_it_a_commit_id(arg)) {
    char commit_dir[FILENAME_SIZE] = ".beargit/";
    strcat(commit_dir, arg);
    printf("check out: commitdir %s\n", commit_dir);
    if (!fs_check_dir_exists(commit_dir)) {
      fprintf(stderr, "ERROR: Commit %s does not exist\n", arg);
      return 1;
    }

    // Set the current branch to none (i.e., detached).
    write_string_to_file(".beargit/.current_branch", "");

    return checkout_commit(arg);
  }

  // Just a better name, since we now know the argument is a branch name.
  const char* branch_name = arg;

  // Read branches file (giving us the HEAD commit id for that branch).
  int branch_exists = (get_branch_number(branch_name) >= 0);
  printf("checkout: branch_exists %d\n", branch_exists);
  printf("checkout: new_branch %d\n", new_branch);

  // Check for errors.
  if (branch_exists && new_branch) {
    fprintf(stderr, "ERROR: A branch named %s already exists\n", branch_name);
    return 1;
  } else if (!branch_exists && !new_branch) {
    fprintf(stderr, "ERROR: No branch %s exists\n", branch_name);
    return 1;
  }

  

  // File for the branch we are changing into.
  char branch_file[BRANCHNAME_SIZE + 50] = ".beargit/.branch_"; 
  printf("checkout: branch_file %s branch_name %s\n", branch_file, branch_name);
  strcat(branch_file, branch_name);
  
  // Update the branch file if new branch is created (now it can't go wrong anymore)
  if (new_branch) {
    
    FILE* fbranches = fopen(".beargit/.branches", "a");
    fprintf(fbranches, "%s\n", branch_name);
    fclose(fbranches);
    fs_cp(".beargit/.prev", branch_file); 
  }

  write_string_to_file(".beargit/.current_branch", branch_name);

  // Read the head commit ID of this branch.
  char branch_head_commit_id[COMMIT_ID_SIZE];
  read_string_from_file(branch_file, branch_head_commit_id, COMMIT_ID_SIZE);

  // Check out the actual commit.
  return checkout_commit(branch_head_commit_id);
}
