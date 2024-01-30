#include <stdio.h>
//2_1 adds the elements of an array together
int foo(int *arr, size_t n) {
    return n ? arr[0] + foo(arr + 1, n - 1) : 0;
}


//2_2 returns the negative of the number of zero elements in an array
int bar(int *arr, size_t n) {
    int sum = 0, i;
    for (i = n; i > 0; i--) {
        sum += !arr[i - 1];
    }
    return ~sum + 1;
}

//2_3 assigns x to be the value of y (locally, not globally)
void baz(int x, int y) {
    x = x ^ y;
    y = x ^ y;
    x = x ^ y;
}

//3_1
int flip(int *x, int *y){
    int temp = *x;
    *x = *y;
    *y = temp;
    return 0;
}

//3_2
int add_one(int *x){
    (*x)++;
    return 0;
}

//3_3
int strlen_mine(char str[]){
   int length = 0;
   while (str[length] != 0){
       length++;
   }
   return length; 
}

/* 4_1 Returns the sum of all the elements in SUMMANDS. */
int sum(int* summands, int summands_size) {
    int sum = 0;
    for (int i = 0; i < summands_size; i++){
        sum += *(summands + i);
    }
    return sum;
}

/* 4_2 Increments all the letters in the string STRING, held in an array of length N.
* Does not modify any other memory which has been previously allocated. */
void increment(char* string, int n) {
    for (int i = 0; i < n; i++)
    (*(string + i))++;
    puts(string);
}

 /* 4_3 Copies the string SRC to DST. */
void copy(char* src, char* dst) {
    while (*dst++ = *src++){
        printf("%c \n", *src);
    };
}


void main() {
    char test[] = "abc";
    char tesdp[3];
    copy(test, tesdp);
}