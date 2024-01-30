#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// Return the nth bit of x.
// Assume 0 <= n <= 31
unsigned get_bit(unsigned x,
                 unsigned n) {
    // YOUR CODE HERE
    // Returning -1 is a placeholder (it makes
    // no sense, because get_bit only returns 
    // 0 or 1)
    unsigned bit_mask = 0b00000001 << n;
    unsigned x_nth_bit = (x & bit_mask);// >> n;
    //printf("%x \n", bit_mask);
    //printf("%x \n", x_nth_bit);
    //printf("%x \n", x);
    x_nth_bit = x_nth_bit >> n;
    //printf("%x \n", x_nth_bit);
    return x_nth_bit;
}
// Set the nth bit of the value of x to v.
// Assume 0 <= n <= 31, and v is 0 or 1
void set_bit(uint16_t *x,
             unsigned n,
             unsigned v) {
    // YOUR CODE HERE
    unsigned nth_bit = get_bit(*x, n);
    unsigned xor_v_nth_bit = nth_bit ^ v;
    unsigned bit_mask = xor_v_nth_bit << n;
    unsigned x_nth_bit_flipped = (*x ^ bit_mask);// >> n;
    //printf("%x \n", x_nth_bit);
    *x = x_nth_bit_flipped;
}
// Flip the nth bit of the value of x.
// Assume 0 <= n <= 31
/* void flip_bit(unsigned * x,
              unsigned n) {
    // YOUR CODE HERE
    unsigned bit_mask = 0b00000001 << n;
    unsigned x_nth_bit_flipped = (*x ^ bit_mask);// >> n;
    //printf("%x \n", (!nth_bit));
    *x = x_nth_bit_flipped;
}
 */


// YOU CAN IGNORE THE REST OF THIS FILE
 
/* 
void test_get_bit(unsigned x,
                  unsigned n,
                  unsigned expected) {
    unsigned a = get_bit(x, n);
    if(a!=expected) {
        printf("get_bit(0x%08x,%u): 0x%08x, expected 0x%08x\n",x,n,a,expected);
    } else {
        printf("get_bit(0x%08x,%u): 0x%08x, correct\n",x,n,a);
    }
} */
/* void test_set_bit(unsigned x,
                  unsigned n,
                  unsigned v,
                  unsigned expected) {
    unsigned o = x;
    set_bit(&x, n, v);
    if(x!=expected) {
        printf("set_bit(0x%08x,%u,%u): 0x%08x, expected 0x%08x\n",o,n,v,x,expected);
    } else {
        printf("set_bit(0x%08x,%u,%u): 0x%08x, correct\n",o,n,v,x);
    }
} */
/* void test_flip_bit(unsigned x,
                   unsigned n,
                   unsigned expected) {
    unsigned o = x;
    flip_bit(&x, n);
    if(x!=expected) {
        printf("flip_bit(0x%08x,%u): 0x%08x, expected 0x%08x\n",o,n,x,expected);
    } else {
        printf("flip_bit(0x%08x,%u): 0x%08x, correct\n",o,n,x);
    }
} */
/* int main(int argc,
         const char * argv[]) {
    printf("\nTesting get_bit()\n\n");
    test_get_bit(0b1001110,0,0);
    test_get_bit(0b1001110,1,1);
    test_get_bit(0b1001110,5,0);
    test_get_bit(0b1101110,5,0);
    test_get_bit(0b11011,3,1);
    test_get_bit(0b11011,2,0);
    test_get_bit(0b11011,9,0);
    printf("\nTesting set_bit()\n\n");
    test_set_bit(0b1001110,2,0,0b1001010);
    test_set_bit(0b1101101,0,0,0b1101100);
    test_set_bit(0b1001110,2,1,0b1001110);
    test_set_bit(0b1101101,0,1,0b1101101);
    test_set_bit(0b1001110,9,0,0b1001110);
    test_set_bit(0b1101101,4,0,0b1101101);
    test_set_bit(0b1001110,9,1,0b1001001110);
    test_set_bit(0b1101101,7,1,0b11101101); 
    printf("\nTesting flip_bit()\n\n");
    test_flip_bit(0b1001110,0,0b1001111);
    test_flip_bit(0b1001110,1,0b1001100);
    test_flip_bit(0b1001110,2,0b1001010);
    test_flip_bit(0b1001110,5,0b1101110);
    test_flip_bit(0b1001110,9,0b1001001110);
    printf("\n");
    return 0;
}
 */



void lfsr_calculate(uint16_t *reg) {

  // YOUR CODE HERE
  uint16_t xor_sum = get_bit(*reg, 0) ^ get_bit(*reg, 2);
  //printf("%08x \n", *reg);
  //printf("%x \n", xor_sum);
  xor_sum = xor_sum ^ get_bit(*reg, 3);
  //printf("%x \n", xor_sum);
  xor_sum = xor_sum ^ get_bit(*reg, 5);
  //printf("xor_sum %x \n", xor_sum);
  *reg = *reg >> 1;
  //printf("reg bfore set bit %08x \n", *reg);
  set_bit(reg, 15, xor_sum);
  //printf("reg_final %08x \n", *reg);
  //printf("done\n");

}

int main() {
  int8_t *numbers = (int8_t*) malloc(sizeof(int8_t) * 65535);
  if (numbers == NULL) {
    printf("Memory allocation failed!");
    exit(1);
  }

  memset(numbers, 0, sizeof(int8_t) * 65535);
  uint16_t reg = 0x1;
  uint32_t count = 0;
  int i;

  do {
    count++;
    numbers[reg] = 1;
    if (count < 24) {
      printf("My number is: %u\n", reg);
    } else if (count == 24) {
      printf(" ... etc etc ... \n");
    }
    // lfsr_calculate(&reg);
    for (i = 0; i < 32; i++){
      lfsr_calculate(&reg);
      // printf("%08x \n", reg);
    }
  } while (numbers[reg] != 1);

  printf("Got %u numbers before cycling!\n", count);

  if (count == 65535) {
    printf("Congratulations! It works!\n");
  } else {
    printf("Did I miss something?\n");
  }

  free(numbers);

  return 0;
}
