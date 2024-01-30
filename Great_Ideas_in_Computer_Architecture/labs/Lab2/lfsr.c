#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>


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
