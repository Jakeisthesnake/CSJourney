

/* Unsigned
Decimal 	Binary 	        Hexadecimal
10       	0b0000 1010 	0x0A
240      	0b1111 0000     0xF0
16  	 	0b0000 1111     0x0F
161  	    0b1010 0001	    0xA1
250 	  	0b1111 1010     0xFA
255     	0b1111 1111 	0xFF
204  	 	0b1100 1100     0xCC
-35 	  	n/a             n/a 
128 	  	0b1000 0000     0x80
105     	0b0110 1001 	0x69

Two's Complement
Decimal 	Binary      	Hexadecimal
-10     	0b1111 0110 	0xF6
-8      	0b1111 0000     0xF0
16  	  	0b0000 1111     0x0F
-95   	    0b1010 0001     0xA1
250 	  	n/a             n/a
-1      	0b1111 1111 	0xFF
-52        	0b1100 1100 	0xCC
-35 	  	0b1101 1101     0xDD
128 	  	n/a             n/a 
105  	    0b0110 1001 	0x69 */

// 2a) both - temp can be negative so depending on which range of the temp scale you are working with, you'll pick either signed or unsigned
// 2b) both - depends what range of numbers you want to maximize, whether including negative numbers of not will inform the choice
// 2c) unsigned - the number of boxes a factory has shipped is a positive number

// 3a: 4^(n)
// 3b: 2^(n-1)
// 3c: 1
// 3d: (2^n)-1

//4a: 64 + 64 = 0b0100 0000 + 0b0100 0000 = 0b1000 0000 OVERFLOW
//4b: -127 + 30 = 0b1000 0001 + 0b0001 1110 = 0b1001 1110 CORRECT
//4c: -127 -1  = -127 + (-1) = 0b1000 0001 + 0b1111 1111 = 0b1000 0000 CORRECT
//4d: 38 - 40 = 38 + (-40) =  0b0010 0110 + 0b1101 1000 = 0b1111 1110 CORRECT
