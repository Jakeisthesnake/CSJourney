# CS 61C Spring 2015 Project 1-2 
# string.s

#==============================================================================
#                              Project 1-2 Part 1
#                               String README
#==============================================================================
# In this file you will be implementing some utilities for manipulating strings.
# The functions you need to implement are:
#  - strlen()
#  - strncpy()
#  - copy_of_str()
# Test cases are in linker-tests/test_string.s
#==============================================================================

.data
newline:	.asciiz "\n"
tab:	.asciiz "\t"

.text
#------------------------------------------------------------------------------
# function strlen()
#------------------------------------------------------------------------------
# Arguments:
#  $a0 = string input
#
# Returns: the length of the string
#------------------------------------------------------------------------------
strlen:
	# YOUR CODE HERE
	addu $t0, $zero, $zero #t0 is the counter
	addiu $t1, $a0, $zero
strlen_loop:
    lbu $t2, 0($t1)
	beq $t2, $t0, strlen_ret
	addiu $t0, $t0, 1
	addiu $t1, $t1, 1
	j strlen_loop
strlen_ret:
	addu $v0, $t0, $zero
	jr $ra

#------------------------------------------------------------------------------
# function strncpy()
#------------------------------------------------------------------------------
# Arguments:
#  $a0 = pointer to destination array
#  $a1 = source string
#  $a2 = number of characters to copy
#
# Returns: the destination array
#------------------------------------------------------------------------------
strncpy:
	# YOUR CODE HERE
	addu $t0, $a0, $zero #pointer to dst
	addu $t1, $a0, $zero #pointer to src
	addu $t2, $zero, $zero #counter
strncpy_loop:
	beq $t2, $a2, strncpy_ret
	lb $t3, 0($t1)
	sb $t3, 0($t0)
	addiu $t0, $t0, 1
	addiu $t1, $t1, 1
	addiu $t2, $t2, 1
    beq $t3, $zero, strncpy_pad
	j strncpy_loop
strncpy_pad:
	beq $t2, $a2, strncpy_ret
	sb $t3, 0($t0)
	addiu $t0, $t0, 1
	addiu $t2, $t2, 1
	j strncpy_pad
strncpy_ret:
	addu $v0, $a0, $zero
	jr $ra
	

#------------------------------------------------------------------------------
# function copy_of_str()
#------------------------------------------------------------------------------
# Creates a copy of a string. You will need to use sbrk (syscall 9) to allocate
# space for the string. strlen() and strncpy() will be helpful for this function.
# In MARS, to malloc memory use the sbrk syscall (syscall 9). See help for details.
#
# Arguments:
#   $a0 = string to copy
#
# Returns: pointer to the copy of the string
#------------------------------------------------------------------------------
copy_of_str:
	# YOUR CODE HERE
	addiu $sp, $sp, -20
	sw $ra, 16(sp)
	sw $s0, 12(sp)
	sw $s1, 8(sp)
	sw $s2, 4(sp)
	sw $s3, 0(sp)
	jal strlen
	addiu $s0, $v0, 1
	addu $s1, $a0, $zero
	addiu $a0, $v0, 1
	li $v0, 9
	syscall

	addu $a0, $v0, $zero
    addu $s2, $a1, $zero
	addu $s3, $a2, $zero
	addu $a1, $s1, $zero
	addu $a2, $s0, $zero
	jal strncpy
copy_of_str_ret:
	addu $a0, $s1, $zero
	addu $a1, $s2, $zero
	addu $a2, $s3, $zero
	lw $ra, 16(sp)
	lw $s0, 12(sp)
	lw $s1, 8(sp)
	lw $s2, 4(sp)
	lw $s3, 0(sp)
    addiu $sp, $sp, 20
	jr $ra
    
###############################################################################
#                 DO NOT MODIFY ANYTHING BELOW THIS POINT                       
###############################################################################

#------------------------------------------------------------------------------
# function streq() - DO NOT MODIFY THIS FUNCTION
#------------------------------------------------------------------------------
# Arguments:
#  $a0 = string 1
#  $a1 = string 2
#
# Returns: 0 if string 1 and string 2 are equal, -1 if they are not equal
#------------------------------------------------------------------------------
streq:
	beq $a0, $0, streq_false	# Begin streq()
	beq $a1, $0, streq_false
streq_loop:
	lb $t0, 0($a0)
	lb $t1, 0($a1)
	addiu $a0, $a0, 1
	addiu $a1, $a1, 1
	bne $t0, $t1, streq_false
	beq $t0, $0, streq_true
	j streq_loop
streq_true:
	li $v0, 0
	jr $ra
streq_false:
	li $v0, -1
	jr $ra			# End streq()

#------------------------------------------------------------------------------
# function dec_to_str() - DO NOT MODIFY THIS FUNCTION
#------------------------------------------------------------------------------
# Convert a number to its unsigned decimal integer string representation, eg.
# 35 => "35", 1024 => "1024". 
#
# Arguments:
#  $a0 = int to write
#  $a1 = character buffer to write into
#
# Returns: the number of digits written
#------------------------------------------------------------------------------
dec_to_str:
	li $t0, 10			# Begin dec_to_str()
	li $v0, 0
dec_to_str_largest_divisor:
	div $a0, $t0
	mflo $t1		# Quotient
	beq $t1, $0, dec_to_str_next
	mul $t0, $t0, 10
	j dec_to_str_largest_divisor
dec_to_str_next:
	mfhi $t2		# Remainder
dec_to_str_write:
	div $t0, $t0, 10	# Largest divisible amount
	div $t2, $t0
	mflo $t3		# extract digit to write
	addiu $t3, $t3, 48	# convert num -> ASCII
	sb $t3, 0($a1)
	addiu $a1, $a1, 1
	addiu $v0, $v0, 1
	mfhi $t2		# setup for next round
	bne $t2, $0, dec_to_str_write
	jr $ra			# End dec_to_str()
