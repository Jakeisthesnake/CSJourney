.data
shouldben1:	.asciiz "Should be -1, and firstposshift and firstposmask returned: "
shouldbe0:	.asciiz "Should be 0 , and firstposshift and firstposmask returned: "
shouldbe16:	.asciiz "Should be 16, and firstposshift and firstposmask returned: "
shouldbe31:	.asciiz "Should be 31, and firstposshift and firstposmask returned: "

.text
main:
	la	$a0, shouldbe31
	jal	print_str
	lui	$a0, 0x8000	# should be 31
	jal	first1posshift
	move	$a0, $v0
	jal	print_int
	jal	print_space

	lui	$a0, 0x8000
	jal	first1posmask
	move	$a0, $v0
	jal	print_int
	jal	print_newline

	la	$a0, shouldbe16
	jal	print_str
	lui	$a0, 0x0001	# should be 16
	jal	first1posshift
	move	$a0, $v0
	jal	print_int
	jal	print_space

	lui	$a0, 0x0001
	jal	first1posmask
	move	$a0, $v0
	jal	print_int
	jal	print_newline

	la	$a0, shouldbe0
	jal	print_str
	li	$a0, 1		# should be 0
	jal	first1posshift
	move	$a0, $v0
	jal	print_int
	jal	print_space

	li	$a0, 1
	jal	first1posmask
	move	$a0, $v0
	jal	print_int
	jal	print_newline

	la	$a0, shouldben1
	jal	print_str
	move	$a0, $0		# should be -1
	jal	first1posshift
	move	$a0, $v0
	jal	print_int
	jal	print_space

	move	$a0, $0
	jal	first1posmask
	move	$a0, $v0
	jal	print_int
	jal	print_newline

	li	$v0, 10
	syscall

first1posshift:
	addiu	$sp, $sp, -20
	sw	$ra, 0($sp)
	sw	$a0, 4($sp)
	sw	$a1, 8($sp)
	sw	$s0, 12($sp)
	sw	$s1, 16($sp)
	addiu $a1, $0, 31
	addiu $s0, $0, -1
	lui $s1, 0x8000
	ori $s1, $s1, 0x0000
	jal shiftcounter
	add $v0, $a1, $0
	lw	$s1, 16($sp)
	lw	$s0, 12($sp)
	lw	$a1, 8($sp)
	lw	$a0, 4($sp)
	lw	$ra, 0($sp)
	addiu	$sp, $sp, 20
	jr	$ra

shiftcounter:
	and $t0, $a0, $s1
	beq $t0, $s1, shiftcounterend
	sll $a0, $a0, 1
	addiu $a1, $a1, -1
	bne $a1, $s0, shiftcounter
	jr $ra

shiftcounterend:
    jr $ra



first1posmask:
	addiu	$sp, $sp, -20
	sw	$ra, 0($sp)
	sw	$a0, 4($sp)
	sw	$a1, 8($sp)
	sw	$s0, 12($sp)
	sw	$s1, 16($sp)
	add $s1, $a0, $0
	addiu $a1, $0, 31
	lui $a0, 0x8000
	ori $a0, $a0, 0x0000
	addiu $s0, $0, -1
	jal 
	
	
	add $v0, $a1, $0
	lw	$s1, 16($sp)
	lw	$s0, 12($sp)
	lw	$a1, 8($sp)
	lw	$a0, 4($sp)
	lw	$ra, 0($sp)
	addiu	$sp, $sp, 20
	jr	$ra

maskshifter:
	and $t0, $a0, $s1
	beq $t0, $a0, maskshifterend
	srl $a0, $a0, 1
	addiu $a1, $a1, -1
	bne $a1, $s0, maskshifter
	jr $ra

maskshifterend:
    jr $ra

print_int:
	move	$a0, $v0
	li	$v0, 1
	syscall
	jr	$ra

print_str:
	li	$v0, 4
	syscall
	jr	$ra

print_space:
	li	$a0, ' '
	li	$v0, 11
	syscall
	jr	$ra

print_newline:
	li	$a0, '\n'
	li	$v0, 11
	syscall
	jr	$ra
