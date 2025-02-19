
  .text
  .globl our_code_starts_here
  our_code_starts_here:
  mov $5, %rax
  mov %rax, -8(%rsp)
  mov -8(%rsp), %rax
  mov %rax, -16(%rsp)
  mov $6, %rax
  cmp %rax, -16(%rsp)
  mov $0, %rax
  jge after0
  mov $1, %rax
  after0: 
  cmp $0, %rax
  je else1
  mov $4, %rax
  jmp after2
  else1: 
  mov $7, %rax
  after2: 
  ret
  
  

