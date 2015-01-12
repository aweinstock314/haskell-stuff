constant42:
mov $42, %al
ret

identity:
mov %edi, %eax
ret

factorial:
mov $1, %al
mov %edi, %ebx
fact_loop:
    cmp $1, %ebx
    jl fact_end
    imul %ebx, %eax
    dec %ebx
    jmp fact_loop
fact_end:
ret
