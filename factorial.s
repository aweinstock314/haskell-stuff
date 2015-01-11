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
    imul %ebx, %eax
    dec %ebx
    cmp $1, %ebx
    jne fact_loop
ret
