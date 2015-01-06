#include <unistd.h>
#ifdef VERSION2
#include <stdio.h>
#endif

int main(int argc, char** argv)
{
    write(2, "Hello, world!\n", 14);
#ifdef VERSION2
    exit(0);
#endif
    return 0;
}

/*
gcc -static -fPIC hello.c && objdump -d ./a.out | less

00000000004004d0 <main>:
  4004d0:       55                      push   %rbp
  4004d1:       48 89 e5                mov    %rsp,%rbp
  4004d4:       48 83 ec 10             sub    $0x10,%rsp
  4004d8:       89 7d fc                mov    %edi,-0x4(%rbp)
  4004db:       48 89 75 f0             mov    %rsi,-0x10(%rbp)
  4004df:       ba 0e 00 00 00          mov    $0xe,%edx
  4004e4:       48 8d 35 39 2a 08 00    lea    0x82a39(%rip),%rsi        # 482f24 <_IO_stdin_used+0x4>
  4004eb:       bf 02 00 00 00          mov    $0x2,%edi
  4004f0:       e8 cb c1 00 00          callq  40c6c0 <__libc_write>
  4004f5:       b8 00 00 00 00          mov    $0x0,%eax
  4004fa:       c9                      leaveq
  4004fb:       c3                      retq

%edx contains the length, %edi contains the file descriptor, %rsi (probably) points to the buffer

000000000040c6c0 <__libc_write>:
  40c6c0:       83 3d d5 34 2a 00 00    cmpl   $0x0,0x2a34d5(%rip)        # 6afb9c <__libc_multiple_threads>
  40c6c7:       75 14                   jne    40c6dd <__write_nocancel+0x14>
000000000040c6c9 <__write_nocancel>:
  40c6c9:       b8 01 00 00 00          mov    $0x1,%eax
  40c6ce:       0f 05                   syscall
  40c6d0:       48 3d 01 f0 ff ff       cmp    $0xfffffffffffff001,%rax
  40c6d6:       0f 83 84 23 00 00       jae    40ea60 <__syscall_error>
  40c6dc:       c3                      retq
  40c6dd:       48 83 ec 08             sub    $0x8,%rsp
  40c6e1:       e8 7a 10 00 00          callq  40d760 <__libc_enable_asynccancel>
  40c6e6:       48 89 04 24             mov    %rax,(%rsp)
  40c6ea:       b8 01 00 00 00          mov    $0x1,%eax
  40c6ef:       0f 05                   syscall
  40c6f1:       48 8b 3c 24             mov    (%rsp),%rdi
  40c6f5:       48 89 c2                mov    %rax,%rdx
  40c6f8:       e8 c3 10 00 00          callq  40d7c0 <__libc_disable_asynccancel>
  40c6fd:       48 89 d0                mov    %rdx,%rax
  40c700:       48 83 c4 08             add    $0x8,%rsp
  40c704:       48 3d 01 f0 ff ff       cmp    $0xfffffffffffff001,%rax
  40c70a:       0f 83 50 23 00 00       jae    40ea60 <__syscall_error>
  40c710:       c3                      retq

__libc_write and __write_nocancel are contiguous

__libc_write looks like it starts with a check of some parallelism flag?

Assuming the first segment (40c6c9-40c6dc) is a useful 
version, and truncating the error checking and stack 
fixing, the shellcode should start with something like:

#somehow get the string into %rsi, probably not with an ip-relative load
#48 8d 35 39 2a 08 00    lea    0x82a39(%rip),%rsi        # 482f24 <_IO_stdin_used+0x4>
ba 0e 00 00 00          mov    $0xe,%edx
bf 02 00 00 00          mov    $0x2,%edi
b8 01 00 00 00          mov    $0x1,%eax
0f 05                   syscall

Deal with the null bytes first:

cat > hello1.s <<END_ASM
xor %rdx, %rdx
inc %rdx
shl \$4, %rdx
dec %rdx
dec %rdx # %rdx is now 14
xor %eax, %eax
inc %eax
mov %eax, %edi
inc %edi
syscall
END_ASM

as hello1.s && objdump -d ./a.out

0000000000000000 <.text>:
   0:   48 31 d2                xor    %rdx,%rdx
   3:   48 ff c2                inc    %rdx
   6:   48 c1 e2 04             shl    $0x4,%rdx
   a:   48 ff ca                dec    %rdx
   d:   48 ff ca                dec    %rdx
  10:   31 c0                   xor    %eax,%eax
  12:   ff c0                   inc    %eax
  14:   89 c7                   mov    %eax,%edi
  16:   ff c7                   inc    %edi
  18:   0f 05                   syscall

Try AlephOne's jmp/call/pop mechanism for the string?

let quotify = (>>= \x-> ['\'', x, '\'', ',', ' '])
quotify "Hello, world!\n"
"'H', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd', '!', '\n', "

cat > hello2.s <<END_ASM
jmp L1
L2: pop %rsi
xor %rdx, %rdx
inc %rdx
shl \$4, %rdx
dec %rdx
dec %rdx # %rdx is now 14
xor %eax, %eax
inc %eax
mov %eax, %edi
inc %edi
syscall
L1: call L2
.byte 'H', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd', '!', '\n'
END_ASM

as hello2.s && objdump -d ./a.out

0000000000000000 <L2-0x2>:
   0:   eb 1b                   jmp    1d <L1>
0000000000000002 <L2>:
   2:   5e                      pop    %rsi
   3:   48 31 d2                xor    %rdx,%rdx
   6:   48 ff c2                inc    %rdx
   9:   48 c1 e2 04             shl    $0x4,%rdx
   d:   48 ff ca                dec    %rdx
  10:   48 ff ca                dec    %rdx
  13:   31 c0                   xor    %eax,%eax
  15:   ff c0                   inc    %eax
  17:   89 c7                   mov    %eax,%edi
  19:   ff c7                   inc    %edi
  1b:   0f 05                   syscall
000000000000001d <L1>:
  1d:   e8 e0 ff ff ff          callq  2 <L2>
  22:   48                      rex.W
  23:   65                      gs
  24:   6c                      insb   (%dx),%es:(%rdi)
  25:   6c                      insb   (%dx),%es:(%rdi)
  26:   6f                      outsl  %ds:(%rsi),(%dx)
  27:   2c 20                   sub    $0x20,%al
  29:   77 6f                   ja     9a <L1+0x7d>
  2b:   72 6c                   jb     99 <L1+0x7c>
  2d:   64 21 0a                and    %ecx,%fs:(%rdx)

eb 1b 5e 48 31 d2 48 ff c2 48 c1 e2 04 48 ff ca 48 ff ca 31 c0 ff c0 89 c7 ff c7 0f 05 e8 e0 ff ff ff 48 65 6c 6c 6f 2c 20 77 6f 72 6c 64 21 0a

\xeb\x1b\x5e\x48\x31\xd2\x48\xff\xc2\x48\xc1\xe2\x04\x48\xff\xca\x48\xff\xca\x31\xc0\xff\xc0\x89\xc7\xff\xc7\x0f\x05\xe8\xe0\xff\xff\xff\x48\x65\x6c\x6c\x6f\x2c\x20\x77\x6f\x72\x6c\x64\x21\x0a

"\xeb\x1b\x5e\x48\x31\xd2\x48\xff\xc2\x48\xc1\xe2\x04\x48\xff\xca" ++
"\x48\xff\xca\x31\xc0\xff\xc0\x89\xc7\xff\xc7\x0f\x05\xe8\xe0\xff" ++
"\xff\xff\x48\x65\x6c\x6c\x6f\x2c\x20\x77\x6f\x72\x6c\x64\x21\x0a"

"\xeb\x1b\x5e\x48\x31\xd2\x48\xff\xc2\x48\xc1\xe2\x04\x48\xff\xca" ++
"\x48\xff\xca\x31\xc0\xff\xc0\x89\xc7\xff\xc7\x0f\x05\xe8\xe0\xff" ++
"\xff\xffHello, world!\n"

This runs properly from Haskell! It gets into an infinite 
loop though, so upgrade it to include an exit call.

gcc -static -fPIC -DVERSION2 hello.c && objdump -d ./a.out | less

00000000004004d0 <main>:
  4004d0:       55                      push   %rbp
  4004d1:       48 89 e5                mov    %rsp,%rbp
  4004d4:       48 83 ec 10             sub    $0x10,%rsp
  4004d8:       89 7d fc                mov    %edi,-0x4(%rbp)
  4004db:       48 89 75 f0             mov    %rsi,-0x10(%rbp)
  4004df:       ba 0e 00 00 00          mov    $0xe,%edx
  4004e4:       48 8d 35 39 2a 08 00    lea    0x82a39(%rip),%rsi        # 482f24 <_IO_stdin_used+0x4>
  4004eb:       bf 02 00 00 00          mov    $0x2,%edi
  4004f0:       e8 cb c1 00 00          callq  40c6c0 <__libc_write>
  4004f5:       bf 00 00 00 00          mov    $0x0,%edi
  4004fa:       e8 51 09 00 00          callq  400e50 <exit>
0000000000400e50 <exit>:
  400e50:       48 83 ec 08             sub    $0x8,%rsp
  400e54:       ba 01 00 00 00          mov    $0x1,%edx
  400e59:       be 78 c1 6a 00          mov    $0x6ac178,%esi
  400e5e:       e8 bd fe ff ff          callq  400d20 <__run_exit_handlers>

__run_exit_handlers seems to be very long, and based on the 
name it probably runs the sort of stuff installed by 
atexit(3). AlephOne's paper just disassembles _exit (a call 
to which is included in __run_exit_handlers), so assume 
that's the "relevant" bit and ignore the extra-tidy cleanup.

000000000040c4e0 <_exit>:
  40c4e0:       48 63 d7                movslq %edi,%rdx
  40c4e3:       49 c7 c1 d0 ff ff ff    mov    $0xffffffffffffffd0,%r9
  40c4ea:       41 b8 e7 00 00 00       mov    $0xe7,%r8d
  40c4f0:       be 3c 00 00 00          mov    $0x3c,%esi
  40c4f5:       eb 19                   jmp    40c510 <_exit+0x30>
  40c4f7:       66 0f 1f 84 00 00 00    nopw   0x0(%rax,%rax,1)
  40c4fe:       00 00
  40c500:       48 89 d7                mov    %rdx,%rdi
  40c503:       89 f0                   mov    %esi,%eax
  40c505:       0f 05                   syscall
  40c507:       48 3d 00 f0 ff ff       cmp    $0xfffffffffffff000,%rax
  40c50d:       77 19                   ja     40c528 <_exit+0x48>
  40c50f:       f4                      hlt
  40c510:       48 89 d7                mov    %rdx,%rdi
  40c513:       44 89 c0                mov    %r8d,%eax
  40c516:       0f 05                   syscall
  40c518:       48 3d 00 f0 ff ff       cmp    $0xfffffffffffff000,%rax
  40c51e:       76 e0                   jbe    40c500 <_exit+0x20>
  40c520:       f7 d8                   neg    %eax
  40c522:       64 41 89 01             mov    %eax,%fs:(%r9)
  40c526:       eb d8                   jmp    40c500 <_exit+0x20>
  40c528:       f7 d8                   neg    %eax
  40c52a:       64 41 89 01             mov    %eax,%fs:(%r9)
  40c52e:       eb df                   jmp    40c50f <_exit+0x2f>

Blindly(ish) guessing that the "f4" is "halt", and that it's the only important bit:

cat > hello3.s <<END_ASM
jmp L1
L2: pop %rsi
xor %rdx, %rdx
inc %rdx
shl \$4, %rdx
dec %rdx
dec %rdx # %rdx is now 14
xor %eax, %eax
inc %eax
mov %eax, %edi
inc %edi
syscall
hlt
L1: call L2
.byte 'H', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd', '!', '\n'
END_ASM

as hello3.s && hexdump -C ./a.out | head

00000000  7f 45 4c 46 02 01 01 00  00 00 00 00 00 00 00 00  |.ELF............|
00000010  01 00 3e 00 01 00 00 00  00 00 00 00 00 00 00 00  |..>.............|
00000020  00 00 00 00 00 00 00 00  a0 00 00 00 00 00 00 00  |................|
00000030  00 00 00 00 40 00 00 00  00 00 40 00 07 00 04 00  |....@.....@.....|
00000040  eb 1c 5e 48 31 d2 48 ff  c2 48 c1 e2 04 48 ff ca  |..^H1.H..H...H..|
00000050  48 ff ca 31 c0 ff c0 89  c7 ff c7 0f 05 f4 e8 df  |H..1............|
00000060  ff ff ff 48 65 6c 6c 6f  2c 20 77 6f 72 6c 64 21  |...Hello, world!|
00000070  0a 00 00 00 00 2e 73 79  6d 74 61 62 00 2e 73 74  |......symtab..st|
00000080  72 74 61 62 00 2e 73 68  73 74 72 74 61 62 00 2e  |rtab..shstrtab..|
00000090  74 65 78 74 00 2e 64 61  74 61 00 2e 62 73 73 00  |text..data..bss.|

eb 1c 5e 48 31 d2 48 ff  c2 48 c1 e2 04 48 ff ca
48 ff ca 31 c0 ff c0 89  c7 ff c7 0f 05 f4 e8 df
ff ff ff 48 65 6c 6c 6f  2c 20 77 6f 72 6c 64 21
0a

"\xeb\x1c\x5e\x48\x31\xd2\x48\xff\xc2\x48\xc1\xe2\x04\x48\xff\xca" ++
"\x48\xff\xca\x31\xc0\xff\xc0\x89\xc7\xff\xc7\x0f\x05\xf4\xe8\xdf" ++
"\xff\xff\xffHello, world!\n"

And it works!
*/
