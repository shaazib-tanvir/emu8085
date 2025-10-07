.org 2050
.db 05
.db 20
.db 30
.db 1f
.db 2e
.db 1c

.org 2000
.start
lxi h, 2050
mov e, m
mov c, m
loop0:
mov d, c
dcr d
lxi h, 2051
loop1:
mov a, m
inx h
cmp m
jc next
mov b, m
mov m, a
dcx h
mov m, b
inx h
next:
dcr d
jnz loop1
dcr e
jnz loop0
hlt
