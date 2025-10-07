.org 19ff
.db 03

.org 2000
.db 00
.db 00
.db 00

.org 2100
.db 01
.db 02
.db 03

.org 2200
.db 34
.db 1f
.db ef

.org 0000
.start
lxi h, 2100
lxi d, 2200
lda 19ff
mov c, a
push h
lxi h, 2000
xthl
loop: ldax d
add m
xthl
mov m, a
inx h
xthl
inx h
inx d
dcr c
jnz loop
hlt
