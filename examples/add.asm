.org 2050
.db 8
.db 2

.org 2000
lxi h, 2050
mov a, m
inx h
add m
inx h
mov m, a
hlt
