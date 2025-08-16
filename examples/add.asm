.org 2050
data: .db 8
.db 2

.org 2000
.start
lxi h, data
mov a, m
inx h
add m
inx h
mov m, a
hlt
