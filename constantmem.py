n = 256

s = ''
for i in range(n):
    s +=  'let a%d = 0 in ' % i
print s
def unroll(a,b,base):
    if a < b:
        m = (a+b)//2
        s = 'if n <= %d then ' % m
        s += unroll(a,m,base)
        s +=  ' else '
        s += unroll(m+1,b,base)
        return s
    else:
        return 'a%d' % a + base
print 'fun aget(n) {' + unroll(0,n-1,'') + '}'
print 'fun aset(n,v) {' + unroll(0,n-1,' <- v') + '}'
