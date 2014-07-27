
for ar,n in [ ('map',256 * 8), ('map_dist',256), ('queue',1000) ]:
    s = ''
    for i in range(n):
        s +=  'let %s%d = 0 in ' % (ar,i)
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
            return '%s%d' % (ar,a) + base
    print 'fun %s_get(n) {' % ar + unroll(0,n-1,'') + '}'
    print 'fun %s_set(n,v) {' %ar + unroll(0,n-1,' <- v') + '}'
