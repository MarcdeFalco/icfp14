import sys
w = int(sys.argv[1])
h = int(sys.argv[2])

print '#'*w
for i in range(h/2 - 2):
    print '#' + '.'*(w-2) + '#'
    print '#' + '.#'*((w-2)/2) + '#'
print '#'*w
