import sys
import math
def is_square(apositiveint):
    x = apositiveint / 2
    seen = set([x])
    while x * x != apositiveint:
        x = (x + (apositiveint / x)) / 2
        if x in seen: return False
        seen.add(x)
    return True
 
print_s = ''
for i in range(29):
    print_s = str(i) + ' & '
    y = i*i*i - 3*i + 4
    y = y % 29
    print_s += str(y) + ' & '
    y_s = None
    if y == 0 or y == 1:
        y_s = y
        print_s += '$\pm'+str(y_s)+'$ & '
    elif is_square(y):
        y_s = int(math.sqrt(y))
        y_s = y_s % 29
        print_s += '$\pm'+str(y_s)+'$ & '
    else:
        print_s += '- & '
    if y_s is not None:
        print_s += '('+str(i)+','+str(y_s)+'), (' + str(i) + ',' + str(29-y_s) +') \\\\' + '\n'
    else:
        print_s += ' - \\\\\n'
    print_s += '\\hline'
    print print_s
        
    
