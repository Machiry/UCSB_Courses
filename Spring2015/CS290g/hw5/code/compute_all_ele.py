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

def get_residue_point(target,mod):
    sol = (target**((mod-1)/2))%mod
    # check if its a quadratic residue
    if sol == 1:
        # if yes, 
        #assert((mod+1)%4 == 0)
        return (target**((mod+1)/4))%mod
    else:
        return None

print_s = ''
mod = 29
order = 0
for i in range(mod):
    print_s = str(i) + ' & '
    y = i*i*i - 3*i + 4
    #y = i*i*i + i + 1
    y = y % mod
    print_s += str(y) + ' & '
    y_s = None
    if y == 0 or y == 1:
        y_s = y
        if y == 1:        
            print_s += '$\pm'+str(y_s)+'$ & '
        else:
            print_s += '0 &'
    elif is_square(y):
        y_s = int(math.sqrt(y))
        y_s = y_s % mod
        print_s += '$\pm'+str(y_s)+'$ & '
    else:
        resi = get_residue_point(y, mod)
        if resi is None:
            print_s += '- & '
        else:
            y_s = resi
            print_s += '$\pm'+str(y_s) +'$ & '
    if y_s is not None:
        if y_s > 0:
            order += 2
            print_s += '('+str(i)+','+str(y_s)+'), (' + str(i) + ',' + str(mod-y_s) +') \\\\' + '\n'
        else:
            order += 1
            print_s += '('+str(i)+','+str(y_s)+') \\\\' + '\n'
    else:
        print_s += ' - \\\\\n'
    print_s += '\\hline'
    print print_s

print '\n\n Order of ECG:' + str(order+1)
        
    
