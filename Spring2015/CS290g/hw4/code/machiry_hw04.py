import random
import sys
'''
    MonPro implementation that return sub_happened flag
'''
def mon_pro(aarg,barg,n_arg,n1_arg,r_arg):
    a = long(aarg)
    b = long(barg)
    r = r_arg
    n1 = n1_arg
    n = n_arg
    t = a*b
    m = (t*n1) % r
    
    u = (t + m*n)
    
    assert(u%r == 0)
    ret_val = -1
    u = u/r
    if u >= n:
        ret_val = u - n
    else:
        ret_val = u
    return ret_val

'''
    Extended Euclid algorithm.
'''
def eea(n,r):
    g0 = r
    g1 = n
    u0 = 1
    u1 = 0
    v0 = 0
    v1 = 1
    while not (g1 == 0):
        q = g0 / g1
        n_g0 = g1
        n_g1 = g0 - g1*q
        n_u0 = u1
        n_u1 = u0 - u1*q
        n_v0 = v1
        n_v1 = v0 - v1*q
        u0 = n_u0
        u1 = n_u1
        v0 = n_v0
        v1 = n_v1
        g0 = n_g0
        g1 = n_g1
        
    return (g0,u0,v0)
        
'''
    Gets r such that r = log2(n)+1
'''
def get_next_log_2(n):
    i = long("1")
    count = 0
    while(n > 0):
        n = n >> 1
        count = count + 1
    return (count, i<<count)


    
def mon_pro_expo(m_arg,e_arg,n_arg):
    n = long(n_arg)
    e = long(e_arg)
    m = long(m_arg)
    (_,r) = get_next_log_2(n)
    (gcd,r1,n1) = eea(n,r)
    n1 = -n1
    assert(gcd == 1)
    m1 = (m * r) % n
    c1 = r % n
    (k,_) = get_next_log_2(e)
    while k > 0:
        curr_b = (1 << (k-1)) & e
        c1 = mon_pro(c1,c1,n,n1,r)
        if curr_b:
            c1 = mon_pro(m1,c1,n,n1,r)
        k = k -1
    c = mon_pro(c1,1,n,n1,r)
    return c

def find_fermat_smallest_witness(n):
    i = 2
    min_witness = None
    min_liar = None
    while i < n:
        result = mon_pro_expo(i,long(n)-1,n)
        if (result != 1) and (min_witness == None):
            min_witness = i
        if (result == 1) and (min_liar == None):
            min_liar = i
        if min_witness and min_liar:
            break
        i = i + 1
    return (min_witness,min_liar)

def find_miller_smallest_witness(n):
    n_1 = long(n) - 1
    m = n_1
    k = 0
    #Find K
    while ((m&1) != 1):
        m = m >> 1
        k = k + 1
    #print 'Crossed'
    #print 'n-1=' + str(n_1)
    i = 2
    min_witness = None
    min_liar = None
    while i < n_1:
        #print 'Trying:' + str(i)
        fir = mon_pro_expo(i,m,n)
        j = 0
        is_witness = (fir != 1)
        while j <= k-1:
            sec = mon_pro_expo(i,(1<<j)*m,n)
            is_witness = is_witness and not (sec == -1 or ((sec+1)%long(n) == 0))
            j = j + 1
        if is_witness and (min_witness == None):
            min_witness = i
        if not is_witness and (min_liar == None):
            min_liar = i
        if min_witness and min_liar:
            break
        i = i + 1
    return (min_witness,min_liar)

def print_latex_table_header():
    print '\\begin{center}'
    print '\\begin{tabular}{|c|c|c|}'
    print '\\hline'
    print 'Target Number & Smallest Witness & Smallest Liar\\\\'
    print '\\hline'

def print_latex_table_footer(label):
    print '\\label{'+label+'}'
    print '\\end{tabular}'
    print '\\end{center}'

def check_smallest_prime_factor(n,i):
    target_num = [2,3,5,7,11,13,17,19]
    for curr_num in target_num:
        if n % curr_num == 0:
            return curr_num == i
    return False

numbers = ['41041', '62745', '63973', '75361', '101101', '126217', '172081', '188461', '278545', '340561', '449065', '552721', '656601', '658801', '670033', '748657', '838201', '852841', '997633', '1033669', '1082809', '1569457', '1773289', '2100901', '2113921', '2433601', '2455921']
print_latex = True
#Fermat Primality Testing
print 'Fermat\'s Primality Testing Results\\\\'
if print_latex:
    print_latex_table_header()
minimal_witness_property = True
for curr_num in numbers:
    (min_witness,min_liar) = find_fermat_smallest_witness(curr_num)
    if print_latex:
        print curr_num + ' & ' + str(min_witness) + ' & ' + str(min_liar) + '\\\\'
        print '\\hline'
    else:
        print curr_num + ',' + str(min_witness) + ',' + str(min_liar)
    minimal_witness_property = minimal_witness_property and check_smallest_prime_factor(long(curr_num),min_witness)

if print_latex:
    print_latex_table_footer('tab:fermat')
if minimal_witness_property:
    print 'ALL SMALLEST WITNESS ARE SMALLEST PRIME FACTORS OF CORRESPONDING NUMBERS'
else:
    print 'ALL SMALLEST WITNESS ARE PRIME NUMBERS'

#Miller-Rabin primality Testing
print '\nMiller-Rabin Primality Testing Results\\\\'
if print_latex:
    print_latex_table_header()
for curr_num in numbers:
    (min_witness,min_liar) = find_miller_smallest_witness(curr_num)
    if print_latex:
        print curr_num + ' & ' + str(min_witness) + ' & ' + str(min_liar) + '\\\\'
        print '\\hline'
    else:
        print curr_num + ',' + str(min_witness) + ',' + str(min_liar)
if print_latex:
    print_latex_table_footer('tab:miller')
