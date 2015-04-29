import random
def mon_pro(aarg,barg,n_arg,n1_arg,r_arg):
    a = long(aarg)
    b = long(barg)
    r = r_arg
    n1 = n1_arg
    n = n_arg
    t = a*b
    #print '\\\\'
    #print '$MonPro(' + str(a) + ',' + str(b) + ')$\\\\'
    #print 't = ' + str(a) + ' * ' + str(b) + ' = ' + str(t) + '\\\\'
    m = (t*n1) % r
    #print 'm = ' + str(t) + ' * ' + str(n1) + ' mod ' + str(r) + ' = ' + str(m) + '\\\\'
    
    u = (t + m*n)
    #if u%r != 0:
    #print str(u) + ':' + str(r)
    
    assert(u%r == 0)
    sub_happ = False
    ret_val = -1
    u = u/r
    if u >= n:
        #print 'u = ' + '((' + str(t) + ' + ' + str(m) + ' * ' + str(n) + ') / ' + str(r) +') - ' + str(n) + ' = ' + str(u-n) +'\\\\'
        ret_val = u - n
        sub_happ = True
        #print str(u - n)
    else:
        #print 'u = ' + '(' + str(t) + ' + ' + str(m) + ' * ' + str(n) + ') / ' + str(r) +' = ' +str(u) +'\\\\'
        ret_val = u
        sub_happ = False
        #print str(u)
    return (ret_val,sub_happ)

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
        

def get_next_log_2(n):
    i = long("1")
    count = 0
    while(n > 0):
        n = n >> 1
        count = count + 1
    return (count, i<<count)
    
def mon_exp(m_arg,e_arg,n_arg,d_arr):
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
        if not k in d_arr:
            d_arr[k] = {0:[],1:[]}
        curr_b = (1 << (k-1)) & e
        (c1,st) = mon_pro(c1,c1,n,n1,r)
        if curr_b:
            (c1,st) = mon_pro(m1,c1,n,n1,r)
            if st:
                d_arr[k][1].append(m)
            else:
                d_arr[k][0].append(m)
        k = k -1
    c = mon_pro(c1,1,n,n1,r)
    return c

def mon_exp_total(m_arg,e_arg,n_arg,d_arr):
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
    total_subs = 0
    while k > 0:
        curr_b = (1 << (k-1)) & e
        (c1,st) = mon_pro(c1,c1,n,n1,r)
        if st:
            total_subs += 1
        if curr_b:
            (c1,st) = mon_pro(m1,c1,n,n1,r)
            if st:
                total_subs += 1
        k = k -1
    (c,st) = mon_pro(c1,1,n,n1,r)
    if st:
        total_subs += 1
    d_arr[m] = total_subs        
    return c
    


def generate_random_numbers():
    nums = []
    for i in range(100):
        nums.append(random.randint(1000,(1L<<16)))
    return nums
d_arr = {}
'''print str(mon_exp(175,85,391,d_arr))
print str(mon_exp(175,87,391,d_arr))
print str(mon_exp(11,13,53,d_arr))'''
rand_msgs = generate_random_numbers()
d_arr = {}
nums_subs = {}
for curr_msg in rand_msgs:
    mon_exp(curr_msg,"85","391",d_arr)
    mon_exp_total(curr_msg,"85","391",nums_subs)

for curr_b in d_arr:
    curr_bit = curr_b
    false_diffs = 0
    for curr_m in d_arr[curr_b][0]:
        false_diffs += nums_subs[curr_m]
    true_diffs = 0
    for curr_m in d_arr[curr_b][1]:
        true_diffs += nums_subs[curr_m]

    if true_diffs != 0 or false_diffs != 0:
        avg_false_diff =  false_diffs/float(len(d_arr[curr_b][0]))
        avg_true_diff = true_diffs/float(len(d_arr[curr_b][1]))
        print str(curr_bit) + ':' + str(avg_false_diff) + ':'+str(avg_true_diff) + ':' + str(avg_true_diff - avg_false_diff)
        

print str(nums_subs)
for curr_b in d_arr:
    print str(curr_b) + ':' + str(len(d_arr[curr_b][1])) + ':' + str(len(d_arr[curr_b][0]))
    
    
    
