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
    sub_happened = False
    ret_val = -1
    u = u/r
    if u >= n:
        ret_val = u - n
        sub_happened = True
    else:
        ret_val = u
    return (ret_val,sub_happened)

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


    
def mon_exp_attacker(m_arg,e_arg,n_arg,attack_arr):
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
        if not k in attack_arr:
            attack_arr[k] = {0:[],1:[]}
        curr_b = (1 << (k-1)) & e
        (c1,st) = mon_pro(c1,c1,n,n1,r)
        '''
            Attack:
            Here, we assume that curr_b is 1.
            Do, mon_pro and bin the messages accordingly.
        '''
        (c_temp,st_temp) = mon_pro(m1,c1,n,n1,r)
        if st_temp:
                attack_arr[k][1].append(m)
        else:
                attack_arr[k][0].append(m)
        if curr_b:
            (c1,st) = mon_pro(m1,c1,n,n1,r)
        k = k -1
    c = mon_pro(c1,1,n,n1,r)
    return c


'''
    Compute Mon Pro Exponent along with total number of subtractions happened 
'''
def mon_exp_compute_total_subtractions(m_arg,e_arg,n_arg,sub_dictionary):
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

        #Add to the number of subtractions
        if st:
            total_subs += 1

        if curr_b:
            (c1,st) = mon_pro(m1,c1,n,n1,r)

            #Add to the number of subtractions
            if st:
                total_subs += 1
        k = k -1
    (c,st) = mon_pro(c1,1,n,n1,r)

    #Add to the number of subtractions
    if st:
        total_subs += 1

    sub_dictionary[m] = total_subs        
    return c
    

'''
    Generate given number of random numbers or messages
'''
def generate_random_numbers(total_nums):
    nums = []
    for i in range(total_nums):
        nums.append(random.randint(1000,(1L<<16)))
    return nums

if len(sys.argv) != 3:
    print 'Usage:' + sys.argv[0] +' <exponent (or d)> <modules (or n)>'
    sys.exit(-1)

no_of_msgs = 10000
rand_msgs = generate_random_numbers(no_of_msgs)
d_arr = {}
nums_subs = {}
input_d = long(sys.argv[1])
input_n = long(sys.argv[2])
print 'Computing MonPro along with timings for: d=' + str(input_d) +', n=' + str(input_n) +' with no of messages = ' + str(no_of_msgs)
for curr_msg in rand_msgs:
    mon_exp_attacker(curr_msg,input_d,input_n,d_arr)
    mon_exp_compute_total_subtractions(curr_msg,input_d,input_n,nums_subs)

print 'Computing Differences for each bit of exponent'
difference_array = {}
for curr_b in d_arr:
    curr_bit = curr_b
    false_diffs = 0
    for curr_m in d_arr[curr_b][0]:
        false_diffs += nums_subs[curr_m]
    true_diffs = 0
    for curr_m in d_arr[curr_b][1]:
        true_diffs += nums_subs[curr_m]

    if true_diffs != 0 or false_diffs != 0:
        if len(d_arr[curr_b][0]) > 0:
            avg_false_diff =  false_diffs/float(len(d_arr[curr_b][0]))
        else:
            avg_false_diff = 0
        if len(d_arr[curr_b][1]) > 0:
            avg_true_diff = true_diffs/float(len(d_arr[curr_b][1]))
        else:
            avg_true_diff = 0

        difference = avg_true_diff-avg_false_diff
        difference_array[curr_bit] = difference

        #print str(curr_bit) + ':' + '''str(avg_false_diff) + ':'+str(avg_true_diff) + ':'+'''  str(avg_true_diff-avg_false_diff)
        #print str(curr_bit) + ':'+ str(avg_true_diff-avg_false_diff)
        
print '[+] Timing Attack Results:'
result = ''
for curr_bit in difference_array:
    if difference_array[curr_bit] > 1.0 or difference_array[curr_bit] < 0:
        print '    For bit:' + str(curr_bit) + ' difference:' + str(difference_array[curr_bit]) + ' Guessing bit to be 1'
        result = '1' + result
    else:
        print '    For bit:' + str(curr_bit) + ' difference:' + str(difference_array[curr_bit]) + ' Guessing bit to be 0'
        result = '0' + result

print '\n[+] Attack Predicted : ' + result
print '[+] Actual Value : ' + bin(input_d)[2:]
if result == bin(input_d)[2:]:
    print '\n\033[92m' + '\033[1m' + '++++++ VALUES MATCH: TIMING ATTACK SUCCESSFULL ++++++' + '\033[0m\n'
'''print str(nums_subs)
for curr_b in d_arr:
    print str(curr_b) + ':' + str(len(d_arr[curr_b][1])) + ':' + str(len(d_arr[curr_b][0]))'''
    
    
    
