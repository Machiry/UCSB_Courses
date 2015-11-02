#!/usr/bin/env python
import os

def run_tests(target_dir, check_fail=False):
    no_failed_tests = 0
    for curr_file in os.listdir(target_dir):
        if not curr_file.endswith('lwnn'):
            print '[!] Ignoring:' + curr_file
            continue
        curr_file_path = os.path.join(target_dir, curr_file)
        print '[*] Running Test:' + curr_file
        exit_code = os.system('scala cs260.lwnn.concrete.interpreter.Concrete ' + curr_file_path)
        if exit_code== 0 and (not check_fail):
            print '[+] Test:' + curr_file + ' successful'
        elif check_fail and exit_code != 0:
            print '[-] Test:' + curr_file + ' successful'
        else:
            print '[+] Test:' + curr_file + ' Failed'
            no_failed_tests += 1
    return no_failed_tests


print '[*] Running Test Harness'
print '[*] Running Positive Tests'
failed_tests = run_tests(os.path.join("tests", "positive"))
if failed_tests > 0:
    print '[-] Few Positive Tests Failed'
print '[*] Running Negative Tests:'
failed_tests_neg = run_tests(os.path.join("tests", "negative"), check_fail=True)
if failed_tests_neg > 0:
    print '[-] Few Negative Tests Failed'
if failed_tests + failed_tests_neg == 0:
    print '[+] ALL TESTS SUCCESSFULLY PASSED'
    

