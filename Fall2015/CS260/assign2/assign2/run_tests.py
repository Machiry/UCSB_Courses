#!/usr/bin/env python
import os

print '[*] Running Test Harness'
for curr_file in os.listdir('tests'):
    if not curr_file.endswith('lwnn'):
        print '[!] Ignoring:' + curr_file
        continue
    curr_file_path = os.path.join('tests', curr_file)
    print '[*] Running Test:' + curr_file
    exit_code = os.system('scala cs260.lwnn.concrete.interpreter.Concrete ' + curr_file_path)
    if exit_code == 0:
        print '[+] Test:' + curr_file + ' successful'
    else:
        print '[-] Test:' + curr_file + ' Failed'
    

