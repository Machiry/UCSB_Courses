__author__ = 'machiry'
import os
import glob
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


cache_info = get_complete_cache_info()
level_info = defaultdict(list)
for curr_info in cache_info:
    level_info[curr_info.level].append(curr_info)

for curr_level in level_info:
    print 'Level ' + str(curr_level) + ' Info:\n'
    for curr_info in level_info[curr_level]:
        print str(curr_info)
    print '\n\n'
