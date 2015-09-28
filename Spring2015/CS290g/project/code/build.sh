echo
echo "[+] Building Sources"
gcc -o cache_timing -O0 cache_timing.c
gcc -o occupy_ca -O0 occupy_cache.c -lpthread
echo
echo "[+] Build Successful"
echo
echo ---------------------------------------------
echo
echo "[+] Run python get_cpu_cache_info.py to know information about your cache"
echo
echo ---------------------------------------------
echo
echo "[+] Run python l1_timing_attack.py to run the timing attack"
echo
echo ---------------------------------------------
echo
echo "[*] NOTE: Make sure that you have Intel i7-930 Processor"
echo
echo "[*] More Details, Contact: machiry@cs.ucsb.edu or var.kulkarni@gmail.com"
echo
