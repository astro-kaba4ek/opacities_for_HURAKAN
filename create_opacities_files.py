
import numpy as np

import subprocess
import os


# запускает из питона стороннюю программу 
def run_comm(command, path):
	subprocess.run(command, 
					encoding="utf-8", shell=True, cwd=f"{path}")
	

def create_setup(opac_path, meta_name):

	with open(f'../input_opacities/setup_{meta_name}.inp', 'w') as file:
		file.write("Mstar                0.6           MSun\n")
		file.write("Rstar                3.6           RSun\n")
		file.write("Tstar                4000.0        K\n")
		file.write("Tacc_fixed           10000.0       K\n")
		file.write("flare_profile_file   flare_profile_FUOri.inp\n")
		file.write("Mgas_ini             0.0066        MSun\n")
		file.write("dust2gas             1e-2\n")
		file.write("Rmin                 0.03          au\n")
		file.write("Rmax                 250.0         au\n")
		file.write("Rtap_in              1.0           au\n")
		file.write("Rtap_out             11.0          au\n")
		file.write("sigma_index          1.0\n")
		file.write("n_a                  1e4           cm-3\n")
		file.write("T_a                  50.0          K\n")
		file.write(f"opacfile1            '{opac_path}/opacity_PR_{meta_name}.txt'\n")
		file.write(f"opacfile2            '{opac_path}/opacity_nu_{meta_name}.txt'\n")
		file.write("dist                 402.0         pc\n")
		file.write("incl                 37.0          deg\n")
		file.write("useHD                .false.\n")
		file.write("kmax                 200\n")
		file.write("lstatic_RT           .false.\n")
		file.write("N_CPU                4\n")





with open("param_space.txt") as f:
	lines = f.readlines()

for l in lines:
	l_arr = l.split()[:-1]

	if l_arr[0] == "amax":
		amax0_micron = float(l_arr[1])
		amax_arr_micron = np.array(list(map(float, l_arr[2:])))
		amax0 = amax0_micron * 1e-4
		amax_arr = amax_arr_micron * 1e-4
	elif l_arr[0] == "frac1":
		frac10 = float(l_arr[1])
		frac1_arr = np.array(list(map(float, l_arr[2:])))
	elif l_arr[0] == "p":
		p0 = float(l_arr[1])
		p_arr = np.array(list(map(float, l_arr[2:])))




pwd_path = f"{os.getcwd()}"

run_comm("rm -rf build res", pwd_path)
run_comm("mkdir ../input_opacities", pwd_path)
run_comm("mkdir ../meta_output", pwd_path)
# run_comm("mkdir res_a_max", pwd_path)
run_comm("./compil.sh", pwd_path)

# path = pwd_path + "/build"



opac_path = "/".join(pwd_path.split("/")[:-1]+["input_opacities"])

run_comm("rm -rf ../meta_names.txt", pwd_path)
meta_names_files = open("../meta_names.txt", "a")

for sca in ["no", "yes"]:

	meta_name = f"p{p0}f{frac10}amax{amax0_micron}sca{sca}"
	print("\n#", meta_name, ":")
	run_comm(f"./main --p {p0} --frac1 {frac10} --amax {amax0} --sca {sca}", pwd_path+"/build")
	run_comm(f"cp res/mix.txt ../input_opacities/opacity_nu_{meta_name}.txt", pwd_path)
	run_comm(f"cp res/PRmix.txt ../input_opacities/opacity_PR_{meta_name}.txt", pwd_path)

	create_setup(opac_path, meta_name)

	run_comm(f"mkdir ../meta_output/{meta_name}", pwd_path)
	meta_names_files.write(f"{meta_name}\n")


	for i, amax in enumerate(amax_arr):
		meta_name = f"p{p0}f{frac10}amax{amax_arr_micron[i]}sca{sca}"
		print("\n#", meta_name, ":")
		run_comm(f"./main --p {p0} --frac1 {frac10} --amax {amax} --sca {sca}", pwd_path+"/build")
		run_comm(f"cp res/mix.txt ../input_opacities/opacity_nu_{meta_name}.txt", pwd_path)
		run_comm(f"cp res/PRmix.txt ../input_opacities/opacity_PR_{meta_name}.txt", pwd_path)

		create_setup(opac_path, meta_name)

		run_comm(f"mkdir ../meta_output/{meta_name}", pwd_path)
		meta_names_files.write(f"{meta_name}\n")


	for i, frac1 in enumerate(frac1_arr):
		meta_name = f"p{p0}f{frac1}amax{amax0_micron}sca{sca}"
		print("\n#", meta_name, ":")
		run_comm(f"./main --p {p0} --frac1 {frac1} --amax {amax0} --sca {sca}", pwd_path+"/build")
		run_comm(f"cp res/mix.txt ../input_opacities/opacity_nu_{meta_name}.txt", pwd_path)
		run_comm(f"cp res/PRmix.txt ../input_opacities/opacity_PR_{meta_name}.txt", pwd_path)

		create_setup(opac_path, meta_name)

		run_comm(f"mkdir ../meta_output/{meta_name}", pwd_path)
		meta_names_files.write(f"{meta_name}\n")

	for i, p in enumerate(p_arr):
		meta_name = f"p{p}f{frac10}amax{amax0_micron}sca{sca}"
		print("\n#", meta_name, ":")
		run_comm(f"./main --p {p} --frac1 {frac10} --amax {amax0} --sca {sca}", pwd_path+"/build")
		run_comm(f"cp res/mix.txt ../input_opacities/opacity_nu_{meta_name}.txt", pwd_path)
		run_comm(f"cp res/PRmix.txt ../input_opacities/opacity_PR_{meta_name}.txt", pwd_path)

		create_setup(opac_path, meta_name)

		run_comm(f"mkdir ../meta_output/{meta_name}", pwd_path)
		meta_names_files.write(f"{meta_name}\n")

meta_names_files.close()