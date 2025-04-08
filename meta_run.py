

import subprocess
import os


# запускает из питона стороннюю программу 
def run_comm(command, path):
	subprocess.run(command, 
					encoding="utf-8", shell=True, cwd=f"{path}")
	




pwd_path = f"{os.getcwd()}"

run_comm("rm -rf input_opacities meta_output meta_names.txt ", pwd_path)

print("")
print("# Creating files of opacities in parameter space:")
run_comm("cd opacities_for_HURAKAN && python3 ./create_opacities_files.py", pwd_path)


with open("meta_names.txt") as f:
	meta_names = f.readlines()

print("")
for name in meta_names:
	print(f"# HURAKAN Run with {name[:-1]}:")

	run_comm(f"cd hurakan && time ./Run.sh -i {pwd_path}/input_opacities/setup_{name[:-1]}.inp", pwd_path)

	run_comm(f"cp -r hurakan/figures meta_output/{name[:-1]}", pwd_path)
	run_comm(f"cp -r hurakan/output_data meta_output/{name[:-1]}", pwd_path)



