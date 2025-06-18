using ABAQUS


save_path = "/Users/crismoen/.julia/dev/ABAQUS/test"
filename = "ding_uel.f"
uel_output_path = "/home/cdmoen/job/r0/uel_output/"
KPNT = 3
KSEC = 0
KORIENT = 1
KOUTPUT = 1

ABAQUS.Scripts.write_ding_connector_uel_f_file(save_path, filename, uel_output_path, KPNT, KSEC, KORIENT, KOUTPUT)
