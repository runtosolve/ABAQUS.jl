
using ReadWriteFind 

model_name = "Verco_diaphragm_36-6_18g_r1"
model_remote_path_and_filename = "/enc/uprod_vNTzbg/storage_poDtd/Verco_Vulcraft/diaphragm_FEA_2025/FEA/model_runs/r1/Verco_diaphragm_36-6_18g_r1.inp"

save_path = @__DIR__
filename = "update_input_file_test.py"


function write_update_inp_macro(model_name, model_remote_path_and_filename, save_path, save_filename)

    function_file_source = joinpath(@__DIR__, "assets/update_inp_file.py")

    lines = ReadWriteFind.read_text_file(function_file_source)


    call_lines = ["model_name = '$model_name'"; "path_with_filename = '$model_remote_path_and_filename'"; "updateinp(model_name, path_with_filename)"]

    lines = [lines; call_lines]

    save_path_and_filename = joinpath(save_path, save_filename)
    ReadWriteFind.write_file(save_path_and_filename, lines)

end


# target_string = "**ModelName1**"
# index = ReadWriteFind.find_target_line_in_text_file(target_string, lines)
# lines[index] = "    del mdb.models['$model_name']"


# target_string = "**ModelName2**"
# index = ReadWriteFind.find_target_line_in_text_file(target_string, lines)
# lines[index] = "    mdb.ModelFromInputFile(name='$model_name', " 

# target_string = "**FileName**"
# index = ReadWriteFind.find_target_line_in_text_file(target_string, lines)
# lines[index] = "        inputFileName='$model_remote_path_and_filename')" 

# target_string = "**ModelName3**"
# index = ReadWriteFind.find_target_line_in_text_file(target_string, lines)
# lines[index] = "    a = mdb.models['$model_name'].rootAssembly" 

# lines = [lines; "updateinp(model_name, path_with_filename)"]

# save_filename = joinpath(save_path, filename)
# ReadWriteFind.write_file(save_filename, lines)