module Scripts

using ReadWriteFind


function generate_shell_mesh_from_stp_file(stp_source_path, stp_source_filename, job_name, save_path)

    function_file_path = "/Users/crismoen/Library/CloudStorage/GoogleDrive-cris.moen@runtosolve.com/Shared drives/RunToSolve/Projects/Nucor_Buildings_Group/CFSCB/tooling_study/mesh/mesh_scripts/base_mesh_script_general.py"
    part_name = "general_part"
    instance_name = "general_part-1"

    function_lines = ReadWriteFind.read_text_file(function_file_path)

    stp_filename = joinpath(stp_source_path, stp_source_filename)

    call_lines = ["stp_filename = '$stp_filename'"; "job_name = '$job_name'"; "part_name = '$part_name'"; "instance_name = '$instance_name'"; "MeshPart(stp_filename, job_name, part_name, instance_name)"]

    lines = [function_lines; call_lines]

    save_filename = joinpath(save_path, job_name * ".py")
    ReadWriteFind.write_file(save_filename, lines)

end

function grab_connector_forces_from_odb(odb_source_path, odb_source_filename, output_save_path, output_save_filename, macro_name, macro_save_path)

    function_file_source = joinpath(@__DIR__, "assets/get_connector_macro_general.py")
    
    function_lines = ReadWriteFind.read_text_file(function_file_source)

    odb_filename = joinpath(odb_source_path, odb_source_filename)

    output_filename = joinpath(output_save_path, output_save_filename)

    call_lines = ["odb_filename = '$odb_filename'"; "output_filename = '$output_filename'"; "get_connector_forces(odb_filename, output_filename)"]

    lines = [function_lines; call_lines]

    save_filename = joinpath(macro_save_path, macro_name * ".py")
    ReadWriteFind.write_file(save_filename, lines)

end




end  #module