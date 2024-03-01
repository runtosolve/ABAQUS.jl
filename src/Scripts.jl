module Scripts

using ReadWriteFind


function generate_shell_mesh_from_stp_file(stp_source_path, stp_source_filename, job_name, save_path)

    function_file_path = "/Users/crismoen/Library/CloudStorage/GoogleDrive-cris.moen@runtosolve.com/Shared drives/RunToSolve/Projects/Nucor_Buildings_Group/CFSCB/tooling_study/mesh/mesh_scripts/base_mesh_script_general.py"
    part_name = "general_part"
    instance_name = "general_part-1"

    function_lines = ReadWriteFind.read_text_file(function_file_path)

    stp_filename = joinpath(stp_source_path, stp_source_filename)

    call_lines = ["stp_filename = '$stp_filename'"; "job_name = '$job_name'"; "part_name = '$part_name'"; "instance_name = '$instance_name'"; "MeshCFSCB(stp_filename, job_name, part_name, instance_name)"]

    lines = [function_lines; call_lines]

    save_filename = joinpath(save_path, job_name * ".py")
    ReadWriteFind.write_file(save_filename, lines)

end




end  #module