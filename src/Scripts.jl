module Scripts

using ReadWriteFind


function generate_shell_mesh_from_stp_file(stp_source_path, stp_source_filename, job_name, save_path, seed_size, stp_stitch_tolerance, maximum_deviation_factor, minimum_size_control_fraction)

    function_file_source = joinpath(@__DIR__, "assets/base_mesh_script_general.py")

    part_name = "general_part"
    instance_name = "general_part-1"

    function_lines = ReadWriteFind.read_text_file(function_file_source)

    stp_filename = joinpath(stp_source_path, stp_source_filename)

    call_lines = ["stp_filename = '$stp_filename'"; "job_name = '$job_name'"; "part_name = '$part_name'"; "instance_name = '$instance_name'"; "seed_size = $seed_size"; "stp_stitch_tolerance = $stp_stitch_tolerance"; "maximum_deviation_factor = $maximum_deviation_factor"; "minimum_size_control_fraction = $minimum_size_control_fraction"; "MeshPart(stp_filename, job_name, part_name, instance_name, seed_size, stp_stitch_tolerance, maximum_deviation_factor, minimum_size_control_fraction)"]

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


function write_mesh_bash_script(all_filenames, script_filename)

    lines = []

    for i in eachindex(all_filenames)

        push!(lines, "abaqus cae noGUI=" * all_filenames[i][1:end-2] * ".py")

    end

    filename = joinpath(@__DIR__, script_filename)
    ReadWriteFind.write_file(filename, lines)

end


end  #module