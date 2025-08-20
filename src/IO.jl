module IO

using ReadWriteFind, DelimitedFiles, DataFrames


write_file(save_filename, lines) = ReadWriteFind.write_file(save_filename, lines)


function parse_nodal_output(data_lines, data_width)

    data = Matrix{Union{Float64, Int64}}(undef, (size(data_lines)[1], data_width))

    for i in eachindex(data_lines)
        io = split(data_lines[i][1:end], " ")
        io = filter(x->x != "", io)

        for j in eachindex(io)
            if j == 1
                data[i, j] = parse(Int, io[j])
            else
                data[i, j] = parse(Float64, io[j])
            end
        end
    end

    return data

end


function get_number_of_steps_in_analysis(lines, result_type)

    # "N O D E   O U T P U T"
    data_type_search_name = result_type
    step_line_numbers = ReadWriteFind.find_phrase_in_string_chunk(lines, data_type_search_name)
    num_steps = length(step_line_numbers)

    return num_steps

end


function get_nodal_output(lines, step_number, search_names, line_offsets, data_width, result_type)

    # "N O D E   O U T P U T"
    data_type_search_name = result_type
    step_line_numbers = ReadWriteFind.find_phrase_in_string_chunk(lines, data_type_search_name)

    string_chunk = lines[step_line_numbers[step_number]:end]
    data_lines = ReadWriteFind.get_specific_string_chunk(string_chunk, search_names[1], line_offsets[1], search_names[2], line_offsets[2])
    data = parse_nodal_output(data_lines, data_width)

    return data

end

function write_model_qsub_batch_file(inp_folder, model_details, batch_filename)

    file_list = readdir(joinpath(inp_folder, model_details))

    file_list_ext = [file_list[i][end-2:end] for i in eachindex(file_list)]


    index = findall(fileext -> fileext == "sub", file_list_ext)

    lines = []
    for i in eachindex(index)

        push!(lines, "qsub " * file_list[index[i]])

    end

    ReadWriteFind.write_file(joinpath(inp_folder, model_details, batch_filename), lines)

end




function get_sta(filename, file_path)


    data = DelimitedFiles.readdlm(joinpath(file_path, filename))
    data = data[6:end, 1:end-3]

    index = findall(inc -> typeof(inc)==SubString{String}, data[:, 3]) #remove U rows 

    keep_index = setdiff(1:size(data)[1], index)

    data = data[keep_index, :]

    column_names = [:step, :increment, :attempts, :severe_discontinuity_iterations, :equilibrium_iterations, :total_iterations, :total_time, :step_time, :time_increment]
    sta = DataFrame(data, column_names)

    for i=1:6
        sta[!, column_names[i]] = Vector{Int64}(sta[!, column_names[i]])
    end

    for i = 7:9
        sta[!, column_names[i]] = Vector{Float64}(sta[!, column_names[i]])
    end

    return sta

end


function get_buckling_loads_from_dat(file_path, filename)

    lines = ReadWriteFind.read_text_file(joinpath(file_path, filename))

    target_string = "E I G E N V A L U E    O U T P U T "
    line_index = ReadWriteFind.find_target_line_in_text_file(target_string, lines)

    lines = lines[line_index:end]

    target_string = "                    E I G E N V A L U E    N U M B E R     1"
    line_index = ReadWriteFind.find_target_line_in_text_file(target_string, lines)

    lines = lines[11:line_index-5]

    buckling_loads = [parse(Float64, split(lines[i])[2]) for i in eachindex(lines)]

    return buckling_loads 

end

end #module