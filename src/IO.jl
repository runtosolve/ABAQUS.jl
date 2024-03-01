module IO

using ReadWriteFind


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


function get_nodal_output(lines, step_number, search_names, line_offsets, data_width)

    data_type_search_name = "N O D E   O U T P U T"
    step_line_numbers = ReadWriteFind.find_phrase_in_string_chunk(lines, data_type_search_name)

    string_chunk = lines[step_line_numbers[step_number]:end]
    data_lines = ReadWriteFind.get_specific_string_chunk(string_chunk, search_names[1], line_offsets[1], search_names[2], line_offsets[2])
    data = parse_nodal_output(data_lines, data_width)

    return data

end



end #module