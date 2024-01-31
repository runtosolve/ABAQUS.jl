module Mesh

using ReadWriteFind

function get_nodes_from_file(mesh_filename, start_target, end_target)

    lines = ReadWriteFind.read_text_file(mesh_filename)

    target_string = start_target
    start_index = ReadWriteFind.find_target_line_in_text_file(target_string, lines)

    target_string = end_target
    end_index = ReadWriteFind.find_target_line_in_text_file(target_string, lines)

    node_lines = lines[start_index+1:end_index-1]

    node_geometry = Matrix{Float64}(undef, (size(node_lines)[1], 3))
    node_numbers = Vector{Int64}(undef, size(node_lines)[1])

    for i in eachindex(node_lines)
            
        io = split(node_lines[i][1:end], " ")
        io = filter(x->x != "", io)
        node_number = parse(Int, io[1][1:end-1])
        x = parse(Float64, io[2][1:end-1])
        y = parse(Float64, io[3][1:end-1])
        z = parse(Float64, io[4])

        node_geometry[i, :] = [x, y, z]
        node_numbers[i] = node_number

    end

    nodes = Union{Float64, Int64}[node_numbers node_geometry]

    return nodes

end


function get_elements_from_file(mesh_filename, start_target, end_target, nodes_per_element)

    lines = ReadWriteFind.read_text_file(mesh_filename)

    target_string = start_target
    start_index = ReadWriteFind.find_target_line_in_text_file(target_string, lines)

    target_string = end_target
    end_index = ReadWriteFind.find_target_line_in_text_file(target_string, lines)

    lines = lines[start_index+1:end_index-1]

    elements = Matrix{Int64}(undef, (size(lines)[1], nodes_per_element + 1))

    for i in eachindex(lines)
            
        io = split(lines[i][1:end], " ")
        io = filter(x->x != "", io)

        for j = 1:nodes_per_element + 1

            if j == nodes_per_element + 1
                elements[i, j] = parse(Int, io[j])
            else
                elements[i, j] = parse(Int, io[j][1:end-1])
            end

        end

    end

    return elements

end


end #module