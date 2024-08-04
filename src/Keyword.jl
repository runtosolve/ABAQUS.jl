module Keyword

using Formatting, Printf


function BOUNDARY(node_set_name, degrees_of_freedom)

    lines = "*Boundary"

    fmt = "{:s}, {:d2}"

    for i in eachindex(degrees_of_freedom)

        lines = [lines; format(fmt, node_set_name, degrees_of_freedom[i])]

    end

    return lines

end


function BOUNDARY(node::Int, degrees_of_freedom)

    lines = "*Boundary"

    fmt = "{:d8}, {:d2}"

    for i in eachindex(degrees_of_freedom)

        lines = [lines; format(fmt, node, degrees_of_freedom[i])]

    end

    return lines

end

function BOUNDARY(node_set_name, degrees_of_freedom, displacement_magnitude)

    lines = "*Boundary"

    # fmt = "{:s}, {:d2},  , {:9.3f}"
    fmt = "%s, %o,  , %9.3f"
    

    for i in eachindex(degrees_of_freedom)

        # lines = [lines; format(fmt, node_set_name, degrees_of_freedom[i], displacement_magnitude)]
        lines = [lines; @sprintf "%s, %o,  , %9.3f" node_set_name  degrees_of_freedom[i]  displacement_magnitude]


    end

    return lines

end




function BUCKLE(num_modes, max_eigenvalue, num_vectors, max_iterations)

    lines = "*Buckle"

    if isempty(max_eigenvalue)
        fmt = "{:d5}, , {:d5}, {:d5}"
        lines = [lines; format(fmt, num_modes, num_vectors, max_iterations)]
    else
        fmt = "{:d5}, {:9.3f}, {:d5}, {:d5}"
        lines = [lines; format(fmt, num_modes, max_eigenvalue, num_vectors, max_iterations)]
    end


    return lines

end


function CLOAD(node_set_name, degree_of_freedom, magnitude)

    lines = "*Cload"

    fmt = "{:s}, {:d2}, {:7.4f}"

    lines = [lines; format(fmt, node_set_name, degree_of_freedom, magnitude)]

    return lines

end


function CONNECTOR_BEHAVIOR(name)

    fmt = "*Connector Behavior, name={:s}"

    lines = format(fmt, name)

    return lines

end

function CONNECTOR_ELASTICITY(component, magnitude)


    fmt = "*Connector Elasticity, component={:2d}"
    lines = format(fmt, component)

    fmt = "{:7.4E},"
    lines = [lines; format(fmt, magnitude)]

    return lines

end

function CONNECTOR_SECTION(elset, behavior, coordinate_system)

    fmt = "*Connector Section, elset={:s}, behavior={:s}"
    lines = format(fmt, elset, behavior)

    fmt = "{:s},"
    lines = [lines; format(fmt, coordinate_system)]

    return lines

end


function CONNECTOR_SECTION(elset, coordinate_system)

    fmt = "*Connector Section, elset={:s}"
    lines = format(fmt, elset)

    fmt = "{:s},"
    lines = [lines; format(fmt, coordinate_system)]

    return lines

end



function CONTACT()

    lines = "*Contact"

end

function CONTACT_INCLUSIONS(all_exterior)

    if all_exterior == true

        lines = "*Contact Inclusions, ALL EXTERIOR"

    end

    return lines

end

function CONTACT_PROPERTY_ASSIGNMENT(surface_name_1, surface_name_2, surface_interaction_name)

    lines = "*Contact Property Assignment"

    fmt = "{:s}, {:s}, "

    line = format(fmt, surface_name_1, surface_name_2)
    line_end = @sprintf("\"%s\"", surface_interaction_name)

    lines = [lines; line * line_end]

    return lines

end


function CONTROLS_RESET()

    lines = "*Controls, reset"

end

# function CONTROLS_FIELD(displacement_tolerance, rotation_tolerance)

#         lines = "*Controls, parameters=field, field=displacement"
        
#         fmt = "{:9.5f}, {:9.5f}, , , , , ,"
#         lines = [lines; format(fmt, displacement_tolerance, rotation_tolerance)]
    
#     return lines 

# end


function CONTROLS_FIELD(residual_tolerance, correction_tolerance, field_type)

    fmt = "*Controls, parameters=field, field={:s}"
    lines = format(fmt, field_type)
    
    fmt = "{:9.5f}, {:9.5f}, , , , , ,"
    lines = [lines; format(fmt, residual_tolerance, correction_tolerance)]

return lines 

end



function CONTROLS_LINE_SEARCH(num_iterations)

    lines = "*Controls, parameters=line search"
    
    fmt = "{:d2}, , , ,"
    lines = [lines; format(fmt, num_iterations)]

return lines 

end


function DENSITY(ρ)

    fmt = "{:9.6f},"

    lines = "*Density"

    lines = [lines; format(fmt, ρ)]

    return lines

end


function DLOAD(element_set_name, degree_of_freedom, magnitude)

    lines = "*Dload"

    fmt = "{:s}, {:d2}, {:7.4f}"

    lines = [lines; format(fmt, node_set_name, degree_of_freedom, magnitude)]

    return lines

end


function DSLOAD(follower, constant_resultant, surface_name, load_type, load_magnitude, load_direction)

    lines = @sprintf "*Dsload, follower=%s, constant resultant=%s" follower constant_resultant
    lines = [lines; @sprintf "%s, %s, %7.4f, %7.4f, %7.4f, %7.4f" surface_name load_type load_magnitude load_direction[1] load_direction[2] load_direction[3]]

    return lines

end



function ELASTIC(E, ν)

    fmt = "{:9.6f}, {:9.6f}"

    lines = "*Elastic"

    lines = [lines; format(fmt, E, ν)]

    return lines

end




#need to add multiple dispatch here to cover other element types
function ELEMENT(elements, type, nodes_per_element)   

    lines = Matrix{String}(undef, size(elements)[1]+1, 1)

    if nodes_per_element == 4

        # fmt = "{:7d},{:7d},{:7d},{:7d},{:7d}"

        lines[1] = "*Element, type=" * type

        for i=1:size(elements)[1]

            # lines[i+1] = format(fmt, elements[i,1], elements[i,2], elements[i,3], elements[i,4],
            #                     elements[i,5])
            lines[i+1] = @sprintf "%7d,%7d,%7d,%7d,%7d" elements[i,1] elements[i,2] elements[i,3] elements[i,4] elements[i,5]
            #                  
        end

    elseif nodes_per_element == 3

        # fmt = "{:7d},{:7d},{:7d},{:7d}"

        lines[1] = "*Element, type=" * type

        for i=1:size(elements)[1]

            # lines[i+1] = format(fmt, elements[i,1], elements[i,2], elements[i,3], elements[i,4])
   
            lines[i+1] = @sprintf "%7d,%7d,%7d,%7d" elements[i,1] elements[i,2] elements[i,3] elements[i,4]
                           
        end

    elseif nodes_per_element == 8

        # fmt = "{:7d},{:7d},{:7d},{:7d},{:7d}{:7d}{:7d}{:7d}{:7d}"

        lines[1] = "*Element, type=" * type

        for i=1:size(elements)[1]

            # lines[i+1] = format(fmt, elements[i,1], elements[i,2], elements[i,3], elements[i,4],
                                # elements[i,5], elements[i,6], elements[i,7], elements[i,8], elements[i,9])
            lines[i+1] = @sprintf "%7d,%7d,%7d,%7d,%7d,%7d,%7d,%7d,%7d" elements[i,1] elements[i,2] elements[i,3] elements[i,4] elements[i,5] elements[i,6] elements[i,7] elements[i,8] elements[i,9]
                           
        end

    end

    return lines 

end


function ELEMENT_OUTPUT(directions, fields)

    fmt = "*Element Output, directions={:s}"
    lines = format(fmt, directions)

    line = ""
    for i = 1:size(fields)[1]
        
        if i == size(fields)[1]
            line = line * fields[i]
        else
            line = line * fields[i] * ", " 
        end

    end

    lines = [lines; line]

    return lines

end

function ELEMENT_OUTPUT(directions, fields, elset)

    fmt = "*Element Output, elset={:s}, directions={:s}"
    lines = format(fmt, elset, directions)

    line = ""
    for i in eachindex(fields)
        
        if i == size(fields)[1]
            line = line * fields[i]
        else
            line = line * fields[i] * ", " 
        end

    end

    lines = [lines; line]

    return lines

end


function ELSET(elements, name)

    #Define number of elements in set.
    num_elements=size(elements)[1]

    #Figure out the number of rows in the set.
    residual = num_elements/16 - floor(Int, num_elements/16)

    if residual == 0.0

        num_rows = floor(Int, num_elements/16)

    else

        num_rows = floor(Int, num_elements/16) + 1
        elements = [elements; zeros(Int, 16 - (num_elements - (num_rows - 1) * 16))]

    end

    lines = "*Elset, elset=" * name

    for i=1:num_rows

        #Define the element list formatting.
        fmt = "{:7d},{:7d},{:7d},{:7d},{:7d},{:7d},{:7d},{:7d},{:7d},{:7d},{:7d},{:7d},{:7d},{:7d},{:7d},{:7d}"

        range_start = (i-1) * 16 + 1
        range_end = i * 16  
        range = range_start:range_end

        lines = [lines; format(fmt, elements[range[1]], elements[range[2]], elements[range[3]], elements[range[4]], elements[range[5]], elements[range[6]], elements[range[7]], elements[range[8]], elements[range[9]], elements[range[10]], elements[range[11]], elements[range[12]], elements[range[13]], elements[range[14]], elements[range[15]], elements[range[16]])]

    end


    if residual != 0.0

        index = findfirst(" 0", lines[end])[1]
        lines[end] = lines[end][1:index - 7]

    end
        
    return lines

end


function EL_PRINT(variables, elset_name)

    fmt = "*El Print, elset={:s}"
    lines = format(fmt, elset_name)

    line = ""
    for i in eachindex(variables)
        
        if i == size(variables)[1]
            line = line * variables[i]
        else
            line = line * variables[i] * ", " 
        end

    end

    lines = [lines; line]

    return lines

end

function FASTENER(name, property, reference_node_set, elset, coupling, attachment_method, weighting_method, adjust_orientation, number_of_layers, search_radius, radius_of_influence, projection_direction)

    fmt = "*Fastener, interaction name={:s}, property={:s}, reference node set={:s}, elset={:s}, coupling={:s}, attachment method={:s}, weighting method={:s}, adjust orientation={:s},"

    lines = format(fmt, name, property, reference_node_set, elset, coupling, attachment_method, weighting_method, adjust_orientation)

    fmt = "number of layers={:2d},"
    lines = [lines; format(fmt, number_of_layers)]

    fmt = "search radius={:9.5f},"
    lines = [lines; format(fmt, search_radius)]

    fmt = "radius of influence={:9.5f}"
    lines = [lines; format(fmt, radius_of_influence)]

    fmt = "{:7.4f}, {:7.4f}, {:7.4f}"
    lines = [lines; format(fmt, projection_direction[1], projection_direction[2], projection_direction[3])]

    return lines

end


function FASTENER(name, property, reference_node_set, elset, coupling, attachment_method, weighting_method, adjust_orientation, number_of_layers, radius_of_influence, projection_direction)

    fmt = "*Fastener, interaction name={:s}, property={:s}, reference node set={:s}, elset={:s}, coupling={:s}, attachment method={:s}, weighting method={:s},"

    lines = format(fmt, name, property, reference_node_set, elset, coupling, attachment_method, weighting_method)

    fmt = "adjust orientation={:s},"
    lines = [lines; format(fmt, adjust_orientation)]

    fmt = "number of layers={:2d},"
    lines = [lines; format(fmt, number_of_layers)]

    fmt = "radius of influence={:9.5f}"
    lines = [lines; format(fmt, radius_of_influence)]

    fmt = "{:7.4f}, {:7.4f}, {:7.4f}"
    lines = [lines; format(fmt, projection_direction[1], projection_direction[2], projection_direction[3])]

    return lines

end


function FASTENER_PROPERTY(name, radius)

    lines = "*Fastener Property, name=" * name
    
    fmt = "{:9.5f}"
    lines = [lines; format(fmt, radius)]

    return lines
end




function FRICTION(slip_tolerance, friction_coeff)

    fmt = "*Friction, slip tolerance={:7.5f}"
    lines = format(fmt, slip_tolerance)
    
    fmt = "{:7.5f},"
    lines = [lines; format(fmt, friction_coeff)]

    return lines

end

function HEADING(heading_lines)

    lines = Matrix{String}(undef, size(heading_lines)[1]+1, 1)

    lines[1] = "*Heading"

    for i in eachindex(heading_lines)

        lines[i+1] = "**" * heading_lines[i]

    end

    return lines

end


function INSTANCE(instance_name, part_name, offset_coordinates)
    
    lines = @sprintf "*Instance, name=%s, part=%s" instance_name part_name

    lines = [lines; @sprintf "%7.4f, %7.4f, %7.4f" offset_coordinates[1] offset_coordinates[2] offset_coordinates[3]]

    lines = [lines; "*End Instance"]
 

    # fmt = "*Instance, name={:s}, part={:s}"
    # lines = format(fmt, instance_name, part_name)

    # fmt = "{:7.4f}, {:7.4f}, {:7.4f}"
    # lines = [lines; format(fmt, offset_coordinates[1], offset_coordinates[2], offset_coordinates[3])]

    # lines = [lines; "*End Instance"]

    return lines

end


function INSTANCE(instance_name, part_name, offset_coordinates, point_a_coordinates, point_b_coordinates, rotation_angle_a_b)

    lines = @sprintf "*Instance, name=%s, part=%s" instance_name part_name

    lines = [lines; @sprintf "%7.4f, %7.4f, %7.4f" offset_coordinates[1] offset_coordinates[2] offset_coordinates[3]]

    lines = [lines; @sprintf "%7.4f, %7.4f, %7.4f, %7.4f, %7.4f, %7.4f, %7.4f" point_a_coordinates[1] point_a_coordinates[2] point_a_coordinates[3] point_b_coordinates[1] point_b_coordinates[2] point_b_coordinates[3] rotation_angle_a_b]

    lines = [lines; "*End Instance"]
 

    # fmt = "*Instance, name={:s}, part={:s}"
    # lines = format(fmt, instance_name, part_name)

    # fmt = "{:7.4f}, {:7.4f}, {:7.4f}"
    # lines = [lines; format(fmt, offset_coordinates[1], offset_coordinates[2], offset_coordinates[3])]

    # lines = [lines; "*End Instance"]

    return lines

end



function MATERIAL(name)

    lines = "*Material, name=" * name

end


function NODE(nodes)

 
    lines = Matrix{String}(undef, size(nodes)[1]+1, 1)

    lines[1] = "*Node"

    fmt = "{:14d},{:14.8f},{:14.8f},{:14.8f}"

    for i=1:size(nodes)[1]
        lines[i+1] = format(fmt, nodes[i, 1], nodes[i, 2], nodes[i, 3], nodes[i, 4])
    end

    return lines

end


function NODE_OUTPUT(fields)

    lines = "*Node Output"

    line = ""
    for i in eachindex(fields)
        
        if i == size(fields)[1]
            line = line * fields[i]
        else
            line = line * fields[i] * ", " 
        end

    end

    lines = [lines; line]

    return lines

end


function NODE_PRINT(variables, nset_name)

    fmt = "*Node Print, nset={:s}"
    lines = format(fmt, nset_name)

    line = ""
    for i in eachindex(variables)
        
        if i == size(variables)[1]
            line = line * variables[i]
        else
            line = line * variables[i] * ", " 
        end

    end

    lines = [lines; line]

    return lines

end

function NSET(nodes, name)

    #Define number of nodes in set.
    num_nodes=size(nodes)[1]

    #Figure out the number of rows in the set.
    residual = num_nodes/16 - floor(Int, num_nodes/16)

    if residual == 0.0

        num_rows = floor(Int, num_nodes/16)

    else

        num_rows = floor(Int, num_nodes/16) + 1
        nodes = [nodes; zeros(Int, 16 - (num_nodes - (num_rows - 1) * 16))]

    end

    lines = "*Nset, nset=" * name

    for i=1:num_rows

        #Define the node list formatting.
        fmt = "{:7d},{:7d},{:7d},{:7d},{:7d},{:7d},{:7d},{:7d},{:7d},{:7d},{:7d},{:7d},{:7d},{:7d},{:7d},{:7d}"

        range_start = (i-1) * 16 + 1
        range_end = i * 16  
        range = range_start:range_end

        lines = [lines; format(fmt, nodes[range[1]], nodes[range[2]], nodes[range[3]], nodes[range[4]], nodes[range[5]], nodes[range[6]], nodes[range[7]], nodes[range[8]], nodes[range[9]], nodes[range[10]], nodes[range[11]], nodes[range[12]], nodes[range[13]], nodes[range[14]], nodes[range[15]], nodes[range[16]])]

    end


    if residual != 0.0

        index = findfirst(" 0", lines[end])[1]
        lines[end] = lines[end][1:index - 7]

    end
        
    return lines

end


function NSET(name, nset_names::Vector{String})

    #Define number of groups in set.
    num_sets=size(nset_names)[1]

    #Figure out the number of rows in the set.
    residual = num_sets/4 - floor(Int, num_sets/4)

    if residual == 0.0

        num_rows = floor(Int, num_sets/4)

    else

        num_rows = floor(Int, num_sets/4) + 1
        nset_names = [nset_names; fill("", 4 - (num_sets - (num_rows - 1) * 4))]

    end

    lines = "*Nset, nset=" * name

    for i=1:num_rows

        #Define the node list formatting.
        fmt = "{:s},{:s},{:s},{:s}"

        range_start = (i-1) * 4 + 1
        range_end = i * 4  
        range = range_start:range_end

        lines = [lines; format(fmt, nset_names[range[1]], nset_names[range[2]], nset_names[range[3]], nset_names[range[4]])]

    end


    # if residual != 0.0

    #     index = findfirst("", lines[end])[1]
    #     lines[end] = lines[end][1:index - 7]

    # end
        
    return lines

end







# function NSET(name, range)

#     lines = "*Nset, nset=" * name * ", generate"

#     fmt = "{:7d},{:7d},{:7d}"

#     lines = [lines; format(fmt, range[1], range[2], range[3])]

#     return lines

# end


function OUTPUT(field_or_history, variable)

    if isempty(variable)
        fmt = "*Output, {:s}"
        lines = format(fmt, field_or_history)
    else
        fmt = "*Output, {:s}, variable={:s}"
        lines = format(fmt, field_or_history, variable)
    end

    return lines

end


function PART(name, node_lines, element_lines, nset_lines, elset_lines, section_lines)

    lines = "*Part, name=" * name

    lines = [lines; node_lines; element_lines; nset_lines; elset_lines; section_lines; "*End Part"]

    return lines

end

function PLASTIC(curve)

    fmt = "{:9.6f}, {:9.6f}"
    lines = Matrix{String}(undef, size(curve)[1]+1, 1)

    lines[1] = "*Plastic"

    for i = 1:size(curve)[1]
        lines[i+1] = format(fmt, curve[i, 1], curve[i, 2])
    end

    return lines

end


function PREPRINT(echo, model, history, contact)

    lines = "*Preprint, echo=" * echo * ", model=" * model * ", history=" * history * ", contact=" * contact

end

function RESTART(read_or_write, frequency)

    fmt = "*Restart, {:s}, frequency={:2d}"
    lines = format(fmt, read_or_write, frequency)

    return lines

end

function RIGID_BODY(ref_node, pin_or_tie, nset_name)

    lines = "*Rigid Body, ref node=" * string(ref_node) * ", " * pin_or_tie * " nset=" * nset_name

end

function RIGID_BODY(ref_node::String, pin_or_tie, nset_name)

    lines = "*Rigid Body, ref node=" * ref_node * ", " * pin_or_tie * " nset=" * nset_name

end

function SHELL_SECTION(elset_name, material_name, offset, t, num_integration_points)

    start_line = "*Shell Section, elset=" * elset_name * ", material=" * material_name * ", offset=" 
    
    if typeof(offset) == String
        fmt = "{:s}{:s}"
    elseif typeof(offset) == Float64
        fmt = "{:s}{:2.1f}"
    end

    lines = format(fmt, start_line, offset)

    fmt = "{:7.4f},{:2d}"
    lines = [lines; format(fmt, t, num_integration_points)]

    return lines

end

function SOLID_SECTION(elset_name, material_name)

    lines = "*Solid Section, elset=" * elset_name * ", material=" * material_name
    
    return lines

end


function STATIC(stabilize, allsdtol, continue_flag, initial_time_increment, step_time_period, minimum_time_increment, maximum_time_increment)

    fmt = "*Static, stabilize={:7.5f}, allsdtol={:7.5f}, continue={:s}"
    lines = format(fmt, stabilize, allsdtol, continue_flag)

    fmt = "{:7.4f},{:7.4f}, {:7.4E},{:7.4f}"
    lines = [lines; format(fmt, initial_time_increment, step_time_period, minimum_time_increment, maximum_time_increment)]

end


function STEP(name, nlgeom, inc::Int)

    fmt = "*Step, name={:s}, nlgeom={:s}, inc={:d16}"

    lines = format(fmt, name, nlgeom, inc)

end

function STEP(name, nlgeom, perturbation::String)

    fmt = "*Step, name={:s}, nlgeom={:s}, {:s}"

    lines = format(fmt, name, nlgeom, perturbation)

end

function STEP(name, nlgeom, inc::Int, convert_SDI)

    fmt = "*Step, name={:s}, nlgeom={:s}, inc={:d16}, convert SDI={:s}"

    lines = format(fmt, name, nlgeom, inc, convert_SDI)

end

function SURFACE(surface_type, surface_name, elset_name, surface_face)

    lines = @sprintf "*Surface, type=%s, name=%s" surface_type surface_name
    lines = [lines; @sprintf "%s, %s" elset_name surface_face]

    return lines

end


function SURFACE_BEHAVIOR(pressure_overclosure)

    fmt = "*Surface Behavior, pressure-overclosure={:s}"
    lines = format(fmt, pressure_overclosure)

    return lines

end


function SURFACE_INTERACTION(name, surface_out_of_plane_thickness)

    # lines = "*Surface Interaction, " * name

    lines = @sprintf("*Surface Interaction, name=\"%s\"", name)

    fmt = "{:7.5f},"

    lines = [lines; format(fmt, surface_out_of_plane_thickness)]

    return lines

end




end #module