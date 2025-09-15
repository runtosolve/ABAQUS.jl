module Keyword

using Formatting, Printf



function AMPLITUDE(name, x, y)



    lines = @sprintf "*Amplitude, name=%s" name

 
    num_rows=size(x)[1]

    #Figure out the number of rows in the set.
    residual = num_rows/4 - floor(Int, num_rows/4)

    if residual == 0.0

        num_rows = floor(Int, num_rows/4)

    else

        num_rows = floor(Int, num_rows/4) + 1
        
    end


    for i=1:num_rows 


        if (i== num_rows) & (residual > 0.0)   

            residual_inputs = Int(residual * 4)
            x_last_row =  x[end-residual_inputs + 1:end]
            y_last_row = y[end-residual_inputs + 1:end]


            if residual_inputs == 1

                line = @sprintf "%9.5f,%9.5f" x_last_row[1] y_last_row[1]

            elseif residual_inputs == 2

                line = @sprintf "%9.5f,%9.5f,%9.5f,%9.5f" x_last_row[1] y_last_row[1] x_last_row[2] y_last_row[2]

            elseif residual_inputs == 3

                line = @sprintf "%9.5f,%9.5f,%9.5f,%9.5f,%9.5f,%9.5f" x_last_row[1] y_last_row[1] x_last_row[2] y_last_row[2] x_last_row[3] y_last_row[3]

            elseif residual_inputs == 3

                line = @sprintf "%9.5f,%9.5f,%9.5f,%9.5f,%9.5f,%9.5f,%9.5f,%9.5f" x_last_row[1] y_last_row[1] x_last_row[2] y_last_row[2] x_last_row[3] y_last_row[3] x_last_row[4] y_last_row[4]

            end

            lines = [lines; line]

        else

            range_start = (i-1) * 4 + 1
            range_end = i * 4  
            range = range_start:range_end
       
            line =  @sprintf "%9.5f,%9.5f,%9.5f,%9.5f,%9.5f,%9.5f,%9.5f,%9.5f" x[range[1]] y[range[1]] x[range[2]] y[range[2]] x[range[3]] y[range[3]] x[range[4]] y[range[4]]
            lines = [lines; line]

        end

    end


    # lines = [lines; line]

    return lines 

end


function BOUNDARY(node_set_name, degrees_of_freedom, op)

    if isempty(op)

        lines = "*Boundary"

    else

        lines = @sprintf"*Boundary, OP=%s" op

    end

    fmt = "{:s}, {:d2}"

    for i in eachindex(degrees_of_freedom)

        lines = [lines; format(fmt, node_set_name, degrees_of_freedom[i])]

    end

    return lines

end


function BOUNDARY(node::Int, degrees_of_freedom, op)

    # lines = "*Boundary"
    if isempty(op)

        lines = "*Boundary"

    else

        lines = @sprintf"*Boundary, OP=%s" op

    end


    fmt = "{:d8}, {:d2}"

    for i in eachindex(degrees_of_freedom)

        lines = [lines; format(fmt, node, degrees_of_freedom[i])]

    end

    return lines

end

function BOUNDARY(node_set_name, degrees_of_freedom, displacement_magnitude, op)

    # lines = "*Boundary"

    if isempty(op)

        lines = "*Boundary"

    else

        lines = @sprintf"*Boundary, OP=%s" op

    end

    # fmt = "{:s}, {:d2},  , {:9.3f}"
    fmt = "%s, %o,  , %9.3f"
    

    for i in eachindex(degrees_of_freedom)

        # lines = [lines; format(fmt, node_set_name, degrees_of_freedom[i], displacement_magnitude)]
        lines = [lines; @sprintf "%s, %o,  , %9.3f" node_set_name  degrees_of_freedom[i]  displacement_magnitude]


    end

    return lines

end


function BOUNDARY(node_set_name, degrees_of_freedom, displacement_magnitude, amplitude_table_name::String, op)


    if isempty(op)

        lines = @sprintf "*Boundary, amplitude=%s"  amplitude_table_name

    else

        lines = @sprintf "*Boundary, OP=%s, amplitude=%s"  op, amplitude_table_name

    end



    # lines = @sprintf "*Boundary, amplitude=%s"  amplitude_table_name

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



function BUCKLE(num_modes, min_eigenvalue, max_eigenvalue, block_size, max_num_block_steps)

    lines = "*Buckle, eigensolver=LANCZOS" 

    if isempty(block_size)
        lines = [lines; @sprintf "%o, %9.5f, %9.5f, ," num_modes min_eigenvalue max_eigenvalue]
    else
        lines = [lines; @sprintf "%o, %9.5f, %9.5f, %9.5f, %o" num_modes min_eigenvalue max_eigenvalue block_size max_num_block_steps]
    end

    # lines = [lines; line]

    return lines

end






function CLOAD(node_set_name, degree_of_freedom, magnitude)

    lines = "*Cload"

    fmt = "{:s}, {:d2}, {:7.4f}"

    lines = [lines; format(fmt, node_set_name, degree_of_freedom, magnitude)]

    return lines

end


function CONN3D2(element_number, node_i, node_j)

    lines = "*Element, type=CONN3D2"
    lines = [lines; @sprintf "%d, %d, %d" element_number node_i node_j]
    
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


function CONNECTOR_FRICTION(inputs)

    lines = "*Connector Friction, predefined"
    
    line = @sprintf "%9.5f, %9.5f, %9.5f, %9.5f" inputs[1] inputs[2] inputs[3] inputs[4]
    lines = [lines; line]

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



function CONNECTOR_SECTION(elset, behavior, type, orientation)

    lines = @sprintf "*Connector Section, elset=%s, behavior=%s" elset behavior

    line = @sprintf "%s," type 
    lines = [lines; line]

    line = @sprintf "\"%s\"," orientation
    lines = [lines; line]

    return lines

end


function CONSTRAINT_CONTROLS(print)

    lines = @sprintf "*CONSTRAINT controls, print=%s" print 

    return lines 

end



function CONTACT()

    lines = "*Contact"

end


function CONTROLS(field_type)

    fmt = "*Controls, parameters=field, field={:s}"
    lines = format(fmt, field_type)
    
    fmt = "{:9.5f}, {:9.5f}, , , , , ,"
    lines = [lines; format(fmt, residual_tolerance, correction_tolerance)]

return lines 

end

function CONTACT_CONTROLS(parameter)

    lines = @sprintf "*Contact Controls, %s" parameter
    
return lines 

end

function CONTACT_CONTROLS(parameter, damping_coeff, fraction_of_damping_at_end_of_step, clearance_at_which_damping_becomes_zero)

    lines = @sprintf "*Contact Controls, %s" parameter

    line = @sprintf "%s, %9.5f, %9.5f" damping_coeff fraction_of_damping_at_end_of_step clearance_at_which_damping_becomes_zero

    lines = [lines; line]
    
return lines 

end


function CONTACT_INCLUSIONS(all_exterior, surface_pairs)

    if all_exterior == true

        lines = "*Contact Inclusions, ALL EXTERIOR"

    else

        lines = "*Contact Inclusions"

        for i in eachindex(surface_pairs)
            lines = [lines; @sprintf "%s, %s" surface_pairs[i][1] surface_pairs[i][2]]
        end

    end

    return lines

end






function CONTACT_INITIALIZATION_ASSIGNMENT(surface_pairs, initialization_name)


        lines = "*Contact Initialization Assignment"

        for i in eachindex(surface_pairs)
            lines = [lines; @sprintf "%s, %s, %s" surface_pairs[i][1] surface_pairs[i][2] initialization_name[i]]
        end


    return lines

end




function CONTACT_INITIALIZATION_DATA(name, search_above, search_below)

    lines = @sprintf("*Contact Initialization Data, name=%s, SEARCH ABOVE=%9.5f, SEARCH BELOW=%9.5f", name, search_above, search_below)

end


function CONTACT_PAIR(interaction, type, surface_pair)

    lines = @sprintf("*Contact Pair, interaction=\"%s\", type=%s", interaction, type)
    line = @sprintf "%s, %s" surface_pair[1] surface_pair[2]
    lines = [lines; line]
    
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


function CONTACT_STABILIZATION(surface_pair)

    lines = "*Contact Stabilization"

    if !isempty(surface_pair)
        lines = [lines; @sprintf "%s, %s" surface_pair[1] surface_pair[2]]
    end

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


function CONTROLS_CONSTRAINTS(Tvol, Taxial, Ttshear, Tcont, Tsoft, Tdisp, Trot, Tcfe)

    lines = "*Controls, parameters=constraints"

    lines = [lines; @sprintf "%9.5f, %9.5f, %9.5f, %9.5f, %9.5f, %9.5f, %9.5f, %9.5f" Tvol Taxial Ttshear Tcont Tsoft Tdisp Trot Tcfe]

return lines 

end


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

function CONTROLS_TIME_INCREMENTATION(attempts_per_increment)

    lines = "*Controls, parameters=TIME INCREMENTATION"
    lines = [lines; @sprintf ", , , , , , , %2d" attempts_per_increment]

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


function DLOAD_GRAV(acceleration_magnitude, acceleration_direction)

    lines = "*Dload"

    lines = [lines; @sprintf ", GRAV, %7.4f, %7.4f, %7.4f, %7.4f" acceleration_magnitude acceleration_direction[1] acceleration_direction[2] acceleration_direction[3]]

    return lines

end



function DSLOAD(follower, constant_resultant, surface_name, load_type, load_magnitude, load_direction)

    lines = @sprintf "*Dsload, follower=%s, constant resultant=%s" follower constant_resultant
    lines = [lines; @sprintf "%s, %s, %7.4f, %7.4f, %7.4f, %7.4f" surface_name load_type load_magnitude load_direction[1] load_direction[2] load_direction[3]]

    return lines

end


function DSLOAD(surface_name, load_type, load_magnitude)

    lines = @sprintf "*Dsload"
    lines = [lines; @sprintf "%s, %s, %7.4f" surface_name load_type load_magnitude]

    return lines

end

function DYNAMIC(application, initial_time_increment, step_time_period, minimum_time_increment, maximum_time_increment)

        lines = @sprintf "*Dynamic, application=%s" application
        lines = [lines; @sprintf "%7.4f, %7.4f, %e, %7.4f" initial_time_increment step_time_period minimum_time_increment maximum_time_increment]
    
    return lines 

end   

function DYNAMIC_EXPLICIT(step_time_period, maximum_time_increment)

    lines = "*Dynamic, explicit" 
    lines = [lines; @sprintf ", %7.4f, , %7.4f" step_time_period maximum_time_increment]

return lines 

end   


function EL_FILE(elset, variable)

    lines = @sprintf "*El File, elset =%s" elset 
    lines = [lines; @sprintf "%s" variable]

    return lines 

end


function ELASTIC(E, ν)

    fmt = "{:9.6f}, {:9.6f}"

    lines = "*Elastic"

    lines = [lines; format(fmt, E, ν)]

    return lines

end

function ELEMENT_SPRING(elements, type, elset)

    lines = @sprintf "*Element, type= %s, elset= %s" type elset

    for i=1:size(elements)[1]

        lines = [lines; @sprintf "%1d, %s" elements[i, 1] elements[i, 2] ]

    end

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

    elseif nodes_per_element == 2

         
        lines[1] = "*Element, type=" * type

        for i=1:size(elements)[1]

            lines[i+1] = @sprintf "%7d,%7d,%7d" elements[i,1] elements[i,2] elements[i,3]
                           
        end

    elseif nodes_per_element == 8

        # fmt = "{:7d},{:7d},{:7d},{:7d},{:7d}{:7d}{:7d}{:7d}{:7d}"

        lines[1] = "*Element, type=" * type

        for i=1:size(elements)[1]

            # lines[i+1] = format(fmt, elements[i,1], elements[i,2], elements[i,3], elements[i,4],
                                # elements[i,5], elements[i,6], elements[i,7], elements[i,8], elements[i,9])
            lines[i+1] = @sprintf "%7d,%7d,%7d,%7d,%7d,%7d,%7d,%7d,%7d" elements[i,1] elements[i,2] elements[i,3] elements[i,4] elements[i,5] elements[i,6] elements[i,7] elements[i,8] elements[i,9]
                           
        end

    elseif nodes_per_element == 10

        # fmt = "{:7d},{:7d},{:7d},{:7d},{:7d}{:7d}{:7d}{:7d}{:7d}"

        lines[1] = "*Element, type=" * type

        for i=1:size(elements)[1]

            # lines[i+1] = format(fmt, elements[i,1], elements[i,2], elements[i,3], elements[i,4],
                                # elements[i,5], elements[i,6], elements[i,7], elements[i,8], elements[i,9])
            lines[i+1] = @sprintf "%7d,%7d,%7d,%7d,%7d,%7d,%7d,%7d,%7d,%7d,%7d" elements[i,1] elements[i,2] elements[i,3] elements[i,4] elements[i,5] elements[i,6] elements[i,7] elements[i,8] elements[i,9] elements[i,10] elements[i,11]
                           
        end

    end

    return lines 

end



#need to add multiple dispatch here to cover other element types
function ELEMENT(elements, type, nodes_per_element, elset_name)   

    lines = Matrix{String}(undef, size(elements)[1]+1, 1)

    if nodes_per_element == 2

         
        lines[1] = @sprintf "*Element, type=%s, elset=%s" type elset_name

        for i=1:size(elements)[1]

            lines[i+1] = @sprintf "%7d,%7d,%7d" elements[i,1] elements[i,2] elements[i,3]
                           
        end

    end
    

    return lines 

end


#need to add multiple dispatch here to cover other element types
function ELEMENT(element_number, node_i, node_j, type, nodes_per_element, elset_name)   

    lines = Matrix{String}(undef, size(element_number)[1]+1, 1)

    if nodes_per_element == 2

        lines[1] = @sprintf "*Element, type=%s, elset=%s" type elset_name

        for i=1:size(element_number)[1]

            lines[i+1] = @sprintf "%d,%s,%s" element_number[i] node_i[i] node_j[i]
                           
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


function EQUATION(num_of_equations, node_label_i, dof_i, magnitude_i, node_label_j, dof_j, magnitude_j)
    
    lines = "*Equation"

    for i in eachindex(node_label_i)

        line = @sprintf "%d" num_of_equations[i]
        lines = [lines; line]

        line = @sprintf "%s, %d, %7.4f, %s, %d, %7.4f" node_label_i[i] dof_i[i] magnitude_i[i] node_label_j[i] dof_j[i] magnitude_j[i]
        lines = [lines; line]

    end

    return lines

end



function FASTENER(name, property, reference_node_set, elset, coupling, attachment_method, weighting_method, adjust_orientation, number_of_layers, search_radius, projection_direction)

    fmt = "*Fastener, interaction name={:s}, property={:s}, reference node set={:s}, elset={:s}, coupling={:s}, attachment method={:s}, weighting method={:s}, "

    lines = format(fmt, name, property, reference_node_set, elset, coupling, attachment_method, weighting_method)

    # fmt = "number of layers={:2d},"
    # lines = [lines; format(fmt, number_of_layers)]

    fmt = "adjust orientation={:s}, number of layers={:2d}, search radius={:9.5f}"
    lines = [lines; format(fmt, adjust_orientation, number_of_layers, search_radius)]

    # fmt = "radius of influence={:9.5f},"
    # lines = [lines; format(fmt, radius_of_influence)]

    fmt = "{:7.4f}, {:7.4f}, {:7.4f}"
    lines = [lines; format(fmt, projection_direction[1], projection_direction[2], projection_direction[3])]

    return lines

end


# function FASTENER(name, property, reference_node_set, elset, coupling, attachment_method, weighting_method, adjust_orientation, number_of_layers, radius_of_influence, projection_direction)

#     fmt = "*Fastener, interaction name={:s}, property={:s}, reference node set={:s}, elset={:s}, coupling={:s}, attachment method={:s}, weighting method={:s},"

#     lines = format(fmt, name, property, reference_node_set, elset, coupling, attachment_method, weighting_method)

#     fmt = "adjust orientation={:s},"
#     lines = [lines; format(fmt, adjust_orientation)]

#     fmt = "number of layers={:2d},"
#     lines = [lines; format(fmt, number_of_layers)]

#     fmt = "radius of influence={:9.5f}"
#     lines = [lines; format(fmt, radius_of_influence)]

#     fmt = "{:7.4f}, {:7.4f}, {:7.4f}"
#     lines = [lines; format(fmt, projection_direction[1], projection_direction[2], projection_direction[3])]

#     return lines

# end


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


function FRICTION(friction_coeff)

    lines = "*Friction"
    
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


function KINEMATIC_COUPLING(ref_node, node_set_name, degrees_of_freedom)

    lines = @sprintf "*Kinematic Coupling, ref node=%d" ref_node


    for i in eachindex(degrees_of_freedom)

        lines = [lines; @sprintf "%s, %o, " node_set_name  degrees_of_freedom[i] ] 


    end

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


function ORIENTATION(name, local_x_axis, local_y_axis)

    lines = @sprintf "*Orientation, name=%s" name
    
    line = @sprintf "%9.5f, %9.5f, %9.5f, %9.5f, %9.5f, %9.5f" local_x_axis[1] local_x_axis[2] local_x_axis[3] local_y_axis[1] local_y_axis[2] local_y_axis[3]
    lines = [lines; line]

    line = @sprintf "1, 0."
    lines = [lines; line]

    return lines

end


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


function OUTPUT_TIME_POINTS(field_or_history, time_points_name)

        lines = @sprintf "*Output, %s, TIME POINTS=%s" field_or_history time_points_name
      
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


function SPRING(elset, dof, stiffness)

    lines = @sprintf "*Spring, elset=%s" elset
    lines = [lines; @sprintf "%1d, %1d" dof dof]
    lines = [lines; @sprintf "%7.4f" stiffness]

    return lines 

end

# function STATIC(stabilize, allsdtol, continue_flag, initial_time_increment, step_time_period, minimum_time_increment, maximum_time_increment)

#     if stabilize == true

#         fmt = "*Static, stabilize"
#         lines = format(fmt, stabilize, allsdtol, continue_flag)

#         fmt = "{:7.4f},{:7.4f}, {:7.4E},{:7.4f}"
#         lines = [lines; format(fmt, initial_time_increment, step_time_period, minimum_time_increment, maximum_time_increment)]
    
#     else

#         fmt = "*Static, stabilize={:7.5f}, allsdtol={:7.5f}, continue={:s}"
#         lines = format(fmt, stabilize, allsdtol, continue_flag)

#         fmt = "{:7.4f},{:7.4f}, {:7.4E},{:7.4f}"
#         lines = [lines; format(fmt, initial_time_increment, step_time_period, minimum_time_increment, maximum_time_increment)]

#     end

# end


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

function STEP(name, nlgeom)

    lines = @sprintf "*Step, name=%s, nlgeom=%s" name   nlgeom

end

#shell element based surface 
function SURFACE(surface_type, surface_name, elset_name, surface_face::String)

    lines = @sprintf "*Surface, type=%s, name=%s" surface_type surface_name
    lines = [lines; @sprintf "%s, %s" elset_name surface_face]

    return lines

end

#node based surface
function SURFACE(surface_type, surface_name, nset_name, node_area_factor::Float64)

    lines = @sprintf "*Surface, type=%s, name=%s" surface_type surface_name
    lines = [lines; @sprintf "%s, %7.4f" nset_name node_area_factor]

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


function TIE(name, master_surface, slave_surface, adjust)

    lines = @sprintf"*Tie, name=%s, ADJUST=%s" name adjust

    line = @sprintf"%s, %s" slave_surface master_surface

    lines = [lines; line]

    return lines

end





function TIME_POINTS(name, points)

    lines = @sprintf "*TIME POINTS, name=%s" name 

    for i in eachindex(points)

        line = @sprintf "%9.5f" points[i]
        lines = [lines; line]

    end

    return lines 

end


function UEL_PROPERTY_DING_CONNECTOR(elset, inputs, dof)

    (; d,        # Displacement/strain history
    dmgtype,  
    strain1p,
    strain2p,
    strain3p,
    strain4p,
    strain1n,
    strain2n,
    strain3n,
    strain4n,

    stress1p,
    stress2p,
    stress3p,
    stress4p,
    stress1n,
    stress2n,
    stress3n,
    stress4n,


    rDispP,
    rForceP,
    uForceP,
    rDispN,
    rForceN,
    uForceN,


    gammaK1,
    gammaK2,
    gammaK3,
    gammaK4,
    gammaKLimit,

    gammaD1,
    gammaD2,
    gammaD3,
    gammaD4,
    gammaDLimit,

    gammaF1,
    gammaF2,
    gammaF3,
    gammaF4,
    gammaFLimit,

    gE) = inputs

    if dmgtype == "energy"
        dmgtype_key = 0
    elseif dmgtype == "cycle"
        dmgtype_key = 1
    end

    lines = @sprintf "*UEL property, elset = %s" elset

    line = @sprintf "%9.6e, %9.6e, %9.6e, %9.6e, %9.6e, %9.6e, %9.6e, %9.6e" strain1p strain2p strain3p strain4p stress1p stress2p stress3p stress4p
    lines = [lines; line]

    line = @sprintf "%9.6e, %9.6e, %9.6e, %9.6e, %9.6e, %9.6e, %9.6e, %9.6e" strain1n strain2n strain3n strain4n stress1n stress2n stress3n stress4n
    lines = [lines; line]

    line = @sprintf "%9.6e, %9.6e, %9.6e, %9.6e, %9.6e, %9.6e, %9.6e, %9.6e" rDispP rForceP uForceP rDispN rForceN uForceN gammaK1 gammaK2
    lines = [lines; line]

    line = @sprintf "%9.6e, %9.6e, %9.6e, %9.6e, %9.6e, %9.6e, %9.6e, %9.6e"  gammaK3 gammaK4 gammaKLimit gammaD1 gammaD2 gammaD3 gammaD4 gammaDLimit
    lines = [lines; line]

    line = @sprintf "%9.6e, %9.6e, %9.6e, %9.6e, %9.6e, %9.6e, %d, %d"  gammaF1 gammaF2 gammaF3 gammaF4 gammaFLimit gE dmgtype_key dof[1] 
    lines = [lines; line]

     line = @sprintf "%d"  dof[2]
    lines = [lines; line]

    return lines

end




function USER_ELEMENT(num_nodes, type, properties, coordinates, variables, dof)

    lines = @sprintf "*USER Element, nodes=%d, type=%s, properties=%d, coordinates=%d, variables=%d" num_nodes type properties coordinates variables

    line = @sprintf "%d, %d" dof[1] dof[2]
    lines = [lines; line]

    return lines 

end




end #module