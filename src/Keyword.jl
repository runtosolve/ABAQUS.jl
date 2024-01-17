module Keyword

using Formatting

function HEADING(heading_lines)

    lines = Matrix{String}(undef, size(heading_lines)[1]+1, 1)

    lines[1] = "*Heading"

    for i in eachindex(heading_lines)

        lines[i+1] = "**" * heading_lines[i]

    end

    return lines

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

#need to add multiple dispatch here to cover other element types
#for now this only works for 4 node shells 
function ELEMENT(elements, type)   

    lines = Matrix{String}(undef, size(elements)[1]+1, 1)

    fmt = "{:7d},{:7d},{:7d},{:7d},{:7d}"

    lines[1] = "*Element, type=" * type

    for i=1:size(elements)[1]

        lines[i+1] = format(fmt, elements[i,1], elements[i,2], elements[i,3], elements[i,4],
                            elements[i,5])
    end

    return lines 

end


function ELSET(name, range)

    lines = "*Elset, elset=" * name * ", generate"

    fmt = "{:7d},{:7d},{:7d}"

    lines = [lines; format(fmt, range[1], range[2], range[3])]

    return lines

end

function NSET(name, range)

    lines = "*Nset, nset=" * name * ", generate"

    fmt = "{:7d},{:7d},{:7d}"

    lines = [lines; format(fmt, range[1], range[2], range[3])]

    return lines

end


function PART(name, node_lines, element_lines, nset_lines, elset_lines, section_lines)

    lines = "*Part, name=" * name

    lines = [lines; node_lines; element_lines; nset_lines; elset_lines; section_lines; "*End Part"]

    return lines

end

function PREPRINT(echo, model, history, contact)

    lines = "*Preprint, echo=" * echo * ", model=" * model * ", history=" * history * ", contact=" * contact

end


function SHELL_SECTION(elset_name, material_name, offset, t, formulation)

    lines = "*Shell Section, elset=" * elset_name * ", material=" * material_name * ", offset=" * offset

    fmt = "{:7.5f},{:2d}"

    lines = [lines; format(fmt, t, formulation)]

    return lines

end

end #module