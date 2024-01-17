# ABAQUS.jl

```julia

using ABAQUS, DelimitedFiles

#Bring in model geometry
node_data = readdlm("readme_nodes_source.txt", ',', Float64)
element_data = readdlm("readme_elements_source.txt", ',', Int)

#Define inp file name
inpfile_name = "angle.inp"


#HEADING
heading_lines = ["Angle model"; "January 17, 2024"; "CDM"]
lines = ABAQUS.Keyword.HEADING(heading_lines)
inpfile = lines

#PREPRINT
echo = "NO"
model = "NO"
history = "NO"
contact = "NO"
lines = ABAQUS.Keyword.PREPRINT(echo, model, history, contact)
inpfile = [inpfile; lines]

#DEFINE ANGLE PART

part_name = "ANGLE"

#Nodes
node_lines = ABAQUS.Keyword.NODE(node_data)

#Elements
type = "S4R"
element_lines = ABAQUS.Keyword.ELEMENT(element_data, type)

#Node set
name = "ANGLE_SET"
range = (1, Int(maximum(node_data[:, 1])), 1)
nset_lines = ABAQUS.Keyword.NSET(name, range)

#Elements set
name = "ANGLE_SET"
range = (1, Int(maximum(element_data[:, 1])), 1)
elset_lines = ABAQUS.Keyword.ELSET(name, range)

#Section definition
elset_name = "ANGLE_SET"
material_name = "\"A653 SS Gr 55\""
offset = "SPOS"
t = 0.12
formulation = 5
range = (1, Int(maximum(element_data[:, 1])), 1)
section_lines = ABAQUS.Keyword.SHELL_SECTION(elset_name, material_name, offset, t, formulation)

lines = ABAQUS.Keyword.PART(part_name, node_lines, element_lines, nset_lines, elset_lines, section_lines)
inpfile = [inpfile; lines]

#CREATE INP FILE
ABAQUS.IO.write_file(inpfile_name, inpfile)
```