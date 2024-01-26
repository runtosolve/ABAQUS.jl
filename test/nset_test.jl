 using Formatting

 nodes = 1:2
 name = "CDM"
 
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

list = "*Nset, nset=" * name


for i=1:num_rows

    #Define the node list formatting.
    fmt = "{:7d},{:7d},{:7d},{:7d},{:7d},{:7d},{:7d},{:7d},{:7d},{:7d},{:7d},{:7d},{:7d},{:7d},{:7d},{:7d}"

    range_start = (i-1) * 16 + 1
    range_end = i * 16  
    range = range_start:range_end

    list = [list; format(fmt, nodes[range[1]], nodes[range[2]], nodes[range[3]], nodes[range[4]], nodes[range[5]], nodes[range[6]], nodes[range[7]], nodes[range[8]], nodes[range[9]], nodes[range[10]], nodes[range[11]], nodes[range[12]], nodes[range[13]], nodes[range[14]], nodes[range[15]], nodes[range[16]])]

end


if residual != 0.0

    index = findfirst(" 0,", list[end])[1]
    list[end] = list[end][1:index - 7]

end
