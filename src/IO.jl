module IO 


function write_file(save_filename, lines)

    file = open(save_filename,"w")

    for i in eachindex(lines)
        write(file,"\n",lines[i])
    end

    close(file)

end



end #module