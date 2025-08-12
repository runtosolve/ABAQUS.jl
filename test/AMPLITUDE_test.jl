using Printf

x = collect(range(0.0, 10.0, 23))
y = collect(range(0.0, 1.0, 23))
name = "test"


    lines = @sprintf "Amplitude, name=%s" name

 
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

        else

            range_start = (i-1) * 4 + 1
            range_end = i * 4  
            range = range_start:range_end
       
            line =  @sprintf "%9.5f,%9.5f,%9.5f,%9.5f,%9.5f,%9.5f,%9.5f,%9.5f" x[range[1]] y[range[1]] x[range[2]] y[range[2]] x[range[3]] y[range[3]] x[range[4]] y[range[4]]
            lines = [lines; line]

        end

    end


    lines = [lines; line]

