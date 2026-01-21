#!/bin/bash

cmpflags="-Wextra -Wall -ffixed-line-length-none -fimplicit-none -pedantic -fcheck=all -fbacktrace"
gdbflags=""
supplemental_args=""
filename=""
extension_out=".exe"
# cmpl -echo <filename> <flags> <.o files> > cmp_<name>.sh
echo_only=false  # use -echo flag to echo $command, useful for creating context specific .sh to give to profs.
echo_only_1=false  # prints also the first lines
run_exe=false  # run after compile

for arg in "${@}"; do
    if [[ "${arg}" == "-g" ]]; then  # -g for debug options
        gdbflags="-g -O0"
        if ! $echo_only; then
            echo "Debug mode"
        fi
    elif [[ "${arg}" == "-c" ]]; then  # -c for compiling without linking
        extension_out=".o"
        supplemental_args="${supplemental_args} -c"
        if ! $echo_only; then
            echo "Compiling to object file"
        fi
    elif [[ "${arg}" == "-echo" ]]; then  # -echo for echo only the command
        echo_only=true
    elif [[ "${arg}" == "-echo1" ]]; then  # -echo1 for echo only the command with first lines
        echo_only=true
        echo_only_1=true
    elif [[ "${arg}" == "-ex" ]]; then  # run after compile
        run_exe=true
    elif [[ "${arg}" != -* ]]; then  # object file
        if [[ $filename == "" ]]; then
            if $echo_only; then
                filename="\${perc}/${arg}"
            else
                filename=${arg}
            fi 
        else
            if $echo_only; then
                supplemental_args="${supplemental_args} \${perc}/${arg}.o"
            else
                supplemental_args="${supplemental_args} ${arg}.o"
            fi
        fi
    else  # every other flag is passed directly to gfortran
        supplemental_args="${supplemental_args} ${arg}"
        if ! $echo_only; then
            echo "With supplemental: ${arg}"
        fi
    fi
done

if [[ filename == "" ]]; then
    echo "usage: cmpl file_fortran_with_no_ext"
    echo "cmpl -echo (or -echo1) ...  # to generate an equivalent script"
    echo "cmpl ... -g -c ...  # to specify debug or compile only mode"
    echo "cmpl ... -ex ...  # execute after compiled"
    echo "general usage:"
    echo "cmpl (-echo or -echo1) (-g) (-c) (gfortran-flags) filename(.f90) filename1(.o) ..."
    echo "DO NOT PASS EXPLICITLY the extensions .f90 and .o"
fi

command="gfortran ${gdbflags} ${cmpflags} ${supplemental_args} ${filename}.f90 -o ${filename}${extension_out}"

if $echo_only_1; then
    echo "#!/bin/bash"
    echo "perc=\"${PWD}\""
    echo
fi
if $echo_only; then
    echo "${command}"
else
    echo "(cmpl)   Compiling ${filename}.f90 -> ${filename}${extension_out}"
    if $command; then
        echo "(cmpl)   Success!"
        echo
        if [[ $run_exe && "${extension_out}" == ".exe" ]]; then
            if ./${filename}.exe; then
                echo
                echo "(cmpl)   OK!"
            else
                echo
                echo "(cmpl)   NOPE!"
            fi
        elif  $run_exe; then
            echo "Cannot execute an object file"
        fi
    else
        echo "(cmpl)   Insuccess!"
    fi
fi
