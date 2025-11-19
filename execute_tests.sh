if dune build; then
    for f in tests/*; 
        do echo $f; 
        ./mgoc.exe $f; 
    done
fi