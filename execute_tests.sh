# if dune build; then (jai une console wsl cheloue dsl)

if opam exec -- dune build;then
    for f in tests/*; 
        do echo $f; 
        ./mgoc.exe $f; 
    done
fi