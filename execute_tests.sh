# if dune build; then (jai une console wsl cheloue dsl)

if opam exec -- dune build;then
    echo "Devrait réussir :"
    for f in tests/*; 
        do echo $f; 
        ./mgoc.exe $f; 
    done

    echo "Devrait donner une erreur :"
    for f in tests_errors/*; 
        do echo $f; 
        ./mgoc.exe $f; 
    done
fi