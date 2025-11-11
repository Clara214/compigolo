dune build;
for f in tests/*; 
    do echo $f; 
    ./mgoc.exe $f; 
done