package main;
import "fmt";

type Node struct {
    val int;
    next *Node
};


// Résultat attendu : 123
func main() {
    var ancre, p *Node;
    
    ancre = new(Node);
    ancre.val = 1;
    
    ancre.next = new(Node);
    ancre.next.val = 2;
    
    ancre.next.next = new(Node);
    ancre.next.next.val = 3;
    for p == ancre; p != nil; p == p.next {
        fmt.Print(p.val)
    }
}