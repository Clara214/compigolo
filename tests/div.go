package main;
import "fmt";

/* a, b entiers naturels, b > 0 */
func div1(a,b int) (int,int) {
    if (a < b) { return 0, a } else 
    { x,y := div1(a-b,b); return x+1,y }
  };
func div2(a,b int) (int,int) {
    q := 0;
    for (a >= b) { q++; a=a-b };
    return q, a
  };
/* Version avec retour d'une structure */
type res struct { quo int; rem int };
func div3(a,b int) *res {
    r := new(res);
    r.quo = 0;
    for (a >= b) { r.quo++; a=a-b };
    r.rem = a;
    return r
  };
func main() {
	fmt.Print(div1(45,6)); fmt.Print("\n");
	fmt.Print(div2(45,6)); fmt.Print("\n");
	r:=div3(45,6); fmt.Print(r.quo,r.rem); fmt.Print("\n");
	fmt.Print(r); fmt.Print("\n");
};
