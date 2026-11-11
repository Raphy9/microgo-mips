// (Le test ultime des points-virgules optionnels)
package main
import "fmt"
func main() {
    if true { fmt.Print(1) } else { fmt.Print(2); fmt.Print(3) }
    for i:=0; i<10; i++ {
        if i%2 == 0 { fmt.Print("pair") }
    }; ;; // Points virgules vides autorisés
    return
}