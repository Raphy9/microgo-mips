// (Fonctions à résultats multiples et appels imbriqués)
package main
import "fmt"

func swap(a, b int) (int, int) {
    return b, a
}

func add(a, b int) int {
    return a + b
}

func main() {
    x, y := 10, 20
    
    // Affectation multiple via appel
    a, b := swap(x, y) 
    res := add(a, b)
    
    fmt.Print(res)
}