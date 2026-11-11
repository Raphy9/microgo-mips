package main
import "fmt"

// Test des retours multiples
func div_euclid(a, b int) (int, int) {
    q := 0
    r := a
    for r >= b {
        r = r - b
        q++
    }
    return q, r
}

func main() {
    // 1. Test arithmétique et boucle
    sum := 0
    for i := 0; i < 5; i++ {
        sum = sum + i
    }
    fmt.Print(sum) // 0+1+2+3+4 = 10
    fmt.Print("\n")

    // 2. Test appel fonction tuple (retours multiples)
    q, r := div_euclid(14, 3)
    fmt.Print(q) // 4
    fmt.Print(",")
    fmt.Print(r) // 2
    fmt.Print("\n")

    // 3. Test Short-circuit (ET logique)
    // On vérifie que le deuxième opérande (1/0) n'est PAS évalué si le premier est faux.
    // Si le short-circuit échoue, le programme crashera (division par zéro).
    
    if false && (1/0 == 0) {
        fmt.Print("KO") // Ne doit jamais s'afficher
    } else {
        fmt.Print("ShortCircuit:OK")
    }
    fmt.Print("\n")
}