package main
import "fmt"

func check(x int) int {
    if x > 0 {
        return 1
    } else {
        x = x + 1
        // Manque le return ici !
    }
}

func main() {
    fmt.Print(check(-10))
}