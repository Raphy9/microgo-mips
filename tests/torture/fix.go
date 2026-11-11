package main
import "fmt"

type Point struct {
    x int
    y int
}

type Line struct {
    start *Point
    end   *Point
    name  string
}

func main() {
    p1 := new(Point)
    p1.x = 10
    
    l := new(Line)
    l.start = p1
    l.name = "ligne1"

    fmt.Print(l.start.x) 
    fmt.Print("\n")

    s1 := "hello"
    s2 := "hello"
    s3 := "world"
    if (s1 == s2) {
        fmt.Print("Egalite chaines: OK")
    } else {
        fmt.Print("Egalite chaines: ERREUR")
    }
    fmt.Print("\n")

    if (s1 == s3) {
        fmt.Print("Inegalite chaines: ERREUR")
    } else {
        fmt.Print("Inegalite chaines: OK")
    }
    fmt.Print("\n")
}