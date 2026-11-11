package main
import "fmt"

type Coords struct {
    x int
    y int
}

type Player struct {
    pos *Coords
    name string
}

func main() {
    p := new(Player)
    c := new(Coords)
    c.x = 100
    c.y = 200
    p.pos = c
    p.name = "Mario"

    fmt.Print(p.pos.x) // Devrait afficher 100
    fmt.Print(",")
    fmt.Print(p.pos.y) // Devrait afficher 200
    fmt.Print("\n")

    name1 := "Mario"
    name2 := "Mario"
    name3 := "Luigi"

    if name1 == name2 {
        fmt.Print("Mario==Mario:OK")
    } else {
        fmt.Print("Mario==Mario:KO")
    }
    fmt.Print("\n")

    if name1 != name3 {
        fmt.Print("Mario!=Luigi:OK")
    } else {
        fmt.Print("Mario!=Luigi:KO")
    }
    fmt.Print("\n")
}