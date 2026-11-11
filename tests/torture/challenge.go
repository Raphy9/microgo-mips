package main
import "fmt"

type Point struct { x int; y int }

func transform(p *Point, d int) (int, int) {
    if d > 0 {
        d := d * 2; // Shadowing : ce d masque le paramètre d
        return p.x + d, p.y + d
    };
    return p.x, p.y
}

func main() {
    pt := new(Point);
    pt.x = 10;
    pt.y = 20;
    /* transform(pt, 5) devrait utiliser d=10 (5*2) et renvoyer (20, 30) */
    resX, resY := transform(pt, 5);
    fmt.Print(resX);
    fmt.Print(resY);
}