package main

import (
	"fmt"
	"log"
	"os/exec"
	"os"
)

func main() {
	arguments := os.Args[1:]

	cmd := exec.Command(arguments[0], arguments[1:]...)

	out, err := cmd.Output()
	if err != nil {
		log.Fatal(err)
	}

	fmt.Printf("%s\n", out)
}

