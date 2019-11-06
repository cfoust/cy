package main

import (
	//"fmt"
	"log"
	"os/exec"
	"os"
)

func main() {
	arguments := os.Args[1:]

	cmd := exec.Command(arguments[0], arguments[1:]...)
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	cmd.Start()
	err := cmd.Wait()
	if err != nil {
		log.Fatal(err)
	}

	log.Println("ok")
}

