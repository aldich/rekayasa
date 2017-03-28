package main

import (
	"bufio"
	"fmt"
	"io"
	"net"
	"os"
)

func main() {

	file, err := os.Open(os.Args[2])
	checkError(err)
	defer file.Close()
	fmt.Println("File : ", file.Name()) // name file

	service := os.Args[1] // server

	tcpAddr, err := net.ResolveTCPAddr("tcp4", service)
	checkError(err)
	conn, err := net.DialTCP("tcp", nil, tcpAddr)
	checkError(err)

	reader := bufio.NewReader(file) // read file
	buf := make([]byte, 1024)       // create buffer

	for {
		n, err := reader.Read(buf) // read a line
		if err != nil && err != io.EOF {
			panic(err)
		}
		if n == 0 {
			break
		}
		if _, err := conn.Write(buf[:n]); err != nil { // write a line
			panic(err)
		}
	}
}

func checkError(err error) {
	if err != nil {
		fmt.Fprintf(os.Stderr, "Fatal error: %s", err.Error())
		os.Exit(1)
	}
}
