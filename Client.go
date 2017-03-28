package main

import (
	"fmt"
	"io"
	"net"
	"os"
)

func main() {

	service := ":1200"
	tcpAddr, err := net.ResolveTCPAddr("tcp4", service)
	fmt.Println("TCP Address:", tcpAddr)
	checkError(err)

	listener, err := net.ListenTCP("tcp", tcpAddr)
	checkError(err)

	for {
		conn, err := listener.Accept()
		if err != nil {
			continue
		}

		go handleClient(conn)

	}

}

func handleClient(conn net.Conn) {
	defer conn.Close()
	file := (conn.RemoteAddr().String()) // file name
	f, err := os.Create(file)            // create file
	checkError(err)
	f.Close()
	fmt.Println("File name : " + file) // show file name
	var buf [512]byte
	for {
		end, err := conn.Read(buf[0:]) // read lines from connection
		if err != nil {
			return
		}
		WriteFile(file, buf[0:end], 02) // 02 : os write
	}
}

func checkError(err error) {
	if err != nil {
		fmt.Fprintf(os.Stderr, "Fatal error: %s", err.Error())
		os.Exit(1)
	}
}

// WriteFile writes data to a file named by filename.
// If the file does not exist, WriteFile creates it with permissions perm;
// otherwise WriteFile truncates it before writing.

func WriteFile(filename string, data []byte, perm os.FileMode) error {

	f, err := os.OpenFile(filename, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, perm)
	if err != nil {
		return err
	}
	n, err := f.Write(data)
	if err == nil && n < len(data) {

		err = io.ErrShortWrite
	}
	if err1 := f.Close(); err == nil {
		err = err1
	}
	return err
}
