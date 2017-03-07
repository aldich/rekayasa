package main

import (
	"bufio"
	"fmt"
	"io"
	"net"
	"os"
)

func main() {

	service := ":1200"
	tcpAddr, err := net.ResolveTCPAddr("tcp4", service)
	fmt.Println("listen at : ", tcpAddr)
	checkError(err)

	listener, err := net.ListenTCP("tcp", tcpAddr)
	checkError(err)

	for {
		conn, err := listener.Accept()
		if err != nil {
			continue
		}
		handleClient(conn)
		conn.Close()
	}
}

func checkError(err error) {
	if err != nil {
		fmt.Fprintf(os.Stderr, "Fatal error : %s ", err.Error())
		os.Exit(1)
	}
}

func handleClient(conn net.Conn) {
	for {
		// open input file
		fi, err := os.Open("input.csv")
		if err != nil {
			panic(err)
		}
		// close fi on exit and check for its returned error
		//defer func() {
		//	if err := fi.Close(); err != nil {
		//		panic(err)
		//	}
		//}()

		//get user name
		var home string = os.Getenv("HOME")
		//fmt.Println(home)

		// open output file
		var directory string = home + "/Desktop/output.csv"
		fo, err := os.Create(directory)
		if err != nil {
			panic(err)
		}
		// close fo on exit and check for its returned error
		defer func() {
			if err := fo.Close(); err != nil {
				panic(err)
			}
		}()

		// make a read buffer
		r := bufio.NewReader(fi)
		// make a write buffer
		w := bufio.NewWriter(fo)

		// make a buffer to keep line that are read
		buf := make([]byte, 1024)
		for {
			// read a line
			n, err := r.Read(buf)
			if err != nil && err != io.EOF {
				panic(err)
			}
			if n == 0 {
				break
			}

			// write a line
			if _, err := w.Write(buf[:n]); err != nil {
				panic(err)
			}

			if err = w.Flush(); err != nil {
				panic(err)
			}
		}
	}
}
