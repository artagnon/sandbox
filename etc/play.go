package main

import (
	"fmt"
	"net"
	"io"
	"os"
	"bufio"
	"strings"
)

func readerClient(r io.Reader) {
	buf := make([]byte, 512)
	for {
		_, err := r.Read(buf)
		if err != nil {
			fmt.Println("Read error", err.Error())
			return
		}
		fmt.Println("Client got:", string(buf))
	}
}

func main() {
	conn, err := net.Dial("tcp", ":8080")
	if err != nil {
		panic(err.Error())
	}
	defer conn.Close()
	go readerClient(conn)
	for {
		fmt.Printf("> ")
		reader := bufio.NewReader(os.Stdin)
		line, err := reader.ReadString('\n')
		line = strings.TrimRight(line, "\t\r\n")
                if err != nil {
                        fmt.Println("Read error", err.Error())
                        break
                }
                _, err2 := conn.Write([]byte(line))
                if err2 != nil {
                        fmt.Println("Write error", err2.Error())
                        break
                }
        }
}
