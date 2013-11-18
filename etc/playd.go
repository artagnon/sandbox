package main

import (
	"fmt"
	"net"
)

func echoServer(conn net.Conn) {
	buf := make([]byte, 512)
	_, err := conn.Read(buf)
	if err != nil {
		fmt.Println("Read error", err.Error())
		return
	}
	mbuf := string(buf)
	fmt.Println("Server got:", mbuf)
	mbuf = mbuf + "!"
	_, err = conn.Write([]byte(mbuf))
	if err != nil {
		panic(err.Error())
	}
}

func main() {
	ln, err := net.Listen("tcp", ":8080")
	if err != nil {
		fmt.Println("Can't listen", err.Error())
		return
	}
	for {
		conn, err := ln.Accept()
		if err != nil {
			fmt.Println("Accept error", err.Error())
			return
		}
		go echoServer(conn)
	}
}
