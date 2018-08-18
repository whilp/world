package main

import (
	"bytes"
	"flag"
	"fmt"
	"io"
	"log"
	"net"
	"os"
	"os/exec"
)

var (
	maxSize = 4096
	fPort   = flag.Int("port", 8837, "TCP port")
	fHost   = flag.String("host", "localhost", "host")
	fListen = flag.Bool("listen", false, "listen?")
)

func main() {
	flag.Parse()

	var err error
	if !*fListen {
		err = send(os.Stdin, *fPort, *fHost)
	} else {
		err = listen(*fPort, *fHost)
	}
	if err != nil {
		log.Fatal(err)
	}
}

func listen(port int, host string) error {
	ln, err := net.Listen("tcp", fmt.Sprintf(":%d", port))
	if err != nil {
		return err
	}
	for {
		conn, err := ln.Accept()
		if err != nil {
			log.Print(err)
			continue
		}
		go logged(handle)(conn)
	}
}

type handler func(net.Conn) error

func logged(h handler) func(net.Conn) {
	return func(c net.Conn) {
		if err := h(c); err != nil {
			log.Print(err)
		}
	}
}

func handle(c net.Conn) error {
	b := make([]byte, maxSize)

	defer c.Close()
	_, err := c.Read(b)
	if err != nil {
		return err
	}
	ns := &netstring{}
	if err := ns.Read(b); err != nil {
		return err
	}
	return pbcopy(ns.str)
}

func pbcopy(s string) error {
	in := bytes.NewBufferString(s)
	cmd := exec.Command("pbcopy", "-")
	cmd.Stdin = in
	return cmd.Run()
}

type netstringer interface {
	NetString() (string, error)
}

type netstring struct {
	size int
	str  string
}

func (ns *netstring) String() string {
	return ns.str
}

func (ns *netstring) Size() int {
	return ns.size
}

func (ns *netstring) Read(b []byte) error {
	buf := bytes.NewBuffer(b)
	_, err := fmt.Fscanf(buf, "%d:", &ns.size)
	if err != nil {
		return err
	}
	ns.str = string(buf.Next(ns.size))
	return nil
}

type raw struct {
	in io.Reader
}

func (r raw) NetString() (string, error) {
	b := make([]byte, maxSize)
	n, err := r.in.Read(b)
	if err != nil {
		return "", err
	}
	str := fmt.Sprintf("%d:%s", n, b)
	return str, nil
}

func send(r io.Reader, port int, host string) error {
	to := fmt.Sprintf("%s:%d", host, port)
	conn, err := net.Dial("tcp", to)
	if err != nil {
		return err
	}
	ns := &raw{r}
	str, err := ns.NetString()
	if err != nil {
		return err
	}
	_, err = fmt.Fprintf(conn, str)
	return err
}
