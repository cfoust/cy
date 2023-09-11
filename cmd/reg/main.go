package main

import (
	"bytes"
	"fmt"
	"regexp"
)

type PartialReader struct {
	re     *regexp.Regexp
	reader *bytes.Reader
}

func (p PartialReader) ReadRune() (r rune, size int, err error) {
	r, size, err = p.reader.ReadRune()
	fmt.Printf("read '%c' size=%d err=%+v\n", r, size, err)
	return
}

func (p PartialReader) poll() {
}

func main() {
	br := PartialReader{
		reader: bytes.NewReader([]byte("blah")),
	}
	re := regexp.MustCompile("^hhh")
	for i := 0; ; i++ {
		q := re.FindReaderIndex(br)
		fmt.Printf("step %d q=%v\n", i, q)
		if q == nil {
			break
		}
	}
}
