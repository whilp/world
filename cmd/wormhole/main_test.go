package main

import "testing"

func TestMain(t *testing.T) {
	tests := []struct {
		in   string
		str  string
		size int
	}{
		{"5:abcd", "abcd", 5},
		{"2:abcd", "ab", 2},
		{"5:2:abcd", "2:abc", 5},
	}

	for _, test := range tests {
		ns := &netstring{}
		ns.Read([]byte(test.in))
		if str := ns.String(); str != test.str {
			t.Errorf("wanted netstring %v, got %v", test.str, str)
		}
		if size := ns.Size(); size != test.size {
			t.Errorf("wanted netstring size %d, got %d", test.size, size)
		}
	}

}
