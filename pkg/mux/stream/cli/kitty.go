package cli

import (
	"io"
	"regexp"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/keys"
)

var (
	DA1_RESPONSE   = regexp.MustCompile("\x1b\\[\\?64;*(\\d+;?)*c")
	KITTY_RESPONSE = regexp.MustCompile("\x1b\\[\\?\\d+u")
)

type kittyDetector struct {
	w io.Writer
	r io.Reader

	gotResponse    bool
	kittySupported bool
}

var _ io.Reader = (*kittyDetector)(nil)

func (k *kittyDetector) Query() (err error) {
	_, err = k.w.Write([]byte("\x1b[?u\x1b[c"))
	return
}

func contains(r *regexp.Regexp, b []byte) bool {
	return r.FindSubmatch(b) != nil
}

func (k *kittyDetector) Read(p []byte) (n int, err error) {
	n, err = k.r.Read(p)
	if err != nil || k.gotResponse {
		return
	}

	// We got the DA1 reply before the key protocol status, so this terminal
	// does not support the key protocol.
	if contains(DA1_RESPONSE, p[:n]) {
		k.gotResponse = true
		return
	}

	if contains(KITTY_RESPONSE, p[:n]) {
		k.gotResponse = true
		k.kittySupported = true
		_, err = k.w.Write([]byte(keys.GenerateKittyEnableSequence(
			emu.KeyReportAll,
		)))
		return
	}

	return
}
