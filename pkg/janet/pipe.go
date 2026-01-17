package janet

import (
	"context"
	"io"
	"os"
)

type wrappedPipe struct {
	r, w  *os.File
	value *Value
}

func (vm *VM) wrapPipe(
	ctx context.Context,
	flag int32,
) (*wrappedPipe, error) {
	r, w, err := os.Pipe()
	if err != nil {
		return nil, err
	}

	file := r
	if flag == FileFlagWrite {
		file = w
	}

	value, err := vm.WrapFile(ctx, file, flag)
	if err != nil {
		_ = r.Close()
		_ = w.Close()
		return nil, err
	}

	return &wrappedPipe{r: r, w: w, value: value}, nil
}

func (wp *wrappedPipe) Close() {
	if wp == nil {
		return
	}
	_ = wp.r.Close()
	_ = wp.w.Close()
	wp.value.Free()
}

// Pipe creates os.Pipe pairs and wraps them as Janet file values for use as
// :in, :out, and :err dynamic bindings. Data written to In appears on the :in
// dyn's read end; data written to :out/:err dyns can be read from Out/Err.
type Pipe struct {
	// In is the write end for stdin - write here to send data to Janet's :in
	In io.WriteCloser
	// Out is the read end for stdout - read here to get data from Janet's :out
	Out io.ReadCloser
	// Err is the read end for stderr - read here to get data from Janet's :err
	Err io.ReadCloser

	in, out, err *wrappedPipe
}

// Pipe creates a new Pipe with :in, :out, and :err streams.
func (vm *VM) Pipe(ctx context.Context) (*Pipe, error) {
	p := &Pipe{}
	var err error

	p.in, err = vm.wrapPipe(ctx, FileFlagRead)
	if err != nil {
		return nil, err
	}
	p.In = p.in.w

	p.out, err = vm.wrapPipe(ctx, FileFlagWrite)
	if err != nil {
		p.Close()
		return nil, err
	}
	p.Out = p.out.r

	p.err, err = vm.wrapPipe(ctx, FileFlagWrite)
	if err != nil {
		p.Close()
		return nil, err
	}
	p.Err = p.err.r

	return p, nil
}

// Dyns returns the dynamic bindings map for use with ExecuteCall.
func (p *Pipe) Dyns() map[Keyword]any {
	return map[Keyword]any{
		Keyword("in"):  p.in.value,
		Keyword("out"): p.out.value,
		Keyword("err"): p.err.value,
	}
}

// Close releases all resources associated with the pipe.
func (p *Pipe) Close() {
	if p == nil {
		return
	}
	p.in.Close()
	p.out.Close()
	p.err.Close()
}
