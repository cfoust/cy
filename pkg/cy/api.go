package cy

import (
	_ "embed"
	"fmt"
	"os"
	"runtime"
	"runtime/pprof"
	"runtime/trace"
	"time"

	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen/toasts"
)

type CyModule struct {
	cy *Cy
}

var _ janet.Documented = (*CyModule)(nil)

//go:embed docs-cy.md
var DOCS_CY string

func (i *CyModule) Documentation() string {
	return DOCS_CY
}

func (c *CyModule) KillServer() {
	_ = c.cy.Shutdown()
}

func (c *CyModule) Detach(user interface{}) {
	client, ok := user.(*Client)
	if !ok {
		return
	}

	client.Detach()
}

func (c *CyModule) ReloadConfig() error {
	return c.cy.reloadConfig()
}

func (c *CyModule) CpuProfile(user interface{}) error {
	client, ok := user.(*Client)
	if !ok {
		return fmt.Errorf("no user")
	}

	socketPath := c.cy.options.SocketPath
	if len(socketPath) == 0 {
		return fmt.Errorf("no socket path")
	}

	path := fmt.Sprintf(
		"%s-cpu-%s.prof",
		socketPath,
		time.Now().Format("2006.01.02.15.04.05"),
	)

	f, err := os.Create(path)
	if err != nil {
		return err
	}

	if err := pprof.StartCPUProfile(f); err != nil {
		return err
	}

	client.Toast(toasts.Toast{
		Message: "started cpu profile: " + path,
	})

	go func() {
		time.Sleep(15 * time.Second)
		pprof.StopCPUProfile()
		_ = f.Close()

		client.Toast(toasts.Toast{
			Message: "finished cpu profile: " + path,
		})
	}()
	return nil
}

func (c *CyModule) MemoryProfile(user interface{}) error {
	client, ok := user.(*Client)
	if !ok {
		return fmt.Errorf("no user")
	}

	socketPath := c.cy.options.SocketPath
	if len(socketPath) == 0 {
		return fmt.Errorf("no socket path")
	}

	path := fmt.Sprintf(
		"%s-memory-%s.prof",
		socketPath,
		time.Now().Format("2006.01.02.15.04.05"),
	)

	f, err := os.Create(path)
	if err != nil {
		return err
	}
	defer func() {
		_ = f.Close()
	}()

	// Run GC to get more accurate heap snapshot
	runtime.GC()

	if err := pprof.WriteHeapProfile(f); err != nil {
		return err
	}

	client.Toast(toasts.Toast{
		Message: "saved memory profile: " + path,
	})

	return nil
}

func (c *CyModule) Trace(user interface{}) error {
	client, ok := user.(*Client)
	if !ok {
		return fmt.Errorf("no user")
	}

	socketPath := c.cy.options.SocketPath
	if len(socketPath) == 0 {
		return fmt.Errorf("no socket path")
	}

	path := fmt.Sprintf(
		"%s-trace-%s.out",
		socketPath,
		time.Now().Format("2006.01.02.15.04.05"),
	)

	f, err := os.Create(path)
	if err != nil {
		return err
	}

	if err := trace.Start(f); err != nil {
		return err
	}

	client.Toast(toasts.Toast{
		Message: "started trace: " + path,
	})

	go func() {
		time.Sleep(15 * time.Second)
		trace.Stop()
		_ = f.Close()

		client.Toast(toasts.Toast{
			Message: "finished trace: " + path,
		})
	}()
	return nil
}

func (c *CyModule) Paste(user interface{}, register string) error {
	client, ok := user.(*Client)
	if !ok {
		return fmt.Errorf("no user")
	}

	if register != "+" {
		buffer := client.buffer
		if len(buffer) == 0 {
			return nil
		}

		client.binds.Input([]byte(buffer))
		return nil
	}

	// + reads from the system clipboard instead
	data, err := c.cy.options.Clipboard.Read()
	if err != nil {
		return fmt.Errorf(
			"failed to read system clipboard: %w",
			err,
		)
	}

	client.binds.Input([]byte(data))
	return nil
}
