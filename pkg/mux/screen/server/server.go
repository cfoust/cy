package server

import (
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"

	"github.com/sasha-s/go-deadlock"
)

type Server struct {
	deadlock.RWMutex
	clients []*Client
}

// refreshPane resizes screen to the minimum of all of its clients' screen sizes.
func (s *Server) refreshPane(screen mux.Screen) {
	if screen == nil {
		return
	}

	s.Lock()
	defer s.Unlock()

	// Get a list of all clients attached to this screen
	attached := make([]*Client, 0)
	for _, client := range s.clients {
		if screen == client.Screen() {
			attached = append(attached, client)
		}
	}

	// Don't do anything if no clients are attached to this pane
	if len(attached) == 0 {
		return
	}

	// Set the pane's size to the maximum that all clients can fit
	size := attached[0].Size()
	for _, client := range attached {
		// some clients don't want to impose size constraints on other
		// clients, just see a pane for a moment
		if client.Size().IsZero() {
			continue
		}

		size = geom.GetMaximum(size, client.Size())
	}

	if size.IsZero() {
		return
	}

	go screen.Resize(size)
}

func New() *Server {
	return &Server{}
}
