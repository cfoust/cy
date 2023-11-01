package cy

import (
	"github.com/cfoust/cy/pkg/mux/screen/toasts"
)

func (c *Cy) sendQueuedToasts() {
	c.Lock()
	defer c.Unlock()

	clients := c.clients
	queued := c.queuedToasts
	if len(clients) == 0 {
		return
	}

	c.queuedToasts = make([]toasts.Toast, 0)
	for _, client := range clients {
		for _, toast := range queued {
			client.toast.Send(toast)
		}
	}
}

// Sends a toast to every client but the provided one.
func (c *Cy) broadcastToast(except *Client, toast toasts.Toast) {
	c.RLock()
	defer c.RUnlock()
	for _, client := range c.clients {
		if client == except {
			continue
		}
		client.toast.Send(toast)
	}
}

// Sends a toast to all clients.
func (c *Client) sendToast(toast toasts.Toast) {
	c.toaster.Send(toast)
}

func (c *Cy) sendToast(toast toasts.Toast) {
	c.Lock()
	clients := c.clients
	if len(clients) == 0 {
		c.queuedToasts = append(c.queuedToasts, toast)
	} else {
		for _, client := range clients {
			client.toast.Send(toast)
		}
	}
	c.Unlock()
}

type ToastLogger struct {
	send func(toasts.Toast)
}

func (t *ToastLogger) Send(toast toasts.Toast) {
	t.send(toast)
}

func (t *ToastLogger) Error(msg string) {
	t.send(toasts.Toast{
		Message: msg,
		Level:   toasts.ToastLevelError,
	})
}

func (t *ToastLogger) Warn(msg string) {
	t.send(toasts.Toast{
		Message: msg,
		Level:   toasts.ToastLevelWarn,
	})
}

func (t *ToastLogger) Info(msg string) {
	t.send(toasts.Toast{
		Message: msg,
		Level:   toasts.ToastLevelInfo,
	})
}

func NewToastLogger(send func(toasts.Toast)) *ToastLogger {
	return &ToastLogger{
		send: send,
	}
}
