package connect

import (
	"log"
	"net/http"

	"github.com/gorilla/websocket"
)

var upgrader = websocket.Upgrader{
	CheckOrigin: func(req *http.Request) bool {
		return true // Allow all origins
	},
}

func HandleWebSocket(writer http.ResponseWriter, req *http.Request) {
	// Upgrade the HTTP connection to a WebSocket connection
	conn, readErr := upgrader.Upgrade(writer, req, nil)
	if readErr != nil {
		log.Println("Error upgrading connection:", readErr)
		return
	}
	defer conn.Close()

	log.Println("New connection established, client connected")

	go handleConnection(conn)
}

func handleConnection(conn *websocket.Conn) {
	for {
		// Read message from client
		var messageType int
		var message []byte
		var err error

		messageType, message, err = conn.ReadMessage()
		if err != nil {
			log.Println("Error reading message:", err)
			break
		}

		log.Printf("Received message: %s", message)

		var writerErr error
		// Echo the message back to the client
		if writerErr = conn.WriteMessage(messageType, message); writerErr != nil {
			log.Println("Error writing message:", writerErr)
			break
		}
	}
}
