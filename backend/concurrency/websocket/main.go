package main

import (
	"fmt"
	"log"
	"net/http"

	"backend/concurrency/websocket/middlewares"

	"github.com/gorilla/websocket"
)

var upgrader = websocket.Upgrader{
	CheckOrigin: func(req *http.Request) bool {
		return true // Allow all origins
	},
}

func handleConnection(writer http.ResponseWriter, req *http.Request) {
	// Upgrade the HTTP connection to a WebSocket connection
	conn, readErr := upgrader.Upgrade(writer, req, nil)
	if readErr != nil {
		log.Println("Error upgrading connection:", readErr)
		return
	}
	defer conn.Close()

	log.Println("New connection established, client connected")

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

func main() {
	http.HandleFunc("/ws", handleConnection) // Set up the WebSocket endpoint

	fmt.Println("Starting server on :8080")
	var err error
	if err = http.ListenAndServe(":8080", middlewares.EnableCors(http.DefaultServeMux)); err != nil {
		log.Fatal("Error starting server:", err)
	}
}
