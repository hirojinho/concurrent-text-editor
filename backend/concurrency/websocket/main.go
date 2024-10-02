package main

import (
	"fmt"
	"log"
	"net/http"

	"backend/concurrency/websocket/connect"
	"backend/concurrency/websocket/middlewares"
)

func main() {
	http.HandleFunc("/ws", connect.HandleConnection) // Set up the WebSocket endpoint

	fmt.Println("Starting server on :8080")
	var err error
	if err = http.ListenAndServe(":8080", middlewares.EnableCors(http.DefaultServeMux)); err != nil {
		log.Fatal("Error starting server:", err)
	}
}
