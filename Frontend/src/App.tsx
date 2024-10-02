import React from 'react';
import { Container, Typography, TextField } from '@mui/material';
import { useEffect, useState } from 'react';

const App: React.FC = () => {
	const [socket, setSocket] = useState<WebSocket | null>(null);
	const [document, setDocument] = useState('');

	useEffect(() => {
		// Create a WebSocket connection to the Go Server
		const ws =  new WebSocket('ws://localhost:8080/ws');

		// Event listener for messages from the server
		ws.onmessage = event => {
			setDocument(event.data);
		}

		// Event listener for the connection close event
		ws.onclose = () => {
			console.log('WebSocket connection closed');
		}

		// Set the WebSocket connection to the state
		setSocket(ws);

		// Clean up the WebSocket connection on component unmount
		return () => {
			ws.close();
		};
	}, []);

	const handleTextChange = (event: React.ChangeEvent<HTMLInputElement | HTMLTextAreaElement>) => {
		const newText = event.target.value;
		setDocument(newText);
		// Emit the new text to the server
		if (socket) {
			socket.send(newText);
		}
	};

	return (
		<Container maxWidth="sm" style={{ marginTop: '2rem' }}>
			<Typography variant="h4" component="h1" gutterBottom>
				Collaborative Concurrent Text Editor
			</Typography>
			<TextField
				label="Document"
				variant="outlined"
				fullWidth
				multiline
				rows={20}
				margin="normal"
				value={document}
				onChange={handleTextChange}
			/>
		</Container>
	);
};

export default App;
