import React from 'react';
import { useEffect, useState } from 'react';
import { Container, Typography, TextField } from '@mui/material';

const App: React.FC = () => {
	const [document, setDocument] = useState('');
	const [socket, setSocket] = useState<WebSocket | null>(null);

	useEffect(() => {
		window.onload = () => {
			// Create a WebSocket connection to the Go Server
			const ws =  new WebSocket('ws://localhost:8080/ws');

			// Event listener for the connection close event
			ws.onclose = () => {
				console.log('WebSocket connection closed');
			}

			ws.onmessage = event => {
				console.log('WebSocket message received:', event.data);
				setDocument(event.data);
			}

			// Set the WebSocket connection to the state
			setSocket(ws);

			// Clean up the WebSocket connection on component unmount
			return () => {
				console.log('Closing WebSocket connection');
				ws.close();
			};
		}
	}, []);

	const handleTextChange = (event: React.ChangeEvent<HTMLInputElement | HTMLTextAreaElement>) => {
		const newText = event.target.value;
		setDocument(newText);
		// Emit the new text to the server
		if (socket) {
			socket.send(newText);
		}
	};

	if (!socket) {
		return <div>Loading...</div>;
	}

	return (<Container maxWidth="sm" style={{ marginTop: '2rem' }}>
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
