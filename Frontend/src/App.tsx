import React from 'react';
import { Container, Typography, TextField } from '@mui/material';
import { useEffect, useState } from 'react';
import { io } from 'socket.io-client';

const socket = io('http://localhost:4000'); // Change this to your server URL if different

const App: React.FC = () => {
	const [document, setDocument] = useState('');

	useEffect(() => {
		// Listen for document updates from the server
		socket.on('document-update', (data: string) => {
			setDocument(data);
		});

		// Clean up the socket connection on component unmount
		return () => {
			socket.off('document-update');
		};
	}, []);

	const handleTextChange = (event: React.ChangeEvent<HTMLInputElement | HTMLTextAreaElement>) => {
		const newText = event.target.value;
		setDocument(newText);
		// Emit the new text to the server
		socket.emit('document-edit', newText);
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
