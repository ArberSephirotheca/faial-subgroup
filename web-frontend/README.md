# Faial DRF Web Frontend

A simple web interface for the Faial DRF (Data-Race Freedom) analysis tool.

## Features

- Code editor with CUDA syntax highlighting (CodeMirror)
- Load example CUDA files from the examples/drf/ directory
- Real-time analysis using the faial-drf binary
- Clean, minimal interface with no heavy frameworks

## Requirements

- Python 3.x (uses built-in http.server)
- Faial binary (faial-drf) built and available

## Usage

1. Navigate to the web-frontend directory:
   ```bash
   cd web-frontend
   ```

2. Start the server:
   ```bash
   python3 server.py
   ```

3. Open your browser and go to:
   ```
   http://localhost:8000
   ```

## How it works

- The Python server serves static files and handles API requests
- `/analyze` endpoint runs faial-drf on submitted CUDA code
- `/examples` endpoint loads example files from ../examples/drf/
- Frontend uses vanilla JavaScript and CodeMirror for the editor

## File Structure

```
web-frontend/
├── server.py          # Python HTTP server
├── static/
│   ├── index.html     # Single page application
│   ├── style.css      # Minimal CSS styling
│   └── script.js      # Vanilla JavaScript frontend
└── README.md          # This file
```