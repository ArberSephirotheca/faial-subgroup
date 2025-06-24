# Faial DRF Web Frontend

## Project Overview
A minimal web-based interface for the `faial-drf` tool (Data-Race Freedom analysis for CUDA kernels) to make it more accessible and user-friendly.

## Implementation Summary

### Technology Stack (Final)
- **Backend**: Python 3 with built-in `http.server` module (no external dependencies)
- **Frontend**: Vanilla JavaScript with minimal CSS
- **Editor**: CodeMirror for CUDA syntax highlighting
- **Architecture**: Direct integration - spawns `faial-drf` as subprocess

### Features Implemented
1. **Code Editor**: CodeMirror with CUDA/C syntax highlighting
2. **Example Gallery**: Dropdown to load examples from `examples/drf/` directory
3. **Real-time Analysis**: Direct execution of faial-drf binary
4. **Clean Results Display**: Formatted output with success/error states
5. **Command Line Options**: Configurable port (`-p/--port`)

### File Structure
```
web-frontend/
├── server.py          # Python HTTP server with API endpoints
├── static/
│   ├── index.html     # Single page application
│   ├── style.css      # Minimal CSS styling
│   └── script.js      # Vanilla JavaScript frontend
└── README.md          # Usage instructions
```

### API Endpoints
- `GET /`: Serves the main HTML page
- `POST /analyze`: Runs faial-drf on submitted CUDA code
- `GET /examples`: Returns list of example CUDA files with content

### Usage
```bash
cd web-frontend
python3 server.py -p 8080  # or any available port
# Open http://localhost:8080 in browser
```

### Key Design Decisions
- **Minimal Dependencies**: Uses only Python standard library
- **No Build Process**: Direct file serving, no bundling required
- **Security**: Currently no sandboxing (suitable for local development)
- **Error Handling**: Graceful handling of faial-drf execution errors
- **Responsive**: Works on desktop and mobile browsers

### Future Enhancements (Potential)
- JSON output parsing for better error visualization
- Line highlighting for data race locations
- File upload functionality
- Integration with other faial tools (faial-bc, faial-sync)
- Docker containerization
- Security sandboxing for production deployment