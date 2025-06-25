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
6. **Analysis Options**: UI controls for faial-drf parameters:
   - `--all-dims`: Range over all possible dimensions (checkbox)
   - `--all-levels`: Block-level AND grid-level verification (checkbox)
   - `-g`: Grid dimension configuration (text input, default: 1)
   - `-b`: Block dimension configuration (text input, default: 1024)
   - **Conflict Resolution**: `--all-dims` automatically disables dimension inputs to prevent invalid combinations

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

**Status: ✅ FULLY FUNCTIONAL**
- Web server starts properly and serves interface
- faial-drf binary integration works with `--json` output  
- Frontend processes analysis requests successfully
- Example loading and CodeMirror editor operational
- Analysis options UI controls working and properly passed to faial-drf
- Option conflict resolution prevents invalid faial-drf command combinations
- Grid-level analysis correctly displays block configuration from task locals
- Individual blockIdx display per thread in grid-level analysis with --all-dims
- Global parameters displayed in organized table format
- Parameter grouping by prefix for cleaner display (e.g., threadIdx.x/y/z → threadIdx{x,y,z})

### Key Design Decisions
- **Minimal Dependencies**: Uses only Python standard library
- **No Build Process**: Direct file serving, no bundling required
- **Security**: Currently no sandboxing (suitable for local development)
- **Error Handling**: Graceful handling of faial-drf execution errors
- **Responsive**: Works on desktop and mobile browsers

### Future Enhancements (Potential)
- Expose additional options from faial-drf (beyond the 4 currently implemented)
- The test examples should use the options listed in `../examples/drf/test.ml`
- File upload functionality
- Integration with other faial tools (faial-bc, faial-sync)
- Docker containerization
- Security sandboxing for production deployment