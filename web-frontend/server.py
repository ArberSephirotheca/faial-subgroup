#!/usr/bin/env python3
import http.server
import socketserver
import json
import subprocess
import tempfile
import os
import glob
import argparse
from urllib.parse import urlparse, parse_qs
from urllib.parse import unquote

DEFAULT_PORT = 8080
FAIAL_DRF_PATH = "../faial-drf"
EXAMPLES_PATH = "../examples/drf"

class FaialRequestHandler(http.server.SimpleHTTPRequestHandler):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, directory="static", **kwargs)
    
    def do_POST(self):
        if self.path == '/analyze':
            self.handle_analyze()
        elif self.path == '/examples':
            self.handle_examples()
        else:
            self.send_error(404)
    
    def do_GET(self):
        if self.path == '/examples':
            self.handle_examples()
        else:
            super().do_GET()
    
    def handle_analyze(self):
        try:
            content_length = int(self.headers['Content-Length'])
            post_data = self.rfile.read(content_length)
            data = json.loads(post_data.decode('utf-8'))
            
            cuda_code = data.get('code', '')
            
            # Create temporary file
            with tempfile.NamedTemporaryFile(mode='w', suffix='.cu', delete=False) as f:
                f.write(cuda_code)
                temp_file = f.name
            
            try:
                # Run faial-drf with JSON output
                result = subprocess.run(
                    [FAIAL_DRF_PATH, temp_file, '--json'],
                    capture_output=True,
                    text=True,
                    timeout=30
                )
                
                response = {
                    'success': result.returncode == 0,
                    'returncode': result.returncode
                }
                
                # Parse JSON output if successful
                if result.returncode == 0 and result.stdout.strip():
                    try:
                        parsed_json = json.loads(result.stdout)
                        response['data'] = parsed_json
                    except json.JSONDecodeError:
                        response['stdout'] = result.stdout
                        response['stderr'] = result.stderr
                else:
                    response['stdout'] = result.stdout
                    response['stderr'] = result.stderr
                
            finally:
                # Clean up temporary file
                os.unlink(temp_file)
            
            self.send_response(200)
            self.send_header('Content-Type', 'application/json')
            self.send_header('Access-Control-Allow-Origin', '*')
            self.end_headers()
            self.wfile.write(json.dumps(response).encode())
            
        except Exception as e:
            self.send_response(500)
            self.send_header('Content-Type', 'application/json')
            self.send_header('Access-Control-Allow-Origin', '*')
            self.end_headers()
            self.wfile.write(json.dumps({'error': str(e)}).encode())
    
    def handle_examples(self):
        try:
            examples = []
            cu_files = glob.glob(os.path.join(EXAMPLES_PATH, "*.cu"))
            
            for file_path in sorted(cu_files):
                filename = os.path.basename(file_path)
                try:
                    with open(file_path, 'r') as f:
                        content = f.read()
                    examples.append({
                        'name': filename,
                        'content': content
                    })
                except Exception as e:
                    print(f"Error reading {file_path}: {e}")
            
            self.send_response(200)
            self.send_header('Content-Type', 'application/json')
            self.send_header('Access-Control-Allow-Origin', '*')
            self.end_headers()
            self.wfile.write(json.dumps(examples).encode())
            
        except Exception as e:
            self.send_response(500)
            self.send_header('Content-Type', 'application/json')
            self.send_header('Access-Control-Allow-Origin', '*')
            self.end_headers()
            self.wfile.write(json.dumps({'error': str(e)}).encode())

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Faial DRF Web Frontend Server")
    parser.add_argument("-p", "--port", type=int, default=DEFAULT_PORT,
                        help=f"Port to serve on (default: {DEFAULT_PORT})")
    args = parser.parse_args()
    
    with socketserver.TCPServer(("", args.port), FaialRequestHandler) as httpd:
        print(f"Serving at http://localhost:{args.port}")
        httpd.serve_forever()