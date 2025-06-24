// Configuration
const DEFAULT_EXAMPLE = 'racy-cav21.cu';

// Global variables
let editor;
let examples = [];

// Initialize the application
document.addEventListener('DOMContentLoaded', function() {
    initializeEditor();
    loadExamples();
    setupEventListeners();
});

// Initialize CodeMirror editor
function initializeEditor() {
    const textarea = document.getElementById('code-editor');
    editor = CodeMirror.fromTextArea(textarea, {
        mode: 'text/x-csrc',
        lineNumbers: true,
        theme: 'default',
        indentUnit: 2,
        tabSize: 2,
        indentWithTabs: false,
        lineWrapping: true,
        matchBrackets: true,
        autoCloseBrackets: true
    });
}

// Load examples from server
async function loadExamples() {
    try {
        const response = await fetch('/examples');
        if (response.ok) {
            examples = await response.json();
            populateExamplesDropdown();
        } else {
            console.error('Failed to load examples:', response.statusText);
        }
    } catch (error) {
        console.error('Error loading examples:', error);
    }
}

// Populate the examples dropdown
function populateExamplesDropdown() {
    const select = document.getElementById('examples-select');
    
    // Clear existing options except the first one
    select.innerHTML = '<option value="">Select an example...</option>';
    
    let defaultExampleIndex = -1;
    
    examples.forEach((example, index) => {
        const option = document.createElement('option');
        option.value = index;
        option.textContent = example.name;
        select.appendChild(option);
        
        // Check if this is the default example
        if (example.name === DEFAULT_EXAMPLE) {
            defaultExampleIndex = index;
        }
    });
    
    // Auto-load default example if found
    if (defaultExampleIndex >= 0) {
        select.value = defaultExampleIndex;
        editor.setValue(examples[defaultExampleIndex].content);
    }
}

// Setup event listeners
function setupEventListeners() {
    // Examples dropdown change
    document.getElementById('examples-select').addEventListener('change', function(e) {
        const selectedIndex = e.target.value;
        if (selectedIndex !== '' && examples[selectedIndex]) {
            editor.setValue(examples[selectedIndex].content);
        }
    });
    
    // Analyze button click
    document.getElementById('analyze-btn').addEventListener('click', analyzeCode);
    
    // Enter key in editor triggers analysis (Ctrl+Enter)
    editor.setOption('extraKeys', {
        'Ctrl-Enter': analyzeCode
    });
}

// Analyze the code
async function analyzeCode() {
    const code = editor.getValue().trim();
    
    if (!code) {
        showResults('Please enter some CUDA code to analyze.', 'error');
        return;
    }
    
    const analyzeBtn = document.getElementById('analyze-btn');
    const loading = document.getElementById('loading');
    const results = document.getElementById('results');
    
    // Show loading state
    analyzeBtn.disabled = true;
    analyzeBtn.textContent = 'Analyzing...';
    loading.classList.remove('hidden');
    results.innerHTML = '';
    
    try {
        const response = await fetch('/analyze', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({ code: code })
        });
        
        if (response.ok) {
            const result = await response.json();
            displayAnalysisResults(result);
        } else {
            const errorText = await response.text();
            showResults(`Server error: ${errorText}`, 'error');
        }
    } catch (error) {
        showResults(`Network error: ${error.message}`, 'error');
    } finally {
        // Reset UI state
        analyzeBtn.disabled = false;
        analyzeBtn.textContent = 'Analyze Code';
        loading.classList.add('hidden');
    }
}

// Parse faial-drf JSON output
function parseFaialOutput(jsonData) {
    return {
        kernels: jsonData.kernels || [],
        metadata: {
            argv: jsonData.argv || [],
            executable: jsonData.executable_name || '',
            z3_version: jsonData.z3_version || ''
        }
    };
}

// Display analysis results
function displayAnalysisResults(result) {
    const resultsContainer = document.getElementById('results');
    
    if (result.error) {
        showResults(`Error: ${result.error}`, 'error');
        return;
    }
    
    // Clear any existing line markers
    clearErrorMarkers();
    
    // Handle JSON structured output
    if (result.data) {
        displayStructuredResults(result.data);
        return;
    }
    
    // Fallback to plain text output
    let output = '';
    
    if (result.success) {
        output += '<div class="success">✓ Analysis completed successfully</div>';
    } else {
        output += '<div class="error">✗ Analysis found issues</div>';
    }
    
    if (result.stdout) {
        output += `<div class="output-section">
            <strong>Output:</strong>
            <pre>${escapeHtml(result.stdout)}</pre>
        </div>`;
    }
    
    if (result.stderr) {
        output += `<div class="error-section">
            <strong>Errors/Warnings:</strong>
            <pre>${escapeHtml(result.stderr)}</pre>
        </div>`;
    }
    
    output += `<div class="info-section">
        <small>Exit code: ${result.returncode}</small>
    </div>`;
    
    resultsContainer.innerHTML = output;
}

// Display structured JSON results
function displayStructuredResults(data) {
    const resultsContainer = document.getElementById('results');
    const parsed = parseFaialOutput(data);
    
    let output = '<div class="analysis-results">';
    
    // Overall status
    const hasRacyKernels = parsed.kernels.some(k => k.status === 'racy');
    output += `<div class="overall-status ${hasRacyKernels ? 'error' : 'success'}">
        ${hasRacyKernels ? '⚠ Data races detected' : '✓ All kernels are data-race free'}
    </div>`;
    
    // Kernel results
    parsed.kernels.forEach(kernel => {
        output += createKernelResultCard(kernel);
    });
    
    // Metadata
    output += `<div class="metadata">
        <details>
            <summary>Analysis Details</summary>
            <div class="metadata-content">
                <p><strong>Tool:</strong> ${parsed.metadata.executable}</p>
                <p><strong>Z3 Version:</strong> ${parsed.metadata.z3_version}</p>
                <p><strong>Command:</strong> ${parsed.metadata.argv.join(' ')}</p>
            </div>
        </details>
    </div>`;
    
    output += '</div>';
    resultsContainer.innerHTML = output;
    
    // Automatically highlight all error locations in the editor
    highlightAllErrorLocations(parsed.kernels);
}

// Create kernel result card
function createKernelResultCard(kernel) {
    const isDrf = kernel.status === 'drf';
    const statusIcon = isDrf ? '✓' : '⚠';
    const statusClass = isDrf ? 'drf-status' : 'racy-status';
    
    let card = `<div class="kernel-card">
        <div class="kernel-header">
            <h3><span class="${statusClass}">${statusIcon}</span> Kernel: ${kernel.kernel_name}</h3>
            <div class="status-badge ${statusClass}">${kernel.status.toUpperCase()}</div>
        </div>`;
    
    // Show errors if any
    if (kernel.errors && kernel.errors.length > 0) {
        card += '<div class="errors-section">';
        kernel.errors.forEach((error, index) => {
            card += createErrorCard(error, index);
        });
        card += '</div>';
    }
    
    // Show logics used
    if (kernel.logics && kernel.logics.length > 0) {
        card += `<div class="logics-info">
            <strong>Analysis Logic:</strong> ${kernel.logics.join(', ')}
        </div>`;
    }
    
    card += '</div>';
    return card;
}

// Create error card for data races
function createErrorCard(error, index) {
    const summary = error.summary;
    const counterExample = error.counter_example;
    
    let card = `<div class="error-card">
        <div class="error-summary">
            <h4>Data Race #${index + 1}</h4>
            <p><strong>Array:</strong> ${summary.array_name}</p>
        </div>`;
    
    if (counterExample) {
        card += '<div class="counter-example">';
        card += '<div class="thread-conflicts">';
        
        // Task 1
        const task1 = counterExample.task1;
        card += createThreadCard('Thread 1', task1, 1);
        
        // Task 2  
        const task2 = counterExample.task2;
        card += createThreadCard('Thread 2', task2, 2);
        
        card += '</div>';
        
        // Globals
        if (counterExample.globals) {
            card += `<div class="globals-info">
                <strong>Block Configuration:</strong>
                <span class="coord">blockIdx(${counterExample.globals['blockIdx.x']}, ${counterExample.globals['blockIdx.y']}, ${counterExample.globals['blockIdx.z']})</span>
            </div>`;
        }
        
        card += '</div>';
    }
    
    card += '</div>';
    return card;
}

// Create thread card
function createThreadCard(title, task, threadNum) {
    const modeClass = task.mode === 'rw' ? 'read-write' : 'read-only';
    const modeText = task.mode === 'rw' ? 'Read/Write' : 'Read Only';
    
    let card = `<div class="thread-card thread-${threadNum}">
        <div class="thread-header">
            <strong>${title}</strong>
            <span class="access-mode ${modeClass}">${modeText}</span>
        </div>`;
    
    // Thread coordinates
    card += `<div class="thread-coords">
        threadIdx(${task.locals['threadIdx.x']}, ${task.locals['threadIdx.y']}, ${task.locals['threadIdx.z']})
    </div>`;
    
    card += '</div>';
    return card;
}

// Show simple results message
function showResults(message, type = 'info') {
    const resultsContainer = document.getElementById('results');
    const className = type === 'error' ? 'error' : type === 'success' ? 'success' : '';
    resultsContainer.innerHTML = `<div class="${className}">${escapeHtml(message)}</div>`;
}

// Escape HTML to prevent XSS
function escapeHtml(text) {
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
}

// Error markers for CodeMirror
let errorMarkers = [];

// Clear error markers from editor
function clearErrorMarkers() {
    errorMarkers.forEach(marker => marker.clear());
    errorMarkers = [];
}

// Highlight all error locations automatically
function highlightAllErrorLocations(kernels) {
    clearErrorMarkers();
    
    kernels.forEach(kernel => {
        if (kernel.errors) {
            kernel.errors.forEach(error => {
                if (error.counter_example) {
                    const counterExample = error.counter_example;
                    
                    // Highlight task1 location
                    if (counterExample.task1 && counterExample.task1.location) {
                        const loc1 = counterExample.task1.location;
                        const from1 = { line: loc1.line - 1, ch: loc1.col_start - 1 };
                        const to1 = { line: loc1.line - 1, ch: loc1.col_finish - 1 };
                        const marker1 = editor.markText(from1, to1, { 
                            className: 'thread-1-highlight',
                            title: `Thread 1 (${counterExample.task1.mode === 'rw' ? 'Read/Write' : 'Read Only'})`
                        });
                        errorMarkers.push(marker1);
                    }
                    
                    // Highlight task2 location
                    if (counterExample.task2 && counterExample.task2.location) {
                        const loc2 = counterExample.task2.location;
                        const from2 = { line: loc2.line - 1, ch: loc2.col_start - 1 };
                        const to2 = { line: loc2.line - 1, ch: loc2.col_finish - 1 };
                        const marker2 = editor.markText(from2, to2, { 
                            className: 'thread-2-highlight',
                            title: `Thread 2 (${counterExample.task2.mode === 'rw' ? 'Read/Write' : 'Read Only'})`
                        });
                        errorMarkers.push(marker2);
                    }
                }
            });
        }
    });
    
    // Scroll to first error if any
    if (errorMarkers.length > 0) {
        const firstError = kernels.find(k => k.errors && k.errors.length > 0);
        if (firstError && firstError.errors[0].counter_example && firstError.errors[0].counter_example.task1.location) {
            const firstLine = firstError.errors[0].counter_example.task1.location.line - 1;
            editor.scrollIntoView({ line: firstLine, ch: 0 }, 100);
        }
    }
}

// Add some utility styles for the output
const style = document.createElement('style');
style.textContent = `
    .output-section, .error-section, .info-section {
        margin: 10px 0;
    }
    
    .output-section pre, .error-section pre {
        background: #f8f9fa;
        border: 1px solid #e9ecef;
        border-radius: 4px;
        padding: 10px;
        margin: 5px 0;
        white-space: pre-wrap;
        word-wrap: break-word;
    }
    
    .error-section pre {
        background: #fff5f5;
        border-color: #fed7d7;
        color: #c53030;
    }
    
    .info-section {
        color: #6c757d;
        font-size: 12px;
        text-align: right;
    }
    
    /* New structured results styles */
    .analysis-results {
        margin-top: 20px;
    }
    
    .overall-status {
        padding: 1em;
        border-radius: 8px;
        margin-bottom: 20px;
        font-weight: bold;
        font-size: 16px;
    }
    
    .overall-status.success {
        background: #d4edda;
        color: #155724;
        border: 1px solid #c3e6cb;
    }
    
    .overall-status.error {
        background: #f8d7da;
        color: #721c24;
        border: 1px solid #f5c6cb;
    }
    
    .kernel-card {
        border: 1px solid #e9ecef;
        border-radius: 8px;
        margin-bottom: 20px;
        background: white;
    }
    
    .kernel-header {
        display: flex;
        justify-content: space-between;
        align-items: center;
        padding: 0.8em;
        background: #f8f9fa;
        border-bottom: 1px solid #e9ecef;
        border-radius: 8px 8px 0 0;
    }
    
    .kernel-header h3 {
        margin: 0;
        display: flex;
        align-items: center;
        gap: 10px;
    }
    
    .status-badge {
        padding: 4px 12px;
        border-radius: 20px;
        font-size: 12px;
        font-weight: bold;
    }
    
    .drf-status {
        color: #155724;
    }
    
    .status-badge.drf-status {
        background: #d4edda;
        color: #155724;
    }
    
    .racy-status {
        color: #721c24;
    }
    
    .status-badge.racy-status {
        background: #f8d7da;
        color: #721c24;
    }
    
    .errors-section {
        padding: 1em;
    }
    
    .error-card {
        border: 1px solid #f5c6cb;
        border-radius: 6px;
        margin-bottom: 15px;
        background: #fff5f5;
    }
    
    .error-summary {
        padding: 0.8em;
        border-bottom: 1px solid #f5c6cb;
        background: #f8d7da;
    }
    
    .error-summary h4 {
        margin: 0 0 8px 0;
        color: #721c24;
    }
    
    .counter-example {
        padding: 1em;
    }
    
    .thread-conflicts {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 15px;
        margin-bottom: 15px;
    }
    
    .thread-card {
        border: 1px solid #dee2e6;
        border-radius: 4px;
        padding: 0.8em;
        background: white;
    }
    
    .thread-1 {
        border-left: 4px solid #007bff;
    }
    
    .thread-2 {
        border-left: 4px solid #dc3545;
    }
    
    .thread-header {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-bottom: 8px;
    }
    
    .access-mode {
        padding: 2px 8px;
        border-radius: 12px;
        font-size: 11px;
        font-weight: bold;
    }
    
    .read-write {
        background: #fff3cd;
        color: #856404;
    }
    
    .read-only {
        background: #d1ecf1;
        color: #0c5460;
    }
    
    .thread-coords {
        font-family: monospace;
        background: #f8f9fa;
        padding: 4px 8px;
        border-radius: 3px;
        margin: 8px 0;
        font-size: 13px;
    }
    
    .source-location {
        margin-top: 10px;
        font-size: 13px;
    }
    
    .highlight-btn {
        background: #007bff;
        color: white;
        border: none;
        padding: 4px 8px;
        border-radius: 3px;
        font-size: 11px;
        cursor: pointer;
        margin-left: 10px;
    }
    
    .highlight-btn:hover {
        background: #0056b3;
    }
    
    .globals-info {
        margin-top: 10px;
        padding: 10px;
        background: #e9ecef;
        border-radius: 4px;
        font-size: 13px;
    }
    
    .coord {
        font-family: monospace;
    }
    
    .logics-info {
        padding: 12px;
        border-top: 1px solid #e9ecef;
        background: #f8f9fa;
        font-size: 13px;
    }
    
    .metadata {
        margin-top: 20px;
        border: 1px solid #e9ecef;
        border-radius: 6px;
    }
    
    .metadata summary {
        padding: 10px 15px;
        background: #f8f9fa;
        cursor: pointer;
        border-radius: 6px 6px 0 0;
    }
    
    .metadata-content {
        padding: 1em;
    }
    
    .metadata-content p {
        margin: 5px 0;
        font-size: 13px;
    }
    
    /* CodeMirror highlighting styles */
    .error-highlight {
        background-color: rgba(255, 0, 0, 0.2);
        border-bottom: 2px solid #dc3545;
    }
    
    .thread-1-highlight {
        background-color: rgba(0, 123, 255, 0.3);
        border-bottom: 3px solid #007bff;
        border-radius: 3px;
    }
    
    .thread-2-highlight {
        background-color: rgba(220, 53, 69, 0.3);
        border-bottom: 3px solid #dc3545;
        border-radius: 3px;
    }
    
    /* Add legend for highlighting */
    .highlighting-legend {
        margin: 15px 0;
        padding: 12px;
        background: #f8f9fa;
        border-radius: 6px;
        border: 1px solid #e9ecef;
    }
    
    .legend-item {
        display: inline-flex;
        align-items: center;
        margin-right: 20px;
        font-size: 13px;
    }
    
    .legend-color {
        width: 20px;
        height: 12px;
        border-radius: 3px;
        margin-right: 8px;
        border: 1px solid #ccc;
    }
    
    .legend-thread-1 {
        background-color: rgba(0, 123, 255, 0.3);
        border-bottom: 3px solid #007bff;
    }
    
    .legend-thread-2 {
        background-color: rgba(220, 53, 69, 0.3);
        border-bottom: 3px solid #dc3545;
    }
    
    @media (max-width: 768px) {
        .thread-conflicts {
            grid-template-columns: 1fr;
        }
        
        .kernel-header {
            flex-direction: column;
            align-items: stretch;
            gap: 10px;
        }
    }
`;
document.head.appendChild(style);