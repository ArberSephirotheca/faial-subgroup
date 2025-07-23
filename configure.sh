#!/bin/sh

# Configure script for Faial development environment

set -e  # Exit on any error

show_usage() {
    echo "Usage: $0 [--local|--system] [opam install options]"
    echo ""
    echo "Options:"
    echo "  --local   Create/use local opam switch (isolated environment)"
    echo "  --system  Install in current opam switch"
    echo ""
    echo "Examples:"
    echo "  $0 --local              # Create local switch and install deps"
    echo "  $0 --system             # Install in current switch"
    echo "  $0 --local --yes        # Auto-confirm installation"
}

if ! which opam > /dev/null; then
    >&2 echo "ERROR: Install opam first!"
    exit 1
fi

# Parse arguments
USE_LOCAL=false
USE_SYSTEM=false
OPAM_ARGS=""

if [ $# -eq 0 ]; then
    show_usage
    exit 1
fi

for arg in "$@"; do
    case $arg in
        --local)
            USE_LOCAL=true
            ;;
        --system)
            USE_SYSTEM=true
            ;;
        --help|-h)
            show_usage
            exit 0
            ;;
        *)
            OPAM_ARGS="$OPAM_ARGS $arg"
            ;;
    esac
done

# Validate arguments
if [ "$USE_LOCAL" = true ] && [ "$USE_SYSTEM" = true ]; then
    >&2 echo "ERROR: Cannot use both --local and --system"
    exit 1
fi

if [ "$USE_LOCAL" = false ] && [ "$USE_SYSTEM" = false ]; then
    >&2 echo "ERROR: Must specify either --local or --system"
    show_usage
    exit 1
fi

echo "Setting up Faial development environment..."

if [ "$USE_SYSTEM" = true ]; then
    echo "Using current opam switch: $(opam switch show)"
else
    # Check if local switch already exists
    if [ -d "_opam" ] || opam switch show 2>/dev/null | grep -q "$(pwd)"; then
        echo "Local switch already exists, using it..."
        eval $(opam env)
    else
        echo "Creating local switch with OCaml 5.3.0..."
        opam switch create . 5.3.0 --no-install
        eval $(opam env)
    fi
fi

# Install all dependencies from committed faial.opam file
echo "Installing dependencies from faial.opam..."
opam install --deps-only . $OPAM_ARGS

echo "✅ Setup complete!"
if [ "$USE_LOCAL" = true ]; then
    echo "To activate the environment, run: eval \$(opam env)"
fi
