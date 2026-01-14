#!/bin/sh

# Configure script for Faial development environment

set -e  # Exit on any error

show_usage() {
    echo "Usage: $0 [--create-switch] [opam options...]"
    echo ""
    echo "Options:"
    echo "  --create-switch   Create/use local opam switch (recommended)"
    echo ""
    echo "All other arguments are passed to 'opam install'."
    echo ""
    echo "Examples:"
    echo "  $0 --create-switch              # Create local switch, install pinned deps"
    echo "  $0 --create-switch --yes        # Auto-confirm installation"
    echo "  $0 --yes                        # Install in current switch"
}

if ! which opam > /dev/null; then
    >&2 echo "ERROR: Install opam first!"
    exit 1
fi

# Parse --create-switch flag
CREATE_SWITCH=false
OPAM_ARGS=""

for arg in "$@"; do
    case $arg in
        --create-switch)
            CREATE_SWITCH=true
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

echo "Setting up Faial development environment..."

if [ "$CREATE_SWITCH" = true ]; then
    # Check if local switch already exists
    if [ -d "_opam" ] || opam switch show 2>/dev/null | grep -q "$(pwd)"; then
        echo "Local switch already exists, using it..."
        eval $(opam env)
    else
        echo "Creating local switch with OCaml 5.3.0..."
        opam switch create . 5.3.0 --no-install
        eval $(opam env)
    fi
else
    echo "Using current opam switch: $(opam switch show)"
fi

# Install dependencies using lock file for reproducibility
echo "Installing dependencies from faial.opam.locked..."
CMD="opam install . --locked --deps-only$OPAM_ARGS"
echo "$ $CMD"
opam install . --locked --deps-only $OPAM_ARGS

echo "✅ Setup complete!"
if [ "$CREATE_SWITCH" = true ]; then
    echo "To activate the environment, run: eval \$(opam env)"
fi
