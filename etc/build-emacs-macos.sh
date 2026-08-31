#!/bin/bash
# Build Emacs from source on macOS with native compilation support
# Uses Homebrew for dependencies

set -e

EMACS_SRC_DIR="${EMACS_SRC_DIR:-$HOME/code/emacs}"
HOMEBREW_PREFIX="$(brew --prefix)"

# Detect latest GCC version installed via Homebrew
GCC_VERSION=$(ls -1 "$HOMEBREW_PREFIX/lib/gcc" | grep -E '^[0-9]+$' | sort -n | tail -1)
GCC_LIB_DIR="$HOMEBREW_PREFIX/lib/gcc/$GCC_VERSION"
GCC_INCLUDE_DIR="$GCC_LIB_DIR/include"

echo "=== Build Configuration ==="
echo "Emacs source: $EMACS_SRC_DIR"
echo "Homebrew prefix: $HOMEBREW_PREFIX"
echo "GCC version: $GCC_VERSION"
echo "GCC lib dir: $GCC_LIB_DIR"
echo ""

# Check if source directory exists
if [ ! -d "$EMACS_SRC_DIR" ]; then
    echo "Emacs source not found at $EMACS_SRC_DIR"
    echo "Clone with: git clone git://git.savannah.gnu.org/emacs.git $EMACS_SRC_DIR"
    exit 1
fi

cd "$EMACS_SRC_DIR"

# Install dependencies
echo "=== Installing dependencies ==="
brew install pkg-config texinfo libgccjit autoconf automake make jansson \
     gnutls mailutils imagemagick tree-sitter

# Set up environment
export CFLAGS="-O2 -I$GCC_INCLUDE_DIR -L$GCC_LIB_DIR"
export LDFLAGS="-L$GCC_LIB_DIR -Wl,-rpath,$GCC_LIB_DIR"
export PKG_CONFIG_PATH="$HOMEBREW_PREFIX/lib/pkgconfig:$HOMEBREW_PREFIX/opt/libxml2/lib/pkgconfig"

# Clean slate: drop all build products so nothing stale survives into the app
# bundle (a leftover Emacs.pdmp will not match the new binary and Emacs.app
# refuses to start).
echo "=== Cleaning previous build ==="
[ -f Makefile ] && gmake distclean || true
rm -rf nextstep/Emacs.app

# Run autogen if needed
if [ ! -f configure ] || [ configure.ac -nt configure ]; then
    echo "=== Running autogen.sh ==="
    ./autogen.sh
fi

# Configure
echo "=== Configuring ==="
./configure \
    --prefix=/usr/local \
    --with-ns \
    --with-tree-sitter \
    --with-native-compilation \
    --with-imagemagick \
    --with-mailutils \
    --without-compress-install

# Build
NCPU=$(sysctl -n hw.ncpu)
echo "=== Building with $NCPU cores ==="
gmake -j"$NCPU"

# Populate the app bundle. Plain `gmake` only copies the binary into
# Emacs.app; `gmake install` is what puts Emacs.pdmp beside it. This build is
# self-contained (--with-ns), so install writes into Emacs.app, not /usr/local.
echo "=== Installing into Emacs.app ==="
gmake install

# Fail loudly here rather than at double-click time, where Emacs.app exits
# silently if the dump file does not match the binary.
echo "=== Verifying ==="
./nextstep/Emacs.app/Contents/MacOS/Emacs --batch \
    --eval '(message "Emacs %s starts cleanly" emacs-version)'

echo "=== Done! ==="
echo "Build complete. Emacs.app is at $EMACS_SRC_DIR/nextstep/Emacs.app"
