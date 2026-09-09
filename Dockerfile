# Start from the official Nix image
FROM nixos/nix:latest

# Enable flakes and Nix commands
RUN echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf

# Set up your working directory
WORKDIR /app

# Copy your source code and flake files
COPY . .

# Build the flake's default package
RUN nix build .#bully-server

# Run the binary generated in the ./result symlink
CMD ["./result/bin/bully-server"]