# Start the server
run:
    cabal run blog:blog-server -- \
      --data .blog-data \
      run \
      --cert tls/localhost.crt \
      --key tls/localhost.key \
      --port 8080

# Format project
format:
    fd -e hs | xargs -n 1 -P $(nproc) fourmolu -i -q

# Regenerate cabal2nix files
cabal2nix:
    cabal2nix . > blog.nix

# Generate a certificate authority
ca:
    @# https://learn.microsoft.com/en-us/azure/application-gateway/self-signed-certificates
    mkdir -p tls

    openssl ecparam \
        -out tls/root.key \
        -name prime256v1 \
        -genkey

    openssl req \
        -new \
        -sha256 \
        -key tls/root.key \
        -out tls/root.csr \
        -subj "/C=AU/O=blog.localhost"

    openssl x509 \
        -req \
        -sha256 \
        -in tls/root.csr \
        -signkey tls/root.key \
        -out tls/root.crt

# Generate a self-signed certificate
tls:
    @# https://learn.microsoft.com/en-us/azure/application-gateway/self-signed-certificates
    openssl ecparam \
        -out tls/localhost.key \
        -name prime256v1 \
        -genkey

    openssl req \
        -new \
        -sha256 \
        -key tls/localhost.key \
        -out tls/localhost.csr \
        -subj "/C=AU/O=blog.localhost/CN=localhost"

    openssl x509 \
        -req \
        -sha256 \
        -in tls/localhost.csr \
        -CA tls/root.crt \
        -CAkey tls/root.key \
        -CAcreateserial \
        -out tls/localhost.crt
