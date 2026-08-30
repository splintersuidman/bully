# Bully

Bullies documents into bulletins.

Automate assembling regular bulletins with contributions from various sources using various formats.

## Usage

Run the server with `nix run github:splintersuidman/bully` if you have Nix with flakes, or with `cabal run bully-server` if you have the repository checked out.
This will by default listen on port 8080.
Compilation requests are to be sent to the `/compile` endpoint, with a JSON body.

For an example, see the `example/` directory and [`Example.hs`](./app/Main/Example.hs).
The input files are [`template.typ`](./example/template.typ), [`contribution1.md`](./example/contribution1.md), and [`contribution2.md`](./example/contribution2.md).
The Haskell program, which you can run with `cabal run bully-example`, produces the request body [`example/request.json`](./example/request.json).
With the server running, use
```bash
curl \
  -X GET \
  -d @example/request.json \
  -H 'Accept: application/octet-stream' \
  -H 'Content-type: application/json' \
  http://localhost:8080/compile \
  --output example/result.pdf
```
to produce the output file [`result.pdf`](./example/result.pdf).
