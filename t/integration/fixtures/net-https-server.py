import gzip
import http.server
import ssl
import sys
import time


class Handler(http.server.BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def _respond(self, include_body):
        content_encoding = None
        if self.path == "/large":
            body = b"x" * (16 * 1024 * 1024 + 1)
        elif self.path == "/compressed-large":
            body = gzip.compress(b"x" * (16 * 1024 * 1024 + 1))
            content_encoding = "gzip"
        else:
            body = b"loopback response\n"
        self.send_response(200)
        self.send_header("Content-Type", "text/plain; charset=utf-8")
        if content_encoding:
            self.send_header("Content-Encoding", content_encoding)
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        if include_body:
            try:
                self.wfile.write(body)
            except (BrokenPipeError, ConnectionResetError):
                pass

    def do_GET(self):
        if self.path == "/delay":
            time.sleep(0.5)
        self._respond(True)

    def do_HEAD(self):
        self._respond(False)

    def log_message(self, format, *args):
        pass


class Server(http.server.ThreadingHTTPServer):
    daemon_threads = True
    allow_reuse_address = True


server = Server(("127.0.0.1", 0), Handler)
context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
context.load_cert_chain(sys.argv[1], sys.argv[2])
context.set_alpn_protocols(["http/1.1"])
server.socket = context.wrap_socket(server.socket, server_side=True)
print(server.server_port, flush=True)
server.serve_forever()
