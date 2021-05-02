#!/usr/bin/env python
"""
This script allows you to test the lambda in your local browser.

It is a simple proxy which emulates API gateway.  It spawns a simple python
server on localhost, reads a request, converts it to the expected format and
then sends it to the 'Process' class which in turn sends it to the Haskell
binary.  We read the result back out and create a proper HTTP response.
"""

from http.server import BaseHTTPRequestHandler, HTTPServer

import json
import sys
import urllib

import main

def make_proxy_server(process):
  class ProxyServer(BaseHTTPRequestHandler):
    def do_GET(self):
      self.proxy('GET')

    def do_POST(self):
      self.proxy('POST')

    def proxy(self, method):
      # Set up event
      event = {}
      event['httpMethod'] = method

      # Parse URL bits
      url = urllib.parse.urlparse(self.path)
      event['path'] = url.path
      event['queryStringParameters'] = urllib.parse.parse_qs(url.query)

      # Parse body
      content_length = self.headers.get('content-length')
      if content_length:
        event['body'] = self.rfile.read(int(content_length)).decode()

      # Forward event to process
      response = process.handler(event)

      # Return response code
      self.send_response(response['statusCode'])

      # Return response headers
      for k in response['headers']:
        self.send_header(k, response['headers'][k])
      self.end_headers()

      # Return response body
      self.wfile.write(response['body'].encode())

  return ProxyServer

if __name__ == '__main__':
  print('starting lambda process...', file=sys.stderr)
  process = main.Process(['stack', 'exec', 'zureg-web'])

  port = 8080
  print('starting server on http://localhost:{}/ ...'.format(port),
      file=sys.stderr)
  server_address = ('0.0.0.0', port)
  proxy_server = make_proxy_server(process)
  httpd = HTTPServer(server_address, proxy_server)

  print('running server...', file=sys.stderr)
  httpd.serve_forever()
