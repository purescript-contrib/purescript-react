# This is originally from https://github.com/nebstrebor/react-tutorial-simpleserver
# It was modified a bit in the process.

import BaseHTTPServer
import SimpleHTTPServer
import json
import urlparse


class ReactTutorialHTTPRequestHandler(SimpleHTTPServer.SimpleHTTPRequestHandler):
    """
    Extension of SimpleHTTPRequestHandler that works with the react.js tutorial.
    In addition to standard SimpleHTTPServer file-webserver functionality, adds
    POST-ability.

    USAGE: python server.py to serve files from the cwd
    (works the same as running python -m SimpleHTTPServer in the directory)
    """

    def do_POST(self):
        # (1) get posted data & convert it to python dict
        content_length = int(self.headers['Content-Length'])
        post_data = dict(urlparse.parse_qsl(self.rfile.read(content_length).decode('utf-8')))
        # (2) open the file at the requested URL (404 if bad)
        with open(self.translate_path(self.path), 'r+w') as f:
            comments = json.loads(f.read())
            comments.append(post_data)
            f.seek(0, 0)
            f.write(json.dumps(comments))
            f.truncate()

        return self.do_GET()

def test(HandlerClass=ReactTutorialHTTPRequestHandler,
         ServerClass=BaseHTTPServer.HTTPServer):
    BaseHTTPServer.test(HandlerClass, ServerClass)


if __name__ == '__main__':
    test()
