#!/usr/bin/env python
import os
import re
# from threading import Lock
from flask import Flask, render_template, session, request, url_for
from flask_socketio import SocketIO, disconnect#, emit

from clippy import Clippy
from display_result import processFinal

# Set this variable to "threading", "eventlet" or "gevent" to test the
# different async modes, or leave it set to None for the application to choose
# the best option based on installed packages.
async_mode = None

app = Flask(__name__)
app.config['SECRET_KEY'] = 'secret!'
socketio = SocketIO(app, async_mode=async_mode)
namespace = "/"
# thread = None
# thread_lock = Lock()
clippies = {}
dbnames = {}

def background_thread(sid):
	final = clippies[sid].run()
	dbname = dbnames[sid]
	del clippies[sid]
	del dbnames[sid]
	processFinal(final, dbname, socketio, sid)

@app.context_processor
def override_url_for():
	return dict(url_for=dated_url_for)

def dated_url_for(endpoint, **values):
	if endpoint == 'static':
		filename = values.get('filename', None)
		if filename:
			file_path = os.path.join(app.root_path, endpoint, filename)
			values['q'] = int(os.stat(file_path).st_mtime)
	return url_for(endpoint, **values)

@app.route('/')
def landing():
	return render_template('landing.html')

@app.route('/<source>/<dbname>')
def index(source, dbname):
	return render_template('index.html', async_mode=socketio.async_mode)

@socketio.on('connect', namespace=namespace)
def test_connect():
	print('Client connected', request.sid)
	match = re.search(r"/(\w+)/(\w+)$", request.environ["HTTP_REFERER"])
	source = match.group(1) if match else "bto_test"
	dbnames[request.sid] = match.group(2) if match else "units"
	clippies[request.sid] = Clippy(socketio, request.sid, source)
	socketio.start_background_task(background_thread, request.sid)
	# global thread
	# with thread_lock:
	# 	if thread is None:
	# 		thread = socketio.start_background_task(background_thread, request.sid)

@socketio.on('disconnect_request', namespace=namespace)
def disconnect_request():
	disconnect()

@socketio.on('disconnect', namespace=namespace)
def test_disconnect():
	if request.sid in clippies:
		del clippies[request.sid]
	print('Client disconnected', request.sid)

@socketio.on('answer', namespace=namespace)
def test_answer(message):
	print("received from socket {}: {}".format(request.sid, message['data']))
	clippies[request.sid].setAns(message['data'])

if __name__ == '__main__':
	socketio.run(app, debug=True, port=5000, host="0.0.0.0")
