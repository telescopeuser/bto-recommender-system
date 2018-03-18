#!/usr/bin/env python
import eventlet as eventlib
eventlib.monkey_patch()
import clips

namespace = "/"

ENV_SPECIFIC_FUNCTIONS = {}

def envCallSpecificFunction(e_id, funcname, *args):
	f = ENV_SPECIFIC_FUNCTIONS[e_id][funcname]
	return f(*args)

clips.RegisterPythonFunction(envCallSpecificFunction, 'env-call-specific-func')

def PrepareEnvironment(e, sid):
	eid = clips.Symbol("eid-" + sid)
	ENV_SPECIFIC_FUNCTIONS[eid] = {}
	e.Identifier = eid

def DestroyEnvironment(e):
	del ENV_SPECIFIC_FUNCTIONS[e.Identifier]
 
def AddSpecificFunction(e, func, funcname=None):
	try:
		eid = e.Identifier
	except:
		raise ValueError("The Environment has not been prepared")
	if funcname is None:
		funcname = func.__name__
	ENV_SPECIFIC_FUNCTIONS[eid][funcname] = func
	num_args = func.func_code.co_argcount
	seq_args = " ".join(['?a%s' % x for x in range(1, num_args)]) # must put 1 for instance functions
	command = "(return (python-call env-call-specific-func %s %s %s))" % (eid, funcname, seq_args)
	e.BuildFunction(
		funcname,
		seq_args,
		command
	)

class TempAns:
	def clearAns(self):
		self.temp_ans = None

	def __init__(self):
		self.temp_ans = self.clearAns()

	def setAns(self, ans):
		self.temp_ans = ans

	def getAns(self):
		return self.temp_ans

	def hasAns(self):
		return self.temp_ans != None

class Clippy:
	def __init__(self, socket, sid, source):
		# clips.RegisterPythonFunction(self.clips_debug, "debug")
		# clips.RegisterPythonFunction(self.clips_alert, "print")
		# clips.RegisterPythonFunction(self.clips_prompt, "ask")
		clipsEnv = clips.Environment()
		PrepareEnvironment(clipsEnv, sid)
		AddSpecificFunction(clipsEnv, self.clips_debug, "debug")
		AddSpecificFunction(clipsEnv, self.clips_alert, "alert")
		AddSpecificFunction(clipsEnv, self.clips_prompt, "prompt")
		AddSpecificFunction(clipsEnv, self.clips_prompt2, "prompt2")
		AddSpecificFunction(clipsEnv, self.clips_final, "final")
		# clipsEnv.Load("header_python.clp")
		clipsEnv.Load("{}.clp".format(source))
		self.ta = TempAns()
		self.socket = socket
		self.sid = sid
		self.clips = clips
		self.clipsEnv = clipsEnv
		self.final = None

	def clips_debug(self, message):
		print(message)
		self.socket.emit('debug', {'data': message}, namespace=namespace, room=self.sid)
		eventlib.sleep(.01)

	def clips_alert(self, message):
		print(message)
		self.socket.emit('alert', {'data': message}, namespace=namespace, room=self.sid)
		eventlib.sleep(.01)

	def clips_prompt(self, message, options):
		print(message)
		print(options)
		self.socket.emit('prompt', {'data': message, 'options': [str(i) for i in options]}, namespace=namespace, room=self.sid)
		self.ta.clearAns()
		while not self.ta.hasAns():
			eventlib.sleep(1)
		user_input = self.ta.getAns()
		try:
			int(user_input)
			return self.clips.Integer(user_input)
		except:
			return self.clips.Symbol(user_input)

	def clips_prompt2(self, message, display, options):
		print(message)
		zipped = zip([str(i) for i in options], display.split("\n"))
		print(zipped)
		self.socket.emit('prompt2', {'data': message, 'options': zipped}, namespace=namespace, room=self.sid)
		self.ta.clearAns()
		while not self.ta.hasAns():
			eventlib.sleep(1)
		user_input = self.ta.getAns()
		try:
			int(user_input)
			return self.clips.Integer(user_input)
		except:
			return self.clips.Symbol(user_input)

	def clips_final(self, message):
		print(message)
		self.socket.emit('debug', {'data': message}, namespace=namespace, room=self.sid)
		self.final = message

	def run(self):
		eventlib.sleep(.01) # necessary with eventlet or first question won't appear (too soon after connect)
		self.clipsEnv.Reset()
		self.clipsEnv.Run()
		DestroyEnvironment(self.clipsEnv)
		return self.final

	def setAns(self, ans):
		self.ta.setAns(ans)
