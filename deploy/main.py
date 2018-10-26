from __future__ import print_function

import json
import os
import subprocess
import sys

class Process:
  def __init__(self, command):
    self.__command = command
    self.__process = None
    self.__requests = 0

  def __spawn(self):
    if self.__process is None or self.__process.poll() is not None:
      self.__process = subprocess.Popen(self.__command,
          stdin=subprocess.PIPE, stdout=subprocess.PIPE)
      print('Spawned {} process'.format(' '.join(self.__command)),
          file=sys.stderr)
      self.__requests = 0

  def handler(self, event):
    self.__spawn()
    serialized_event = json.dumps(event) + '\n'
    self.__process.stdin.write(serialized_event.encode('utf-8'))
    self.__process.stdin.flush()
    response = json.loads(self.__process.stdout.readline())
    self.__requests += 1
    print('Served {} requests'.format(self.__requests), file=sys.stderr)
    return response

hsmain = None

def handler(event, context):
  global hsmain

  if not hsmain:
    binary = os.environ['LAMBDA_TASK_ROOT'] + '/hsmain'
    config = os.environ['LAMBDA_TASK_ROOT'] + '/zureg.json'
    hsmain = Process([binary, config])

  return hsmain.handler(event)
