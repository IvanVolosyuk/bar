#!/usr/bin/python

import time
import math
import sys
import datetime
import thread

height = 22
show_battery = True
show_cpugraph = True
show_clock = True

padding = 4
battery_width = 120
cpugraph_width = 75

battery_xpos = 1090
cpugraph_xpos = battery_xpos + battery_width + padding
clock_xpos = cpugraph_xpos + cpugraph_width + padding

def red(val):
  return "^fg(red)%s^fg()" % val

def move(x):
  return "^p(%d)" % x

def frame(xpos='', width='', cmd='', offset=0):
  return "^pa(%d)^p()^ib(1)^fg(#181838)^ca(1,%s)^r(%dx%d)^ca()^p(-%d)^fg()" % (
    xpos, cmd, width, height, width - offset)

def bar(val):
  """Draw bar from 0 to maximum height.

  Args:
    val: float value from 0 to 1
  """
  h = int(val * height)
  return "^pa(;%d)^r(1x%d)" % (height-h, h)



def cpugraph_read():
  f = open('/proc/stat')
  values = f.readline().rstrip().split(' ')
  f.close()
  user = int(values[2])
  nice = int(values[3])
  system = int(values[4])
  idle = int(values[5])
  iowait = int(values[6])
  system += iowait
  system_nice = system + nice
  busy = user + system_nice
  total = idle + busy

  return (total, system, system_nice, busy)

old_cpudata = None

def cpugraph_delta():
  global old_cpudata
  if not old_cpudata:
    old_cpudata = cpugraph_read()
  cpudata = cpugraph_read()
  delta = [n-o for n,o in zip(cpudata, old_cpudata)]
  old_cpudata = cpudata
  total = float(delta[0])
  if total == 0:
    return (0,0,0)
  system = delta[1] / total
  system_nice = delta[2] / total
  busy = delta[3] / total
  return (busy, system_nice, system)
 
def color(col, string):
  return "^fg(%s)%s^fg()" % (col, string)
 
user_color = "#600060"
system_color = "#7F0000"
nice_color = "#007F00"
length = cpugraph_width

def make_empty():
  return [bar(0) for _ in xrange(length)]
    
samples = [make_empty(),make_empty(),make_empty()]

def cpugraph():
  """Draw several bars of CPU graph in a frame."""

  global samples
  sample = cpugraph_delta()
  for i, val in enumerate(sample):
    samples[i].append(bar(val))
    samples[i] = samples[i][-length:]
  
  res = frame(xpos=cpugraph_xpos, cmd="top.sh", width=cpugraph_width)
  res += color(user_color, "".join(samples[0]))
  res += move(-length)
  res += color(nice_color, "".join(samples[1]))
  res += move(-length)
  res += color(system_color, "".join(samples[2]))

  return res

def file_reader(filename):
  f = open(filename)
  values = {}
  for line in f:
    parts = line.split(':')
    values[parts[0]] = parts[1].strip().split(' ')[0]
  f.close()
  return values

bat_max_capacity = float(file_reader('/proc/acpi/battery/BAT0/info')['design capacity'])
  

def battery():
  """Draw battery charge indicator.

  Charge indicator can be:
    3%C - charging, 3% current charge
    99%(5:12) - charge percent + time remaining with current discharge rate
    If time remaining is less than 10 minutes, should be displayed in red
  """
  values = file_reader('/proc/acpi/battery/BAT0/state')
  state = values['charging state']
  remaining = float(values['remaining capacity']) # mAh
  rate = float(values['present rate']) # mA
  percent = remaining * 100 / bat_max_capacity

  if state == 'discharging' and rate != 0:
    tm = remaining / float(rate)
    timef = math.modf(tm)
    tm = "%02d:%02d" % (timef[1], int(timef[0] * 60))
    info = "%d%%(%s)" %( percent, tm)
  else:
    info = "%d%%C" % percent
  res = frame(xpos=battery_xpos, cmd="powertop.sh", width=battery_width, offset=5) + color("#C7AE86", info)
  return res

def clock():
  """Draw clock in 24H mode: "22:24"."""
  tm = datetime.datetime.now()
  res = frame(xpos = clock_xpos, cmd="clock.sh", width=60, offset=5)
  res += color("#C7AE86", "%02d:%02d" % (tm.hour, tm.minute))
  return res

num = 1
titlebar = ""
sysinfo = ""

def input_reader():
  global titlebar
  while True:
    line = sys.stdin.readline()
    titlebar = "^pa(10)^p()%s" % color('black', line.strip())
    print titlebar + sysinfo
    sys.stdout.flush()
  sys.exit(1)

thread.start_new_thread(input_reader, ())


def draw():
  global sysinfo
  sysinfo = ""
  if show_battery: sysinfo += battery()
  if show_cpugraph: sysinfo += cpugraph()
  if show_clock: sysinfo += clock()
  print titlebar + sysinfo
  sys.stdout.flush()
  

while True:
  t = time.time()
  time.sleep(2 - math.modf(t)[0])
  draw()
