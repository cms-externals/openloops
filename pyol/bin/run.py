#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright 2015 Jonas Lindert, Philipp Maierhoefer, Stefano Pozzorini
#
# This file is part of OpenLoops.
#
# OpenLoops is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# OpenLoops is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with OpenLoops.  If not, see <http://www.gnu.org/licenses/>.


# TODO
# * Python version check.
# * Accept internal channel identifier to register a process
#   (distingish from library name).
# * set model automatically, print message.


import sys
import argparse
import time
import openloops

# =============== #
# argument parser #
# =============== #

def amptype_conv(a):
    a = a.lower()
    try:
        return int(a)
    except ValueError:
        return a

parser = argparse.ArgumentParser()
parser.add_argument('process', metavar='PROCESS',
                    help='The process to calculate')
parser.add_argument(
    '-a', '--amptype', type=amptype_conv, default=openloops.LOOP,
    choices=openloops.AMPTYPES.keys()+openloops.AMPTYPES.values(),
    help='amptype')
parser.add_argument(
    '-e', '--energy', type=float, default=openloops.default_energy,
    help='Energy to be used in phase space point generation.')
parser.add_argument(
    '-n', type=int, metavar='POINTS', default=None,
    help='Number of phase space points to calculate matrix elements for.')
parser.add_argument(
    '-t', '--time', dest='timing', action='store_true', default=False,
    help='''Measure runtime per phase space point
         (may be inaccurate for very simple processes).''')
parser.add_argument(
    '--tt', type=float, dest='mintime', metavar='MINTIME', default=10,
    help='Minimal time for runtime measurements in seconds.')
parser.add_argument(
    '--tn', type=int, dest='minn', metavar='MINN', default=1,
    help='Minimal number of points in runtime measurements.')
parser.add_argument(
    '-v', '--verbose', type=int, default=1, help='Verbosity level')
# This is just to create a help message, opt=val pairs are extracted manually
# to avoid ordering conflicts between options and positional arguments.
parser.add_argument(
    'options', metavar='OPT=VAL', nargs='*',
    help='Options to pass to directly to OpenLoops')

options = [arg for arg in sys.argv[1:] if ('=' in arg and not arg.startswith('-'))]
args = parser.parse_args(
    [arg for arg in sys.argv[1:] if (arg.startswith('-') or '=' not in arg)])
if not args.timing and args.n is None:
    args.n = 1

# ======================== #
# parameter initialisation #
# ======================== #

openloops.set_parameter('splash',0)

for opt in options:
    try:
        key, val = opt.split('=',1)
    except ValueError:
        print '[PYOL] ERROR: invalid option \'{}\''.format(opt)
        sys.exit(1)
    if key.startswith('alpha') and '/' in val:
        try:
            valnum, valden = val.split('/')
            val = float(valnum)/float(valden)
        except ValueError:
            print '[PYOL] ERROR: invalid option \'{}\''.format(opt)
    if args.verbose >= 3:
        print 'call set_parameter({},{})'.format(key,val)
    openloops.set_parameter(key, val)

# ================== #
# register processes #
# ================== #

is_library = False
if not '>' in args.process:
    is_library = True
    try:
        procinfo = openloops.ProcessInfo(args.process)
    except openloops.ProcessInfoError:
        is_library = False

if not is_library:
    # register a partonic channel
    try:
        processes = [openloops.Process(args.process, args.amptype)]
    except openloops.RegisterProcessError:
        print ('[PYOL] ERROR registering process \'{}\' ' +
               'with amptype {}.').format(args.process, args.amptype)
        if not '>' in args.process:
            print '(reading info file failed, too)'
        sys.exit(1)
else:
    # register channels from a process library
    try:
        processes = procinfo.register()
    except openloops.RegisterProcessError as err:
        print ('[PYOL] ERROR while registering process from library ' +
                '{}:\n       {}').format(args.process, err.args[0])
        sys.exit(1)

# === #
# run #
# === #

def eval_and_print(proc, first=False):
    me = proc.evaluate(args.energy)
    if args.verbose >= 2:
        print me.psp
    if args.verbose >= 1:
        if first:
            if me.amptype == openloops.TREE:
                print '{:>23}'.format('tree')
            elif me.amptype == openloops.LOOP:
                print (5*'{:>23}').format(
                    'tree', 'finite', 'ir1', 'ir2', 'acc')
            elif me.amptype == openloops.LOOP2:
                print (2*'{:>23}').format('loop2', 'acc')
        print me.valuestr()

for proc in processes:
    print
    print '"' + proc.process + '"'
    if not args.timing:
        for n in range(args.n):
            eval_and_print(proc, first=not(n))
    else:
        eval_and_print(proc, first=True)
        eval_and_print(proc)
        starttime = time.clock()
        if args.n is not None:
            npoints = args.n
            for n in range(args.n):
                eval_and_print(proc, first=not(n))
        else:
            npoints = 0
            while (time.clock() < starttime + args.mintime or
                   npoints < args.minn):
                npoints = npoints + 1
                eval_and_print(proc)
        endtime = time.clock()
        print ('time per phase space point: {:3f} ms (avg. of {} ' +
               'points)').format(1000*(endtime - starttime)/npoints, npoints)

print
