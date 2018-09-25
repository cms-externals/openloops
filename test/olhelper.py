#!/usr/bin/env python

from __future__ import print_function

import math
import collections
from openloops import set_parameter, Process, LOOP

huge = 1e99
point = collections.namedtuple('point', ['pp', 'mu', 'alphas', 'perm', 'me'])
ticontrib = collections.namedtuple('ticontrib', ['m2add', 'm2', 'me'])
tensorint = collections.namedtuple(
    'tensorint', ['m2add', 'rank', 'masses2', 'psp', 'mominv'])
timon = collections.namedtuple('timon', ['tis', 'm2', 'me'])

floatform = '{:>22.15e}'


def relative_deviation(a, b, weight=1):
    if a == b:
        return 0.
    elif a == 0 or b == 0:
        return huge
    else:
        return max(abs(a / b - 1), abs(b / a - 1)) * abs(weight)


def to_digits(deviation):
    if deviation == 0:
        return 16.
    elif deviation == huge:
        return 0.
    else:
        return -math.log(deviation, 10)


def digit_agreement(a, b, weight=1):
    return to_digits(relative_deviation(a, b, abs(weight)))


def read_mefile(f, ispointsfile=None):
    """Read matrix elements from a file.
    Supported formats:
    * files as created by OpenLoops write_points
    * values in every line (Born virtual ...)"""
    dat = []
    if ispointsfile is None:
        ispointsfile = f.startswith('points')
    with open(f) as fh:
        for line in fh.readlines():
            if not line.strip():
                continue
            if ispointsfile:
                if not line.strip().startswith('me='):
                    continue
                line = line.strip()[3:]
            dat.append(map(float, line.split()))
    return dat


def readpoint(fh, required=[]):
    """Read a point (psp, mu, perm, me) from a file handle to file created by
    write_points. Return it as a named tuple of type 'points'."""
    # skip trailing empty lines
    line = ''
    while not line:
        line = fh.readline()
        if not line:
            # end of file
            return None
        line = line.strip()
    pp = []
    mu = None
    alphas = None
    perm = None
    me = None
    while line:
        if line.startswith('p='):
            pp.append(map(float, line[3:].split()))
        elif line.startswith('mu='):
            mu = float(line[3:])
        elif line.startswith('as='):
            alphas = float(line[3:])
        elif line.startswith('perm='):
            perm = tuple(map(int, line[5:].split()))
        elif line.startswith('me='):
            me = map(float, line[3:].split())
        line = fh.readline()
        if not line:
            break
        line = line.strip()
    for var in required:
        if var not in locals():
            raise OpenLoopsError('readpoint: \'required\' parameter {} ' +
                                 'doesn\'t exist'.format(var))
        if not locals()[var]:
            raise OpenLoopsError(
                'readpoint: required entry \'{}\' missing'.format(var))
    return point(pp, mu, alphas, perm, me)


def read_pointsfile(pointsfile, required=['pp', 'me']):
    """Read all points from a file created by write_points.
    Return points as a list of 'poont' named tuples."""
    points = []
    with open(pointsfile) as fh:
        while True:
            pt = readpoint(fh, required=required)
            if not pt:
                break
            points.append(pt)
    return points


def read_ticontrib(filename):
    """Read tensor integral contributions from a ti_monitor=1 file.
    record format:
      n= <n1>
      m2add= <m2add1> <m2current1>
      m2add= <m2add2> <m2current2>
      ...
      me= <me> # record must end with me
    Result: records[n]=[m2add1,m2add2,...]
    """
    records = {}
    with open(filename) as fh:
        for line in fh:
            cols = line.split()
            if not cols:
                continue
            elif cols[0] == 'n=':
                # begin record
                n = int(cols[1])
                m2add = []
            elif cols[0] == 'm2add=':
                m2add.append(float(cols[1]))
                m2 = float(cols[2])
            elif cols[0] == 'me=':
                # end record
                me = map(float, cols[1:])
                records[n] = ticontrib(m2add, m2, me)
    return records


def read_timon(filename):
    """Read tensor integral records from a ti_monitor=2 file.
    record format (m2add must be the first entry in each tensor integral,
    mominv the last; me must be the last in each record):
      n= <n>
        m2add= <m2add> <m2current> # first integral
        rank= <rank>
        masses2= <masses2>
        p= <p1>
        p= <p2>
        ...
        mominv= <mominv>
        m2add= ... # second integral
        ...
        me= <me>
    Result: records[n]=timon(tis=[tensorint(m2add,m2,rank,masses2,psp,mominv),
                                  tensorint(...), ...], m2=<m2>, me=<mee>)
    """
    records = {}
    with open(filename) as fh:
        for line in fh:
            cols = line.split()
            if not cols:
                continue
            tag = cols[0]
            vals = cols[1:]
            if tag == 'n=':
                # begin record
                n = int(vals[0])
                tis = []
            elif tag == 'm2add=':
                # begin tensor integral
                m2add = float(vals[0])
                m2 = float(vals[1])
                psp = []
            elif tag == 'rank=':
                rank = int(vals[0])
            elif tag == 'masses2=':
                masses2 = map(float, ' '.join(vals).replace('(', ' ').replace(')', ' ').replace(',', ' ').split())
                # complex numbers as pairs
                masses2 = [tuple(masses2[i:i + 2])
                           for i in range(0, len(masses2), 2)]
            elif tag == 'p=':
                psp.append(tuple(map(float, vals)))
            elif tag == 'mominv=':
                # end tensor integral
                mominv = map(float, vals)
                tis.append(tensorint(m2add, rank, masses2, psp, mominv))
            elif tag == 'me=':
                # end record
                me = map(float, vals)
                records[n] = timon(tis, m2, me)
    return records


###############################################################################
#                                                                           ###
#                    OpenLoops related functions                            ###
#                                                                           ###
###############################################################################

def read_procs(pointsfile, process, verbose=1):
    perm_set = set()
    with open(pointsfile, 'r') as fh:
        for line in fh:
            if line.strip().startswith('perm='):
                perm_set.add(tuple(map(int, line.split()[1:])))
    # register all crossings as procs[perm]
    procs = {}
    for perm in perm_set:
        perm_str = '[' + ','.join(map(str, perm)) + ']'
        procs[perm] = Process(process + perm_str, LOOP)
    if verbose:
        print('Registered process: ', process + perm_str)
    return procs


def stabilitymode(mode, verbose=1):
    """Sets the stability mode in OL"""
    set_parameter('stability_mode', mode)


def run_point(pt, procs, verbose=1):
    """Calculate the matrix element(s) for point 'pt'
    and return it as a tuple."""
    if pt.mu:
        set_parameter('mu', pt.mu)
    if pt.alphas:
        set_parameter('alphas', pt.alphas)
    me = procs[pt.perm].evaluate(pt.pp)
    if verbose:
        print(me.valuestr())
    return me


def run_file(pointsfile, procs, nstart=1, npoints=None, verbose=1):
    """Calculate the matrix elements for all points in the file 'pointsfile'
    and return them as a list of tuples."""
    mes = []
    with open(pointsfile) as fh:
        nnext = 0
        ndone = 0
        while True:
            nnext = nnext + 1
            pt = readpoint(fh, required=['pp'])
            if not pt:
                break
            if nnext < nstart:
                continue
            me = run_point(pt, procs, verbose=verbose)
            mes.append(me)
            ndone = ndone + 1
            if npoints and ndone == npoints:
                break
    return mes
