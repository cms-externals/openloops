import os
import sys
from subprocess import check_output, CalledProcessError
from nose import SkipTest
from nose.tools import nottest


thispath = os.path.dirname(os.path.realpath(__file__))
rootpath = os.path.dirname(thispath)
ol_lib_path = os.path.join(rootpath, 'lib')
ol_proclib_path = os.path.join(rootpath, 'proclib')

if ol_lib_path not in os.environ['LD_LIBRARY_PATH']:
  os.environ['LD_LIBRARY_PATH'] += (':' + ol_lib_path) + (':' + ol_proclib_path)
  try:
    os.execv(sys.argv[0], sys.argv)
  except Exception as e:
    sys.exit('EXCEPTION: Failed to Execute under modified environment, ' +
             str(e))

ol_tools_path = os.path.join(rootpath, 'pyol/tools')
sys.path.append(ol_tools_path)

# Folder with points
pointspath = os.path.dirname(os.path.abspath(__file__))
pointspath = os.path.join(pointspath, 'points')


class suppress_stdout_stderr(object):
  """ A context manager for deep suppressionof stdout and stderr.  """
  def __init__(self):
      # Open a pair of null files
      self.null_fds = [os.open(os.devnull, os.O_RDWR) for x in range(2)]
      # Save the actual stdout (1) and stderr (2) file descriptors.
      # os.dup makes a copy -> must be closed too
      self.save_fds = (os.dup(1), os.dup(2))

  def __enter__(self):
      # Assign the null pointers to stdout and stderr.
      os.dup2(self.null_fds[0], 1)
      os.dup2(self.null_fds[1], 2)

  def __exit__(self, *_):
      # Re-assign the real stdout/stderr back to (1) and (2)
      os.dup2(self.save_fds[0], 1)
      os.dup2(self.save_fds[1], 2)

      # Close the null files
      os.close(self.save_fds[0])
      os.close(self.save_fds[1])
      os.close(self.null_fds[0])
      os.close(self.null_fds[1])


def function_named(fn, name):
    """Return a function with a given __name__.
    Will assign to __name__ and return the original function if possible on
    the Python implementation, otherwise a new function will be constructed.
    """
    fn.__name__ = name
    return fn


@nottest
def skip_test_if(condition):
    """Skip unit test if prerequisite not fulfilled."""
    def decorate(fn):
        fn_name = fn.__name__

        def maybe(*args, **kw):
            check_passed, ret = condition()
            if not check_passed:
                msg = "'%s' skipped: %s" % (fn_name, ret)
                raise SkipTest(msg)
            else:
                process_ol_id = ret['process'] + '_' + ret['process_perm'] + '_' + ret['process_id']
                args[0].process = process_ol_id
                return fn(*args, **kw)

        maybe.__name__ = fn_name
        return maybe

    return decorate


def parse_ol_info(olinfo):
  olinfos = olinfo.split(' ')
  return {'process': olinfos[0],
          'process_perm': olinfos[1],
          'process_id': olinfos[2],
          'EW': olinfos[3].split('=')[1],
          'QCD': olinfos[4].split('=')[1],
          'Model': olinfos[5].split('=')[1],
          'OLMode': olinfos[6].split('=')[1],
          'Type': olinfos[7].split('=')[1],
          'CKMORDER': olinfos[8].split('=')[1]}


def process_available(prid, **process_specs):
  """ Runs the `openloops` script to see if process with `prid` is available. """
  try:
      with suppress_stdout_stderr():
          prs = run_openloops_script('info', prid)
      mismatches = []
      for pr in (u for u in prs if len(u) > 0):
          olinfo = parse_ol_info(pr)
          if any(str(process_specs[spec]) != olinfo[spec] for spec in process_specs):
              mismatch = ('Required ' + spec + ' = ' + str(process_specs[spec]) + ", got " +
                          spec + ' = ' + olinfo[spec] for spec in process_specs
                          if str(process_specs[spec]) != olinfo[spec])
              mismatch = "Process property mismatch: " + " ".join(mismatch)
              mismatches.append(mismatch)
          else:
              return True, olinfo
      return False, mismatches
  except CalledProcessError:
      # Process sources not found? Invoke run_openloops_script to download
      # sources?
      return False, "Process sources (" + prid + ") not available."


def oltest(prid, **kwargs):
    """ Unit test decorator allowing to impose availability of a certain process. """
    def condition():
        return process_available(prid, **kwargs)
    decorator = skip_test_if(condition)
    return decorator


def run_openloops_script(*args):
    """ Call the `openloops` script.

    >>> run_openloops_script("info", "ggtt")
    """
    currpath = os.getcwd()
    os.chdir(rootpath)
    olcmd = './openloops'
    run_cmd = [olcmd] + list(args)
    out = check_output(run_cmd).decode('utf-8').split('\n')
    os.chdir(currpath)
    return out

if __name__ == "__main__":
  print(process_available('ggttgg', OLMode='1'))
