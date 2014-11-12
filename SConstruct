
#dd_version = ''
dd_version = '_dp19032014'
#dd_version = '_06082014'
if dd_version != '_06082014':
    dd_cppdef = ['COLLIER_LEGACY']
else:
    dd_cppdef = []


# TODO
# - Use kind types from kind_types module in CutTools.
# - When compiling a process: check for compatibility with the OpenLoops version.

# Coli cpp defines (default):
# - ALLCHECK (false) -- print maximal information
# - CHECK (false) -- perform various checks
# - SING (true) -- take singular contributions into account
# - PUBCHECK (false) -- check against publication


help_message = """
OpenLoops build system

Usage: scons [options] <loops>=proc1,proc2,...

<loops> determines the content of the process library
- t=tree, l=loop, s=loop_squared, p=pseudo_tree;
  if 'l' is present, any combination of these is allowed (inalphabetic order),
  otherwise 't' must be alone
- If a processes ends with a slash (/) it is treated as a process collection,
  i.e. the corresponding list of processes from
  the collection file on the web server is used.
- Several <loops> arguments can be given, even with the same <loops>.

Options

-c (SCons built-in)
  Clean, i.e. delete object files and libraries.
  See also clean=... option.

gjobs=<n>
  Generate 'n' processes simultaneously.
  n > 1 implies --glog
  n < 1 uses the number of CPU core of the machine

glog=0/1
  Write generator output to a log file; implied if gjobs > 1.

force-download=0/1
  Download processes even if they seem to be up-to-date.

process-update=0/1
  If the downloader is used: select all downloaded processes
    (i.e. their version.info file contains 'process_update').
  If the generator is used: select all processes
    which have been generated or downloaded before.

release=<r>
  A string of max. length 8 to specify the OpenLoops library version number
  (this does not affect the process libraries).

generator=0/1/2
  0: off
  1: use the process generator
  2: use the process downloader

compile=0/1/2
  0: don't compile
  1: compile processes,
     compile generic libraries only if no process was specified
  2: compile processes and generic libraries

debug=0/1
  Add debug flags to Fortran compiler flags.

clean=procs,src
  Only effective in conjunction with -c.
  procs: delete the proclib and process_obj directories
         which contain libraries and object code of all processes.
  src: delete process source of the given processes,
       rsp. the process_src directory which contains
       the source of all processes if 'procs' is also given.

Also built-in SCons options can be used. However:
  - no_exec (-n) will not work properly,
    because the process generator is run in a sub-process.
    (the compile script cannot know what to do
    unless the generator finished generating the source code)
  - SCons options are not passed to the process generator.

For more options see ./pyol/config/default.cfg.
User defined options can be set in ./openloops.cfg
in the same way as in default.cfg.
"""

import os
import sys
import subprocess

sys.path.insert(0, os.path.abspath(os.path.join('pyol', 'build')))
sys.path.insert(0, os.path.abspath(os.path.join('pyol', 'config')))
sys.path.insert(0, os.path.abspath(os.path.join('pyol', 'tools')))

import OLBaseConfig
import OLToolbox
from OLLibrary import CPPContainer, OLLibrary


if '--help' in sys.argv or '-h' in sys.argv:
    print help_message
    Exit(0)

process_arguments = filter(lambda el: el[0] in
                           OLBaseConfig.loops_specifications, ARGLIST)
commandline_options = filter(lambda el: el[0] not in
                             OLBaseConfig.loops_specifications, ARGLIST)

config = OLBaseConfig.get_config(commandline_options)

SetOption('num_jobs', config['num_jobs'])

generator_options = ['--jobs=' + str(config['gjobs'])]
if config['glog']:
    generator_options.append('--log')

if config['force_download']:
    force_download_flag = ['--force']
else:
    force_download_flag = []

if config['release']:
    release_version = 'version ' +config['release']
else:
    release_version = ''

svn_revision = "1.0" #str(OLToolbox.get_svn_revision(mandatory = False))

# Install directory; only effect is that this is put into
# the openloops library as a string. TODO: override by command line argument;
# and actually install the libraries there
install_path = os.getcwd()

generate_process_true = ((config['generator'] == 1) and
                         not GetOption('clean') and not GetOption('no_exec'))
download_process_true = ((config['generator'] == 2) and
                         not GetOption('clean') and not GetOption('no_exec'))

if config['compile'] == 0 or (config['compile'] == 1 and
                              (len(process_arguments) > 0 or
                               config['process_update'])):
    compile_libraries = []
else:
    compile_libraries = config['compile_libraries']

cpp_defines = map(lambda lib: 'USE_' + lib.upper(), config['link_libraries'])
cpp_defines += [('KIND_TYPES', 'kind_types'),
                ('DREALKIND', 'dp'),
                ('QREALKIND', 'qp'),
                'USE_' + config['fortran_compiler'].upper(),
                ('OL_INSTALL_PATH', '\\"' + install_path + '\\"'),
                'SING'] + dd_cppdef


# ================= #
# Generic libraries #
# ================= #

lib_src_dirs = {}
lib_obj_dirs = {}
lib_mod_dirs = {}
for libname in ['olcommon', 'rambo', 'qcdloop', 'oneloop', 'cuttools', 'samurai', 'collier', 'openloops']:
    lib_src_dirs[libname] = os.path.join(config['lib_src_dir'], libname, 'src')
    lib_obj_dirs[libname] = os.path.join(config['lib_src_dir'], libname, 'obj')
    lib_mod_dirs[libname] = os.path.join(config['lib_src_dir'], libname, 'mod')

# OLCommon
olcommon_dp_src = ['kind_types.F90']
olcommon_mp_src = ['common.F90']

# Rambo
rambo_dp_src = ['rambo.f']

# QCDLoop
qcdloop_dp_src = [
    'aacbc.f', 'aaccc.f', 'aacinv.f', 'aaxbx.f', 'aaxcx.f', 'aaxdx.f', 'aaxex.f', 'aaxinv.f',
    'auxCD.f', 'ddilog.f', 'ff2dl2.f', 'ffabcd.f', 'ffca0.f', 'ffcb0.f', 'ffcb1.f', 'ffcb2.f',
    'ffcb2p.f', 'ffcc0.f', 'ffcc0p.f', 'ffcc1.f', 'ffcdb0.f', 'ffcel2.f', 'ffcel3.f', 'ffcel4.f',
    'ffcel5.f', 'ffceta.f', 'ffcli2.f', 'ffcrr.f', 'ffcxr.f', 'ffcxs3.f', 'ffcxs4.f', 'ffcxyz.f',
    'ffdcc0.f', 'ffdcxs.f', 'ffdel2.f', 'ffdel3.f', 'ffdel4.f', 'ffdel5.f', 'ffdel6.f', 'ffdl2i.f',
    'ffdl5p.f', 'ffdxc0.f', 'ffinit_mine.f', 'ffrcvr.f', 'fftran.f', 'ffxb0.f', 'ffxb1.f', 'ffxb2p.f',
    'ffxc0.f', 'ffxc0i.f', 'ffxc0p.f', 'ffxc1.f', 'ffxd0.f', 'ffxd0h.f', 'ffxd0i.f', 'ffxd0p.f',
    'ffxd1.f', 'ffxdb0.f', 'ffxdbd.f', 'ffxdi.f', 'ffxdpv.f', 'ffxe0.f', 'ffxe1.f', 'ffxf0.f',
    'ffxf0h.f', 'ffxli2.f', 'ffxxyz.f', 'npoin.f', 'qlbox1.f', 'qlbox10.f', 'qlbox11.f', 'qlbox12.f',
    'qlbox13.f', 'qlbox14.f', 'qlbox15.f', 'qlbox16.f', 'qlbox2.f', 'qlbox3.f', 'qlbox4.f', 'qlbox5.f',
    'qlbox6.f', 'qlbox7.f', 'qlbox8.f', 'qlbox9.f', 'qlcLi2omx2.f', 'qlcLi2omx3.f', 'qlfndd.f',
    'qlfunctions.f', 'qlI1.f', 'qlI2.f', 'qlI2fin.f', 'qlI3.f', 'qlI3fin.f', 'qlI3sub.f', 'qlI4.f',
    'qlI4array.f', 'qlI4DNS41.f', 'qlI4fin.f', 'qlI4sub0m.f', 'qlI4sub1m.f', 'qlI4sub2m.f',
    'qlI4sub2ma.f', 'qlI4sub2mo.f', 'qlI4sub3m.f', 'qlinit.f', 'qlkfn.f', 'qlLi2omprod.f',
    'qlLi2omrat.f', 'qlLi2omx.f', 'qlLi2omx2.f', 'qllnomrat4.f', 'qllnrat.f', 'qlratgam.f',
    'qlratreal.f', 'qlsnglsort.f', 'qlspencer.f', 'qltri1.f', 'qltri2.f', 'qltri3.f', 'qltri4.f',
    'qltri5.f', 'qltri6.f', 'qltrisort.f', 'qlxpicheck.f', 'qlYcalc.f', 'qlzero.f', 'spence.f']

# OneLOop -- contains both, dp and qp routines
oneloop_dp_src = ['avh_olo_qp.f90']

# OpenLoops
openloops_mp_src = [
    'contractions.F90', 'converter.F90', 'counterterms.F90', 'helicity.F90',
    'i-operator.F90', 'kinematics.F90', 'laststep.F90', 'loopmom_tensor.F90',
    'looproutines.F90', 'Lpropagators.F90', 'Lvertices.F90', 'parameters.F90',
    'parameters_init.F90', 'renormalisation_qcd.F90',
    'propagators.F90', 'vertices.F90', 'wavefunctions.F90']

openloops_dp_src = [
    'helicity_init.F90', 'init_ui.F90', 'stability.F90', 'tensor_handling.F90']

if config['interface'] >= 1:
    openloops_dp_src.append('ol_interface.F90')
if config['interface'] >= 2:
    openloops_dp_src.append('blha_interface.F90')

openloops_version_src = 'version.F90'

# CutTools -- contains both, dp and qp routines
cuttools_dp_src = [
    'cts_combinatorics.f90', 'cts_constants.f90', 'cts_cutroutines.f90', 'cts_cuttools.f90',
    'cts_dynamics.f90', 'cts_kinematics.f90', 'cts_loopfunctions.f90', 'cts_tensors.f90',
    'cts_type.f90', 'mpnumdummy.f90']

# Samurai
samurai_dp_src = [
    'constants.f90', 'kinematic.f90', 'ltest.f90', 'maccu.f90', 'madds.f90', 'mcgs.f90',
    'mfunctions.f90', 'mgetbase.f90', 'mgetc1.f90', 'mgetc2.f90', 'mgetc3.f90', 'mgetc4.f90',
    'mgetc5.f90', 'mgetqs.f90', 'mglobal.f90', 'mrestore.f90', 'msamurai.f90', 'mtens.f90',
    'mtests.f90', 'ncuts.f90', 'notfirst.f90', 'options.f90', 'precision.f90', 'save.f90']

# Collier
collier_src_mp = [
    # DD
    'dd_global.F', 'dd_aux.F', 'dd_2pt.F', 'dd_3pt.F', 'dd_3pt_coll.F',
    'dd_4pt.F', 'dd_5pt.F', 'dd_6pt.F', 'dd_newinterface.F',
    # BuildTensors
    'bt_BuildTensors.F90', 'bt_Checks.F90', 'bt_FourVectors.F90',
    'bt_GramCayley.F90', 'bt_LightCone.F90', 'bt_MatrixManipulations.F90',
    'bt_TensorManipulations.F90', 'bt_TensorReduction.F90', 'bt_TI_interface.F90']

collier_src_dp = [
    # Coli
    'coli_aux.F', 'coli_b0.F', 'coli_c0.F', 'coli_cache.F', 'coli_ctoliserg.F',
    'coli_ctolis.F', 'coli_d0.F', 'coli_d0reg.F', 'coli_oint2.F', 'coli_oint.F',
    # DD
    'dd_generic.F', 'dd_generic_interface.F',
    # BuildTensors
    'bt_Generic.F90', 'bt_Combinatorics.F90', 'bt_Interface.F90']

collier_inc_dp = [
    'coli_checkparams.h', 'coli_common_cache.h', 'coli_common.h',
    'coli_params_cache.h', 'coli_params.h']

if dd_version == '_dp19032014':
    collier_src_mp = []
    collier_src_dp = [
        # DD
        'DD_aux.F', 'DD_2pt.F', 'DD_3pt.F', 'DD_3pt_coll.F',
        'DD_4pt.F', 'DD_5pt.F', 'DD_6pt.F', 'DD_newinterface.F', 'dcuhre.F',
        # Coli
        'coli_aux.F', 'coli_b0.F', 'coli_c0.F', 'coli_cache.F', 'coli_ctoliserg.F',
        'coli_ctolis.F', 'coli_d0.F', 'coli_d0reg.F', 'coli_oint2.F', 'coli_oint.F',
        # BuildTensors
        'bt_Combinatorics.F90', 'bt_Interface.F90',
        'bt_BuildTensors.F90', 'bt_Checks.F90', 'bt_FourVectors.F90',
        'bt_GramCayley.F90', 'bt_LightCone.F90', 'bt_MatrixManipulations.F90',
        'bt_TensorManipulations.F90', 'bt_TensorReduction.F90', 'bt_TI_interface.F90']

if dd_version == '_06082014':
    collier_inc_dp = []
    collier_src_mp = []
    collier_src_dp = [
        "BuildTensors.F90", "cache.F90", "coli_aux2.F90", "coli_aux.F", "coli_b0.F", "coli_c0.F", "coli_d0.F", "coli_d0reg.F", "coli_stat.F90", "collier_aux.F90", "collier_coefs.F90", "COLLIER.F90", "collier_global.F90", "collier_init.F90", "collier_tensors.F90", "Combinatorics.F90", "dcuhre.f", "DD_2pt.F", "DD_3pt_coll.F", "DD_3pt.F", "DD_4pt.F", "DD_5pt.F", "DD_6pt.F", "DD_aux.F", "DD_to_COLLIER.F", "InitTensors.F90", "master.F90", "reductionAB.F90", "reductionC.F90", "reductionD.F90", "reductionEFG.F90", "reductionTN.F90", "TensorReduction.F90"]

if compile_libraries:
    cpp_container = CPPContainer(mp = config['precision'],
                                 version = release_version,
                                 revision = svn_revision,
                                 cpp_defs = cpp_defines,
                                 target = 'cpp_generic',
                                 target_prefix = os.path.join('..', 'obj', ''))

if 'olcommon' in compile_libraries:
    olcommon_lib = OLLibrary(name = 'olcommon',
                             linklibs = ['dl'],
                             target_dir = config['generic_lib_dir'],
                             src_dir = lib_src_dirs['olcommon'],
                             dp_src = olcommon_dp_src,
                             mp_src = olcommon_mp_src,
                             to_cpp = cpp_container)

if 'rambo' in compile_libraries:
    VariantDir(lib_obj_dirs['rambo'],
               lib_src_dirs['rambo'], duplicate = 0)
    rambo_lib = OLLibrary(name = 'rambo',
                          target_dir = config['generic_lib_dir'],
                          src_dir = lib_obj_dirs['rambo'],
                          dp_src = rambo_dp_src)

if 'qcdloop' in compile_libraries:
    VariantDir(lib_obj_dirs['qcdloop'],
               lib_src_dirs['qcdloop'], duplicate = 0)
    qcdloop_lib = OLLibrary(name = 'qcdloop',
                            target_dir = config['generic_lib_dir'],
                            src_dir = lib_obj_dirs['qcdloop'],
                            mod_dir = '',
                            dp_src = qcdloop_dp_src)

if 'oneloop' in compile_libraries:
    VariantDir(lib_obj_dirs['oneloop'],
               lib_src_dirs['oneloop'], duplicate = 0)
    oneloop_lib = OLLibrary(name = 'oneloop',
                            target_dir = config['generic_lib_dir'],
                            mod_dependencies = ['olcommon'],
                            src_dir = lib_obj_dirs['oneloop'],
                            dp_src = oneloop_dp_src)

if 'cuttools' in compile_libraries:
    VariantDir(lib_obj_dirs['cuttools'],
               lib_src_dirs['cuttools'], duplicate = 0)
    cuttools_lib = OLLibrary(name = 'cuttools',
                             target_dir = config['generic_lib_dir'],
                             mod_dependencies = ['oneloop'],
                             linklibs = [ll for ll in config['link_libraries']
                                         if ll == 'qcdloop'],
                             src_dir = lib_obj_dirs['cuttools'],
                             dp_src = cuttools_dp_src)

if 'samurai' in compile_libraries:
    VariantDir(lib_obj_dirs['samurai'],
               lib_src_dirs['samurai'], duplicate = 0)
    samurai_lib = OLLibrary(name = 'samurai',
                            target_dir = config['generic_lib_dir'],
                            mod_dependencies = ['oneloop'],
                            linklibs = [ll for ll in config['link_libraries']
                                        if ll == 'qcdloop'],
                            src_dir = lib_obj_dirs['samurai'],
                            dp_src = samurai_dp_src)

if 'collier' in compile_libraries:
    collier_lib = OLLibrary(name = 'collier',
                            target_dir = config['generic_lib_dir'],
                            mod_dependencies = ['olcommon'],
                            src_dir = lib_src_dirs['collier'] + dd_version,
                            mp_src = collier_src_mp,
                            dp_src = collier_src_dp,
                            to_cpp = cpp_container)

    # collier: preprocess include files, but don't add them to the list of source files
    cpp_container.add(src_dir = lib_src_dirs['collier'] + dd_version,
                      dp_src = collier_inc_dp)

if 'openloops' in compile_libraries:
    openloops_lib = OLLibrary(
        name = 'openloops',
        target_dir = config['generic_lib_dir'],
        mod_dependencies = list(set(config['link_libraries'])
            & set(['olcommon', 'collier', 'cuttools',
                   'samurai', 'oneloop', 'rambo'])),
        linklibs = list(set(config['link_libraries']) & set(['rambo'])),
        src_dir = lib_src_dirs['openloops'],
        mp_src = openloops_mp_src,
        dp_src = openloops_dp_src,
        version_src = [openloops_version_src],
        to_cpp = cpp_container)


if compile_libraries:
    if not GetOption('clean'):
        if not cpp_container.run():
            print '*** cpp failed ***'
            Exit(1)


if config['import_path']:
    env_path = os.environ.get('PATH', '')
    env_ld_library_path = os.environ.get('LD_LIBRARY_PATH', '')
else:
    env_path = []
    env_ld_library_path = []

env = Environment(tools = ['default', 'textfile'] + [config['fortran_compiler']],
                  ENV = {"PATH": env_path, "LD_LIBRARY_PATH": env_ld_library_path},
                  FORTRANFLAGS = config['f77_flags'] + config['generic_optimisation'],
                  F90FLAGS = config['f90_flags'] + config['generic_optimisation'],
                  LIBPATH = [config['generic_lib_dir']],
                  RPATH = [Literal('\$$ORIGIN')])


if env.subst('$F90') == 'gfortran':
    # SCons bug: FORTRANMODDIRPREFIX is missing in gfortran tool
    env.Replace(FORTRANMODDIRPREFIX = '-J')
    if tuple(map(int, env.subst('$CCVERSION').split('.')[:2])) < (4,6):
        print 'ERROR: This OpenLoops version requires gfortran 4.6 or later (found %s)' % env.subst('$CCVERSION')
        Exit(1)

env_noautomatic = env.Clone()
env_noautomatic.AppendUnique(F90FLAGS = config['noautomatic'],
                             FORTRANFLAGS = config['noautomatic'])


if 'olcommon' in compile_libraries:
    libolcommon = olcommon_lib.compile(env = env, shared = config['shared_libraries'])
    env.Alias('olcommon', libolcommon)
    Default('olcommon')
    Clean(libolcommon, [lib_obj_dirs['olcommon'], lib_mod_dirs['olcommon']])

if 'rambo' in compile_libraries:
    librambo = rambo_lib.compile(env = env, shared = config['shared_libraries'])
    env.Alias('rambo', librambo)
    Default('rambo')
    Clean(librambo, [lib_obj_dirs['rambo'], lib_mod_dirs['rambo']])

if 'qcdloop' in compile_libraries:
    libqcdloop = qcdloop_lib.compile(env = env_noautomatic, shared = config['shared_libraries'])
    env.Alias('qcdloop', libqcdloop)
    Default('qcdloop')
    Clean(libqcdloop, [lib_obj_dirs['qcdloop']])

if 'oneloop' in compile_libraries:
    liboneloop = oneloop_lib.compile(env = env, shared = config['shared_libraries'])
    env.Alias('oneloop', liboneloop)
    Default('oneloop')
    Clean(liboneloop, [lib_obj_dirs['oneloop'], lib_mod_dirs['oneloop']])

if 'cuttools' in compile_libraries:
    libcuttools = cuttools_lib.compile(env = env_noautomatic, shared = config['shared_libraries'])
    env.Alias('cuttools', libcuttools)
    Default('cuttools')
    Clean(libcuttools, [lib_obj_dirs['cuttools'], lib_mod_dirs['cuttools']])

if 'samurai' in compile_libraries:
    libsamurai = samurai_lib.compile(env = env, shared = config['shared_libraries'])
    env.Alias('samurai', libsamurai)
    Default('samurai')
    Clean(libsamurai, [lib_obj_dirs['samurai'], lib_mod_dirs['samurai']])

if 'collier' in compile_libraries:
    libcollier = collier_lib.compile(env = env, shared = config['shared_libraries'])
    env.Alias('collier', libcollier)
    Default('collier')
    Clean(libcollier, [lib_obj_dirs['collier'], lib_mod_dirs['collier']])

if 'openloops' in compile_libraries:
    libopenloops = openloops_lib.compile(env = env, shared = config['shared_libraries'])
    env.Alias('openloops', libopenloops)
    Default('openloops')
    Clean(libopenloops, [lib_obj_dirs['openloops'], lib_mod_dirs['openloops']])

if GetOption('clean') and compile_libraries:
    if not cpp_container.run(clean = True):
        print '*** cpp cleanup failed ***'
        Exit(1)



# ================= #
# Process libraries #
# ================= #


# parse process arguments
#   <loops>=proc1,proc2,coll/...
#   (several arguments possible, also with the same <loops>)
#   --> process_list = [(loops,proc1),(loops,proc2),(loops,collproc1),...]

version_db_url = (config['remote_process_url'] + '/%s/processes/' +
                  str(config['process_api_version']) + '/versions.db')
collection_url = config['remote_process_url'] + '/%s/collections'

def split_processlist(loops, procs):
    """Convert (loops=L, procs=P1,P2,P3,...) to [(L,P1),(L,P2),(L,P3),...].
    Replace collections (process ending with /) by the list of processes
    from the collection file on the server."""
    proclist = sum([proclist.split(',') for proclist in procs.split()], [])
    proclist = [proc for proc in proclist if proc]
    collections = [coll[:-1] for coll in proclist if coll.endswith('/')]
    proclist = [(loops, proc) for proc in proclist if not proc.endswith('/')]
    for coll in collections:
        process_coll = []
        if coll == 'all':
            for repo in config['process_repositories']:
                process_db = OLToolbox.ProcessDB(db=(version_db_url % repo))
                process_coll += process_db.content.keys()
        else:
            found_collection = False
            for repo in config['process_repositories']:
                process_coll_add = OLToolbox.import_list(
                    os.path.join(collection_url % repo, coll), fatal=False)
                if process_coll_add is not None:
                    found_collection = True
                    process_coll += process_coll_add
            if not found_collection:
                print 'ERROR: process collection ' + coll + ' not found.'
                Exit(1)
        proclist += [(loops, proc) for proc in process_coll]
    return proclist


def get_auto_loops((loops, processlib)):
    """Determine 'auto' loops specifications from version.info in the process source directory."""
    if loops == 'auto':
        loops = OLToolbox.import_dictionary(
            os.path.join(config['process_src_dir'], processlib, 'version.info'),
            error_message = (
                'ERROR: auto loops specification not available for ' + processlib)
        )['loops']
        if loops == 'auto' or loops not in OLBaseConfig.loops_specifications:
            print 'ERROR: invalid loops specification for', processlib
            Exit(1)
    return (loops, processlib)


def find_process_src(generate = True):
    """Find all generated / downloaded processes.
    if generate: return (loops,process) for all processes which exist
    (i.e. there is a directory which contains version.info)
    if only download (generate = False): return only those processes
    for which version.info contains 'process_version'
    (i.e. they are controlled by the downloader)."""
    process_list = []
    process_directories = os.listdir(config['process_src_dir'])

    for procdir in process_directories:
        version_info = OLToolbox.import_dictionary(
            os.path.join(config['process_src_dir'], procdir, 'version.info'),
            fatal = False)
        if version_info:
            loops = version_info['loops']
            process_version = version_info.get('process_version', None)
            if process_version or generate:
                # download: only add if process_version != None;
                # generate: always add
                process_list.append((loops, procdir))

    return process_list


def revoke_processes():
    """Revocation of deprecated processes.
    Remove processes (src, obj, lib) listed in 'revoke' on the server
    if the process source directory contains version.info with process_update."""
    revocation_list = OLToolbox.import_list(os.path.join(collection_url, 'revoke'), fatal = False)
    if revocation_list is None:
        revocation_list = []

    for proc in revocation_list:
        processlib_src_dir = os.path.join(config['process_src_dir'], proc)
        processlib_obj_dir = os.path.join(config['process_obj_dir'], proc)
        if os.path.isdir(processlib_src_dir):
            version_info = OLToolbox.import_dictionary(os.path.join(processlib_src_dir, 'version.info'), fatal = False)
            if version_info and 'process_version' in version_info:
                print 'revoking', proc
                Execute(Delete(processlib_src_dir))
                if os.path.isdir(processlib_obj_dir):
                    Execute(Delete(processlib_obj_dir))
                revoke_libs = [os.path.join(config['process_lib_dir'], 'libopenloops_' + proc + '_' + lps + '.*')
                               for lps in OLBaseConfig.loops_specifications if lps != 'auto']
                revoke_libs = sum([Glob(patt) for patt in revoke_libs], [])
                if revoke_libs:
                    Execute(Delete(revoke_libs))


def download_processes(processes):
    """Download processes"""
    if subprocess.call(['python', config['process_download_script']] + force_download_flag + processes) != 0:
        print 'ERROR: process downloader failed.'
        Exit(1)


def generate_process(loops, processlib):
    """Generate a process library"""
    if subprocess.call(['scons', '-Q'] + generator_options + ['-f', config['code_generator_script'], 'PROC=' + processlib, 'LOOPS=' + loops]) != 0:
        print 'ERROR: code generator failed.'
        Exit(1)



process_list = sum([split_processlist(loops, procs) for (loops, procs) in process_arguments], [])

if config['process_update']:
    process_list.extend(find_process_src(generate_process_true))

if download_process_true:
    if process_list:
        proc_ls = list(set([proc for loops, proc in process_list]))
        revoke_processes()
        download_processes(proc_ls)

process_list = map(get_auto_loops, process_list)

process_list = list(set(process_list))


env_born_process = env.Clone(F90FLAGS = config['f90_flags'] + config['born_optimisation'],
                             RPATH = [Literal('\$$ORIGIN/../lib')])

env_loop_process = env.Clone(F90FLAGS = config['f90_flags'] + config['loop_optimisation'],
                             RPATH = [Literal('\$$ORIGIN/../lib')])


for (loops, processlib) in process_list:

    print 'process library:', processlib + '_' + loops

    # process library name and directories
    processlib_name = 'openloops_' + processlib.lower() + '_' + loops
    processlib_info = os.path.join(config['process_lib_dir'], 'lib' + processlib_name + '.info')
    processlib_src_dir = os.path.join(config['process_src_dir'], processlib)
    processlib_obj_dir = os.path.join(config['process_obj_dir'], processlib)

    # run the process code generator
    if generate_process_true:
        generate_process(loops, processlib)

    # compile process
    if config['compile'] > 0 and not GetOption('clean'):

        # list of process library source files
        process_dp_src, process_mp_src, info_files = OLToolbox.get_processlib_src(loops,
                                                         processlib, config['process_src_dir'])

        # prepend global libary info
        library_info_file = os.path.join(processlib_src_dir, 'info_' + processlib + '.txt')
        if os.path.isfile(library_info_file):
            info_files.insert(0, library_info_file)

        # set up process library source files for preprocessing
        process_cpp_container = CPPContainer(
            mp = config['precision'],
            cpp_defs = cpp_defines + [OLBaseConfig.loops_cppdefs[loopspec] for loopspec in loops],
            target = 'cpp_' + processlib,
            target_prefix = os.path.join('..', '..', processlib_obj_dir, ''))

        # set up process library
        process_lib = OLLibrary(name = processlib_name,
                                target_dir = config['process_lib_dir'],
                                mod_dependencies = ['olcommon', 'openloops'],
                                mod_dir = os.path.join(processlib_obj_dir, 'mod'),
                                mp_src = process_mp_src,
                                dp_src = process_dp_src,
                                to_cpp = process_cpp_container)

        # preprocess process library source files
        if not process_cpp_container.run():
            print '***', processlib, 'cpp failed ***'
            Exit(1)

        # delete all libraries for the process
        delete_libs = [os.path.join(config['process_lib_dir'],
                       'libopenloops_' + processlib.lower() + '_' + lps + '.*')
                       for lps in OLBaseConfig.loops_specifications if lps not in ('auto', loops)]
        delete_libs = sum([Glob(patt) for patt in delete_libs], [])
        if delete_libs:
            Execute(Delete(delete_libs))

        # compile process library
        if 'l' in loops:
            libprocess = process_lib.compile(env = env_loop_process, shared = config['shared_libraries'])
        else:
            libprocess = process_lib.compile(env = env_born_process, shared = config['shared_libraries'])

        # concatenate subprocess info files to a library info file
        libprocess_info = env.Substfile(processlib_info, info_files, LINESEPARATOR = '')
        libprocess += libprocess_info

        Alias('lib' + processlib, libprocess)
        Default('lib' + processlib)

    if GetOption('clean'):
        if config['compile'] > 0:
            # delete process object code
            Execute(Delete(processlib_obj_dir))
            delete_libs = [os.path.join(config['process_lib_dir'],
                        'libopenloops_' + processlib + '_' + lps + '.*')
                        for lps in OLBaseConfig.loops_specifications if lps != 'auto']
            delete_libs = sum([Glob(patt) for patt in delete_libs], [])
            if delete_libs:
                Execute(Delete(delete_libs))
        if 'src' in config['clean']:
            # delete process library source directory
            Execute(Delete(processlib_src_dir))



if GetOption('clean') and 'procs' in config['clean']:
    Execute(Delete(config['process_obj_dir']))
    Execute(Delete(config['process_lib_dir']))
    if 'src' in config['clean']:
        Execute(Delete(config['process_src_dir']))
