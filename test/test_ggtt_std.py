from __future__ import print_function
from nose.tools import *
import os
import unittest
import ol_test_setup as olsetup
import olhelper


# Testing class
class ggtt_std_points(unittest.TestCase):
    # Hardcoded threshold for number of digits of agreement with the benchmark

    #Testing the tree level MEs
    @olsetup.oltest('ggtt')
    def test_ggtt(self):
        # OL run to be tested
        threshold_digits = -13

        # File with 10 standard points for g g -> t t~
        pointsfile = os.path.join(olsetup.pointspath, 'points_ggtt_std.log')

        # OL process registering
        with olsetup.suppress_stdout_stderr():
          procs = olhelper.read_procs(pointsfile, self.process, verbose=1)

        # OL settings
        olhelper.stabilitymode(11)

        # OL benchmark results
        benchmks = olhelper.read_mefile(pointsfile, ispointsfile=True)
        mes = olhelper.run_file(pointsfile, procs, nstart=1, npoints=10, verbose=1)
        for i in range(-1, len(benchmks)):
            digits = -olhelper.digit_agreement(benchmks[i][0], mes[i].tree)
            self.assertLessEqual(digits, threshold_digits)
        for i in range(len(benchmks)):
            digits = -olhelper.digit_agreement(benchmks[i][1], mes[i].loop.finite)
            self.assertLessEqual(digits, threshold_digits)
        for i in range(-1, len(benchmks)):
            digits = -olhelper.digit_agreement(benchmks[i][2], mes[i].loop.ir1)
            self.assertLessEqual(digits, threshold_digits)
        for i in range(-1, len(benchmks)):
            digits = -olhelper.digit_agreement(benchmks[i][3], mes[i].loop.ir2)
            self.assertLessEqual(digits, threshold_digits)
