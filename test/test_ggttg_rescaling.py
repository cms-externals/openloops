from __future__ import print_function
from nose.tools import *
import unittest
from . import ol_test_setup as olsetup
from . import olhelper
from openloops import Process, LOOP


class ggttg_rescaling(unittest.TestCase):

    @olsetup.oltest('ggttg')
    def test_ggttg_rescaling_dp(self):
        olhelper.stabilitymode(12)
        pp = [[500.0, 0.0, 0.0, 500.0],
              [500.0, 0.0, 0.0, -500.0],
              [442.9730634176155, 231.3913515296482, 145.79962788530543, 303.0538662669989],
              [494.29406027133973, -283.46127873151147, -142.2703926866928, -337.86307410576484],
              [62.73287631104489, 52.06992720186313, -3.529235198612626, 34.80920783876596]]

        perm = [3, 4, 1, 2, 5]
        perm_str = '[' + ','.join(map(str, perm)) + ']'
        with olsetup.suppress_stdout_stderr():
          pr = Process(self.process + perm_str, LOOP)
          me = pr.evaluate(pp)
        m_exp = -0.0014891055057249885
        digits = -olhelper.digit_agreement(m_exp, me.loop.finite)
        self.assertGreater(digits, -14)

    @olsetup.oltest('ggttg', OLMode='2')
    def test_ggttg_rescaling_qp(self):
        pp = [[500.0, 0.0, 0.0, 500.0],
              [500.0, 0.0, 0.0, -500.0],
              [442.9730634176155, 231.3913515296482, 145.79962788530543, 303.0538662669989],
              [494.29406027133973, -283.46127873151147, -142.2703926866928, -337.86307410576484],
              [62.73287631104489, 52.06992720186313, -3.529235198612626, 34.80920783876596]]

        olhelper.stabilitymode(32)
        perm = [3, 4, 1, 2, 5]
        perm_str = '[' + ','.join(map(str, perm)) + ']'
        with olsetup.suppress_stdout_stderr():
          pr = Process(self.process + perm_str, LOOP)
          me = pr.evaluate(pp)
        self.assertGreater(me.acc, -32)
