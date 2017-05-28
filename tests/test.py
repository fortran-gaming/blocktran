#!/usr/bin/env python
from pathlib import Path
import subprocess as S

R = Path(__file__).parents[1]

cmd = [R/'bin/tetran', '100']

S.check_call(cmd, timeout = 30, cwd=R)
