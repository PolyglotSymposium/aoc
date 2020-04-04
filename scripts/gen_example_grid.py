#!/usr/bin/env python3

from subprocess import Popen, PIPE
import json
import os

process = Popen(['stack', 'test'], stdout=PIPE, stderr=PIPE)
stdout, stderr = process.communicate()

outputs = [json.loads(line.replace('EXAMPLE_OUTPUT ', '')) for line in stdout.decode('ascii').split('\n') if line.startswith('EXAMPLE_OUTPUT')]

def problem_link(v):
  return f"https://adventofcode.com/{v['year']}/day/{v['day']}"

formatted_rows = "\n".join([
  f"| {v['year']} | {v['day']} | {v['part']} | { 0.000000001 * v['ns'] } | `{v['domain']}` | [{v['source']}]({v['source']}) | [{problem_link(v)}]({problem_link(v)}) |"
  for v in outputs
])

output = f"""
# Working Examples

There are currently **{len(outputs)}** working solutions, all verified by tests

| Year | Day | Part | Time (sec) | Domain | Solution | Problem |
|------|-----|------|------------|--------|----------|---------|
{formatted_rows}
"""

with open(os.path.join(os.path.dirname(__file__), '..', 'EXAMPLES.md'), 'w') as f:
    f.write(output)
