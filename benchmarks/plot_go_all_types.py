#!/usr/bin/env python3
"""
Plot Go gRPC performance across all RPC types.
"""

import json
import matplotlib.pyplot as plt
import numpy as np

# Read Go results
with open('results-go.json', 'r') as f:
    go_results = json.load(f)

# Group by RPC type
rpc_types = ['unary', 'server-streaming', 'client-streaming', 'bidi-streaming']
colors = ['#00ADD8', '#00C9FF', '#007ACC', '#004D7A']

fig, ax = plt.subplots(figsize=(12, 7))

x = np.array([1, 10, 100])
width = 0.2

for i, rpc_type in enumerate(rpc_types):
    data = [r for r in go_results if r['rpc_type'] == rpc_type]
    data.sort(key=lambda x: x['num_clients'])
    rps = [r['requests_per_second'] for r in data]

    offset = (i - len(rpc_types)/2 + 0.5) * width
    bars = ax.bar(x + offset, rps, width, label=rpc_type.title(), color=colors[i])

    # Add value labels
    for bar in bars:
        height = bar.get_height()
        if height > 1000:
            label = f'{height/1000:.0f}k'
        else:
            label = f'{height:.0f}'
        ax.text(bar.get_x() + bar.get_width()/2., height,
                label, ha='center', va='bottom', fontsize=8)

ax.set_xlabel('Number of Concurrent Clients', fontsize=12)
ax.set_ylabel('Requests per Second', fontsize=12)
ax.set_title('Go gRPC Performance Across RPC Types', fontsize=14, fontweight='bold')
ax.set_xticks(x)
ax.set_xticklabels(['1', '10', '100'])
ax.legend()
ax.grid(True, alpha=0.3, axis='y')
ax.set_yscale('log')

plt.tight_layout()
plt.savefig('benchmark_go_all_types.png', dpi=300, bbox_inches='tight')
print('Saved benchmark_go_all_types.png')

# Print summary
print('\n' + '='*100)
print('GO gRPC PERFORMANCE BY RPC TYPE')
print('='*100)
print(f'{"RPC Type":<20} {"1 client":<20} {"10 clients":<20} {"100 clients":<20}')
print('-'*100)

for rpc_type in rpc_types:
    data = [r for r in go_results if r['rpc_type'] == rpc_type]
    data.sort(key=lambda x: x['num_clients'])
    print(f'{rpc_type:<20} {data[0]["requests_per_second"]:<20,.1f} '
          f'{data[1]["requests_per_second"]:<20,.1f} {data[2]["requests_per_second"]:<20,.1f}')

print('='*100)
