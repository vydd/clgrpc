#!/usr/bin/env python3
"""
Plot performance comparison between Go and Common Lisp gRPC implementations.
Focuses on unary RPC performance.
"""

import json
import matplotlib.pyplot as plt
import numpy as np

# Read results
with open('results-go.json', 'r') as f:
    go_results = json.load(f)

with open('results-cl.json', 'r') as f:
    cl_results = json.load(f)

# Extract unary RPC results
go_unary = [r for r in go_results if r['rpc_type'] == 'unary']
cl_unary = [r for r in cl_results if r['rpc_type'] == 'unary']

# Sort by num_clients
go_unary.sort(key=lambda x: x['num_clients'])
cl_unary.sort(key=lambda x: x['num_clients'])

clients = [r['num_clients'] for r in go_unary]
go_rps = [r['requests_per_second'] for r in go_unary]
cl_rps = [r['requests_per_second'] for r in cl_unary]

# Create figure with subplots
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(15, 6))

# Plot 1: Absolute Performance
x = np.arange(len(clients))
width = 0.35

bars1 = ax1.bar(x - width/2, go_rps, width, label='Go', color='#00ADD8')
bars2 = ax1.bar(x + width/2, cl_rps, width, label='Common Lisp', color='#5F5FFF')

ax1.set_xlabel('Number of Concurrent Clients', fontsize=12)
ax1.set_ylabel('Requests per Second', fontsize=12)
ax1.set_title('Unary RPC Performance: Go vs Common Lisp', fontsize=14, fontweight='bold')
ax1.set_xticks(x)
ax1.set_xticklabels(clients)
ax1.legend()
ax1.grid(True, alpha=0.3, axis='y')

# Add value labels on bars
for bars in [bars1, bars2]:
    for bar in bars:
        height = bar.get_height()
        ax1.text(bar.get_x() + bar.get_width()/2., height,
                f'{int(height):,}',
                ha='center', va='bottom', fontsize=9)

# Plot 2: Relative Performance (Go as baseline = 100%)
relative_perf = [(cl/go)*100 for cl, go in zip(cl_rps, go_rps)]

bars3 = ax2.bar(clients, relative_perf, color='#5F5FFF', alpha=0.7)
ax2.axhline(y=100, color='#00ADD8', linestyle='--', linewidth=2, label='Go Baseline (100%)')

ax2.set_xlabel('Number of Concurrent Clients', fontsize=12)
ax2.set_ylabel('Performance Relative to Go (%)', fontsize=12)
ax2.set_title('Common Lisp Performance vs Go Baseline', fontsize=14, fontweight='bold')
ax2.set_xticklabels([str(c) for c in clients])
ax2.legend()
ax2.grid(True, alpha=0.3, axis='y')

# Add value labels
for i, (bar, val) in enumerate(zip(bars3, relative_perf)):
    height = bar.get_height()
    ax2.text(bar.get_x() + bar.get_width()/2., height,
            f'{val:.2f}%',
            ha='center', va='bottom', fontsize=10, fontweight='bold')

    # Add speedup factor below bar
    speedup = go_rps[i] / cl_rps[i]
    ax2.text(bar.get_x() + bar.get_width()/2., height/2,
            f'{speedup:.0f}x slower',
            ha='center', va='center', fontsize=9, color='white', fontweight='bold')

plt.tight_layout()
plt.savefig('benchmark_comparison_unary.png', dpi=300, bbox_inches='tight')
print('Saved benchmark_comparison_unary.png')

# Print summary table
print('\n' + '='*80)
print('PERFORMANCE COMPARISON SUMMARY: Unary RPC')
print('='*80)
print(f'{"Clients":<10} {"Go (req/s)":<15} {"CL (req/s)":<15} {"CL/Go":<10} {"Slowdown":<10}')
print('-'*80)
for i, c in enumerate(clients):
    ratio = (cl_rps[i] / go_rps[i]) * 100
    slowdown = go_rps[i] / cl_rps[i]
    print(f'{c:<10} {go_rps[i]:<15,.1f} {cl_rps[i]:<15,.1f} {ratio:<9.2f}% {slowdown:<9.0f}x')
print('='*80)

# Scaling efficiency
print('\nSCALING EFFICIENCY')
print('='*80)
print(f'{"Implementation":<15} {"1→10 clients":<20} {"10→100 clients":<20} {"1→100 clients":<20}')
print('-'*80)

def scaling_factor(rps_list, from_idx, to_idx):
    expected = (clients[to_idx] / clients[from_idx]) * rps_list[from_idx]
    actual = rps_list[to_idx]
    return (actual / expected) * 100

go_1_10 = scaling_factor(go_rps, 0, 1)
go_10_100 = scaling_factor(go_rps, 1, 2)
go_1_100 = scaling_factor(go_rps, 0, 2)

cl_1_10 = scaling_factor(cl_rps, 0, 1)
cl_10_100 = scaling_factor(cl_rps, 1, 2)
cl_1_100 = scaling_factor(cl_rps, 0, 2)

print(f'{"Go":<15} {go_1_10:<19.1f}% {go_10_100:<19.1f}% {go_1_100:<19.1f}%')
print(f'{"Common Lisp":<15} {cl_1_10:<19.1f}% {cl_10_100:<19.1f}% {cl_1_100:<19.1f}%')
print('='*80)
print('\nNote: 100% = perfect linear scaling')
