#!/usr/bin/env python3
"""
plot_results.py - Generate performance comparison graphs

Creates visualizations comparing Common Lisp and Go gRPC performance
across different RPC types and client counts.
"""

import json
import matplotlib.pyplot as plt
import numpy as np
from pathlib import Path

def load_results(filename):
    """Load benchmark results from JSON file."""
    with open(filename, 'r') as f:
        return json.load(f)

def create_comparison_graphs():
    """Create comparison graphs for CL vs Go performance."""

    # Load results
    try:
        cl_results = load_results('results-cl.json')
        go_results = load_results('results-go.json')
    except FileNotFoundError as e:
        print(f"Error: {e}")
        print("Please run the benchmarks first to generate result files.")
        return

    # Organize data by RPC type and client count
    rpc_types = ['unary', 'server-streaming', 'client-streaming', 'bidi-streaming']
    client_counts = [1, 10, 100]

    cl_data = {}
    go_data = {}

    for result in cl_results:
        key = (result['rpc_type'], result['num_clients'])
        cl_data[key] = result['requests_per_second']

    for result in go_results:
        key = (result['rpc_type'], result['num_clients'])
        go_data[key] = result['requests_per_second']

    # Create figure with subplots
    fig, axes = plt.subplots(2, 2, figsize=(15, 12))
    fig.suptitle('gRPC Performance Comparison: Common Lisp vs Go',
                 fontsize=16, fontweight='bold')

    axes = axes.flatten()

    # Plot each RPC type
    for idx, rpc_type in enumerate(rpc_types):
        ax = axes[idx]

        cl_values = [cl_data.get((rpc_type, c), 0) for c in client_counts]
        go_values = [go_data.get((rpc_type, c), 0) for c in client_counts]

        x = np.arange(len(client_counts))
        width = 0.35

        bars1 = ax.bar(x - width/2, cl_values, width, label='Common Lisp',
                       color='#3498db', alpha=0.8)
        bars2 = ax.bar(x + width/2, go_values, width, label='Go',
                       color='#2ecc71', alpha=0.8)

        ax.set_xlabel('Number of Concurrent Clients', fontweight='bold')
        ax.set_ylabel('Requests per Second', fontweight='bold')
        ax.set_title(f'{rpc_type.replace("-", " ").title()} RPC',
                     fontweight='bold')
        ax.set_xticks(x)
        ax.set_xticklabels(client_counts)
        ax.legend()
        ax.grid(True, alpha=0.3, axis='y')

        # Add value labels on bars
        for bars in [bars1, bars2]:
            for bar in bars:
                height = bar.get_height()
                if height > 0:
                    ax.text(bar.get_x() + bar.get_width()/2., height,
                           f'{int(height)}',
                           ha='center', va='bottom', fontsize=9)

    plt.tight_layout()
    plt.savefig('performance_comparison.png', dpi=300, bbox_inches='tight')
    print("Saved: performance_comparison.png")

    # Create overall comparison chart
    fig, ax = plt.subplots(figsize=(14, 8))

    # Prepare data for grouped bar chart
    n_groups = len(rpc_types)
    n_clients = len(client_counts)

    # Calculate positions
    bar_width = 0.12
    group_width = bar_width * (n_clients * 2 + 1)
    group_positions = np.arange(n_groups) * (group_width + 0.3)

    colors_cl = ['#3498db', '#5dade2', '#85c1e9']
    colors_go = ['#2ecc71', '#58d68d', '#82e0aa']

    for i, clients in enumerate(client_counts):
        cl_values = [cl_data.get((rpc, clients), 0) for rpc in rpc_types]
        go_values = [go_data.get((rpc, clients), 0) for rpc in rpc_types]

        offset_cl = -bar_width * (n_clients - i)
        offset_go = bar_width * (i + 1)

        ax.bar(group_positions + offset_cl, cl_values, bar_width,
               label=f'CL {clients} client{"s" if clients > 1 else ""}',
               color=colors_cl[i], alpha=0.9)
        ax.bar(group_positions + offset_go, go_values, bar_width,
               label=f'Go {clients} client{"s" if clients > 1 else ""}',
               color=colors_go[i], alpha=0.9)

    ax.set_xlabel('RPC Type', fontweight='bold', fontsize=12)
    ax.set_ylabel('Requests per Second', fontweight='bold', fontsize=12)
    ax.set_title('Overall Performance Comparison by RPC Type and Client Count',
                 fontweight='bold', fontsize=14)
    ax.set_xticks(group_positions)
    ax.set_xticklabels([rpc.replace('-', '\n').title() for rpc in rpc_types])
    ax.legend(loc='upper left', ncol=2)
    ax.grid(True, alpha=0.3, axis='y')

    plt.tight_layout()
    plt.savefig('performance_overall.png', dpi=300, bbox_inches='tight')
    print("Saved: performance_overall.png")

    # Create speedup ratio chart
    fig, ax = plt.subplots(figsize=(12, 8))

    ratios = {}
    for rpc_type in rpc_types:
        for clients in client_counts:
            key = (rpc_type, clients)
            cl_val = cl_data.get(key, 0)
            go_val = go_data.get(key, 0)
            if go_val > 0:
                ratios[key] = (cl_val / go_val) * 100

    x_pos = []
    ratio_values = []
    labels = []
    colors = []

    pos = 0
    for rpc_type in rpc_types:
        for i, clients in enumerate(client_counts):
            key = (rpc_type, clients)
            if key in ratios:
                x_pos.append(pos)
                ratio_values.append(ratios[key])
                labels.append(f'{clients}')
                # Color based on performance: green if >80%, yellow if 50-80%, red if <50%
                ratio = ratios[key]
                if ratio >= 80:
                    colors.append('#2ecc71')
                elif ratio >= 50:
                    colors.append('#f39c12')
                else:
                    colors.append('#e74c3c')
                pos += 1
        pos += 0.5  # Gap between RPC types

    bars = ax.barh(x_pos, ratio_values, color=colors, alpha=0.8)
    ax.axvline(x=100, color='gray', linestyle='--', linewidth=2, label='Equal Performance')
    ax.set_yticks(x_pos)
    ax.set_yticklabels(labels)
    ax.set_xlabel('Common Lisp Performance (% of Go)', fontweight='bold', fontsize=12)
    ax.set_title('Common Lisp vs Go Performance Ratio', fontweight='bold', fontsize=14)
    ax.legend()
    ax.grid(True, alpha=0.3, axis='x')

    # Add RPC type labels on the side
    rpc_label_positions = []
    current_pos = 0
    for rpc_type in rpc_types:
        mid = current_pos + (len(client_counts) - 1) / 2
        rpc_label_positions.append(mid)
        current_pos += len(client_counts) + 0.5

    ax2 = ax.twinx()
    ax2.set_ylim(ax.get_ylim())
    ax2.set_yticks(rpc_label_positions)
    ax2.set_yticklabels([rpc.replace('-', ' ').title() for rpc in rpc_types],
                        fontweight='bold')

    # Add value labels on bars
    for bar, val in zip(bars, ratio_values):
        width = bar.get_width()
        ax.text(width + 2, bar.get_y() + bar.get_height()/2,
               f'{val:.1f}%',
               ha='left', va='center', fontsize=9)

    plt.tight_layout()
    plt.savefig('performance_ratio.png', dpi=300, bbox_inches='tight')
    print("Saved: performance_ratio.png")

    # Print summary statistics
    print("\n" + "="*60)
    print("PERFORMANCE SUMMARY")
    print("="*60)

    for rpc_type in rpc_types:
        print(f"\n{rpc_type.upper().replace('-', ' ')}:")
        for clients in client_counts:
            key = (rpc_type, clients)
            cl_val = cl_data.get(key, 0)
            go_val = go_data.get(key, 0)
            ratio = (cl_val / go_val * 100) if go_val > 0 else 0
            print(f"  {clients:3d} clients: CL={cl_val:7.1f} req/s  Go={go_val:7.1f} req/s  Ratio={ratio:5.1f}%")

    # Calculate averages
    print("\n" + "="*60)
    print("AVERAGES:")
    print("="*60)

    for clients in client_counts:
        cl_avg = np.mean([cl_data.get((rpc, clients), 0) for rpc in rpc_types])
        go_avg = np.mean([go_data.get((rpc, clients), 0) for rpc in rpc_types])
        ratio_avg = (cl_avg / go_avg * 100) if go_avg > 0 else 0
        print(f"  {clients:3d} clients: CL={cl_avg:7.1f} req/s  Go={go_avg:7.1f} req/s  Ratio={ratio_avg:5.1f}%")

if __name__ == '__main__':
    create_comparison_graphs()
