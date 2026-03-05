package main

import (
	"fmt"
	"sort"
)

// 给出一些计算机课程，每个课程都有前置课程，只有完成了前置课程才可以开始当前课程学习
// 目标是选择出一组课程，这组课程必须确保按顺序学习，能全部被完成。
// 这类问题被成为拓扑排序， 前置条件构成有向图，顶点表示课程，边表示课程间的依赖关系
var prereqs = map[string][]string{
	"algorithms": {"data structures"},
	"calculus":   {"linear algebra"},
	"compilers": {
		"data structures",
		"formal languages",
		"computer organization",
	},
	"data structures":       {"discrete math"},
	"databases":             {"data structures"},
	"discrete math":         {"intro to programming"},
	"formal languages":      {"discrete math"},
	"networks":              {"operating systems"},
	"operating systems":     {"data structures", "computer organization"},
	"programming languages": {"data structures", "computer organization"},
}

func main() {
	for i, course := range topoSort(prereqs) {
		fmt.Printf("%d:\t%s\n", i+1, course)
	}
}

// 使用深度优先搜索整张图
func topoSort(m map[string][]string) []string {
	var order []string
	seen := make(map[string]bool)
	// 匿名函数被递归调用时，必须首先申明一个变量
	var visitAll func(items []string)
	visitAll = func(items []string) {
		for _, item := range items {
			if !seen[item] {
				seen[item] = true
				visitAll(m[item])
				order = append(order, item)
			}
		}
	}

	var keys []string
	for key := range m {
		keys = append(keys, key)
	}

	sort.Strings(keys)
	visitAll(keys)
	return order
}
