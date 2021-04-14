var jsondata ={
	"title": "newFlow_1",
	"nodes": {
		"demo_node_1": {
			"name": "数据源1",
			"left": 61,
			"top": 146,
			"type": "node",
			"width": 100,
			"height": 24,
			"alt": true
		},
		"demo_node_2": {
			"name": "行处理",
			"left": 217,
			"top": 148,
			"type": "database",
			"width": 100,
			"height": 24,
			"alt": true
		},
		"demo_node_3": {
			"name": "列处理",
			"left": 382,
			"top": 146,
			"type": "database",
			"width": 100,
			"height": 24,
			"alt": true
		},
		"demo_node_6": {
			"name": "视图预览",
			"left": 221,
			"top": 61,
			"type": "node",
			"width": 100,
			"height": 24,
			"alt": true
		},
		"demo_node_9": {
			"name": "数据源2",
			"left": 70,
			"top": 265,
			"type": "node",
			"width": 100,
			"height": 24,
			"alt": true
		},
		"demo_node_10": {
			"name": "行处理",
			"left": 223,
			"top": 266,
			"type": "database",
			"width": 100,
			"height": 24,
			"alt": true
		},
		"demo_node_11": {
			"name": "列处理",
			"left": 385,
			"top": 266,
			"type": "database",
			"width": 100,
			"height": 24,
			"alt": true
		},
		"demo_node_15": {
			"name": "数据整合",
			"left": 542,
			"top": 209,
			"type": "node",
			"width": 100,
			"height": 24,
			"alt": true
		},
		"demo_node_18": {
			"name": "视图预览",
			"left": 381,
			"top": 343,
			"type": "database",
			"width": 100,
			"height": 24,
			"alt": true
		},
		"demo_node_21": {
			"name": "样本管理",
			"left": 686,
			"top": 208,
			"type": "state",
			"width": 100,
			"height": 24,
			"alt": true
		},
		"demo_node_22": {
			"name": "数据优化",
			"left": 821,
			"top": 209,
			"type": "chat",
			"width": 100,
			"height": 24,
			"alt": true
		},
		"demo_node_25": {
			"name": "输出",
			"left": 981,
			"top": 209,
			"type": "plug",
			"width": 100,
			"height": 24,
			"alt": true
		},
		"demo_node_27": {
			"name": "视图预览",
			"left": 826,
			"top": 126,
			"type": "join",
			"width": 100,
			"height": 24,
			"alt": true
		}
	},
	"lines": {
		"demo_line_4": {
			"type": "sl",
			"from": "demo_node_1",
			"to": "demo_node_2",
			"name": "",
			"alt": true
		},
		"demo_line_5": {
			"type": "sl",
			"from": "demo_node_2",
			"to": "demo_node_3",
			"name": "",
			"alt": true
		},
		"demo_line_7": {
			"type": "sl",
			"from": "demo_node_2",
			"to": "demo_node_6",
			"name": "",
			"alt": true
		},
		"demo_line_12": {
			"type": "sl",
			"from": "demo_node_9",
			"to": "demo_node_10",
			"name": "",
			"alt": true
		},
		"demo_line_13": {
			"type": "sl",
			"from": "demo_node_10",
			"to": "demo_node_11",
			"name": "",
			"alt": true
		},
		"demo_line_16": {
			"type": "sl",
			"from": "demo_node_3",
			"to": "demo_node_15",
			"name": "",
			"alt": true
		},
		"demo_line_17": {
			"type": "sl",
			"from": "demo_node_11",
			"to": "demo_node_15",
			"name": "",
			"alt": true
		},
		"demo_line_19": {
			"type": "sl",
			"from": "demo_node_11",
			"to": "demo_node_18",
			"name": "",
			"alt": true
		},
		"demo_line_23": {
			"type": "sl",
			"from": "demo_node_15",
			"to": "demo_node_21",
			"name": "",
			"alt": true
		},
		"demo_line_24": {
			"type": "sl",
			"from": "demo_node_21",
			"to": "demo_node_22",
			"name": "",
			"alt": true
		},
		"demo_line_26": {
			"type": "sl",
			"from": "demo_node_22",
			"to": "demo_node_25",
			"name": "",
			"alt": true
		},
		"demo_line_28": {
			"type": "sl",
			"from": "demo_node_22",
			"to": "demo_node_27",
			"name": "",
			"alt": true
		}
	},
	"areas": {},
	"initNum": 29
}