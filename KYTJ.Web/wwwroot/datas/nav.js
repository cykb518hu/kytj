var postPath = window.document.location.pathname;
if (postPath == "/") postPath = "";
var navs = [{
    "title": "系统首页",
    "icon": "fa-cubes",
    "spread": true,
    "children": [
        {
            "title": "项目主页",
            "icon": "fa-area-chart",
            "href": postPath + "/Project/Index"
        },
        {
            "title": "数据抽取",
            "icon": "fa-chain-broken",
            "href": postPath + "/Dataset/SearchEngine",
        },
        {
            "title": "数据集管理",
            "icon": "fa-align-justify",
            "href": postPath + "/Dataset/DatasetList",
        },

        {
            "title": "数据集检查",
            "icon": "fa fa-check-circle",
            "href": postPath + "/DataManage/DataCheck",
        },
        {
            "title": "操作日志",
            "icon": "fa-envelope-open-o",
            "href": postPath + "http://localhost:8080",
        }
    ]
}
];