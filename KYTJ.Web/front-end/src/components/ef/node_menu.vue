<template>
    <div class="flow-menu" ref="tool" >
        <ul  class="el-pager ef-node-menu-ul">
                <draggable @end="end" @start="move" v-model="menuList" :options="draggableOptions">
                    <li v-for="subMenu in menuList" class="ef-node-menu-li" :key="subMenu.id" :type="subMenu.type" >
                        <i :class="subMenu.ico"></i> {{subMenu.name}}
                    </li>
                </draggable>
            </ul>
    </div>
</template>
<script>
    import draggable from 'vuedraggable'

    var mousePosition = {
        left: -1,
        top: -1
    }

    export default {
        data() {
            return {
                activeNames: '1',
                // draggable配置参数参考 https://www.cnblogs.com/weixin186/p/10108679.html
                draggableOptions: {
                    preventOnFilter: false,
                    sort: false,
                    disabled: false,
                    ghostClass: 'tt',
                    // 不使用H5原生的配置
                    forceFallback: true,
                    // 拖拽的时候样式
                    // fallbackClass: 'flow-node-draggable'
                },
                 menuList: [
                            {
                                id: '1',
                                type: 'dataSource',
                                name: '数据源',
                                ico: 'el-icon-coin'
                            }, {
                                id: '2',
                                type: 'dataFilter',
                                name: '过滤',
                                ico: 'el-icon-shopping-cart-2'
                            },
                            {
                                id: '3',
                                type: 'dataRow',
                                name: '行处理',
                                ico: 'el-icon-s-operation'
                            },
                            {
                                id: '4',
                                type: 'dataColumn',
                                name: '列处理',
                                ico: 'el-icon-c-scale-to-original'
                            },
                            {
                                id: '5',
                                type: 'dataCombine',
                                name: '数据整合',
                                ico: 'el-icon-s-tools'
                            }
                            ,
                            {
                                id: '6',
                                type: 'dataSample',
                                name: '样本管理',
                                ico: 'el-icon-s-grid'
                            },
                            {
                                id: '7',
                                type: 'dataPreview',
                                name: '数据预览',
                                ico: 'el-icon-s-data'
                            },
                            {
                                id: '8',
                                type: 'dataCalculate',
                                name: '统计分析',
                                ico: 'el-icon-s-marketing'
                            }
                 ],
                nodeMenu: {}
            }
        },
        components: {
            draggable
        },
        created() {
            /**
             * 以下是为了解决在火狐浏览器上推拽时弹出tab页到搜索问题
             * @param event
             */
            if (this.isFirefox()) {
                document.body.ondrop = function (event) {
                    // 解决火狐浏览器无法获取鼠标拖拽结束的坐标问题
                    mousePosition.left = event.layerX
                    mousePosition.top = event.clientY - 50
                    event.preventDefault();
                    event.stopPropagation();
                }
            }
        },
        methods: {
            // 根据类型获取左侧菜单对象
            getMenuByType(type) {
                for (let i = 0; i < this.menuList.length; i++) {
                        if (this.menuList[i].type === type) {
                            return this.menuList[i]
                        }
                }
            },
            // 拖拽开始时触发
            move(evt, a, b, c) {
                var type = evt.item.attributes.type.nodeValue
                this.nodeMenu = this.getMenuByType(type)
            },
            // 拖拽结束时触发
            end(evt, e) {
                this.$emit('addNode', evt, this.nodeMenu, mousePosition)
            },
            // 是否是火狐浏览器
            isFirefox() {
                var userAgent = navigator.userAgent
                if (userAgent.indexOf("Firefox") > -1) {
                    return true
                }
                return false
            }
        }
    }
</script>
