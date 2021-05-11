<template>
    <el-dialog
            title="帮助"
            :visible.sync="dialogVisible"
            width="70%"
            customClass="flowHelp"
            top="5vh"
    >
    <div :style="{overflow: 'auto', height:divHeight+'px'}" >
        <el-tabs tab-position="left">
            <el-tab-pane label="如何新增">
                <el-divider content-position="left">如何新增</el-divider>
                <div>按住鼠标拖拽左侧组件到中间画布中松开鼠标即可</div>
            </el-tab-pane>
            <el-tab-pane label="如何删除">
                <el-divider content-position="left">节点删除</el-divider>
                <div>
                    鼠标点击节点的删除按钮即可
                </div>
            </el-tab-pane>
            <el-tab-pane label="如何移动">
                <el-divider content-position="left">如何移动</el-divider>
                <div>鼠标移动到节点中，当鼠标变为可拖拽的图标时按下鼠标移动到新的位置松开鼠标</div>
            </el-tab-pane>
            <el-tab-pane label="如何连线">
                <el-divider content-position="left">如何连线</el-divider>
                <div>鼠标移动到节点中左侧的图标上，当鼠标变为+时按下鼠标移动到另一个节点中松开鼠标</div>
            </el-tab-pane>
            <el-tab-pane label="如何设置内容">
                <el-divider content-position="left">如何设置内容</el-divider>
                <div>双击节点，节点会弹出详细设置页面</div>
            </el-tab-pane>
            <el-tab-pane label="数据源">
                <el-divider content-position="left">数据源</el-divider>
                <div>设置数据源</div>
            </el-tab-pane>
            <el-tab-pane label="过滤">
                <el-divider content-position="left">过滤</el-divider>
                <div>1.根据一定条件筛选出缺失值较多的字段 <br><br> 2.删除二次选择选中的字段</div>
            </el-tab-pane>
            <el-tab-pane label="行处理">
                <el-divider content-position="left">行处理</el-divider>
                <div>筛选：保留符合条件的行记录<br><br>
                </div>
            </el-tab-pane>
            <el-tab-pane label="列处理">
                <el-divider content-position="left">列处理</el-divider>
                <div>导出（新列）：通过编辑器运算生成新列。<br>
                      填充：将某字段中符合条件的单元格填充为特定值<br>
                      重新分类：将某字段按照特定条件分成不同个数的组别<br>
                      哑变量转化：生成哑变量<br>
                      转置：行列转换
                      </div>
            </el-tab-pane>
            <el-tab-pane label="数据整合">
                <el-divider content-position="left">数据整合</el-divider>
                <div>追加：行记录追加<br><br>
                  合并：多份数据集合并
                  </div>
            </el-tab-pane>
            <el-tab-pane label="样本管理">
                <el-divider content-position="left">样本管理</el-divider>
                <div>
                  样本抽样：按照特定条件抽取部分记录<br><br>
                  数据分组：按照特定比例分成测试、训练集
                </div>
            </el-tab-pane>
        </el-tabs>
    </div>
    </el-dialog>
</template>

<script>
    export default {
        data() {
            return {
                dialogVisible: false,
                divHeight:0,
            }
        },
        components: {},
        methods: {
            init() {
                this.dialogVisible = true
                this.divHeight=(window.innerHeight*0.8)-110;
            }
        }
    }
</script>

<style>
    .flowHelp {
        height: 80%;
    }
</style>
