
<style>


  .el-dialog__body{
    padding: 10px 20px !important;
  }

</style>
<template>
  <el-dialog :visible.sync="dialogVisible" width="60%" title="列处理" top="5vh"  >
    <div style="height:400px">
       <el-button @click="Calculate" type="primary" >开始分析</el-button>

 <div id="myChart" :style="{width: '300px', height: '300px'}"></div>

    </div>

  </el-dialog>
</template>

<script>
import qs from "qs";

export default {

  data() {
    return {

      dialogVisible: false,
      dataFlowCache:[],
      dataColumns:[],
      filterColumns:[{"column":"","operation":"","value":"","id":0}],
      nodeId:"",
      dataCount:0


    };
  },
  components: {},
  methods: {
    init(nodeId,cache) {
      this.nodeId=nodeId;//nodeId;
      this.dialogVisible = true;
      this.dataFlowCache=cache;
      this.dataColumns=cache.dataColumns;
      this.dataCount=cache.dataCount;
    },
    Calculate(){
        this.drawLine();
    },
    drawLine(){
        // 基于准备好的dom，初始化echarts实例
        let myChart = this.$echarts.init(document.getElementById('myChart'))
        // 绘制图表
        myChart.setOption({
            title: { text: '在Vue中使用echarts' },
            tooltip: {},
            xAxis: {
                data: ["衬衫","羊毛衫","雪纺衫","裤子","高跟鞋","袜子"]
            },
            yAxis: {},
            series: [{
                name: '销量',
                type: 'bar',
                data: [5, 20, 36, 10, 10, 20]
            }]
        });
    }


  }
};
</script>
