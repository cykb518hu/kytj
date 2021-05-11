
<style>

  .grid-content {
    border-radius: 4px;
    min-height: 36px;
  }
</style>
<template>
  <el-dialog :visible.sync="dialogVisible"  v-if="dialogVisible" width="90%" title="数据预览" top="2vh" :close-on-click-modal="false" >
    <el-row :gutter="20">
      <el-col :span="14">
        <div class="grid-content">
         <el-table
      :data="dataFlowCache.dataColumns"
      stripe
      :max-height="tableConfig.leftTableHeight"
      style="width: 100%">
      <el-table-column
        prop="name"
        label="字段名"
       min-width="40%">
      </el-table-column>
      <el-table-column
        prop="isContinuous"
        :formatter="isContinuousFormatter"
        label="类型"
        min-width="10%">
      </el-table-column>
      <el-table-column
        prop="rem"
        label="重命名"
        min-width="40%"
        >
      </el-table-column>
             <el-table-column label="详情" min-width="10%">
                    <template slot-scope="scope">
                        <el-link type="primary" v-on:click="viewDetail(scope.row)">详情</el-link>
                    </template>
                </el-table-column>
    </el-table>
        </div>
        </el-col>
      <el-col :span="10">
        <div class="grid-content">
          <div>
           {{selectedRow.name}}: 数据分布
          </div>
          <div>
                  <el-table
                  v-if="selectedRow.isContinuous"
      :data="selectedRow.info"
      stripe
      max-height="150"
      style="width: 100%">
      <el-table-column
        prop="min"
        label="min"
       min-width="20%">
      </el-table-column>
      <el-table-column
        prop="max"
        label="max"
        min-width="20%">
      </el-table-column>
      <el-table-column
        label="mean"
        min-width="20%"
        >
         <template slot-scope="scope">
          {{scope.row.mean.toFixed(2) }}
        </template>
        </el-table-column>
         <el-table-column
        label="std"
        min-width="20%"
        >
           <template slot-scope="scope">
          {{scope.row.std.toFixed(2) }}
        </template>
        </el-table-column>
           <el-table-column
        prop="count"
        label="count"
        min-width="20%"
        > </el-table-column>

    </el-table>
      <el-table
                  v-else-if="!selectedRow.isContinuous"
      :data="selectedRow.info"
      stripe
      max-height="150"
      style="width: 100%">
      <el-table-column
        prop="value"
        label="value"
       min-width="40%">
      </el-table-column>

           <el-table-column
        prop="count"
        label="count"
        min-width="30%"
        > </el-table-column>
          <el-table-column
        label="percent"
        min-width="30%"
        >
         <template slot-scope="scope">
          {{scope.row.percent.toFixed(2) }}%
        </template>
        </el-table-column>

    </el-table>

          </div>
          <div style="margin-top:20px">
            <div id="myChart" :style="{width: '100%', height:tableConfig.divHeight+'px'}" ></div>
          </div>

        </div></el-col>
   </el-row>
  </el-dialog>
</template>

<script>

export default {

  data() {
    return {

      dialogVisible: false,
      dataFlowCache:[],
      selectedRow:[],
      tableConfig:{
        leftTableHeight:0,
        divHeight:window.innerHeight-0,

      }

    };
  },
  components: {},
  methods: {
    init(cache) {
      this.dialogVisible = true;
      this.dataFlowCache=cache;
      this.selectedRow=[];
      this.tableConfig.leftTableHeight=window.innerHeight-125;
      this.tableConfig.divHeight=window.innerHeight-300;

    },

        isContinuousFormatter(row, column) {
                if (row.isContinuous) {
                    return '连续';
                } else {
                    return '分类';
                }
            },
        viewDetail(row){
          this.selectedRow=row;
          this.selectedRow.info=JSON.parse(row.statisticsInfo);
          this.drawBar();

        },

        drawBar(){
            // 基于准备好的dom，初始化echarts实例
            let myChart = this.$echarts.init(document.getElementById('myChart'))
            let xAxisData=[];
            let seriesData=[];
            let rotate=0;
            if(this.selectedRow.isContinuous){
              //借用nullPercent 字段
              let colStatis=JSON.parse(this.selectedRow.nullPercent);
              colStatis.map((item)=> {
                  xAxisData.push(item.value);
                  seriesData.push(item.count)
                  })

            }
            else{
                //分类直接获取数据
                this.selectedRow.info.map((item)=> {
                  xAxisData.push(item.value);
                  seriesData.push(item.count)
              })

            }
            if(xAxisData.length>4){
                rotate=45;
            }

            myChart.setOption({
                title: {
                  text: '数据分布图' ,
                  x:'center',
                  y:'top',
                  textAlign:'center'
                },
                tooltip: {},
                xAxis: {
                    type: 'category',
                    data: xAxisData,
                    axisLabel: {
                        interval: 0,
                        rotate: rotate,
                        //倾斜度 -90 至 90 默认为0
                        margin: 5
                    }
                },
                yAxis: {
                    type: 'value'
                },
                grid:{
                  top:40					//---相对位置，top\bottom\left\right
                },
                series: [{
                    data: seriesData,
                    type: 'bar'

                }]
            });
        }

  }
};
</script>
