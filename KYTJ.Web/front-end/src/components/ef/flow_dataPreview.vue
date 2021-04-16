
<style>

  .grid-content {
    border-radius: 4px;
    min-height: 36px;
  }
  .el-dialog__body{
    padding: 10px 20px !important;
  }

</style>
<template>
  <el-dialog :visible.sync="dialogVisible" width="80%" title="数据预览" top="5vh">
    <el-row :gutter="20">
      <el-col :span="12">
        <div class="grid-content">
         <el-table
      :data="dataFlowCache.dataColumns"
      stripe
      max-height="500"
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
      <el-col :span="12">
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
          <div>
            <div id="myChart" ></div>
          </div>

        </div></el-col>
   </el-row>
  </el-dialog>
</template>

<script>
import qs from "qs";

export default {

  data() {
    return {

      dialogVisible: false,
      dataFlowCache:[],
      selectedRow:[]

    };
  },
  components: {},
  methods: {
    init(cache) {
      this.dialogVisible = true;
      this.dataFlowCache=cache;

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
        }

  }
};
</script>
