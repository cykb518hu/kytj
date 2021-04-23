
<style>


  .el-dialog__body{
    padding: 10px 20px !important;
  }

</style>
<template>
  <el-dialog :visible.sync="dialogVisible" width="80%" title="过滤" top="5vh">
    <div>
      <div>
        <el-row>
          <el-col :span="8">
            条件:
             <el-select v-model="filterType" placeholder="请选择条件" style="width:280px">
                  <el-option label="排除具有过多缺失值的字段" value="defect"></el-option>
                  <el-option label="排除具有过多唯一类别的名义字段" value="unique"></el-option>
                  <el-option label="排除单个类别中具有过多值的分类字段" value="single"></el-option>
             </el-select>
          </el-col>
          <el-col :span="4">
              预期: <el-input v-model="filterPercent"
                          placeholder="20" style="width:80px"></el-input>&nbsp;%
          </el-col>
          <el-col :span="4">
            <el-button plain @click="onSearch" type="primary">筛选</el-button>
            <el-button plain @click="onReset" type="primary">重置</el-button>
            </el-col>
          <el-col :span="8">
            <el-button @click="onDelete" type="primary" style="float:right">删除选中字段</el-button>
          </el-col>
        </el-row>
      </div>
      <div style="margin-top:20px">

<div style="float:right"> &nbsp;&nbsp;总记录:{{dataColumns.length}}/选中数据:{{multipleSelection.length}} </div>
            <el-table
            ref="multipleTable"
      :data="dataColumns"
      stripe
      max-height="400"
      style="width: 100%"
      @selection-change="handleSelectionChange">
         <el-table-column
      type="selection"
      min-width="5%">
      </el-table-column>
      <el-table-column
        prop="name"
        label="字段名"
       min-width="45%">
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
        ></el-table-column>
    </el-table>
      </div>


    </div>

  </el-dialog>
</template>

<script>
import qs from "qs";
 import { getDataFlowResult } from './utils'

export default {

  data() {
    return {

      dialogVisible: false,
      dataFlowCache:[],
      dataColumns:[],
      nodeId:"",
      preNodeId:"",
      filterType:"",
      filterPercent:"",
      multipleSelection:[]

    };
  },
  components: {},
  methods: {
    init(nodeId,cache) {
      this.nodeId=nodeId;//nodeId;
      this.dialogVisible = true;
      this.dataFlowCache=cache;
      this.dataColumns=cache.dataColumns;
    },
        isContinuousFormatter(row, column) {
                if (row.isContinuous) {
                    return '连续';
                } else {
                    return '分类';
                }
            },
        onSearch(){
            var param = {};
            param.node = this.nodeId;
            param.filterType=this.filterType;
            param.filterPercent=this.filterPercent;
            this.$axios
                .post("dataFlow/GetDataFilterColumns", qs.stringify(param))
                .then((res) => {
                  if (res.data && res.data.success) {
                     this.dataColumns=res.data.data;
                  } else {
                    this.$message.error(res.data.msg);
                    console.log(res.data.msg);
                  }
                });

        },
        onReset(){
           this.filterType="";
           this.filterPercent="";
           this.dataColumns=this.dataFlowCache.dataColumns;

        },
         handleSelectionChange(val) {
             this.multipleSelection = val;
        },
        onDelete(){
          let ids=[];
          this.multipleSelection.map((item)=> {
              ids.push(item.id)
          })
          var param = {};
          param.node = this.nodeId;
          param.ids=ids;
          this.$axios
                .post("dataFlow/DeleteDataFilterColumns", qs.stringify(param))
                .then((res) => {
                  if (res.data && res.data.success) {
                    this.$message.success(res.data.msg);
                    let cache=getDataFlowResult(this.nodeId,"").data;
                    this.dataFlowCache=cache;
                   // this.dataColumns=cache.dataColumns;
                    this.onSearch();

                  } else {
                    this.$message.error(res.data.msg);
                    console.log(res.data.msg);
                  }
                });

        }

  }
};
</script>
