
<style>


  .el-dialog__body{
    padding: 10px 20px !important;
  }

</style>
<template>
  <el-dialog :visible.sync="dialogVisible" width="60%" title="行处理" top="5vh"  >
    <div style="height:400px">
      <div>
        <el-row>
          <el-col :span="16">
             总记录条数:{{dataCount}}
          </el-col>

          <el-col :span="8">
            <el-button plain @click="onAddFilter" type="primary" >添加筛选条件</el-button>
            <el-button @click="onExecute" type="primary" style="float:right">执行操作</el-button>
          </el-col>
        </el-row>
      </div>
      <div style="margin-top:20px">

            <el-table
            ref="multipleTable"
      :data="filterColumns"
      stripe
      max-height="400"
      style="width: 100%">
      <el-table-column
        label="列名"
       min-width="35%">

         <template  slot-scope="scope">
           <el-select v-model="scope.row.column" placeholder="请选择..." width="100%" >
                                      <el-option
                                        v-for="item in dataColumns"
                                        :key="item.name"
                                        :label="item.name"
                                        :value="item.name">
                                      </el-option>
                                </el-select>
          </template>

      </el-table-column>
      <el-table-column
        label="筛选条件"
        min-width="20%">

           <template  slot-scope="scope">
              <el-select v-model="scope.row.operation" placeholder="请选择..." >
                      <el-option label="等于(=)" value="="></el-option>
                      <el-option label="大于(&gt;)" value="&gt;"></el-option>
                      <el-option label="大于等于(&gt;=)" value="&gt;="></el-option>
                      <el-option label="小于(&lt;)" value="&lt;"></el-option>
                      <el-option label="小于等于(&lt;=)" value="&lt;="></el-option>
                      <el-option label="不等于(!=)" value="!="></el-option>
                      <el-option label="包含" value="like"></el-option>
                      <el-option label="不包含" value="nl"></el-option>
             </el-select>
          </template>
      </el-table-column>
      <el-table-column
        label="预期"
        min-width="30%">

          <template  slot-scope="scope">
                                 <el-input  v-model="scope.row.value" ></el-input>
                              </template>
        </el-table-column>
        <el-table-column
          label="操作"
          min-width="15%">

          <template  slot-scope="scope">
                               <el-link type="primary" @click="onDeleteFilter(scope.row.id)">删除条件</el-link>
                              </template>
        </el-table-column>
    </el-table>
      </div>


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


        onAddFilter(){
          let id=this.filterColumns.length+1;
          this.filterColumns.push({"column":"","operation":"","value":"","id":id});
        },
         onDeleteFilter(id){
          for(let i=0;i<this.filterColumns.length;i++){
            if(this.filterColumns[i].id===id){
              this.filterColumns.splice(i,1);
            }
          }
        },

        onExecute(){
          for(let i=0;i<this.filterColumns.length;i++){
            if(this.filterColumns[i].column===""){
              this.$message.warning("筛选列名不能为空");
            };
            if(this.filterColumns[i].condition===""){
              this.$message.warning("筛选条件不能为空");
            };
            if(this.filterColumns[i].value===""){
              this.$message.warning("筛选值不能为空");
            }
          }
          var param={};
          param.node = this.nodeId;
          param.filterColumns=this.filterColumns;
          this.$axios
                .post("dataFlow/DataRowFilterByColumns", qs.stringify(param))
                .then((res) => {
                  if (res.data && res.data.success) {
                    if(res.data.data>0){
                       this.$message.success("筛选成功，剩余记录:"+res.data.data);
                       this.dataCount=res.data.data;
                    }
                    else{
                      this.$message.warning("筛选记录为0，请检查筛选格式或者数据是否正确");
                    }

                  } else {
                    this.$message.error(res.data.msg);
                    console.log(res.data.msg);
                  }
                });

        }

  }
};
</script>
