<template>
   <el-row :gutter="20">
                    <el-col :span="12">
                            <el-table
                                  ref="multipleTable"
                            :data="dataColumns"
                            stripe
                            :max-height="tableHeight"
                            style="width: 100%"
                            @selection-change="handleSelectionChange">
                              <el-table-column
                            type="selection"
                            min-width="7%">
                            </el-table-column>
                            <el-table-column
                              prop="name"
                              label="自变量"
                            min-width="40%">
                            </el-table-column>

                            <el-table-column
                              prop="rem"
                              label="注解"
                              min-width="38%"
                              ></el-table-column>

                                <el-table-column
                              prop="isContinuous"
                              :formatter="isContinuousFormatter"
                              label="类型"
                              min-width="15%">
                            </el-table-column>
                          </el-table>


                    </el-col>
                    <el-col :span="12">
                      <h4>{{methodName}}</h4>
                      <el-button @click="Calculate" type="primary" >开始分析</el-button>
                    </el-col>
                </el-row>
</template>

<script>

export default {
  props: {
            dataFlowCache: Object,
            methodName: {
              type:String
            },
            tableHeight:{
              type:Number
            }
        },
  data() {
    return {
      dataColumns:[],
      multipleSelection:[],
    };
  },
  created(){
      this.multipleSelection=[];
       this.dataColumns=this.dataFlowCache.dataColumns;
  },

  methods: {
       handleSelectionChange(val) {
             this.multipleSelection = val;
        },
        isContinuousFormatter(row, column) {
                if (row.isContinuous) {
                    return '连续';
                } else {
                    return '分类';
                }
        },
        Calculate(){
            if(this.multipleSelection.length==0){
             this.$message.warning("至少选择一个自变量");
             return;
          }
             if(this.multipleSelection.length>6){
             this.$message.warning("自变量不能超过6个");
             return;
          }
          var param = {};
          let ids=[];
          this.multipleSelection.map((item)=> {
              ids.push(item.id)
          })
          param.selectedFields=ids;
          this.$emit('onCalculate', param)

        },
  }
};

</script>
