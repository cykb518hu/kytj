<template>
   <el-row :gutter="20">
                    <el-col :span="12">
                            <el-table
                                  ref="multipleTable"
                            :data="dataColumns"
                            stripe
                            max-height="400"
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

                          <el-form label-width="80px">
                              <el-form-item label="总体均数:" >
                                <el-input v-model="average" type="number" style="width:200px"></el-input>
                              </el-form-item>
                              <el-form-item>
                                <el-button @click="Calculate" type="primary" >开始分析</el-button>
                              </el-form-item>
                            </el-form>
                    </el-col>

                </el-row>
</template>

<script>

export default {
 props: {
            dataFlowCache: Object,
            methodName: {
              type:String
            }
        },
  data() {
    return {
      dataColumns:[],
      multipleSelection:[],
      average:0
    };
  },
   created(){
      this.multipleSelection=[];
       this.dataColumns=this.dataFlowCache.dataColumns;
  },
  methods: {
    init(cache,methodName) {
      this.multipleSelection=[];
      this.dataFlowCache=cache;
      this.dataColumns=cache.dataColumns;
      this.methodName=methodName;
    },
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
             this.$message.warning("自变量不能为空");
             return;
          }
            if(this.multipleSelection.length>1){
             this.$message.warning("自变量数量不能多于1个");
             return;
          }
          var param = {};
          let ids=[];
          this.multipleSelection.map((item)=> {
              ids.push(item.id)
          })
          param.selectedFields=ids;
          param.average=this.average;
          this.$emit('onCalculate', param)

        },
  }
};

</script>
