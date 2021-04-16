<template>
   <el-row :gutter="20">
                    <el-col :span="16">
                            <el-table
                                  ref="multipleTable"
                            :data="dataColumns"
                            stripe
                            max-height="400"
                            style="width: 100%"
                            @selection-change="handleSelectionChange" >
                              <el-table-column
                            type="selection"
                            min-width="7%">
                            </el-table-column>
                            <el-table-column
                              prop="name"
                              label="自变量"
                            min-width="25%">
                            </el-table-column>

                            <el-table-column
                              prop="rem"
                              label="注解"
                              min-width="25%"
                              ></el-table-column>

                                <el-table-column
                              prop="isContinuous"
                              :formatter="isContinuousFormatter"
                              label="类型"
                              min-width="15%">
                            </el-table-column>

                             <el-table-column
                              label="潜变量名"
                              min-width="28%">
                              <template  slot-scope="scope">
                                 <el-input v-if="scope.row.kindCount==1"  v-model="scope.row.nullPercent" ></el-input>
                              </template>

                            </el-table-column>
                          </el-table>


                    </el-col>
                    <el-col :span="8">
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
            }
        },
  data() {
    return {
      dataColumns:[],
      multipleSelection:[],
      latentNames:[],
    };
  },
  created(){
      this.multipleSelection=[];
      this.dataColumns=this.dataFlowCache.dataColumns;
  },

  methods: {
       handleSelectionChange(val) {
         for(let i=0;i<this.dataColumns.length;i++){
           this.dataColumns[i].kindCount=0;
           for(let j=0;j<val.length;j++){
             if(this.dataColumns[i].id==val[j].id){
               this.dataColumns[i].kindCount=1;
             }
           }
         }
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
             if(this.multipleSelection.length>12){
             this.$message.warning("自变量不能超过12个");
             return;
          }
          var param = {};
          let ids=[];
          this.multipleSelection.map((item)=> {
              if(item.nullPercent===""){
                 this.$message.warning("潜变量不能为空");
                 return;
              }
              this.latentNames.push(item.nullPercent);
              ids.push(item.id)
          })
          param.selectedFields=ids;
          param.latentNames=this.latentNames;
          this.$emit('onCalculate', param)

        },
  }
};

</script>
