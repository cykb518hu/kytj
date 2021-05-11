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

                          <el-form label-width="150px">
                               <el-form-item label="研究对象编号(用于输出Scores):" >
                                <el-select v-model="snId" placeholder="请选择..." >
                                      <el-option
                                        v-for="item in dataColumns"
                                        :key="item.id"
                                        :label="item.rem"
                                        :value="item.id">
                                      </el-option>
                                </el-select>
                              </el-form-item>
                              <el-form-item label="因子:" >
                                <el-select v-model="factor" placeholder="请选择.." >
                                    <el-option label="1" value="1"></el-option>
                                     <el-option label="2" value="2"></el-option>
                                     <el-option label="3" value="3"></el-option>
                                     <el-option label="4" value="4"></el-option>
                                     <el-option label="5" value="5"></el-option>
                                     <el-option label="6" value="6"></el-option>
                                     <el-option label="7" value="7"></el-option>
                                     <el-option label="8" value="8"></el-option>
                                     <el-option label="9" value="9"></el-option>
                                     <el-option label="10" value="10"></el-option>
                                </el-select>
                              </el-form-item>
                               <el-form-item label="Scores:" >
                                <el-select v-model="scores" placeholder="请选择.." >
                                    <el-option label="1: regression" value="1"></el-option>
                                     <el-option label="2: Bartlett" value="2"></el-option>
                                     <el-option label="3: none" value="3"></el-option>
                                </el-select>
                              </el-form-item>
                              <el-form-item label="Rotation:" >
                                <el-select v-model="rotation" placeholder="请选择.." >
                                    <el-option label="1: promax" value="1"></el-option>
                                     <el-option label="2: varimax" value="2"></el-option>
                                     <el-option label="3: none" value="3"></el-option>
                                </el-select>
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
            },
            tableHeight:{
              type:Number
            }
        },

  data() {
    return {
      dataColumns:[],
      multipleSelection:[],
      snId: "",
      factor:"",
      scores:"",
      rotation:""

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
             this.$message.warning("自变量不能为空");
             return;
          }
          if(this.multipleSelection.length>7){
             this.$message.warning("自变量不能多于7个");
             return;
          }
           if(this.snId===""){
             this.$message.warning("研究对象编号不能为空");
             return;
          }
           if(this.factor===""){
             this.$message.warning("因子不能为空");
             return;
          }
          if(this.scores===""){
             this.$message.warning("scores不能为空");
             return;
          }


          var param = {};
          let ids=[];
          this.multipleSelection.map((item)=> {
              ids.push(item.id)
          })
          param.selectedFields=ids;
          param.snId=this.snId;
          param.factor=this.factor;
          param.scores=this.scores;
          param.rotation=this.rotation;
          this.$emit('onCalculate', param)

        },
  }
};

</script>
