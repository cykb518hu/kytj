<template>
   <el-row :gutter="20">

                     <el-col :span="24">
                          <h4>{{methodName}}</h4>

                          <el-form label-width="150px">
                              <el-form-item label="结果变量(0=Censor):" >
                                <el-select v-model="resultField" placeholder="请选择..." >
                                      <el-option
                                        v-for="item in dataColumns"
                                        :key="item.id"
                                        :label="item.rem"
                                        :value="item.id">
                                      </el-option>
                                </el-select>
                              </el-form-item>
                              <el-form-item label="选择时间变量:" >
                                <el-select v-model="timeField" placeholder="请选择..." >
                                      <el-option
                                        v-for="item in dataColumns"
                                        :key="item.id"
                                        :label="item.rem"
                                        :value="item.id">
                                      </el-option>
                                </el-select>
                              </el-form-item>
                               <el-form-item label="分组变量:" >
                                <el-select v-model="groupingField" placeholder="请选择..." >
                                      <el-option
                                        v-for="item in dataColumns"
                                        :key="item.id"
                                        :label="item.rem"
                                        :value="item.id">
                                      </el-option>
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
      resultField:"",
      timeField:"",
      groupingField: ""
    };
  },
   created(){
       this.dataColumns=this.dataFlowCache.dataColumns;
  },
  methods: {
        Calculate(){
          if(this.resultField===""){
             this.$message.warning("结果变量不能为空");
             return;
          }
           if(this.timeField===""){
             this.$message.warning("时间变量不能为空");
             return;
          }
          if(this.groupingField===""){
             this.$message.warning("分组变量不能为空");
             return;
          }
          var param = {};
          param.resultField=this.resultField;
          param.timeField=this.timeField;
          param.groupingField=this.groupingField;
          this.$emit('onCalculate', param)

        },
  }
};

</script>
