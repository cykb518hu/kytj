<template>
   <el-row :gutter="20">

                     <el-col :span="24">
                          <h4>{{methodName}}</h4>

                          <el-form label-width="120px">
                              <el-form-item label="分析变量:" >
                                <el-select v-model="selectedFields" placeholder="请选择分析变量" >
                                      <el-option
                                        v-for="item in dataColumns"
                                        :key="item.id"
                                        :label="item.rem"
                                        :value="item.id">
                                      </el-option>
                                </el-select>
                              </el-form-item>
                              <el-form-item label="与变量:" >
                                <el-select v-model="andFields" placeholder="请选择与变量" >
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
            }
        },

  data() {
    return {
      dataColumns:[],
      selectedFields:"",
      andFields: ""
    };
  },
   created(){
       this.dataColumns=this.dataFlowCache.dataColumns;
  },
  methods: {
        Calculate(){
          if(this.selectedFields===""){
             this.$message.warning("分析变量不能为空");
             return;
          }
           if(this.andFields===""){
             this.$message.warning("与变量不能为空");
             return;
          }
          var param = {};

          param.selectedFields=[];
          param.selectedFields.push(this.selectedFields)
          param.andFields=[];
          param.andFields.push(this.andFields)
          this.$emit('onCalculate', param)

        },
  }
};

</script>
