<template>
   <el-row :gutter="20">

                     <el-col :span="24">
                          <h4>{{methodName}}</h4>

                          <el-form label-width="120px">
                              <el-form-item label="列标签:" >
                                <el-select v-model="columnField" placeholder="请选择列标签" >
                                      <el-option
                                        v-for="item in dataColumns"
                                        :key="item.id"
                                        :label="item.rem"
                                        :value="item.id">
                                      </el-option>
                                </el-select>
                              </el-form-item>
                              <el-form-item label="行标签:" >
                                <el-select v-model="rowField" placeholder="请选择行标签" >
                                      <el-option
                                        v-for="item in dataColumns"
                                        :key="item.id"
                                        :label="item.rem"
                                        :value="item.id">
                                      </el-option>
                                </el-select>
                              </el-form-item>
                              <el-form-item label="检验方法:" >
                                <el-select v-model="jyField" >

                                     <el-option label="1.非配对资料的卡方检验" value="1"></el-option>
                                     <el-option label="2.配对资料的卡方检验" value="2"></el-option>
                                     <el-option label="3.Kappa一致性检验" value="3"></el-option>
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
      rowField: "",
      columnField:"",
      jyField: ""
    };
  },
   created(){
       this.dataColumns=this.dataFlowCache.dataColumns;
  },
  methods: {
        Calculate(){
          if(this.rowField===""){
             this.$message.warning("行标签不能为空");
             return;
          }
           if(this.columnField===""){
             this.$message.warning("列标签不能为空");
             return;
          }
          var param = {};

          param.rowField=this.rowField;
          param.columnField=this.columnField;
          param.jyField=this.jyField;
          this.$emit('onCalculate', param)

        },
  }
};

</script>
