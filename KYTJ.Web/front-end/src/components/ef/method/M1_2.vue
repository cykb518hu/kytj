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

                          <el-form label-width="120px">
                              <el-form-item label="选择列分层变量:" >
                                <el-select v-model="groupingField" placeholder="请选择分组变量" >
                                      <el-option
                                        v-for="item in dataColumns"
                                        :key="item.id"
                                        :label="item.rem"
                                        :value="item.id">
                                      </el-option>
                                </el-select>
                              </el-form-item>
                              <el-form-item label="选择行分层变量:" >
                                <el-select v-model="layeringField" placeholder="请选择分组变量" >
                                      <el-option
                                        v-for="item in dataColumns"
                                        :key="item.id"
                                        :label="item.rem"
                                        :value="item.id">
                                      </el-option>
                                </el-select>
                              </el-form-item>
                              <el-form-item label="输出内容与格式:" >
                                <el-select v-model="scField" >

                                     <el-option label="1.Mean+SD/N(%)" value="1"></el-option>
                                     <el-option label="2.Mean(SD)Median(Min-Max)/N(%)" value="2"></el-option>
                                     <el-option label="3.Mean(SD)Median(Q1-Q3)/N(%)" value="3"></el-option>
                                     <el-option label="4.Geometric Mean(95%CI)/N(%)" value="4"></el-option>
                                     <el-option label="5.(N)Mean+SD/N(%)" value="5"></el-option>
                                     <el-option label="6.(N)Mean(SD)Median(Min-Max)/N(%)" value="6"></el-option>
                                     <el-option label="7.(N)Mean(SD)Median(Q1-Q3)/N(%)" value="7"></el-option>
                                     <el-option label="8.(N)Geometric Mean(95%CI)/N(%)" value="8"></el-option>
                                     <el-option label="9.List N Mean SD Median Q1-Q3 Min-Max/N(%)" value="9"></el-option>
                                </el-select>
                              </el-form-item>
                              <el-form-item label="精确到小数点数:">
                                <el-select v-model="jqField" >
                                     <el-option label="0.1" value="1"></el-option>
                                     <el-option label="0.01" value="2"></el-option>
                                     <el-option label="0.001" value="3"></el-option>
                                     <el-option label="1" value="4"></el-option>
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
      multipleSelection:[],
      groupingField: "",
      layeringField:"",
      scField: "1",
      jqField:"1"
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
             if(this.groupingField===""){
             this.$message.warning("列分层变量不能为空");
             return;
          }
          var param = {};
          let ids=[];
          this.multipleSelection.map((item)=> {
              ids.push(item.id)
          })
          param.selectedFields=ids;
          param.groupingField=this.groupingField;
          param.layeringField=this.layeringField;
          param.scField=this.scField;
          param.jqField=this.jqField;
          this.$emit('onCalculate', param)

        },
  }
};

</script>
