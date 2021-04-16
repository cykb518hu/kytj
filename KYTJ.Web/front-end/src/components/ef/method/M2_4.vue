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

                          <el-form label-width="180px">
                              <el-form-item label="请选择分组变量:" >
                                <el-select v-model="groupingField" placeholder="请选择分组变量" >
                                      <el-option
                                        v-for="item in dataColumns"
                                        :key="item.id"
                                        :label="item.rem"
                                        :value="item.id">
                                      </el-option>
                                </el-select>
                              </el-form-item>
                              <el-form-item label="选择检验方法(Location):" >
                                <el-select v-model="jyfield" >

                                     <el-option label="1.中位数" value="1"></el-option>
                                     <el-option label="2.均数" value="2"></el-option>
                                     <el-option label="3.修剪后的均数（alpha=0.25）" value="3"></el-option>
                                </el-select>
                              </el-form-item>
                              <el-form-item label="选择校正方法:">
                                <el-select v-model="jzfield" >
                                     <el-option label="1.none（无校正）" value="1"></el-option>
                                     <el-option label="2.zero removal（零删除）" value="2"></el-option>
                                     <el-option label="3.zero correction（零校正）" value="3"></el-option>
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
      jyfield: "1",
      jzfield:"1"
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
          var param = {};
          let ids=[];
          this.multipleSelection.map((item)=> {
              ids.push(item.id)
          })
          param.selectedFields=ids;
          param.groupingField=this.groupingField;
          param.jyfield=this.jyfield;
          param.jzfield=this.jzfield;
          this.$emit('onCalculate', param)

        },
  }
};

</script>
