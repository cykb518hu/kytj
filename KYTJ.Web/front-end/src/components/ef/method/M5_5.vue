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

                          <el-form label-width="150px">
                              <el-form-item label="结果变量(1/0两分类):" >
                                <el-select v-model="resultField" placeholder="请选择..." >
                                      <el-option
                                        v-for="item in dataColumns"
                                        :key="item.id"
                                        :label="item.rem"
                                        :value="item.id">
                                      </el-option>
                                </el-select>
                              </el-form-item>
                               <el-form-item label="配对组号变量:" >
                                <el-select v-model="snField" placeholder="请选择..." >
                                      <el-option
                                        v-for="item in dataColumns"
                                        :key="item.id"
                                        :label="item.rem"
                                        :value="item.id">
                                      </el-option>
                                </el-select>
                              </el-form-item>
                                <el-form-item label="分析方法:" >
                                <el-select v-model="jyField" placeholder="请选择..." >
                                    <el-option label="1.approximate" value="1"></el-option>
                                     <el-option label="2.exact" value="2"></el-option>
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
      dictionaryDTOs:[],
      resultField: "",
      jyField:"",
      snField:""
    };
  },
  created(){
       this.multipleSelection=[];
       this.dataColumns=this.dataFlowCache.dataColumns;
       this.getDictionaryDTOs();
  },
  methods: {

       getDictionaryDTOs(){
          this.$axios
            .get("dataFlow/GetDictionaryDTOs?type=distro.type")
            .then((res) => {
              if (res.data && res.data.success) {
                this.dictionaryDTOs=res.data.data;
                console.log(this.dictionaryDTOs);

              } else {
                console.log(res.data.msg);
              }
            });

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
        relativeTypeChange(){
           let obj = {};
           obj = this.dictionaryDTOs.find((item)=>{
            return item.code === this.relativeType;
          });
          this.relativeLinkFuncs=obj.linkedCode;

        },
        Calculate(){
            if(this.multipleSelection.length==0){
             this.$message.warning("自变量不能为空");
             return;
          }
          if(this.multipleSelection.length>5){
             this.$message.warning("自变量不能多于5个");
             return;
          }
           if(this.snField===""){
             this.$message.warning("配对组号变量不能为空");
             return;
          }
           if(this.resultField===""){
             this.$message.warning("结果变量不能为空");
             return;
          }
           if(this.jyField===""){
             this.$message.warning("分析方法不能为空");
             return;
          }
          var param = {};
          let ids=[];
          this.multipleSelection.map((item)=> {
              ids.push(item.id)
          })
          param.selectedFields=ids;
          param.resultField=this.resultField;
          param.snField=this.snField;
          param.jyField=this.jyField;
          this.$emit('onCalculate', param)

        },
  }
};

</script>
