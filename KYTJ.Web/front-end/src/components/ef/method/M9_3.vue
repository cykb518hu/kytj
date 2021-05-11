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

                          <el-form label-width="120px">
                              <el-form-item label="应变量:" >
                                <el-select v-model="relativeField" placeholder="请选择应变量" >
                                      <el-option
                                        v-for="item in dataColumns"
                                        :key="item.id"
                                        :label="item.rem"
                                        :value="item.id">
                                      </el-option>
                                </el-select>
                              </el-form-item>
                              <el-form-item label="应变量分布类型:" >

                                      <el-select v-model="relativeType" placeholder="请选择分布类型" @change="relativeTypeChange" >
                                            <el-option
                                              v-for="item in dictionaryDTOs"
                                              :key="item.code"
                                              :label="item.name"
                                              :value="item.code">
                                            </el-option>
                                      </el-select>

                              </el-form-item>
                              <el-form-item label="联系函数:" >
                                {{relativeLinkFuncs}}
                              </el-form-item>
                              <el-form-item label="基因型/等位基因变量类型:" >
                                 <el-select v-model="fieldType" placeholder="请选择..." >
                                    <el-option label="1:基因型(每个变量代表一个基因)" value="1"></el-option>
                                     <el-option label="2:等位基因(按顺序2个变量代表一个基因)" value="2"></el-option>
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
      dictionaryDTOs:[],
      relativeField: "",
      relativeType:"",
      relativeLinkFuncs:"",
      fieldType:""
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
          if(this.multipleSelection.length>7){
             this.$message.warning("自变量不能多于7个");
             return;
          }
           if(this.relativeField===""){
             this.$message.warning("应变量不能为空");
             return;
          }
           if(this.relativeType===""){
             this.$message.warning("分布类型不能为空");
             return;
          }
          var param = {};
          let ids=[];
          this.multipleSelection.map((item)=> {
              ids.push(item.id)
          })
          param.selectedFields=ids;
          param.relativeFields=[];
          param.relativeTypes=[];
          param.relativeLinkFuncs=[];
          param.relativeFields.push(this.relativeField);
          param.relativeTypes.push(this.relativeType);
          param.relativeLinkFuncs.push(this.relativeLinkFuncs);
          param.fieldType=this.fieldType;
          this.$emit('onCalculate', param)

        },
  }
};

</script>
