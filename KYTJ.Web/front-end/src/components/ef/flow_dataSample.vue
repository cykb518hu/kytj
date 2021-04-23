
<style>


  .el-dialog__body{
    padding: 0px 20px !important;
  }

</style>
<template>
  <el-dialog :visible.sync="dialogVisible" width="70%" title="样本抽样" top="5vh">
        <el-tabs v-model="activeName"  style="height:500px;">
          <el-tab-pane label="简单" name="simple">

                    <el-form label-width="120px" style="width:400px">
                              <el-form-item label="抽样方式:" >
                                <el-radio-group v-model="simpleObj.method">
                                  <el-radio label="order">顺序</el-radio>
                                  <el-radio label="extract">N中取1</el-radio>
                                  <el-radio label="pecent">百分比</el-radio>
                                </el-radio-group>
                              </el-form-item>
                              <el-form-item label="抽样个数:" >
                                   <el-input   v-model="simpleObj.simVal" type="number" ></el-input>
                                   <span v-if="simpleObj.method=='pecent'">百分比抽样请输入百分比,例如25.5</span>
                              </el-form-item>
                              <el-form-item label="抽样最大个数:" >
                                   <el-input   v-model="simpleObj.simMax" type="number" ></el-input>
                              </el-form-item>

                              <el-form-item>
                                <el-button @click="simpleClick" type="primary" >开始抽样</el-button>
                              </el-form-item>
                            </el-form>

          </el-tab-pane>
          <!--
          <el-tab-pane label="复杂" name="complex" style="display:none">
             <el-form label-width="120px" style="width:400px">
                              <el-form-item label="抽样类别:" >
                                <el-radio-group v-model="complexObj.category" @change="changeComplexMethod">
                                  <el-radio label="fenceng">分层字段</el-radio>
                                  <el-radio label="julei">聚类字段</el-radio>
                                </el-radio-group>
                              </el-form-item>

                              <el-form-item label="分层字段:" v-if="complexObj.method=='fenceng'">
                                <el-select v-model="complexObj.filedId" placeholder="请选择..."  >
                                      <el-option
                                        v-for="item in fcdataColumns"
                                        :key="item.id"
                                        :label="item.rem"
                                        :value="item.id">
                                      </el-option>
                                </el-select>
                              </el-form-item>
                              <el-form-item label="聚类字段:" v-if="complexObj.method=='julei'">
                                <el-select v-model="complexObj.filedId" placeholder="请选择..." >
                                      <el-option
                                        v-for="item in dataColumns"
                                        :key="item.id"
                                        :label="item.rem"
                                        :value="item.id">
                                      </el-option>
                                </el-select>
                              </el-form-item>
                              <el-form-item label="抽样形式:" >
                                <el-radio-group v-model="complexObj.approach">
                                  <el-radio label="pecent">百分比</el-radio>
                                  <el-radio label="cout">计数</el-radio>
                                </el-radio-group>
                              </el-form-item>
                              <el-form-item label="样本大小:" >
                                   <el-input   v-model="complexObj.sampleSize" type="number" ></el-input>
                                   <span v-if="complexObj.method=='pecent'">百分比抽样请输入百分比,例如25.5</span>
                              </el-form-item>

                              <el-form-item>
                                <el-button @click="simpleClick" type="primary" >开始抽样</el-button>
                              </el-form-item>
                            </el-form>

          </el-tab-pane>
          -->

        </el-tabs>

  </el-dialog>
</template>

<script>
import qs from "qs";


export default {

  data() {
    return {
      dialogVisible: false,
      dataFlowCache:[],
      dataColumns:[],
      fcdataColumns:[],
      nodeId:"",
      activeName:"simple",
      simpleObj:{
            method: '',
            simVal:"",
            simMax:""
        },
      complexObj:{
        category:"",
        filedId:"",
        approach:"",
        sampleSize:""

      }

    };
  },
  components: {},
  methods: {

    init(nodeId,cache) {
      this.activeName="simple";
      this.nodeId=nodeId;//nodeId;
      this.dialogVisible = true;
      this.dataFlowCache=cache;
      this.dataColumns=cache.dataColumns;
      this.dataColumns.map((item)=> {
              if(!item.isContinuous){
                  this.fcdataColumns.push(item);
              }
          })

    },
    simpleClick(){
      var param={};
          param.node = this.nodeId;
          param.simObj=this.simpleObj;
          this.$axios
                .post("dataFlow/DataSampleExtractSimple", qs.stringify(param))
                .then((res) => {
                  if (res.data && res.data.success) {
                    this.$message.success(res.data.msg);

                  } else {
                    this.$message.error(res.data.msg);
                    console.log(res.data.msg);
                  }
                });
    },
    changeComplexMethod(){
      this.complexObj.filedId="";
    }

  }
};

</script>
