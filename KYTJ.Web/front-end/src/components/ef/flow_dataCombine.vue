
<style>


  .el-dialog__body{
    padding: 0px 20px !important;
  }

</style>
<template>
  <el-dialog :visible.sync="dialogVisible" width="70%" title="数据整合" top="5vh">
        <el-tabs v-model="activeName"  style="height:500px;">
          <el-tab-pane label="追加" name="append">
             <div>
                <el-row>
                  <el-col :span="16">
                    包含字段来源:
                    <el-radio-group v-model="fieldSource">
                                  <el-radio label="mainData">仅主数据集</el-radio>
                                  <el-radio label="alldata">所有数据集</el-radio>
                                </el-radio-group>
                  </el-col>

                  <el-col :span="8">
                     <el-button @click="appendClick" type="primary" >执行</el-button>
                  </el-col>
                </el-row>
              </div>
                            <div style="margin-top:20px">

                                 <el-table
                                  ref="multipleTable"
                            :data="dataSource"
                            stripe
                            max-height="350"
                            style="width: 100%">
                            <el-table-column
                              prop="outColumn"
                              label="输出字段"
                            min-width="34%">
                            </el-table-column>

                            <el-table-column
                              prop="mainColumn"
                              label="表1字段"
                              min-width="33%"
                              ></el-table-column>

                                <el-table-column
                              prop="secondColumn"
                              label="表2字段"
                              min-width="33%">
                            </el-table-column>
                          </el-table>

                            </div>

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
      prevNodeIds:[],
      dataSource:[],
      fieldSource:"mainData",
      activeName:"append"

    };
  },
  components: {},
  methods: {

    init(nodeId,prevNodeIds) {
      this.activeName="append";
      this.nodeId=nodeId;//nodeId;
      this.dialogVisible = true;
      //this.prevNodeIds=prevNodeIds;
      this.loadAllData();

    },
    loadAllData(){
      this.prevNodeIds.push("test");
      this.prevNodeIds.push("test");
      var param={};
          param.prevNodeIds=this.prevNodeIds;
          this.$axios
                .post("dataFlow/GetDataCombineSource", qs.stringify(param))
                .then((res) => {
                  if (res.data && res.data.success) {
                    this.dataSource=res.data.data;
                  } else {
                    this.$message.error(res.data.msg);
                    console.log(res.data.msg);
                  }
                });
    },
    appendClick(){

          var param={};
          param.node=this.nodeId;
          param.prevNodeIds=this.prevNodeIds;
          param.fieldSource=this.fieldSource;
          this.$axios
                .post("dataFlow/DataCombineAppend", qs.stringify(param))
                .then((res) => {
                  if (res.data && res.data.success) {
                    this.$message.success(res.data.msg);
                  } else {
                    this.$message.error(res.data.msg);
                    console.log(res.data.msg);
                  }
                });
    }

  }
};

</script>
