<template>
  <el-dialog :visible.sync="dialogVisible" width="50%" title="选择数据" >
    <el-form :model="dataForm" label-width="120px">
      <el-form-item label="选择数据集">
        <el-select
          v-model="dataForm.dataSetId"
          placeholder="请选择"
          @change="dataSetTrigger"
          style="width: 300px"
        >
          <el-option
            v-for="item in dataForm.dataSetList"
            :key="item.dataSetId"
            :label="item.dataSetName"
            :value="item.dataSetId"
          >
          </el-option>
        </el-select>
      </el-form-item>
      <el-form-item label="选择数据">
        <el-select
          v-model="dataForm.resultDataId"
          placeholder="请选择"
          style="width: 300px"
        >
          <el-option
            v-for="item in dataForm.resultDataList"
            :key="item.id"
            :label="item.name"
            :value="item.id"
          >
          </el-option>
        </el-select>
        <el-button
          type="primary"
          plain
          v-if="dataForm.resultDataId != ''"
          @click="copyDataResult"
          >复制数据</el-button
        >
        <el-button
          type="primary"
          plain
          v-if="dataForm.resultDataId != ''"
          @click="reNameDataForm"
          >重命名</el-button
        >
      </el-form-item>
      <el-form-item>
        <el-button @click="dialogVisible = false">取 消</el-button>
        <el-button type="primary" @click="setDataSource"
          >确 定</el-button
        >
      </el-form-item>
    </el-form>
  </el-dialog>
</template>

<script>
import qs from "qs";

export default {
  data() {
    return {
      datasetAndSub: [],
      dataForm: {
        dataSetId: "",
        dataSetList: [],
        resultDataId: "",
        resultDataList: [],
      },
      nodeId: "",
      dialogVisible: false,
      projectId: 0,
    };
  },
  components: {},
  methods: {
    init(id) {
      this.nodeId = id;
      this.dialogVisible = true;
      this.projectId = 4102;
      this.dataForm.dataSetList = [];
      this.dataForm.dataSetId = "";
      this.refreshSelectForm();
    },
    dataSetTrigger() {
      this.dataForm.resultDataList = [];
      this.dataForm.resultDataId = "";
      for (var i = 0; i < this.dataForm.dataSetList.length; i++) {
        if (this.dataForm.dataSetList[i].dataSetId == this.dataForm.dataSetId) {
          this.dataForm.resultDataList = this.dataForm.dataSetList[
            i
          ].resultDataList;
        }
      }
    },
    copyDataResult() {
      var data = {};
      data.resultDataId = this.dataForm.resultDataId;
      this.$axios
        .post("dataManage/CopyResultData", qs.stringify(data))
        .then((res) => {
          if (res.data && res.data.success) {
            this.$message.success(res.data.msg);
            this.refreshSelectForm();
          } else {
            this.$message.error(res.data.msg);
            console.log(res.data.msg);
          }
        });
    },
    refreshSelectForm() {
      this.$axios
        .get("dataset/GetDataSetAndSub?projectid=" + this.projectId)
        .then((res) => {
          if (res.data && res.data.success) {
            this.dataForm.dataSetList = res.data.data;
            this.datasetAndSub = res.data.data;
            this.dataSetTrigger();
          } else {
            console.log(res.data.msg);
          }
        });
    },
    reNameDataForm() {
      this.$prompt("请输入名称", "提示", {
        confirmButtonText: "确定",
        cancelButtonText: "取消",
        inputPattern: /\S/,
        inputErrorMessage: "输入不能為空",
      })
        .then(({ value }) => {
          var data = {};
          data.resultDataId = this.dataForm.resultDataId;
          data.name = value;
          this.$axios
            .post("dataManage/UpdateResultDataName", qs.stringify(data))
            .then((res) => {
              if (res.data && res.data.success) {
                this.$message.success(res.data.msg);
                this.refreshSelectForm();
              } else {
                this.$message.error(res.data.msg);
                console.log(res.data.msg);
              }
            });
        })
        .catch(() => {});
    },
    setDataSource(){
        var param = {};
        param.resultDataId = this.dataForm.resultDataId;
        param.node = this.nodeId;
        this.$axios
            .post("dataFlow/SetDataFlowCache", qs.stringify(param))
            .then((res) => {
              if (res.data && res.data.success) {
                this.$message.success(res.data.msg);
                this.dialogVisible=false;
              } else {
                this.$message.error(res.data.msg);
                console.log(res.data.msg);
              }
            });
    }
  },
};
</script>
