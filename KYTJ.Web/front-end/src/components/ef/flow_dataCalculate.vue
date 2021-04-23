
<style>

    .framePage {
            width: 100%;
            border: none;
            overflow: hidden;
        }

</style>
<template>
  <el-dialog :visible.sync="dialogVisible" width="70%" title="统计分析" top="5vh" :close-on-click-modal="false" >
        <el-tabs v-model="activeName"  style="height:500px;">
          <el-tab-pane label="统计方法" name="method">
              <el-row :gutter="20">
                    <el-col :span="12">
                       <div style="margin:10px 0px">
                        请选择分析方法:
                      </div>
                         <el-row>
                            <el-col :span="20">

                              <el-select v-model="calculateMethod" placeholder="请选择" filterable  style="width:300px">
                                      <el-option
                                        v-for="item in methodList"
                                        :key="item.code"
                                        :label="item.name"
                                        :value="item.code">
                                      </el-option>
                              </el-select>
                            </el-col>
                            <el-col :span="4">
                              <el-button @click="selectMethod" type="primary" style="float:right">下一步</el-button>
                            </el-col>
                          </el-row>

                    </el-col>
                    <el-col :span="12">



                    </el-col>
                </el-row>

          </el-tab-pane>
          <el-tab-pane label="统计参数" name="parameter">
            <component :is="currentComponent" @onCalculate="onCalculate" :dataFlowCache="dataFlowCache" :methodName="methodName" ></component>
          </el-tab-pane>
          <el-tab-pane label="统计结果" name="result">
            <div>
             <iframe class="framePage" id="resultPage" v-bind:src="caclulateResult.outputHTML"></iframe>
             <div style="display:none">
               <div id="imageResult" >
                  <el-image  v-for="item in caclulateResult.pictures" :key="item"
                    :src="item"
                    style="width:70%;margin-bottom:1rem;" ></el-image>
                </div>
                </div>
            </div>


          </el-tab-pane>
        </el-tabs>

  </el-dialog>
</template>

<script>
import qs from "qs";
import M1_1 from  './method/M1_1';
import M1_2 from  './method/M1_2';
import M2_1 from  './method/M2_1';
import M2_2 from  './method/M2_2';
import M2_3 from  './method/M2_3';
import M2_4 from  './method/M2_4';
import M3_1 from  './method/M3_1';
import M3_2 from  './method/M3_2';
import M4_1 from  './method/M4_1';
import M4_2 from  './method/M4_2';
import M4_3 from  './method/M4_3';
import M5_1 from  './method/M5_1';
import M5_2 from  './method/M5_2';
import M5_3 from  './method/M5_3';
import M5_4 from  './method/M5_4';
import M5_5 from  './method/M5_5';
import M5_6 from  './method/M5_6';
import M5_7 from  './method/M5_7';
import M5_8 from  './method/M5_8';
import M5_9 from  './method/M5_9';
import M5_10 from  './method/M5_10';
import M6_1 from  './method/M6_1';
import M6_2 from  './method/M6_2';
import M6_3 from  './method/M6_3';
import M7_1 from  './method/M7_1';
import M7_2 from  './method/M7_2';
import M7_3 from  './method/M7_3';
import M8_1 from  './method/M8_1';
import M8_2 from  './method/M8_2';
import M8_3 from  './method/M8_3';
import M8_4 from  './method/M8_4';
import M8_5 from  './method/M8_5';
import M8_6 from  './method/M8_6';
import M8_8 from  './method/M8_8';
import M8_9 from  './method/M8_9';
import M8_10 from  './method/M8_10';
import M8_11 from  './method/M8_11';
import M8_12 from  './method/M8_12';
import M9_1 from  './method/M9_1';
import M9_2 from  './method/M9_2';
import M9_3 from  './method/M9_3';
import M9_4 from  './method/M9_4';

export default {

  data() {
    return {
      dialogVisible: false,
      dataFlowCache:[],
      nodeId:"",
      activeName:"method",
      methodList:[],
      calculateMethod:"",
      caclulateResult:[],
      currentComponent:"",
      methodName:""
    };
  },
  components: {M1_1,M1_2,M2_1,M2_2,M2_3,M2_4,M3_1,M3_2,M4_1,M4_2,M4_3,M5_1,M5_2,M5_3,M5_4,M5_5,M5_6,M5_7,M5_8,M5_9,M5_10
  ,M6_1,M6_2,M6_3,M7_1,M7_2,M7_3,M8_1,M8_2,M8_3,M8_4,M8_5,M8_6,M8_8,M8_9,M8_10,M8_11,M8_12,M9_1,M9_2,M9_3,M9_4
  },
  methods: {
    test(){
       this.dialogVisible = true;
    },
    init(nodeId,cache) {
      this.activeName="method";
      this.calculateMethod="";
      this.caclulateResult=[];

      this.nodeId=nodeId;//nodeId;
      this.dialogVisible = true;
      this.dataFlowCache=cache;
      this.getCalculateMethodList();
    },
        getCalculateMethodList(){
          this.$axios
            .post("dataFlow/GetStatisticsMethod")
            .then((res) => {
              if (res.data && res.data.success) {
                this.methodList=res.data.data;

              } else {
                console.log(res.data.msg);
              }
            });

        },
        selectMethod(){
          if(this.calculateMethod===""){
             this.$message.warning("必须选择一种计算方法");
             return;
          }
          let obj = {};
          obj = this.methodList.find((item)=>{
            return item.code === this.calculateMethod;
          });
          if(this.calculateMethod=="8_10"){
              for(let i=0;i<this.dataFlowCache.dataColumns.length;i++){
                this.dataFlowCache.dataColumns[i].kindCount=0;
                this.dataFlowCache.dataColumns[i].nullPercent="";
              }
          }
          this.methodName=obj.name;
          this.currentComponent="M"+this.calculateMethod;
          this.activeName="parameter";
          //dynamicly add component

        },
        onCalculate(param){
          for(var i=0;i<this.methodList.length;i++){
            if(this.methodList[i].code===this.calculateMethod){
              param.methodCode=this.methodList[i].code;
              param.methodPath=this.methodList[i].path;
              break;
            }
          }
          param.node = this.nodeId;
          this.$axios
                .post("dataFlow/Calculate", qs.stringify(param))
                .then((res) => {
                  if (res.data && res.data.success) {
                    let domain= this.$axios.defaults.baseURL;
                    res.data.data.outputHTML=domain+ res.data.data.outputHTML;
                    for(var i=0;i<res.data.data.pictures.length;i++){
                      res.data.data.pictures[i]=domain+res.data.data.pictures[i];
                    }
                    this.caclulateResult=res.data.data;
                    this.activeName="result";
                    setTimeout(function () {
                           adjustIframeHeight();
                      }, 1000)

                  } else {
                    this.$message.error(res.data.msg);
                    console.log(res.data.msg);
                  }
                });

        },
  }
};

function  adjustIframeHeight() {
  let ifm= document.getElementById("resultPage");
  let img=document.getElementById("imageResult");
 // let imgCopy=img.cloneNode(true);
  //let body=ifm.contentWindow.document.getElementsByTagName("body")[0];
 // body.appendChild(imgCopy);
  ifm.height="400px";
}
</script>
