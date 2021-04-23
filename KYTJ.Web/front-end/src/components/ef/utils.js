// 是否具有该线
export function hasLine(data, from, to) {
    for (let i = 0; i < data.lineList.length; i++) {
        let line = data.lineList[i]
        if (line.from === from && line.to === to) {
            return true
        }
    }
    return false
}

// 是否含有相反的线
export function hashOppositeLine(data, from, to) {
    return hasLine(data, to, from)
}

// 获取连线
export function getConnector(jsp, from, to) {
    let connection = jsp.getConnections({
        source: from,
        target: to
    })[0]
    return connection
}

// 获取唯一标识
export function uuid() {
    return Math.random().toString(36).substr(3, 10)
}

export function  getDataFlowResult(currentNodeId,prevNodeId,caculateInfo=false){

  var param = {};
  param.node = currentNodeId;
  param.prevNode = prevNodeId;

  var oReq = new XMLHttpRequest();
  oReq.open("GET", "http://localhost:13066/dataFlow/GetDataFlowCache?node="+currentNodeId+"&prevNode="+prevNodeId+"&caculateInfo="+caculateInfo, false); // 同步请求
  oReq.setRequestHeader("Content-type", "application/json");
  oReq.send();

  var res = JSON.parse(oReq.responseText);
  return res;

}
