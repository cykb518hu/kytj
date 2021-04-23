// The Vue build version to load with the `import` command
// (runtime-only or standalone) has been set in webpack.base.conf with an alias.
import Vue from 'vue'
import App from './App'

Vue.config.productionTip = false

import ElementUI from 'element-ui'
import 'element-ui/lib/theme-chalk/index.css'
import '@/components/ef/index.css'
import axios from "axios"
axios.defaults.baseURL= config.apiUrl;// "http://localhost:13066/";
Vue.prototype.$axios=axios;

const echarts = require('echarts')
Vue.prototype.$echarts=echarts


Vue.use(ElementUI, {size: 'small'})

/* eslint-disable no-new */
new Vue({
    el: '#app',
    components: {App},
    template: '<App/>'
})
