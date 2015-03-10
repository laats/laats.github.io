function map(c,d){var a=[];for(var b=0;b<d.length;b++){a.push(c(d[b]))}return a}function sum(a){var c=0;for(var b=0;b<a.length;b++){c+=a[b]}return c}function range(d,c){var a=[];if(d==c){return a}if(d<c){for(var b=d;b<c;b++){a.push(b)}return a}for(var b=d;b>c;b--){a.push(b)}return a}function zip(c,b){var a=Math.min(c.length,b.length);var e=[];for(var d=0;d<a;d++){e.push([c[d],b[d]])}return e}function debug(a){document.getElementById("debug").innerHTML+="<br>"+a}searchArray=function(d,c){if(typeof(c)==="undefined"||!c.length){return -1}var b=c.length-1;var a=0;while(a<=b){mid=Math.floor((a+b)/2);element=c[mid];if(element>d){b=mid-1}else{if(element<d){a=mid+1}else{return mid}}}return mid};cumsum=function(c){var b=[];var d=0;for(var a=0;a<c.length;a++){d+=c[a];b.push(d)}return b};sample=function(a){var c=cumsum(a);var b=Math.random();return searchArray(b,c)};P=[];pchanged=false;oldeu=[];oldep=[];params={real:80,ap:1,an:1,bp:3,bn:1,minr:20,maxr:2000,eta:1,eps:1};presets=[{ap:1,bp:1,an:1,bn:1},{ap:1,bp:1,an:1,bn:3},{ap:1,bp:3,an:1,bn:1}];pnames=["ap","bp","an","bn","eps","minr","maxr","real"];genU=function(b,a){return function(c){return(c>=b?-a.bp*Math.pow((c-b),a.ap):-a.bn*Math.pow((b-c),a.an))}};genEM=function(b,a){return function(c){return Math.exp(a*b(c))}};genPM=function(g,f){var a=genU(g,f);var b=genEM(a,f.eta);var e=sum(map(b,range(f.minr,f.maxr)));var d=function(c){return b(c)/e};d.N=e;return d};function mean(c,d){var b=0;for(var a=0;a<c.length;a++){b+=(a+d)*c[a]}return b}function summary(f,g){var e=0;for(var d=0;d<f.length;d++){e+=(d+g)*f[d]}var b=e;e=0;var c=0;for(var d=0;d<f.length;d++){c=(d+g)-b;e+=c*c*f[d]}var a={variance:e,mean:b};return a}function competa(a){var b=Math.max(a.ap*a.bp,a.an*a.bn);if(a.an>1){b=Math.max(b,a.an*a.bn*Math.pow(a.real-a.minr,a.an-1))}if(a.ap>1){b=Math.max(b,a.ap*a.bp*Math.pow(a.maxr-a.real,a.ap-1))}return a.eps/(2*b)}function pullparams(){var b={};for(var a=0;a<pnames.length;a++){b[pnames[a]]=Number(document.getElementById(pnames[a]).innerHTML)}return b}function pushparams(b){for(var a=0;a<pnames.length;a++){document.getElementById(pnames[a]).innerHTML=b[pnames[a]]}}function clip(b){var a=b.indexOf(" ");if(a==-1){return b}return b.substr(0,a)}function pushvals(a){document.getElementById("eta").innerHTML=(Math.round(a.eta*1000)/1000+(pchanged?(" ("+clip(document.getElementById("eta").innerHTML)+")"):""));document.getElementById("var").innerHTML=(Math.round(a.variance*1000)/1000+(pchanged?(" ("+clip(document.getElementById("var").innerHTML)+")"):""));document.getElementById("ave").innerHTML=(Math.round(a.ave*1000)/1000+(pchanged?(" ("+clip(document.getElementById("ave").innerHTML)+")"):""));document.getElementById("spl").innerHTML=a.sample}function showparms(b){for(var a=0;a<pnames.length;a++){debug(pnames[a]+":"+b[pnames[a]])}}function update(){params=pullparams();init(params)}function init(b){document.getElementById("debug").innerHTML="";var f=Math.min(Math.max(b.real,b.minr),b.maxr);b.eta=competa(b);var m=genU(f,b);var i=genPM(f,b);var g=Math.max(f-10,b.minr);var h=Math.min(f+10,b.maxr);var a=range(g,h);var k=map(m,a);var j=map(i,a);var l=zip(a,k);var n=zip(a,j);P=map(i,range(b.minr,b.maxr));var e=summary(P,b.minr);b.variance=e.variance;b.ave=e.mean;b.sample=(sample(P)+b.minr)+" "+(sample(P)+b.minr)+" "+(sample(P)+b.minr)+" "+(sample(P)+b.minr)+" "+(sample(P)+b.minr);pushvals(b);doplots(l,n,oldeu,oldep);oldep=n;oldeu=l;pchanged=false}function doplots(d,a,c,f){var e=[{label:"U(r)",data:d,color:"red"}];var b=[{label:"P(r)",data:a,color:"blue"}];if(pchanged){e.push({label:"previous U(r)",data:c,color:"magenta"});b.push({label:"previous P(r)",data:f,color:"green"})}$.plot($("#uplot"),e,{series:{points:{show:true},lines:{show:true}},grid:{backgroundColor:{colors:["#fff","#eee"]}}});$.plot($("#pplot"),b,{series:{points:{show:true},lines:{show:true}},grid:{backgroundColor:{colors:["#fff","#eee"]}}})}function changeContent(a){pchanged=true;a.innerHTML='<INPUT type=text name=newname onBlur="javascript:submitNewName(this);" value="'+a.innerHTML+'">';a.firstChild.focus()}function submitNewName(a){a.parentNode.innerHTML=a.value}function setpreset(a){params=pullparams();params.ap=presets[a].ap;params.bp=presets[a].bp;params.an=presets[a].an;params.bn=presets[a].bn;pushparams(params);pchanged=true};