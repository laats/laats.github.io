

function map(func, array) {
    var result = [];
    for(var i = 0; i < array.length; i++)
	result.push(func(array[i]));
    return result;
};


function sum(numbers) {
    var acc = 0;
    for(var i = 0; i < numbers.length; i++)
	acc += numbers[i];
    return acc;
};

function range(start, stop) {
    var result = [];
    if (start == stop) {
        return result;
    }
    if (start < stop) {
        for(var i = start; i < stop; i++)
            result.push(i);
        return result;
    }
    for(var i = start; i > stop; i--)
        result.push(i);
    return result;
};

function zip(l1, l2) {
    var len = Math.min(l1.length, l2.length);
    var res = [];
    for(var i = 0; i < len; i++)
        res.push([l1[i], l2[i]]);
    return res;
}

function debug (thing) {
    document.getElementById("debug").innerHTML += "<br>" + thing;
};


searchArray = function(needle, haystack) {
    if (typeof(haystack) === 'undefined' || !haystack.length) return -1;
	
    var high = haystack.length - 1;
    var low = 0;
    while (low <= high) {
	mid = Math.floor((low + high) / 2);
	element = haystack[mid];
	if (element > needle) {
	    high = mid - 1;
	} else if (element < needle) {
	    low = mid + 1;
	} else {
	    return mid;
	}
    }
	
    return mid;
};

cumsum = function(vec) {
    var res = [];
    var acc = 0;
    for(var i = 0; i < vec.length; i++){
	acc += vec[i];
	res.push(acc);
    }
    return res;
}


sample = function(P) {
    var cdf = cumsum(P);
    var what = Math.random();
    return searchArray(what, cdf);
}
            
P = [];
pchanged = false;
oldeu = [];
oldep = [];
params = {
    real: 80,
    ap:   1,
    an:   1,
    bp:   3,
    bn:   1,
    minr: 20,
    maxr: 2000,
    eta:  1,
    eps:  1
};


presets = [
    {ap: 1, bp: 1, an: 1, bn: 1}, // neutral
    {ap: 1, bp: 1, an: 1, bn: 3},  // over
    {ap: 1, bp: 3, an: 1, bn: 1} // under

];

pnames = ["ap", "bp", "an", "bn", "eps", "minr", "maxr", "real"];

genU = function(c,params){
    return function(r) {
        return (r >= c ? -params.bp*Math.pow((r - c),params.ap) :
                -params.bn*Math.pow((c - r),params.an));
    };
};

genEM = function(rff, eta) {
    return function(r) {
        return Math.exp(eta*rff(r))
    };
};

genPM = function(c, params) {
    var util = genU(c, params);
    var em = genEM(util, params.eta);
    var N = sum(map(em, range(params.minr, params.maxr)));
    var pfun = function(r) { return em(r)/N };
    pfun.N = N;
    return pfun;
};

function mean(P, start) {
    var acc = 0;
    for(var i = 0; i < P.length; i++)
        acc += (i + start)*P[i];
    return acc;
};

function summary(P, start) {
    var acc = 0;
    for(var i = 0; i < P.length; i++){
        acc += (i + start)*P[i];
    }
    var mu = acc;
    acc = 0;
    var tmp = 0;
    for(var i = 0; i < P.length; i++){
        tmp = (i + start) - mu;
        acc += tmp*tmp*P[i];
    }
    var retval = {variance: acc, mean: mu};
    return retval;
};

function competa(params) {
    var delta = Math.max(params.ap * params.bp, params.an * params.bn);
    //debug("delta: " + delta)
    if (params.an > 1)
	delta = Math.max(delta, params.an * params.bn * Math.pow(params.real - params.minr, params.an - 1));
    if (params.ap > 1)
	delta = Math.max(delta, params.ap * params.bp * Math.pow(params.maxr - params.real, params.ap - 1));
    //debug("delta: " + delta)    
    return params.eps/(2*delta);
}

function pullparams() {
    var params = {};
    for(var i = 0; i < pnames.length; i++)
	params[pnames[i]] = Number(document.getElementById(pnames[i]).innerHTML);
//    showparms(params);
    return params;
};

function pushparams(params)  {
    for(var i = 0; i < pnames.length; i++)
	document.getElementById(pnames[i]).innerHTML = params[pnames[i]];
};

function clip(str) {
    var i = str.indexOf(' ');
    if (i == -1)
	return str;
    return str.substr(0,i);
}

function pushvals(params) {
    document.getElementById("eta").innerHTML = (Math.round(params.eta*1000)/1000
						+ (pchanged ?
						   (" (" + clip(document.getElementById("eta").innerHTML) + ")") : ""));
    document.getElementById("var").innerHTML = (Math.round(params.variance * 1000)/1000
						+ (pchanged ?
						   (" (" + clip(document.getElementById("var").innerHTML) + ")") : ""));
    document.getElementById("ave").innerHTML = (Math.round(params.ave * 1000)/1000 
						+ (pchanged ?
						   (" (" + clip(document.getElementById("ave").innerHTML) + ")") : ""));
    document.getElementById("spl").innerHTML = params.sample;
};

function showparms(params) {
    for(var i = 0; i < pnames.length; i++)
	debug(pnames[i] + ":" + params[pnames[i]]);
}

function update() {
    params = pullparams();
    init(params);
}

function init(params) {
    document.getElementById("debug").innerHTML = "";
    var realcount =  Math.min(Math.max(params.real, params.minr), params.maxr)
    params.eta = competa(params);
    var utility = genU(realcount, params);
    var probability = genPM(realcount, params);
    var cstart = Math.max(realcount - 10, params.minr);
    var cend = Math.min(realcount + 10, params.maxr);
    var xs = range(cstart, cend)
    var c = map(utility, xs);
    var d = map(probability, xs);
    var eu = zip(xs,c)
    var ep  = zip(xs,d)
    P = map(probability, range(params.minr, params.maxr));
    var stats = summary(P, params.minr);
    params.variance = stats.variance;
    params.ave = stats.mean;
    params.sample = (sample(P) + params.minr) + " " + (sample(P) + params.minr) + " " + (sample(P) + params.minr) + " " + (sample(P) + params.minr) + " " + (sample(P) + params.minr);
    pushvals(params);
    doplots(eu, ep, oldeu, oldep);
    oldep = ep;
    oldeu = eu;
    pchanged = false;
}

function doplots(eu, ep, oldeu, oldep) {
    var dataU = [ {label: "U(r)", data: eu, color: "red" }];
    var dataP = [ {label: "P(r)", data: ep, color: "blue" }];

    if ( pchanged ) {
	dataU.push({label: "previous U(r)", data: oldeu, color: "magenta" });
	dataP.push({label: "previous P(r)", data: oldep, color: "green" });
    }

    $.plot($("#uplot"), dataU,
	   { series: { points: { show: true }, lines: { show: true } },
	     grid: { backgroundColor: { colors: ["#fff", "#eee"] } }
	   });
    $.plot($("#pplot"), dataP,
	   { series: { points: { show: true }, lines: { show: true } },
	     grid: { backgroundColor: { colors: ["#fff", "#eee"] } }
	   });
};



function changeContent(tablecell)
{
    pchanged = true;
//    document.buttons.innerHTML += "<input type=\"checkbox\" name=\"check\" value=\"Plot previous\">";
    tablecell.innerHTML = "<INPUT type=text name=newname onBlur=\"javascript:submitNewName(this);\" value=\""+tablecell.innerHTML+"\">";
    tablecell.firstChild.focus();
};

function submitNewName(textfield)
{
    textfield.parentNode.innerHTML= textfield.value;
};

function setpreset(preset) {
    params = pullparams();
    params.ap = presets[preset].ap;
    params.bp = presets[preset].bp;
    params.an = presets[preset].an;
    params.bn = presets[preset].bn;
    pushparams(params);
    pchanged = true;
};
    