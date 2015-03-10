

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

debug = function (thing) {
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
            
