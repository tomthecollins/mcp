max_argmax = function(arr){
  // Tom Collins 21/10/2014.
  // In
  // arr Array mandatory
  // Out Array
  // Returns the maximum element in an array and its index (argument).

  var max = arr[0];
  var maxIndex = 0;
  for (var i = 1; i < arr.length; i++) {
    if (arr[i] > max) {
      maxIndex = i;
      max = arr[i];
    }
  }
  return [max, maxIndex];

  // CDC said the following is the same, but it does not retain the index of
  // the maximum element:
  // return arr.reduce(function(a, b){ return a > b?a:b; }, arr[0]);
}


min_argmin = function(arr){
  // Tom Collins 21/10/2014.
  // In
  // arr Array mandatory
  // Out Array
  // Returns the minimum element in an array and its index (argument).

  var min = arr[0];
  var minIndex = 0;
  for (var i = 1; i < arr.length; i++) {
    if (arr[i] < min) {
      minIndex = i;
      min = arr[i];
    }
  }
  return [min, minIndex];

  // CDC said the following is the same, but it does not retain the index of
  // the minimum element:
  // return arr.reduce(function(a, b){ return a < b?a:b; }, arr[0]);
}


corr = function(x, y){
  // Tom Collins 8/11/2015.
  // In
  // x Array mandatory
  // y Array mandatory
  // Out Number
  // This function calculates the Pearson product-moment correlation
  // coefficient between the input arrays x and y. It checks that the arrays
  // are of the same length, but does not check that they each consist of
  // numbers, nor for zero divisors (output NaN in both cases).

  var n = x.length;
  if (n !== y.length){
    throw "Error in call to corr: input arrays must be of the same length.";
  }
  else{
    var x_bar = mean(x);
    var y_bar = mean(y);
    var x2 = 0;
    var y2 = 0;
    var xy = 0;
    for (var i = 0; i < x.length; i++){
      x2 += Math.pow(x[i], 2);
      y2 += Math.pow(y[i], 2);
      xy += x[i]*y[i];
    }
    var r = (xy - n*x_bar*y_bar)/
      (Math.sqrt(x2 - n*Math.pow(x_bar, 2))*Math.sqrt(y2 - n*Math.pow(y_bar, 2)));
    return r;
  }
}


mean = function(arr){
  // Christian Coulon and Tom Collins 17/10/2014.
  // In
  // arr Array mandatory
  // Out Number
  // This function returns the mean of an input numeric array.

  if (!arr.length){
    return 0;
  }
  else{
    var sum = 0;
    for (var i = 0; i < arr.length; i++){
      sum += arr[i];
    }
    return sum/arr.length;
  }
}
