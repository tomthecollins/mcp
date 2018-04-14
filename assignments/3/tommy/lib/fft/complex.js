//-------------------------------------------------
// Add two complex numbers
//-------------------------------------------------
var complex = {};
complex.add = function (a, b)
{
    return [a[0] + b[0], a[1] + b[1]];
};

//-------------------------------------------------
// Subtract two complex numbers
//-------------------------------------------------
complex.subtract = function (a, b)
{
    return [a[0] - b[0], a[1] - b[1]];
};

//-------------------------------------------------
// Multiply two complex numbers
//
// (a + bi) * (c + di) = (ac - bd) + (ad + bc)i
//-------------------------------------------------
complex.multiply = function (a, b) 
{
    return [(a[0] * b[0] - a[1] * b[1]), 
            (a[0] * b[1] + a[1] * b[0])];
};

//-------------------------------------------------
// Calculate |a + bi|
//
// sqrt(a*a + b*b)
//-------------------------------------------------
complex.magnitude = function (c) 
{
    return Math.sqrt(c[0]*c[0] + c[1]*c[1]); 
};

//-------------------------------------------------
// Exports
//-------------------------------------------------
//module.exports = {
//    add: complexAdd,
//    subtract: complexSubtract,
//    multiply: complexMultiply,
//    magnitude: complexMagnitude
//};