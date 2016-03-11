var utils = require('./utils');

// Adding properties to this object will make them available to outside scripts.
module.exports = {

};

// TODO: Tune these.
// The initial (and minimum) capacity of a non-empty list or map object.
var MIN_CAPACITY = 16;

// The rate at which a collection's capacity grows when the size exceeds the
// current capacity. The new capacity will be determined by *multiplying* the
// old capacity by this. Growing geometrically is necessary to ensure that
// adding to a collection has O(1) amortized complexity.
var GROW_FACTOR = 2;

// The maximum percentage of map entries that can be filled before the map is
// grown. A lower load takes more memory but reduces collisions which makes
// lookup faster.
var MAP_LOAD_PERCENT = 75;

// The number of call frames initially allocated when a fiber is created. Making
// this smaller makes fibers use less memory (at first) but spends more time
// reallocating when the call stack grows.
var INITIAL_CALL_FRAMES = 4;
