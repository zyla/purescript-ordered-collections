// arrayUniqSortBy :: forall a. (a -> a -> Int) -> Array a -> Array a
exports.arrayUniqSortBy = function(cmp) {
  return function(arr) {
    var indices = [];
    indices.length = arr.length;
    for(var i = 0; i < arr.length; i++) {
      indices[i] = i;
    }
    indices.sort((a, b) => {
      var c = cmp(arr[a])(arr[b]);
      return c !== 0 ? c : (a - b)
    });
    var result = [];
    for(var i = 0; i < arr.length;) {
      var x = arr[indices[i]];
      do {
        i++;
      } while(i < arr.length && cmp(arr[indices[i]])(x) === 0);
      result.push(arr[indices[i - 1]]);
    }
    return result;
  };
};
