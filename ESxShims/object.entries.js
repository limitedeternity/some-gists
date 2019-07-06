(function () {
  Object.entries = function (obj) {
    var ownProps = Object.getOwnPropertyNames(obj),
      i = ownProps.length,
      resArray = new Array(i);

    while (i--) resArray[i] = [ownProps[i], obj[ownProps[i]]];
    return resArray;
  };
})();
