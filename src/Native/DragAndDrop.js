//import Native.Json //
var _user$project$Native_DragAndDrop = (function() {
  
  var property = function(key, value) {
    return {key: key, value: value};
  };

  // to mark something as a drop target we need to make sure it
  // cancels the "dragover" event
  // https://developer.mozilla.org/en-US/docs/Web/Guide/HTML/Drag_operations#droptargets
  var dropTarget = property('ondragover', function(event) {
    event.preventDefault();
  });

  var andThen = _elm_lang$core$Native_Json.andThen,
      valueDecoder = _elm_lang$core$Native_Json.decodePrimitive('value'),
      succeedDecoder = _elm_lang$core$Native_Json.succeed,
      failDecoder = _elm_lang$core$Native_Json.fail;

  var setData = function(type, value) {
    return A2(andThen, valueDecoder, function(event) {
      // event.target.getBoundingClientRect()
      var rect = event.target.getBoundingClientRect(),
          xoffset = event.clientX - rect.left,
          yoffset = event.clientY - rect.top;
      event.dataTransfer.setDragImage(event.target, xoffset, yoffset);
      event.dataTransfer.setData(type, JSON.stringify(value));
      // we need to fail here so we don't try to send a message back
      // up (which is what would end up happening if we succeed here);
      // we're only here for the side effect!
      return failDecoder('expected fail; dataTransfer object has been initialised');
    });
  };

  var decodeData = function(type) {
    return A2(andThen, valueDecoder, function(event) {
      try {
        return succeedDecoder(JSON.parse(event.dataTransfer.getData(type)));
      } catch (e) {
        return failDecoder('Could not parse JSON');
      }
    });
  };

  return {
    setData: F2(setData),
    decodeData: decodeData,
    dropTarget: dropTarget
  };

})();
