
var scrollIntoViewIfNeeded = function(node, centerIfNeeded) {
  centerIfNeeded = arguments.length === 0 ? true : !!centerIfNeeded;

  var parent = node.parentNode,
      parentComputedStyle = window.getComputedStyle(parent, null),
      parentBorderTopWidth = parseInt(parentComputedStyle.getPropertyValue('border-top-width')),
      parentBorderLeftWidth = parseInt(parentComputedStyle.getPropertyValue('border-left-width')),
      overTop = node.offsetTop - parent.offsetTop < parent.scrollTop,
      overBottom = (node.offsetTop - parent.offsetTop + node.clientHeight - parentBorderTopWidth) > (parent.scrollTop + parent.clientHeight),
      overLeft = node.offsetLeft - parent.offsetLeft < parent.scrollLeft,
      overRight = (node.offsetLeft - parent.offsetLeft + node.clientWidth - parentBorderLeftWidth) > (parent.scrollLeft + parent.clientWidth),
      alignWithTop = overTop && !overBottom;

  if ((overTop || overBottom) && centerIfNeeded) {
    parent.scrollTop = node.offsetTop - parent.offsetTop - parent.clientHeight / 2 - parentBorderTopWidth + node.clientHeight / 2;
  }

  if ((overLeft || overRight) && centerIfNeeded) {
    parent.scrollLeft = node.offsetLeft - parent.offsetLeft - parent.clientWidth / 2 - parentBorderLeftWidth + node.clientWidth / 2;
  }

  if ((overTop || overBottom || overLeft || overRight) && !centerIfNeeded) {
    node.scrollIntoView(alignWithTop);
  }
};

var printErrors = function(f) {
  return function() {
    try {
      return f.apply(this, arguments);
    } catch (e) {
      console.error(e);
      throw e;
    }
  };
};


var handlePorts = function(app) {
  app.ports.scrollToVisible.subscribe(function(id) {
    setTimeout(printErrors(function() {
      var element = document.getElementById(id);
      if (element) {
        scrollIntoViewIfNeeded(element);
      }
    }));
  });

  app.ports.save.subscribe(printErrors(function(data) {
    var body = new Blob([data], {type: 'application/json'});
    var a = document.createElement('a');
    a.href = URL.createObjectURL(body);
    a.download = 'faculty.json';
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
  }));

  setTimeout(function() {
    app.ports.loadedData.send({people: {"abc": {name: "Carlo", gender: "male", role: "leader", free: []}}, groups: {}});
  }, 1000);

  app.ports.load.subscribe(printErrors(function() {
    var input = document.createElement('input');
    input.type = 'file';
    document.body.appendChild(input);
    input.addEventListener('change', function(event) {
      var files = event.target.files;
      if (files.length > 0) {
        var file = files[0],
            reader = new FileReader();
        reader.onload = printErrors(function() {
          app.ports.loadedData.send(JSON.parse(reader.result));
        });
        reader.readAsText(file);
      } else {
        console.error('Zero files selected.');
      }
    }, false);
    input.click();
    document.body.removeChild(input);
  }));
};
