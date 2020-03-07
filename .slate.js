S.cfga({
  "defaultToCurrentScreen": true,
  "windowHintsShowIcons": true,
  "windowHintsIgnoreHiddenWindows": false,
  "repeatOnHoldOps": false
});

var relaunch = S.op("relaunch")

// Focus commands
var focusLeft = S.op("focus", {"direction": "left"});
var focusRight = S.op("focus", {"direction": "right"});
var focusDown = S.op("focus", {"direction": "down"});
var focusUp = S.op("focus", {"direction": "up"});

var hint = S.op("hint", {"characters": "ASDFGHJKL"});

// Resize commands
var fullRectOp = S.op("move", {
  x: "screenOriginX",
  y: "screenOriginY",
  width: "screenSizeX",
  height: "screenSizeY",
});

var lHalfRect = function(fullRect) {
  return _.extend(fullRect, {width: fullRect.width / 2});
};
var lHalfRectOp = S.op("move", {
  x: "screenOriginX",
  y: "screenOriginY",
  width: "screenSizeX/2",
  height: "screenSizeY"
});

var rHalfRect = function(fullRect) {
  return _.extend(fullRect, {x: fullRect.x + fullRect.width / 2, width: fullRect.width / 2});
};
var rHalfRectOp = S.op("move", {
  x: "screenOriginX + screenSizeX/2",
  y: "screenOriginY",
  width: "screenSizeX/2",
  height: "screenSizeY"
});

var throwScreenLeft = function(curScreen) {
  var targetScreen;
  if (curScreen.id() === 0) {
    return S.op("throw", {"screen": "2", "width": "screenSizeX", "height": "screenSizeY"});
  } else if (curScreen.id() === 1) {
    return S.op("throw", {"screen": "0", "width": "screenSizeX", "height": "screenSizeY"});
  } else if (curScreen.id() == 2) {
    return S.op("throw", {"screen": "1", "width": "screenSizeX", "height": "screenSizeY"});
  }
};
var throwScreenRight = function(curScreen) {
  var targetScreen;
  if (curScreen.id() === 0) {
    return S.op("throw", {"screen": "1", "width": "screenSizeX", "height": "screenSizeY"});
  } else if (curScreen.id() === 1) {
    return S.op("throw", {"screen": "2", "width": "screenSizeX", "height": "screenSizeY"});
  } else if (curScreen.id() == 2) {
    return S.op("throw", {"screen": "0", "width": "screenSizeX", "height": "screenSizeY"});
  }
};

S.bnda({
  "j:ctrl,shift": relaunch,
  "[:alt": focusLeft,
  "]:alt": focusRight,
  "j:alt": focusDown,
  "k:alt": focusUp,

  "e:alt": hint,

  "f:alt,ctrl": function (win) {
    win.doOperation(fullRectOp);
  },// this is temp

  "[:alt,ctrl": function (win) { win.doOperation(throwScreenLeft(win.screen())); },
  "]:alt,ctrl": function (win) { win.doOperation(throwScreenRight(win.screen())); },
});

S.log("Finished config");
