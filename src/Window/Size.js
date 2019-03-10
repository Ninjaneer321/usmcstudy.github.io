"use strict";

exports.windowSizeImpl = function windowSizeImpl () {
    return window.innerWidth;
};


exports.attachOnResizeImpl = function attachOnResizeImpl (f) {
    window.addEventListener('resize',f,true);
};
