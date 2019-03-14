"use strict";

exports.attachOnPopStateImpl = function attachOnPopStateImpl (f) {
    window.addEventListener(
        'popstate',
        function onPopStateListenerImpl (e) {
            f(e.state);
        },
        true
    );
};

