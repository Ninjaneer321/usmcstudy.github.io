"use strict";

exports.getRandomValuesImpl = function getRandomValuesImpl (v) {
    window.crypto.getRandomValues(v);
};
